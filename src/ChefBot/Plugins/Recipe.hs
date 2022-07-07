-- |
-- Module      : ChefBot.Plugins.Recipe
-- Description : A command that loads recipes.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- A command that loads recipes.
module ChefBot.Plugins.Recipe (recipePlugin) where

import ChefBot.Utility
import ChefBot.Utility.Discord (sendEmbedMessage)
import ChefBot.Utility.Embed (addFooter, basicEmbed)
import ChefBot.Utility.Search (closestValue)
import ChefBot.Utility.SmartParser (PComm (parseComm), RestOfInput (ROI))
import Control.Monad.Reader (ask, liftIO)
import Data.Aeson (FromJSON)
import Data.Text (Text, intercalate, unpack)
import Data.Yaml (decodeFileEither)
import Data.Yaml.Internal (ParseException)
import Discord.Internal.Types
import GHC.Generics (Generic)
import Text.RawString.QQ

-- | @recipe@.
recipe :: EnvCommand CookBook
recipe = Command "recipe" (parseComm recipeComm) []
  where
    recipeComm :: RestOfInput Text -> Message -> EnvDatabaseDiscord CookBook ()
    recipeComm (ROI q) m = do
      book <- ask
      let res = closestValue (map (\r -> (unpack $ title r, r)) $ recipes book) (unpack q)
      case q of
        "" -> sendEmbedMessage m "" $ basicEmbed (":scroll: **All Recipes**") ("Try `recipe <recipe>` to view one of these:\n" <> allRecipes book)
        _ -> sendEmbedMessage m "" $ embedRecipe res
    allRecipes :: CookBook -> Text
    allRecipes book = intercalate "\n" $ map (\r -> "• " <> title r) $ recipes book
    embedRecipe :: Recipe -> Embed
    embedRecipe res =
      let header = ":page_with_curl: **" <> title res <> "**"
          ings = case ingredients res of
            [] -> ""
            _ -> "**Ingredients**\n" <> (intercalate "\n" $ map (\i -> "• " <> i) $ ingredients res) <> "\n\n"
          meth = "**Method**\n" <> method res
          body = ings <> meth
          footer = "Recipe provided by " <> citation res
       in addFooter footer $ basicEmbed header body

recipeHelp :: HelpPage
recipeHelp =
  HelpPage
    "recipe"
    []
    "displays a recipe"
    [r|Displays a given recipe, or lists all the available recipes if none provided.

**Usage**
`recipe` displays a list of all recipes.
`recipe <recipe>` displays the given recipe.|]
    []
    None

-- | @Recipe@ represents a single recipe.
data Recipe = Recipe
  { title :: Text,
    ingredients :: [Text],
    method :: Text,
    citation :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Recipe

-- | @CookBook@ is the type representing all recipes.
data CookBook = CookBook
  { recipes :: [Recipe]
  }
  deriving (Eq, Show, Generic)

instance FromJSON CookBook

defaultCookBook :: CookBook
defaultCookBook = CookBook {recipes = []}

-- | @getCookBook@ loads the recipes from file.
getCookBook :: IO CookBook
getCookBook = do
  cookbook <- decodeFileEither yamlFile :: IO (Either ParseException CookBook)
  return $ case cookbook of
    Left _ -> defaultCookBook
    Right out -> out
  where
    yamlFile :: FilePath
    yamlFile = "resources/cookbook.yaml"

-- | @recipeStartup@ loads the recipes once at start up.
recipeStartup :: StartUp CookBook
recipeStartup = StartUp $ liftIO getCookBook

-- | @flipPlugin@ assembles the command into a plugin.
recipePlugin :: EnvPlugin CookBook
recipePlugin = (envPlug "recipe" recipeStartup) {commands = [recipe], helpPages = [recipeHelp]}
