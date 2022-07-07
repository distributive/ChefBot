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

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (pack)
import ChefBot.Utility
import ChefBot.Utility.Discord (Message, formatFromEmojiName, sendEmbedMessage)
import ChefBot.Utility.Embed (basicEmbed)
import ChefBot.Utility.Parser
import ChefBot.Utility.Random (chooseOne)
import ChefBot.Utility.SmartParser (PComm (parseComm))
import Text.Megaparsec
import Text.RawString.QQ

-- | @recipe@.
recipe :: Command
recipe = Command "recipe" recipecomm []
  where
    recipecomm :: Parser (Message -> DatabaseDiscord ())
    recipecomm = do
      args <- (try quoted <|> nonSpaceWord) `sepBy` some space
      return $ \m -> do
        c <- case length args of
          0 -> liftIO $ chooseOne [":arrow_up: Heads", ":arrow_down: Tails"]
          _ -> liftIO $ chooseOne $ map (\a -> "> " <> pack a) args
        sendEmbedMessage m "" $ basicEmbed ":game_die: Result :game_die:" c

recipeHelp :: HelpPage
recipeHelp =
  HelpPage
    "recipe"
    []
    "displays a recipe"
    [r|Randomly picks one element from its arguments or, if none are provided, either 'Heads' or 'Tails'.

**Usage**
`flip` outputs 'Heads' or 'Tails' with uniform probability
`flip a b c` outputs 'a', 'b', or 'c' with uniform probability|]
    []
    None

-- | @flipPlugin@ assembles the command into a plugin.
recipePlugin :: Plugin
recipePlugin = (plug "recipe") {commands = [recipe], helpPages = [recipeHelp]}
