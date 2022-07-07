-- -- |
-- Module      : ChefBot.Utility.Help
-- Description : Help text generation and storage
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- This module creates functions and data structures to help generate help text for commands
module ChefBot.Utility.Help where

import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import Discord.Types
import ChefBot.Internal.Permission (getSenderPermission, userHasPermission)
import ChefBot.Internal.Plugins (changeAction)
import ChefBot.Internal.Types
import ChefBot.Utility.Discord (sendEmbedMessage)
import ChefBot.Utility.Embed (addColour)
import ChefBot.Utility.Parser (skipSpace)
import ChefBot.Utility.Permission (requirePermission)
import ChefBot.Utility.Types hiding (helpPages)
import Text.Megaparsec (choice, chunk, eof, try, (<?>), (<|>))
import Text.RawString.QQ (r)

rootBody :: Text
rootBody =
  [r|A Discord recipe bot.

Use the `recipe` command for recipes!|]

helpHelpPage :: HelpPage
helpHelpPage =
  HelpPage
    "help"
    []
    "shows information about a specific command"
    [r|Shows information about bot commands.

**Usage**
`help` shows the general information about this bot
`help <command>` shows the documentation for the given command|]
    []
    None

generateHelp :: CombinedPlugin -> CombinedPlugin
generateHelp p =
  p
    { combinedSetupAction = return (PA [CCommand "help" (handleHelp (helpHelpPage : combinedHelpPages p)) []] [] [] [] [] [] []) : combinedSetupAction p
    }

handleHelp :: [HelpPage] -> Parser (Message -> CompiledDatabaseDiscord ())
handleHelp hp = parseHelpPage root
  where
    root = HelpPage "" [] "" rootBody hp None

parseHelpPage :: HelpPage -> Parser (Message -> CompiledDatabaseDiscord ())
parseHelpPage hp = do
  _ <- choice (map chunk (helpName hp : helpAliases hp))
  skipSpace
  (try eof $> displayHelp hp) <|> choice (map parseHelpPage $ helpSubpages hp) <?> "Unknown Subcommand"

displayHelp :: HelpPage -> Message -> CompiledDatabaseDiscord ()
displayHelp hp m = changeAction () . requirePermission (helpPermission hp) m $ do
  uPerm <- getSenderPermission m
  sendEmbedMessage m "" $ addColour Aqua $ createEmbed $ CreateEmbed "" "" Nothing (formatHelpTitle hp) "" Nothing (formatHelp uPerm hp) [] Nothing "" Nothing Nothing

formatHelpTitle :: HelpPage -> Text
formatHelpTitle hp = ":scroll:  " <> if helpName hp == "" then "ChefBot" else "Help: `$" <> helpName hp <> "`"

formatHelp :: UserPermission -> HelpPage -> Text
formatHelp up hp = helpBody hp <> formatSubpages hp
  where
    formatSubpages :: HelpPage -> Text
    formatSubpages (HelpPage _ _ _ _ [] _) = ""
    formatSubpages hp' = if T.null sp then "" else "\n\n**Subcommands**" <> sp
      where
        sp = T.concat (map formatSubpage (helpSubpages hp'))
    formatSubpage :: HelpPage -> Text
    formatSubpage hp' = if userHasPermission (helpPermission hp') up then "\n`" <> helpName hp' <> "` " <> helpShortText hp' else ""
