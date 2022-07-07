module Main where

import Control.Concurrent.MVar (MVar, newMVar, swapMVar)
import Control.Monad (forever, unless)
import Control.Monad.Extra
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import LoadEnv (loadEnv)
import Paths_chefbot (version)
import ChefBot (runChefBot)
import ChefBot.Internal.Administration
import ChefBot.Plugins (plugins)
import ChefBot.Utility.Types
import System.Environment (getEnv, lookupEnv)
import System.Exit (die)
import Text.Regex.PCRE

-- @main@ runs forever. This allows bot reloading by fully shutting down the bot and letting it restart.
main :: IO ()
main = do
  -- fetch the version info as soon after building to reduce the likelihood that it changes between build and run
  gv <- gitVersion
  let vInfo = VInfo gv version
  rFlag <- newMVar Reload :: IO (MVar ShutdownReason)
  whileM $ do
    _ <- swapMVar rFlag Reload
    loadEnv
    dToken <- pack <$> getEnv "DISCORD_TOKEN"
    unless (encodeUtf8 dToken =~ ("^[A-Za-z0-9_-]{24}[.][A-Za-z0-9_-]{6}[.][A-Za-z0-9_-]{27}$" :: String)) $
      die "Invalid token format. Please check it is a bot token"
    prefix <- pack . fromMaybe "!" <$> lookupEnv "PREFIX"
    dbpath <- getEnv "SQLITE_FILENAME"
    runChefBot vInfo dToken prefix dbpath (plugins rFlag)
    exit <- swapMVar rFlag Reload
    restartAction exit
    pure $ not (restartIsTerminal exit)
  putStrLn "ChefBot closed"
