-- |
-- Module      : ChefBot.Plugin
-- Description : Helpful imports for building plugins.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Imports for when you develop your own plugins. This deliberately hides some
-- functionality as to avoid plugin creation from breaking if the underlying types
-- are ever updated. You should always import this over "ChefBot.Plugin.Types".
module ChefBot.Utility
  ( module Types,
    module Utils,
  )
where

import ChefBot.Utility.Types as Types hiding (Pl)
import ChefBot.Utility.Utils as Utils
