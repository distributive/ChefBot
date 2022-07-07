-- |
-- Module      : ChefBot.Plugins
-- Description : Available plugins for ChefBot.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Here is a collection of existing plugins for ChefBot. If you add new plugins
-- to the Plugins directory, include an import here. This means that users only
-- need to import @ChefBot.Plugins@ to import individual plugins.
module ChefBot.Plugins
  ( plugins,
  )
where

import Control.Concurrent.MVar (MVar)
import ChefBot.Internal.Administration (ShutdownReason)
import ChefBot.Internal.Plugins (compilePlugin)
import ChefBot.Internal.Types (CompiledPlugin)
import ChefBot.Plugins.Administration (administrationPlugin)
import ChefBot.Plugins.Basic (basicPlugin)
import ChefBot.Plugins.Recipe (recipePlugin)

-- Use long list format to make additions and removals non-conflicting on git PRs
plugins :: MVar ShutdownReason -> [CompiledPlugin]
plugins rFlag =
  addAdministrationPlugin
    rFlag
    [ compilePlugin basicPlugin,
      compilePlugin recipePlugin
    ]

-- | @addAdministrationPlugin@ is needed to allow the administration plugin to be aware of the list of current plugins
addAdministrationPlugin :: MVar ShutdownReason -> [CompiledPlugin] -> [CompiledPlugin]
addAdministrationPlugin rFlag cps = compilePlugin (administrationPlugin rFlag cps) : cps
