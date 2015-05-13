{-# LANGUAGE TemplateHaskell #-}
-- | This module implements the public interface of plugins-auto
-- Create a plugin handle with 'initPlugins' and use it later with 'withMonadIO'.
module System.Plugins.Auto
    ( PluginHandle
    , PluginConf(..)
    , initPlugins
    , initPluginsWithConf
    , defaultPluginConf
    , updatePluginConf
    , withMonadIO
    , withMonadIO_
    , withMonadIOFile
    ) where

import Control.Monad.Trans          (MonadIO(liftIO))
import Language.Haskell.TH          (ExpQ, appE, varE)
import Language.Haskell.TH.Syntax   (Name)
import System.Plugins.Auto.LiftName (liftName)
import System.Plugins.Auto.Reloader ( PluginHandle, func, funcTH, initPlugins, initPluginsWithConf
                                    , defaultPluginConf, updatePluginConf, PluginConf(..))


-- |  Dynamically load the specified symbol pass it as an argument to
-- the supplied server monad function.
--
-- This is a wrapper around 'withMonadIO_' which ensures the first
-- and second argument stay in-sync.
-- 
-- Usage:
--
-- > $(withMonadIO 'symbol) pluginHandle id (error "symbol could not be loaded") $ \previous_errors a -> ...
--
withMonadIO :: Name -> ExpQ
withMonadIO name = appE (appE [| withMonadIO_ |] (liftName name)) (varE name)

-- | Dynamically load the specified symbol.
--
withMonadIO_ :: (MonadIO m) => 
                   Name         -- ^ name of the symbol to dynamically load
                -> a            -- ^ the symbol (must be the function refered to by the 'Name' argument)
                -> PluginHandle -- ^ Handle to the function reloader
                -> ([String] -> m b)        -- ^ function called if the symbol is not loaded ( either because the
                                            --   last recompilation attempt failed or because it is being 
                                            --   compiled right now by another thread).
                -> ([String] -> a -> m b)   -- ^ function which uses the loaded result, receives also a 
                                            -- list of errors in the last recompilation attempt
                -> m b
withMonadIO_ name _ ph notloaded use = do
       (errs,ma) <- liftIO $ funcTH ph name
       maybe (notloaded errs) (use errs) ma


-- | Dynamically load the specified symbol from a specific .hs file
--
withMonadIOFile :: (MonadIO m) => 
                   FilePath     -- ^ path to file to load symbol from
                -> String       -- ^ name of the symbol to dynamically load
                -> PluginHandle -- ^ Handle to the function reloader
                -> ([String] -> m b)        -- ^ function called if the symbol is not loaded ( either because the
                                            --   last recompilation attempt failed or because it is being 
                                            --   compiled right now by another thread).
                -> ([String] -> a -> m b)   -- ^ function which uses the loaded result, receives also a 
                                            -- list of errors in the last recompilation attempt
                -> m b
withMonadIOFile fp sym ph notloaded use = do
       (errs,ma) <- liftIO$ func ph fp sym
       maybe (notloaded errs) (use errs) ma




