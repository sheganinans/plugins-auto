{-# LANGUAGE EmptyDataDecls, TemplateHaskell #-}
-- | This module implements recompilation and reloading of symbols.
module System.Plugins.Auto.Reloader
    ( funcTH
    , func
    , PluginHandle
    , PluginConf(..)
    , initPlugins
    , initPluginsWithConf
    , defaultPluginConf
    , updatePluginConf
    ) where

import Control.Applicative        ((<$>))
import Control.Concurrent.MVar    (MVar,readMVar,modifyMVar_,newMVar,modifyMVar)
import Control.Concurrent         (threadDelay,killThread,ThreadId,forkIO)
import Control.Exception          (bracketOnError,bracket)
import Control.Monad(when)
import Data.IORef                 (atomicModifyIORef,IORef,newIORef,readIORef)
import Data.List                  (nub)
import Data.Maybe                 (mapMaybe)
import qualified Data.Map         as Map
import           Data.Map         (Map)
import Language.Haskell.TH.Syntax (Name(Name),NameFlavour(NameG), occString, modString)
import System.FilePath            (addExtension, dropExtension)
import System.Plugins.Load        (Module, Symbol, LoadStatus(..), getImports, load, unloadAll)
import System.Plugins.Make        (Errors, MakeStatus(..), MakeCode(..), makeAll)
import Unsafe.Coerce              (unsafeCoerce)

import System.Plugins.Auto.FileSystemWatcher

-- A very unsafe version of Data.Dynamic

data Sym

toSym :: a -> Sym
toSym = unsafeCoerce

fromSym :: Sym -> a
fromSym = unsafeCoerce

-- | Configuration options for recompiling plugins. So far we store here event callbacks and ghc arguments.
data PluginConf = PluginConf 
       { pcGHCArgs :: [String]                            -- ^Arguments to ghc
       , pcWhenCompiling :: FilePath -> IO ()             -- ^ Called when compilation is about to start. Takes the path of the file to compile.
       , pcWhenCompiled :: FilePath -> [String] -> IO ()  -- ^ Called after compilation finished. 
                                                          --   Takes the path of the compiled file and the compilation errors if any.
       , pcWhenReloaded :: FilePath -> String -> [String] -> IO ()  -- ^ Called after reloading of the symbol finished. 
                                                                    -- Takes the path of the object file, the symbol name and the list 
                                                                    -- of errors if any.
       , pcWhenWatched :: FilePath -> IO ()               -- ^ Called when a file is registered for watching.
       , pcWhenChanged :: FilePath -> IO ()               -- ^ Called when a watched file is modified.
       }

-- | Contains no arguments for GHC, and noop callbacks.
defaultPluginConf :: PluginConf
defaultPluginConf = PluginConf 
  { pcGHCArgs       = []
  , pcWhenCompiling = const$ return ()
  , pcWhenCompiled  = const$ const$ return ()
  , pcWhenReloaded  = \f s errs -> if null errs then return ()
                                     else do putStrLn ("Error when reloading "++f++": "++s)
                                             putStrLn (unlines errs)
  , pcWhenWatched   = const$ return ()
  , pcWhenChanged   = const$ return ()
  }


-- | Represents the state of the recompilation for a source file.
data RecompilationState = RecompilationState 
      { rsRecompiling :: MVar Bool                -- ^ Is recompilation in progress?
      , rsRecompilationNeeded :: MVar Bool        -- ^ Is recompilation needed?
      , rsTimer :: Timer                          -- ^ The timer is used to delay recompilation a fraction of a second
                                                  -- so we can recompile in one pass changes in multiple source code
                                                  -- files. 
      }

-- | A handle holding the reloader state.
data PluginHandle = PluginHandle 
  { phWatcher :: FSWatcher                         -- Inotify handle
  , phConf :: IORef PluginConf
  , phObjMap :: MVar
     ( Map FilePath                                -- source file being observed
         ( [FSWatchDescriptor]                       -- watch descriptor of the source file and its dependecies
         , [FilePath]                                -- depedencies of the source file
         , Maybe Errors                              -- errors when compiling the file if any
         , Map Symbol                                -- symbol defined in the source file
             ( FilePath 
                 -> IO (Either Errors (Module, Sym))   -- function for reloading the symbol
             , Errors                                  -- error from last recompilation attempt if any
             , Maybe (Module, Sym)                     -- last succesfully loaded symbol
             )
         , RecompilationState
         )
     )
  }

-- | Initializes the plugin system and return a 'PluginHandle' 
-- using the default plugin configuration 'defaultPluginConf'.
initPlugins :: IO PluginHandle
initPlugins = initPluginsWithConf defaultPluginConf

-- | Initializes the plugin system and return a 'PluginHandle'.
initPluginsWithConf :: PluginConf -> IO PluginHandle
initPluginsWithConf conf =
    do inotify <- initFSWatcher
       objMap <- newMVar Map.empty
       rconf <- newIORef conf
       return$ PluginHandle inotify rconf objMap


-- | Updates the configuration used by the plugin handle.
--
-- Yields the plugin configuration resulting from modifying the 
-- plugin handle configuration with the given function.
updatePluginConf :: PluginHandle -> (PluginConf -> PluginConf) -> IO PluginConf
updatePluginConf ph f = atomicModifyIORef (phConf ph)$ \c -> let fc = f c in (fc,fc)



newRecompilationState :: IO RecompilationState
newRecompilationState =  do
    recompiling <- newMVar False
    recompilationNeeded <- newMVar False
    timer <- newTimer 
    return RecompilationState 
             { rsRecompiling = recompiling
             , rsRecompilationNeeded = recompilationNeeded
             , rsTimer = timer
             }


-- | Loads a symbol. It may not return the symbol if it was never 
-- loaded before and there are compiler errors, or if another 
-- thread is already loading the symbol. This call should probably
-- be changed to be blocking.
funcTH :: PluginHandle  -- ^ Plugin handle
       -> Name          -- ^ Name of the symbol to load
       -> IO ([String],Maybe a) -- ^ A list of errors if any, and the last succesfully loaded version of the symbol.
funcTH objMap name = 
    do let (fp, sym) = nameToFileSym name
       func objMap fp sym


nameToFileSym :: Name -> (FilePath, Symbol)
nameToFileSym (Name occName (NameG _ _ mn)) =
    let dotToSlash '.' = '/'
        dotToSlash c   = c
        fp  = (map dotToSlash (modString mn)) ++ ".hs"
        sym = occString occName
    in (fp, sym)
nameToFileSym n = error $ "nameToFileSym failed because Name was not the right kind. " ++ show n


-- | Like 'funcTH' but instead of a Name it takes the source file and 
-- the contained symbol to load.
func :: PluginHandle -> FilePath -> Symbol -> IO ([String],Maybe a)
func ph fp sym =
    do om <- readMVar$ phObjMap ph
       case Map.lookup fp om of
         Nothing -> 
             do bracketOnError
                  (addSymbol ph fp sym)
                  (const$ deleteSymbol ph fp sym)
                  (const$ rebuild ph fp True)
                func ph fp sym
         (Just (_, _, merrs, symbols, _)) ->
             case Map.lookup sym symbols of
               Nothing ->
                 case merrs of
                   Nothing -> 
                     do bracketOnError
                          (addSymbol ph fp sym)
                          (const$ deleteSymbol ph fp sym)
                          (const$ rebuild ph fp True)
                        func ph fp sym
                   Just errs -> 
                     return (errs,Nothing)
               Just (_, [], mm) -> 
                    return (maybe [] id merrs, fmap (fromSym . snd) mm)
               Just (_, errs, mm) -> 
                    return (errs,fmap (fromSym . snd) mm)

replaceSuffix :: FilePath -> String -> FilePath
replaceSuffix p sfx = case [ i | (i,'.') <- zip [0..] p ] of
                        [] -> p++sfx
                        ixs -> take (last ixs) p ++ '.':sfx

getPluginConf :: PluginHandle -> IO PluginConf
getPluginConf = readIORef . phConf

rebuild :: PluginHandle   -- ^ list of currently loaded modules/symbols
        -> FilePath -- ^ source file to compile
        -> Bool
        -> IO ()
rebuild p fp forceReload =
    do rs <- readMVar (phObjMap p) >>= maybe newRecompilationState (\(_,_,_,_,rs)->return rs) . Map.lookup fp
       bracket
         (signalRecompilationStarted rs)
         (\compile -> when compile$ signalRecompilationFinished rs (rebuild' rs))
         (\compile -> when compile$ rebuild' rs)
  where
    rebuild' :: RecompilationState -> IO ()
    rebuild' rs = do
       conf <- getPluginConf p
       pcWhenCompiling conf fp
       makeStatus <- makeAll fp (["-odir",".","-hidir",".","-o",replaceSuffix fp "o"]++pcGHCArgs conf)
       case makeStatus of
         (MakeFailure errs) ->
             do modifyMVar_ (phObjMap p) $ \om ->
                           case Map.lookup fp om of
                             Nothing -> do wds <- observeFiles p fp [] conf rs
                                           return$ Map.insert fp (wds, [], Just errs, Map.empty, rs) om
                             Just (wds, deps, _, symbols,_) ->
                                 let symbols' = Map.map (\(loader,_,mm) -> (loader,errs,mm)) symbols -- propogate error to all symbols
                                 in return$ Map.insert fp (wds, deps, Just errs, symbols',rs) om
                pcWhenCompiled conf fp errs
         (MakeSuccess NotReq _objFilePath) | not forceReload -> pcWhenCompiled conf fp []
         (MakeSuccess _makeCode objFilePath) -> 
             do om <- readMVar$ phObjMap p
                case Map.lookup fp om of
                  Nothing -> return ()
                  Just (oldWds, _, _, symbols,_) ->
                      do pcWhenCompiled conf fp []
                         mapM_ unloadAll (unloadList symbols)
                         mapM_ removeWatch oldWds
                         res <- mapM (load' conf objFilePath) (Map.assocs symbols)
                         imports <- map (\bn -> addExtension (mnameToPath bn) ".hs") <$> getImports (dropExtension objFilePath)
                         wds <- observeFiles p fp imports conf rs
                         modifyMVar_ (phObjMap p) $ return . Map.insert fp (wds, [], Nothing, Map.fromList res,rs)
   
    unloadList symbols =
          nub$ mapMaybe (\(_, _, mm) -> fmap fst mm)$ Map.elems symbols

    load' :: PluginConf
            -> FilePath 
            -> (Symbol, (FilePath -> IO (Either Errors (Module, Sym)), Errors, Maybe (Module, Sym)))
            -> IO (Symbol, (FilePath -> IO (Either Errors (Module, Sym)), Errors, Maybe (Module, Sym)))
    load' conf obj (symbol, (reloader, _, _mm)) =
          do r <- reloader obj
             pcWhenReloaded conf obj symbol$ either id (const []) r 
             return (symbol, (reloader, either id (const []) r,either (const Nothing) Just r))

mnameToPath :: FilePath -> FilePath
mnameToPath = replace '.' '/' 
 where replace x y = foldr (\a r -> if x==a then y:r else a:r) []

observeFiles :: PluginHandle -> FilePath -> [FilePath] -> PluginConf -> RecompilationState -> IO [FSWatchDescriptor]
observeFiles p fp imports conf rs = 
        mapM (\depFp -> do let handler = pcWhenChanged conf depFp >> signalRecompilationNeeded rs (rebuild p fp False)
                           wd<-addWatch (phWatcher p) depFp handler
                           pcWhenWatched conf depFp >> return wd
             ) (fp:imports)
                                   

addSymbol :: PluginHandle -> FilePath -> Symbol -> IO ()
addSymbol p sourceFP sym =
    do conf <- getPluginConf p
       let reloader obj = 
               do ldStatus <- load obj ["."] [] sym
                  case ldStatus of
                    (LoadSuccess m s) -> return (Right (m, toSym s))
                    (LoadFailure errs) -> return (Left errs)
           symVal       = (reloader, [], Nothing)
       modifyMVar_ (phObjMap p) $ \om ->
           case Map.lookup sourceFP om of
             Nothing -> do rs <- newRecompilationState
                           wds <- observeFiles p sourceFP [] conf rs
                           return$ Map.insert sourceFP (wds, [], Nothing, Map.singleton sym symVal,rs) om
             (Just (wds, deps, errs, symbols,rs)) ->
                 let symbols' = Map.insert sym symVal symbols
                 in return$ Map.insert sourceFP (wds, deps, errs, symbols',rs) om

deleteSymbol :: PluginHandle -> FilePath -> Symbol -> IO ()
deleteSymbol ph sourceFP sym =
       modifyMVar_ (phObjMap ph) $ \om ->
           case Map.lookup sourceFP om of
             Nothing -> return om
             Just (wds, deps, errs, symbols,rs) ->
                 let symbols' = Map.delete sym symbols
                  in return$ Map.insert sourceFP (wds, deps, errs, symbols',rs) om

--------------------------------
-- Recompilation handling
--------------------------------

-- | Indicates that recompilation is needed. If compilation is in progress
-- recompilation will be done afterwards, otherwise recompilation starts
-- in a few milliseconds after the call.
signalRecompilationNeeded :: RecompilationState -- ^ Internal state for synchronizing recompilation requests
                          -> IO ()              -- ^ Command which starts recompilation
                          -> IO ()
signalRecompilationNeeded rs recomp = do
    modifyMVar_ (rsRecompilationNeeded rs)$ const$ return True
    mvarIf (rsRecompiling rs) (return ()) (testRecompilation rs recomp)


-- | Indicates that recompilation is starting. The returned boolean indicates whether
-- there is any other attempt for recompilation in progress.
signalRecompilationStarted :: RecompilationState -> IO Bool
signalRecompilationStarted rs = do
     modifyMVar (rsRecompiling rs)$ \b -> do
       when (not b)$ modifyMVar_ (rsRecompilationNeeded rs)$ const$ return False
       return (True,not b)


-- | Indicates that recompilation has stopped.
-- It test whether recompilation is needed, and if needed it starts it again.
signalRecompilationFinished :: RecompilationState   -- ^ Internal state for synchronizing recompilation requests
                            -> IO ()                -- ^ Command which starts recompilation
                            -> IO ()
signalRecompilationFinished rs recomp = do 
     modifyMVar_ (rsRecompiling rs)$ const$ return False
     testRecompilation rs recomp

-- | Fires the recompilation command after a small timeout if recompilation is needed
-- after the timeout expired. The timeout allows to delay recompilation if new requests
-- for recompilation arrive before the recompilation starts.
testRecompilation :: RecompilationState -> IO () -> IO ()
testRecompilation rs recomp = setTimer (rsTimer rs) (400*1000)$
                  mvarIf (rsRecompilationNeeded rs) recomp (return ())

mvarIf :: MVar Bool -> IO a -> IO a -> IO a
mvarIf mb th els = readMVar mb >>= \b -> if b then th else els

-------------------------------- 
-- Timer data type
--------------------------------

newtype Timer = Timer (MVar (Maybe ThreadId))

newTimer :: IO Timer
newTimer = fmap Timer$ newMVar Nothing

-- Register an action to perform after a timeout.
-- Cancels previous timer setting.
setTimer :: Timer -> Int -> IO () -> IO ()
setTimer (Timer mmtid) delay action = modifyMVar_ mmtid$ \mtid -> do
          maybe (return ()) killThread mtid
          fmap Just$ forkIO$ threadDelay delay >> modifyMVar_ mmtid (const (return Nothing)) >> action

