{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, DeriveGeneric #-}

{- Provides a logging interface using unsafe routines -}

module EtanolTools.Unsafe 
    ( Configuration(..)
    , VerbosityLevel(..)
    , BackendType(..)
    , globalConfigFile
    , getVerbosity
    , getBackend
    , debugLogger
    , infoLogger
    , seriousLogger
    , debugLoggerM
    , infoLoggerM
    , seriousLoggerM
    , assertCheck
    ) where

import System.Exit (die)
import Control.Monad (when)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as M
import Data.Map ((!?))
import Data.Maybe (isNothing, isJust, fromJust)
import qualified Data.Yaml as Y
import qualified GHC.Generics as G
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import System.Environment (lookupEnv)
import System.FilePath.Posix((</>))
import System.Directory 
    ( doesFileExist
    , doesDirectoryExist
    , createDirectory
    )

confFile :: FilePath
confFile = "config"

confDir :: FilePath
confDir = ".etanol"

data Configuration = Configuration {
    verbosity :: VerbosityLevel,
    backend   :: BackendType
} deriving (Show, Read, Eq, Ord, G.Generic)

instance Y.FromJSON Configuration
instance Y.ToJSON Configuration

globalConfigFile :: IO String
globalConfigFile = do
    hDir <- lookupEnv "HOME"
    when (isNothing hDir) $
        die $ "Please set the HOME environment variable."
    let home = fromJust hDir
    exs <- doesDirectoryExist (home </> confDir)
    when (not exs) $ do
        createDirectory (home </> confDir)

    return (home </> confDir </> confFile)

data VerbosityLevel 
    = DebugLevel
    | InfoLevel
    | SeriousLevel
    | QuietLevel      
    deriving (Eq, Ord, Show, Read, G.Generic)

instance Y.FromJSON VerbosityLevel
instance Y.ToJSON VerbosityLevel

data BackendType
    = DirectoryBackend
    | JARBackend
    deriving (Eq, Ord, Show, Read, G.Generic)

instance Y.FromJSON BackendType
instance Y.ToJSON BackendType

defaultGlobalConfigFile = pack $
    "####### Verbosity Options. Uncomment any one #######\n" ++
    "# verbosity : DebugLevel\n" ++   
    "verbosity : InfoLevel\n" ++      -- info level is the default
    "# verbosity : SeriousLevel\n"++
    "# verbosity : QuietLevel\n\n"++
    "####### Backend Options. Uncomment any one #######\n" ++
    "backend : DirectoryBackend\n" ++ -- default backend
    "# backend : JARBackend\n"

initGlobalConfig :: IO ()
initGlobalConfig = do
    putStrLn "Creating global config.."
    confLocation <- globalConfigFile
    B.writeFile 
        confLocation 
        defaultGlobalConfigFile
    putStrLn "Created.."

loadGlobalConfig :: IO Configuration
loadGlobalConfig = do
    confLocation <- globalConfigFile
    fileExists <- doesFileExist confLocation

    when (not fileExists)
        initGlobalConfig

    fileContents <- B.readFile confLocation
    let decoded :: Maybe Configuration = Y.decode fileContents
    if (isNothing decoded)
    then die "Unexpected error while decoding config file. Aborting."
    else return $ fromJust decoded

globalConfig :: Configuration
{-# NOINLINE globalConfig #-}
globalConfig = unsafePerformIO $ loadGlobalConfig

getVerbosity :: VerbosityLevel
getVerbosity = verbosity globalConfig

assertCheck :: Bool -> String -> IO ()
assertCheck check msg = do
    when (getVerbosity == DebugLevel) $
        when (not check) $
            die $ "ASSERTION FAILURE: " ++ msg

getBackend :: BackendType
getBackend = backend globalConfig

logger :: VerbosityLevel -> String -> a -> a
logger level message computation =
    let currentVerbosity = getVerbosity
         in if currentVerbosity <= level
            then seq
                    (unsafePerformIO $ putStrLn message) 
                    computation
            else computation

loggerM :: (Monad m) => VerbosityLevel -> String -> m ()
loggerM l m = logger l m (return ())

debugLogger, infoLogger, seriousLogger :: String -> a -> a
debugLogger = logger DebugLevel
infoLogger = logger InfoLevel
seriousLogger = logger SeriousLevel

debugLoggerM, infoLoggerM, seriousLoggerM :: (Monad m) => String -> m ()
debugLoggerM = loggerM DebugLevel
infoLoggerM = loggerM InfoLevel
seriousLoggerM = loggerM SeriousLevel
