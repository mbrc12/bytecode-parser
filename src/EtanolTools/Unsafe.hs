{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, DeriveGeneric #-}

{- Provides a logging interface using unsafe routines -}

module EtanolTools.Unsafe 
    (
     Configuration(..),
     VerbosityLevel(..),
     getVerbosity,
     debugLogger,
     infoLogger,
     seriousLogger,
     debugLoggerM,
     infoLoggerM,
     seriousLoggerM
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
import System.Directory (doesFileExist)

confFile :: String
confFile = ".etanolglobalconfig"

data Configuration = Configuration {
    verbosity :: VerbosityLevel
} deriving (Show, Read, Eq, Ord, G.Generic)

instance Y.FromJSON Configuration
instance Y.ToJSON Configuration

globalConfigFile :: IO String
globalConfigFile = do
    hDir <- lookupEnv "HOME"
    if isNothing hDir
    then die $ "Please set the HOME environment variable."
    else return ((fromJust hDir) </> confFile)

data VerbosityLevel = 
    DebugLevel      | 
    InfoLevel       |
    SeriousLevel    |
    QuietLevel      
    deriving (Eq, Ord, Show, Read, G.Generic)

instance Y.FromJSON VerbosityLevel
instance Y.ToJSON VerbosityLevel

defaultGlobalConfigFile = pack $
    "# Uncomment any one of the following lines.\n" ++
    "verbosity : DebugLevel\n" ++   -- currently uncommented : default level
    "# verbosity : InfoLevel\n" ++
    "# verbosity : SeriousLevel\n"++
    "# verbosity : QuietLevel\n"

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