module ByteCodeParser.Reader (
        readRawClassFile
) where

import qualified Data.ByteString.Lazy as BL
import Data.Binary
import Data.Binary.Get (Get, runGet, getWord8, getWord16be, getWord32be)
import System.IO (FilePath, Handle, IOMode, withFile, hGetContents)
import Data.Word (Word8, Word16, Word32)
import Data.Either
import Control.Monad.Trans.Except (ExceptT, runExceptT, except, throwE)
import Control.Monad.Trans.Class  (lift)
import ByteCodeParser.BasicTypes (
        mAGIC, ClassName, 
        getClassFilePath, RawClassFile(..), 
        Error, produceError,
        MajorVersion(..), toMajorVersion)

-- | Gives the Lazy ByteString stream of the input from the class file
getClassFileStream :: ClassName                 -- ^ The input class
                   -> IO BL.ByteString          -- ^ The output bytestring stream, wrapped in IO
getClassFileStream className = 
        BL.readFile classFilePath
        where classFilePath = getClassFilePath className

-- | Reads the magic number, and checks if its okay.
readMagicNumber :: ExceptT Error Get Word32
readMagicNumber = do 
        magic <- lift getWord32be
        if (magic == mAGIC)
           then return magic
           else throwE $ produceError "Magic Number is incorrect."

readVersions :: ExceptT Error Get (Word16, MajorVersion)
readVersions = do
        minor <- lift getWord16be
        major <- lift getWord16be
        let maybeMajorVersion = toMajorVersion $ major
        case maybeMajorVersion of
          Right majorVersion -> return (minor, majorVersion)
          Left  errorMessage -> throwE errorMessage
       

-- | The main reader. This calls many other other sub readers, and produces a RawClassFile structure
reader :: ExceptT Error Get RawClassFile
reader = do  
        magic <- readMagicNumber
        (minor, major) <- readVersions
        return $ RawClassFile magic minor major

-- | Reads the class file and forms a parsed RawClassFile structure
readRawClassFile :: ClassName
                 -> IO (Either Error RawClassFile)
readRawClassFile className = do
        classFileStream <- getClassFileStream className
        return $ runGet (runExceptT reader) classFileStream

