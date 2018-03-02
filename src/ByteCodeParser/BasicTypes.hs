{-# LANGUAGE OverloadedStrings #-}

module ByteCodeParser.BasicTypes (
        mAGIC,
        ClassName,
        getClassFilePath,
        RawClassFile(..),
        Error,
        produceError
) where

import System.IO (FilePath)
import Data.Word (Word8, Word16, Word32)
import Data.Either

-- | The Java Class MAGIC number
mAGIC :: Word32
mAGIC = 0xCAFEBABE

classFileExtension :: String
classFileExtension = ".class"

-- | ClassName stands for just the name of the class.
type ClassName = String

-- | Produces the file path of the class from the ClassName
getClassFilePath :: ClassName -> FilePath
getClassFilePath = (++ classFileExtension)

-- | The data of a raw class file, without any parsing. The bytecode is just represented almost as is in this.
data RawClassFile = RawClassFile {
        magicNumber :: Word32           -- must equal 'mAGIC' for 

                                 } deriving Show
                        
-- | Error is used to indicate an error in the form of a string.
type Error = String 

-- | produceError is used to produce a Error-typed error message
produceError :: String -> Error
produceError = ("Reader Error: " ++)

         



