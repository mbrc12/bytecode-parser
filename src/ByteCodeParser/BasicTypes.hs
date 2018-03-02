{-# LANGUAGE OverloadedStrings #-}

module ByteCodeParser.BasicTypes (
        mAGIC,
        ClassName,
        getClassFilePath,
        RawClassFile(..),
        Error,
        produceError,
        MajorVersion,
        toMajorVersion
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
        magicNumber :: Word32,          -- must equal 'mAGIC' for 
        minorVersion :: Word16,         -- minor version of .class format
        majorVersion :: MajorVersion    -- major version of .class format

                                 } deriving Show

-- | Error is used to indicate an error in the form of a string.
type Error = String 

-- | produceError is used to produce a Error-typed error message
produceError :: String -> Error
produceError = ("Reader Error: " ++)

data MajorVersion = 
                JavaSE9 | 
                JavaSE8 | 
                JavaSE7 |
                JavaSE6 |
                JavaSE5 |
                JDK14   |
                JDK13   |
                JDK12   |
                JDK11           
                deriving Show

toMajorVersion :: Word16 -> Either Error MajorVersion
toMajorVersion major = case major of
                          0x35        -> Right JavaSE9
                          0x34        -> Right JavaSE8
                          0x33        -> Right JavaSE7
                          0x32        -> Right JavaSE6
                          0x31        -> Right JavaSE5
                          0x30        -> Right JDK14
                          0x2F        -> Right JDK13
                          0x2E        -> Right JDK12
                          0x2D        -> Right JDK11
                          otherwise   -> Left $ produceError "Invalid major version."

