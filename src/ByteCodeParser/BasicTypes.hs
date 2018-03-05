{-# LANGUAGE OverloadedStrings #-}

module ByteCodeParser.BasicTypes (
        (!@),
        mAGIC,
        ClassName,
        getClassFilePath,
        RawClassFile(..),
        Error,
        produceError,
        MajorVersion(..),
        toMajorVersion,
        ConstType(..),
        toConstType,    -- 03617151747
        CInfo(..), 
        ConstantInfo(..),
        ReferenceKind,
        toReferenceKind,
        AccessFlag(..),
        toAccessFlags
) where

import System.IO (FilePath)
import Data.Word (Word8, Word16, Word32)
import Data.Either
import Data.Bits
import Data.List (zip, sort)


-- !! for [a] -> Word16
(!@) :: [a] -> Word16 -> a
(!@) xs pos = xs !! fromIntegral pos

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
        magicNumber     :: Word32,              -- must equal 'mAGIC' for 
        minorVersion    :: Word16,              -- minor version of .class format
        majorVersion    :: MajorVersion,        -- major version of .class format
        constantPool    :: [ConstantInfo],      -- Constant Pool
        accessFlags     :: [AccessFlag],       -- Access Flags
        thisClass       :: String,              -- Name of this class
        superClass      :: Maybe String         -- Name of superclass if any
                                 } deriving Show

-- | Error is used to indicate an error in the form of a string.
type Error = String 

-- | produceError is used to produce a Error-typed error message
produceError :: String -> Error
produceError = ("Reader Error: " ++)


-- | Java Major Version.
data MajorVersion = 
                JavaSE9         | 
                JavaSE8         | 
                JavaSE7         |
                JavaSE6         |
                JavaSE5         |
                JDK14           |
                JDK13           |
                JDK12           |
                JDK11           
                deriving Show

-- | Convert integer to the respective major version. See the JVM spec.
toMajorVersion :: Word16 -> Either Error MajorVersion
toMajorVersion major = case major of
                          0x35          -> Right JavaSE9
                          0x34          -> Right JavaSE8
                          0x33          -> Right JavaSE7
                          0x32          -> Right JavaSE6
                          0x31          -> Right JavaSE5
                          0x30          -> Right JDK14
                          0x2F          -> Right JDK13
                          0x2E          -> Right JDK12
                          0x2D          -> Right JDK11
                          _             -> Left $ produceError $ "Invalid major version value " ++ show major

-- | Constant type in ConstPool
data ConstType = 
                CClass                  |
                CFieldRef               |      
                CMethodRef              |    
                CInterfaceMethodRef     |
                CString                 |
                CInteger                |
                CFloat                  |
                CLong                   |
                CDouble                 |
                CNameAndType            |
                CUtf8                   |
                CMethodHandle           |
                CMethodType             |
                CInvokeDynamic
                deriving Show

-- | Convert integer to ConstType
toConstType :: Word8 -> Either Error ConstType
toConstType value = case value of
                        7               -> Right CClass
                        9               -> Right CFieldRef
                        10              -> Right CMethodRef
                        11              -> Right CInterfaceMethodRef
                        8               -> Right CString
                        3               -> Right CInteger
                        4               -> Right CFloat
                        5               -> Right CLong
                        6               -> Right CDouble
                        12              -> Right CNameAndType
                        1               -> Right CUtf8
                        15              -> Right CMethodHandle
                        16              -> Right CMethodType
                        17              -> Right CInvokeDynamic
                        _               -> Left $ produceError $ "Invalid Constant Type value " ++ show value

-- Constant Info structure
data CInfo = 
                CClassI                 { nameIndex :: Word16 }                                                 |
                CFieldRefI              { classIndex :: Word16, nameAndTypeIndex :: Word16 }                    |
                CMethodRefI             { classIndex :: Word16, nameAndTypeIndex :: Word16 }                    |
                CInterfaceMethodRefI    { classIndex :: Word16, nameAndTypeIndex :: Word16 }                    |
                CStringI                { stringIndex :: Word16 }                                               |              
                CIntegerI               { bytei :: Word32 }                                                     |
                CFloatI                 { bytef :: Word32 }                                                     |
                CLongI                  { high  :: Word32, low :: Word32 }                                      |
                CDoubleI                { high  :: Word32, low :: Word32 }                                      |
                CNameAndTypeI           { nameIndex :: Word16, descriptorIndex :: Word16 }                      |
                CUtf8I                  { len :: Word16, bytes :: String }                                      |
                CMethodHandleI          { referenceKind :: ReferenceKind, referenceIndex :: Word16 }            |
                CMethodTypeI            { descriptorIndex :: Word16 }                                           |
                CInvokeDynamicI         { bootstrapMethodAttrIndex :: Word16, nameAndTypeIndex :: Word16 }
                deriving Show

-- Constant Pool Info Structure
data ConstantInfo = ConstantInfo {
                        constType       :: ConstType,
                        info            :: CInfo
                                 } deriving Show

-- Reference Kind for method handles, see 'CMethodHandleI'
data ReferenceKind = 
                RGetField               |
                RGetStatic              |
                RPutField               |
                RPutStatic              |
                RInvokeVirtual          |
                RInvokeStatic           |
                RInvokeSpecial          |
                RNewInvokeSpecial       |
                RInvokeInterface
                deriving Show


-- Convert integer to ReferenceKind
toReferenceKind :: Word8 -> Either Error ReferenceKind
toReferenceKind value = case value of
                          1     -> Right RGetField
                          2     -> Right RGetStatic
                          3     -> Right RPutField
                          4     -> Right RPutStatic
                          5     -> Right RInvokeVirtual
                          6     -> Right RInvokeStatic
                          7     -> Right RInvokeSpecial
                          8     -> Right RNewInvokeSpecial
                          9     -> Right RInvokeInterface
                          _     -> Left $ produceError $ "Invalid reference kind number" ++ show value
                                

-- Access Flags data structure
data AccessFlag = 
        APublic         |
        AFinal          |
        ASuper          |
        AInterface      |
        AAbstract       |
        ASynthetic      |
        AAnnotation     |
        AEnum           
        deriving (Show, Eq, Ord)

-- convert Accessflag integer to [AccessFlag]
toAccessFlags :: Word16 -> [AccessFlag]
toAccessFlags flags = sort $ map (([APublic, AFinal, ASuper, AInterface, AAbstract, ASynthetic, AAnnotation, AEnum] !!).fst) $
                                filter (\(_, bits) -> flags .&. bits /= 0) $
                                        zip [0..] [0x0001, 0x0010, 0x0020, 0x0200, 0x0400, 0x1000, 0x2000, 0x4000]




