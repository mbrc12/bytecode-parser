{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, BangPatterns #-}

module ByteCodeParser.BasicTypes
    ( (!@)
    , computeThenReturn
    , mAGIC
    , ClassName
    , getClassFilePath
    , RawClassFile(..)
    , Error
    , produceError
    , MajorVersion(..)
    , toMajorVersion
    , ConstType(..)
    , toConstType
    , CInfo(..)
    , ConstantInfo(..)
    , ReferenceKind
    , toReferenceKind
    , MethodAccessFlag(..)
    , FieldAccessFlag(..)
    , ClassAccessFlag(..)
    , MethodParameterAccessFlag(..)
    , toMethodAccessFlags
    , toFieldAccessFlags
    , toClassAccessFlags
    , toMethodParameterAccessFlags
    , AttributeType(..)
    , AInfo(..)
    , AttributeInfo(..)
    , toAttributeType
    , parseable
    , FieldInfo(..)
    , MethodInfo(..)
    , MethodParameter(..)
    , CodeAtom
    ) where


import Data.Bits
import Data.Either
import Data.List (sort, zip)
import Data.Word (Word16, Word32, Word8)
import System.IO (FilePath)

import qualified Data.ByteString.Lazy as BL

import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Vector ((!))

-- | !! for [a] -> Word16
(!@) :: V.Vector a -> Word16 -> a
(!@) xs pos = xs ! fromIntegral pos

computeThenReturn :: (Monad m) => a -> m a
computeThenReturn x = x `seq` (return x)

-- | The Java Class MAGIC number
mAGIC :: Word32
mAGIC = 0xCAFEBABE

classFileExtension :: T.Text
classFileExtension = ".class"

-- | ClassName stands for just the name of the class.
type ClassName = T.Text

type CodeAtom = (Int, BL.ByteString)

-- | Produces the file path of the class from the ClassName
getClassFilePath :: ClassName -> FilePath
getClassFilePath cname = T.unpack $ T.append cname classFileExtension
 -- | The data of a raw class file, without any parsing. The bytecode is just represented almost as is in this.

data RawClassFile = RawClassFile
    { magicNumber :: !Word32 -- must equal 'mAGIC' for 
    , minorVersion :: !Word16 -- minor version of .class format
    , majorVersion :: !MajorVersion -- major version of .class format
    , constantPool :: V.Vector ConstantInfo -- Constant Pool
    , accessFlags :: ![ClassAccessFlag] -- Access Flags
    , thisClass :: !T.Text -- Name of this class
    , superClass :: !(Maybe T.Text) -- Name of superclass if any
    , interfaces :: V.Vector Word16
    , fields :: ![FieldInfo] -- fields                         
    , methods :: ![MethodInfo] -- methods
    } deriving (Eq, Show)

-- | Error is used to indicate an error in the form of a string.
type Error = String

-- | produceError is used to produce a Error-typed error message
produceError :: String -> Error
produceError = ("Reader Error: " ++)

-- | Java Major Version.
data MajorVersion
    = JavaSE9
    | JavaSE8
    | JavaSE7
    | JavaSE6
    | JavaSE5
    | JDK14
    | JDK13
    | JDK12
    | JDK11
    deriving (Show, Eq)

-- | Convert integer to the respective major version. See the JVM spec.
toMajorVersion :: Word16 -> Either Error MajorVersion
toMajorVersion major =
    case major of
        0x35 -> Right JavaSE9
        0x34 -> Right JavaSE8
        0x33 -> Right JavaSE7
        0x32 -> Right JavaSE6
        0x31 -> Right JavaSE5
        0x30 -> Right JDK14
        0x2F -> Right JDK13
        0x2E -> Right JDK12
        0x2D -> Right JDK11
        _ -> Left $ produceError $ "Invalid major version value " ++ show major

-- | Constant type in ConstPool
data ConstType
    = CClass
    | CFieldRef
    | CMethodRef
    | CInterfaceMethodRef
    | CString
    | CInteger
    | CFloat
    | CLong
    | CDouble
    | CNameAndType
    | CUtf8
    | CMethodHandle
    | CMethodType
    | CInvokeDynamic
    deriving (Show, Eq)

-- | Convert integer to ConstType
toConstType :: Word8 -> Either Error ConstType
toConstType value =
    case value of
        7 -> Right CClass
        9 -> Right CFieldRef
        10 -> Right CMethodRef
        11 -> Right CInterfaceMethodRef
        8 -> Right CString
        3 -> Right CInteger
        4 -> Right CFloat
        5 -> Right CLong
        6 -> Right CDouble
        12 -> Right CNameAndType
        1 -> Right CUtf8
        15 -> Right CMethodHandle
        16 -> Right CMethodType
        18 -> Right CInvokeDynamic
        _ -> Left $ produceError $ "Invalid Constant Type value " ++ show value

-- Constant Info structure
data CInfo
    = CClassI { nameIndex :: !Word16 }
    | CFieldRefI { classIndex :: !Word16
                 , nameAndTypeIndex :: !Word16 }
    | CMethodRefI { classIndex :: !Word16
                  , nameAndTypeIndex :: !Word16 }
    | CInterfaceMethodRefI { classIndex :: !Word16
                           , nameAndTypeIndex :: !Word16 }
    | CStringI { stringIndex :: !Word16 }
    | CIntegerI { bytei :: !Word32 }
    | CFloatI { bytef :: !Word32 }
    | CLongI { high :: !Word32
             , low :: !Word32 }
    | CDoubleI { high :: !Word32
               , low :: !Word32 }
    | CNameAndTypeI { nameIndex :: !Word16
                    , descriptorIndex :: !Word16 }
    | CUtf8I { len :: !Word16
             , string :: !T.Text }
    | CMethodHandleI { referenceKind :: !ReferenceKind
                     , referenceIndex :: !Word16 }
    | CMethodTypeI { descriptorIndex :: !Word16 }
    | CInvokeDynamicI { bootstrapMethodAttrIndex :: !Word16
                      , nameAndTypeIndex :: !Word16 }
    deriving (Eq, Show)

-- Constant Pool Info Structure
data ConstantInfo = ConstantInfo
    { constType :: !ConstType
    , info :: !CInfo
    } deriving (Eq, Show)

-- Reference Kind for method handles, see 'CMethodHandleI'
data ReferenceKind
    = RGetField
    | RGetStatic
    | RPutField
    | RPutStatic
    | RInvokeVirtual
    | RInvokeStatic
    | RInvokeSpecial
    | RNewInvokeSpecial
    | RInvokeInterface
    deriving (Show, Eq)

-- Convert integer to ReferenceKind
toReferenceKind :: Word8 -> Either Error ReferenceKind
toReferenceKind value =
    case value of
        1 -> Right RGetField
        2 -> Right RGetStatic
        3 -> Right RPutField
        4 -> Right RPutStatic
        5 -> Right RInvokeVirtual
        6 -> Right RInvokeStatic
        7 -> Right RInvokeSpecial
        8 -> Right RNewInvokeSpecial
        9 -> Right RInvokeInterface
        _ -> Left $! produceError $ "Invalid reference kind number" ++ show value

-- | Access Flags data structure
data MethodAccessFlag
    = AMPublic
    | AMPrivate
    | AMProtected
    | AMStatic
    | AMFinal
    | AMSynchronized
    | AMBridge
    | AMVarargs
    | AMNative
    | AMAbstract
    | AMStrict
    | AMSynthetic
    deriving (Show, Eq, Ord)

-- | Convert Accessflag integer to [AccessFlag]
toMethodAccessFlags :: Word16 -> [MethodAccessFlag]
toMethodAccessFlags flags =
    sort $!
    map (([ AMPublic
          , AMPrivate
          , AMProtected
          , AMStatic
          , AMFinal
          , AMSynchronized
          , AMBridge
          , AMVarargs
          , AMNative
          , AMAbstract
          , AMStrict
          , AMSynthetic
          ] !!) .
         fst) $!
    filter (\(_, bits) -> flags .&. bits /= 0) $!
    zip [0 ..] $!
    [ 0x0001
    , 0x0002
    , 0x0004
    , 0x0008
    , 0x0010
    , 0x0020
    , 0x0040
    , 0x0080
    , 0x0100
    , 0x0400
    , 0x0800
    , 0x1000
    ]

data FieldAccessFlag
    = AFPublic
    | AFPrivate
    | AFProtected
    | AFStatic
    | AFFinal
    | AFVolatile
    | AFTransient
    | AFSynthetic
    | AFEnum
    deriving (Show, Eq, Ord)

toFieldAccessFlags :: Word16 -> [FieldAccessFlag]
toFieldAccessFlags flags =
    sort $!
    map (([ AFPublic
          , AFPrivate
          , AFProtected
          , AFStatic
          , AFFinal
          , AFVolatile
          , AFTransient
          , AFSynthetic
          , AFEnum
          ] !!) .
         fst) $!
    filter (\(_, bits) -> flags .&. bits /= 0) $!
    zip [0 ..] $!
    [0x0001, 0x0002, 0x0004, 0x0008, 0x0010, 0x0040, 0x0080, 0x1000, 0x4000]

data ClassAccessFlag
    = ACPublic
    | ACFinal
    | ACSuper
    | ACInterface
    | ACAbstract
    | ACSynthetic
    | ACAnnotation
    | ACEnum
    deriving (Show, Eq, Ord)

toClassAccessFlags :: Word16 -> [ClassAccessFlag]
toClassAccessFlags flags =
    sort $!
    map (([ ACPublic
          , ACFinal
          , ACSuper
          , ACInterface
          , ACAbstract
          , ACSynthetic
          , ACAnnotation
          , ACEnum
          ] !!) .
         fst) $!
    filter (\(_, bits) -> flags .&. bits /= 0) $!
    zip [0 ..] $!
    [0x0001, 0x0010, 0x0020, 0x0200, 0x0400, 0x1000, 0x2000, 0x4000]

data MethodParameterAccessFlag
    = AMPFinal
    | AMPSynthetic
    | AMPMandated
    deriving (Show, Eq, Ord)

toMethodParameterAccessFlags :: Word16 -> [MethodParameterAccessFlag]
toMethodParameterAccessFlags flags =
    sort $!
    map (([AMPFinal, AMPSynthetic, AMPMandated] !!) . fst) $!
    filter (\(_, bits) -> flags .&. bits /= 0) $!
    zip [0 ..] $! [0x0010, 0x1000, 0x8000]

-- | Attributes 
data AttributeType
    = ATConstantValue
    | ATCode
    | ATStackMapTable
    | ATExceptions
    | ATInnerClasses
    | ATEnclosingMethod
    | ATSynthetic
    | ATSignature
    | ATSourceFile
    | ATSourceDebugExtension
    | ATLineNumberTable
    | ATLocalVariableTable
    | ATLocalVariableTypeTable
    | ATDeprecated
    | ATRuntimeVisibleAnnotations
    | ATRuntimeInvisibleAnnotations
    | ATRuntimeVisibleParameterAnnotations
    | ATRuntimeInvisibleParameterAnnotations
    | ATRuntimeVisibleTypeAnnotations
    | ATRuntimeInvisibleTypeAnnotations
    | ATAnnotationDefault
    | ATBootstrapMethods
    | ATMethodParameters
    deriving (Show, Eq)

{- This part contains structures that may be required in the future to properly parse the raw data represented in the AInfo structure. This is
    commented out because some parts have not been implemented -}
{-
-- | See 'AICode'
data ExceptionTableEntry = ExceptionTableEntry {
                                startPc         :: Word16,
                                endPc           :: Word16,
                                handlerPc       :: Word16,
                                catchType       :: Word16
                        } deriving Show

-- | See 'AILineNumberTable'
data LineNumberTableEntry = LineNumberTableEntry {
                                startPc         :: Word16,
                                lineNumber      :: Word16
                        } deriving Show

-- | See 'AILocalVariableTable'
data LocalVariableTableEntry = LocalVariableTableEntry {
                                startPc         :: Word16,
                                length'         :: Word16,
                                nameIndex       :: Word16,
                                descriptorIndex :: Word16,
                                index           :: Word16
                        } deriving Show

-- | See 'AILocalVariableTypeTable'
data LocalVariableTypeTableEntry = LocalVariableTypeTableEntry {
                                startPc         :: Word16,
                                length'         :: Word16,
                                nameIndex       :: Word16,
                                signatureIndex  :: Word16,
                                index           :: Word16
                        } deriving Show

-- | See AIBootstrapMethods
data BootstrapMethod = BootstrapMethod {
                                bootstrapMethodRef :: Word16,
                                bootstrapArguments :: [Word16]
                        } deriving Show


-- | See 'AIRuntimeVisibleParameterAnnotations' etc.
data ParameterAnnotations = ParameterAnnotations {
                                annotations     :: [Annotation]
                        } deriving Show

-}
-- | See 'AIMethodParameters
data MethodParameter = MethodParameter
    { name :: !T.Text
    , accessFlags :: ![MethodParameterAccessFlag]
    } deriving (Eq, Show)

-- | Attribute Info data
{- TODO: Some structures below are represented as just bytes :: BL.ByteString, which are the raw unparsed bytes.
         Since this is not a general purpose library, but is intended to be used within the Purity checker,
         not all the attributes are of value. Please add parsing code for them, as and when required, and
         replace the appropriate structure with its parsed format (possibly adding more structure).

         Template structures for some of them (maybe incomplete) are written in the commented out parts.
         They should be uncommented out once the parsing code is written.

         Also see, the array 'parseable' defined later.

        **EDIT**: Added on 10/5/18. For all constructors AI<x> that has a AIParsed<x> counterpart, the AI<x> part is there
                only for 'parseAttribute' in Reader.hs, and for debugging helps.
-}
data AInfo
    = AIConstantValue { bytes :: BL.ByteString
                        -- constantValueIndex      :: Word16
                       }
    | AICode { bytes :: !BL.ByteString
                        --maxStack                :: Word16,
                        --maxLocals               :: Word16,
                        --code                    :: [Instruction],               -- not defined yet
                        --exceptionTable          :: [ExceptionTableEntry], 
                        --attributes              :: [AttributeInfo]
              }
    | AIParsedCode { maxStack :: !Int
                   , maxLocals :: !Int
                   , codeLength :: !Int
                   , code :: V.Vector CodeAtom -- contains opcodes paired with their arguments
                    }
    | AIStackMapTable { bytes :: BL.ByteString
                        --entries                 :: [StackMapFrameEntry]         -- not defined yet
                       }
    | AIExceptions { bytes :: BL.ByteString
                        --exceptionIndexTable     :: [Word16] 
                    }
    | AIInnerClasses { bytes :: BL.ByteString
                        --innerClassInfoIndex     :: Word16,
                        --outerClassInfoIndex     :: Word16,
                        --innerNameIndex          :: Word16,
                        --innerClassAccessFlags   :: [AccessFlag]
                      }
    | AIEnclosingSingleMethod { bytes :: BL.ByteString
                        --classIndex              :: Word16,
                        --methodIndex             :: Word16
                               }
    | AISynthetic { bytes :: BL.ByteString }
    | AISignature { bytes :: BL.ByteString
                        --signatureIndex          :: Word16
                   }
    | AISourceFile { bytes :: BL.ByteString
                        --sourceFileIndex         :: Word16
                    }
    | AISourceDebugExtension { bytes :: BL.ByteString
                        --debugExtension          :: BL.ByteString
                              }
    | AILineNumberTable { bytes :: BL.ByteString
                        --lineNumberTable         :: [LineNumberTableEntry]
                         }
    | AILocalVariableTable { bytes :: BL.ByteString
                        --localVariableTable      :: [LocalVariableTableEntry]
                            }
    | AILocalVariableTypeTable { bytes :: BL.ByteString
                        --localVariableTypeTable  :: [LocalVariableTypeTableEntry]
                                }
    | AIDeprecated { bytes :: BL.ByteString }
    | AIRuntimeVisibleAnnotations { bytes :: BL.ByteString
                        --annotations             :: [Annotation]                 -- not defined yet
                                   }
    | AIRuntimeInvisibleAnnotations { bytes :: BL.ByteString
                        --annotations             :: [Annotation]
                                     }
    | AIRuntimeVisibleParameterAnnotations { bytes :: BL.ByteString
                        --parameterAnnotations    :: [ParameterAnnotations]
                                            }
    | AIRuntimeInvisibleParameterAnnotations { bytes :: BL.ByteString
                        --parameterAnnotations    :: [ParameterAnnotations]
                                              }
    | AIRuntimeVisibleTypeAnnotations { bytes :: BL.ByteString
                        --annotations             :: [TypeAnnotation]             -- not defined yet
                                       }
    | AIRuntimeInvisibleTypeAnnotations { bytes :: BL.ByteString
                        --annotations             :: [TypeAnnotation]
                                         }
    | AIAnnotationDefault { bytes :: BL.ByteString }
    | AIBootstrapMethods { bytes :: BL.ByteString
                        --bootstrapMethods        :: [BootstrapMethod]
                          }
    | AIMethodParameters { bytes :: BL.ByteString
                        --parameters              :: [MethodParameter]
                          }
    | AIParsedMethodParameters { parameters :: [MethodParameter] }
    | AIDummy
                        -- added just for `parseParseableAttribute' in Reader.hs
    deriving (Eq, Show)

{- TODO: The below list represents the structures which have a structure that 
    is better than the raw 'bytes' format, and must be parsed by a parser.
    See Reader.hs for the implementations of attributes that are parseable.
    Update this as and when a new parser for an attribute is added to
    'parseAttribute' of Reader.hs. Its needed to update the parser in 
    'parseParseableAttribute' in Reader.hs
-}
parseable :: [AttributeType]
parseable = [ATCode, ATMethodParameters]

-- Attribute Info structure containing an AttributeType, and AInfo
data AttributeInfo = AttributeInfo
    { attributeType :: !AttributeType
    , attributeInfo :: !AInfo
    } deriving (Eq, Show)

-- Converts String to represented Attribute Type
toAttributeType :: T.Text -> Either Error AttributeType
toAttributeType s =
    case s of
        "ConstantValue" -> Right ATConstantValue
        "Code" -> Right ATCode
        "StackMapTable" -> Right ATStackMapTable
        "Exceptions" -> Right ATExceptions
        "InnerClasses" -> Right ATInnerClasses
        "EnclosingMethod" -> Right ATEnclosingMethod
        "Synthetic" -> Right ATSynthetic
        "Signature" -> Right ATSignature
        "SourceFile" -> Right ATSourceFile
        "SourceDebugExtension" -> Right ATSourceDebugExtension
        "LineNumberTable" -> Right ATLineNumberTable
        "LocalVariableTypeTable" -> Right ATLocalVariableTypeTable
        "Deprecated" -> Right ATDeprecated
        "RuntimeVisibleAnnotations" -> Right ATRuntimeVisibleAnnotations
        "RuntimeInvisibleAnnotations" -> Right ATRuntimeInvisibleAnnotations
        "RuntimeVisibleParameterAnnotations" ->
            Right ATRuntimeVisibleParameterAnnotations
        "RuntimeInvisibleParameterAnnotations" ->
            Right ATRuntimeInvisibleParameterAnnotations
        "AnnotationDefault" -> Right ATAnnotationDefault
        "BootstrapMethods" -> Right ATBootstrapMethods
        "MethodParameters" -> Right ATMethodParameters
        _ -> Left $! produceError $ "Invalid Attribute Type" ++ T.unpack s

-- | Field Info structure
data FieldInfo = FieldInfo
    { accessFlags :: ![FieldAccessFlag]
    , name :: !T.Text
    , descriptor :: !T.Text
    , attributes :: ![AttributeInfo]
    } deriving (Eq, Show)

data MethodInfo = MethodInfo
    { accessFlags :: ![MethodAccessFlag]
    , name :: !T.Text
    , descriptor :: ![(Int, Bool)] -- see comments on 'descriptorIndices' in Reader.hs for info on what these values mean
    , descriptorString :: !T.Text
    , attributes :: ![AttributeInfo]
    } deriving (Eq, Show)
