{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

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
        toConstType,
        CInfo(..), 
        ConstantInfo(..),
        ReferenceKind,
        toReferenceKind,
        AccessFlag(..),
        toAccessFlags,
        AttributeType(..),
        AInfo(..),
        AttributeInfo(..),
        toAttributeType,
        parseable,
        FieldInfo(..),
        MethodInfo(..),
        MethodParameter(..),
        CodeAtom
) where

import System.IO (FilePath)
import Data.Word (Word8, Word16, Word32)
import Data.Either
import Data.Bits
import Data.List (zip, sort)


-- | !! for [a] -> Word16
(!@) :: [a] -> Word16 -> a
(!@) xs pos = xs !! fromIntegral pos

-- | The Java Class MAGIC number
mAGIC :: Word32
mAGIC = 0xCAFEBABE

classFileExtension :: String
classFileExtension = ".class"

-- | ClassName stands for just the name of the class.
type ClassName = String
type CodeAtom  = (Int, [Word8])

-- | Produces the file path of the class from the ClassName
getClassFilePath :: ClassName -> FilePath
getClassFilePath = (++ classFileExtension)

 -- | The data of a raw class file, without any parsing. The bytecode is just represented almost as is in this.
data RawClassFile = RawClassFile {
        magicNumber     :: !Word32,              -- must equal 'mAGIC' for 
        minorVersion    :: !Word16,              -- minor version of .class format
        majorVersion    :: !MajorVersion,        -- major version of .class format
        constantPool    :: ![ConstantInfo],      -- Constant Pool
        accessFlags     :: ![AccessFlag],       -- Access Flags
        thisClass       :: !String,              -- Name of this class
        superClass      :: !(Maybe String),         -- Name of superclass if any
        interfaces      :: ![Word16],
        fields          :: ![FieldInfo],         -- fields                         
        methods         :: ![MethodInfo]         -- methods
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
                deriving (Show, Eq)

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
                deriving (Show, Eq)

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
                        18              -> Right CInvokeDynamic
                        _               -> Left $ produceError $ "Invalid Constant Type value " ++ show value

-- Constant Info structure
data CInfo = 
                CClassI                 { nameIndex :: !Word16 }                                                |
                CFieldRefI              { classIndex :: !Word16, nameAndTypeIndex :: !Word16 }                  |
                CMethodRefI             { classIndex :: !Word16, nameAndTypeIndex :: !Word16 }                  |
                CInterfaceMethodRefI    { classIndex :: !Word16, nameAndTypeIndex :: !Word16 }                  |
                CStringI                { stringIndex :: !Word16 }                                              |              
                CIntegerI               { bytei :: !Word32 }                                                    |
                CFloatI                 { bytef :: !Word32 }                                                    |
                CLongI                  { high  :: !Word32, low :: !Word32 }                                    |
                CDoubleI                { high  :: !Word32, low :: !Word32 }                                    |
                CNameAndTypeI           { nameIndex :: !Word16, descriptorIndex :: !Word16 }                    |
                CUtf8I                  { len :: !Word16, string:: !String }                                    |
                CMethodHandleI          { referenceKind :: !ReferenceKind, referenceIndex :: !Word16 }          |
                CMethodTypeI            { descriptorIndex :: !Word16 }                                          |
                CInvokeDynamicI         { bootstrapMethodAttrIndex :: !Word16, nameAndTypeIndex :: !Word16 }
                deriving Show

-- Constant Pool Info Structure
data ConstantInfo = ConstantInfo {
                        constType       :: !ConstType,
                        info            :: !CInfo
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
                deriving (Show, Eq)


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
                                

-- | Access Flags data structure
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

-- | Convert Accessflag integer to [AccessFlag]
toAccessFlags :: Word16 -> [AccessFlag]
toAccessFlags flags = sort $ map (([APublic, AFinal, ASuper, AInterface, AAbstract, ASynthetic, AAnnotation, AEnum] !!).fst) $
                                filter (\(_, bits) -> flags .&. bits /= 0) $
                                        zip [0..] [0x0001, 0x0010, 0x0020, 0x0200, 0x0400, 0x1000, 0x2000, 0x4000]


-- | Attributes 
data AttributeType = 
        ATConstantValue                         |
        ATCode                                  |
        ATStackMapTable                         |
        ATExceptions                            |
        ATInnerClasses                          |
        ATEnclosingMethod                       |
        ATSynthetic                             |
        ATSignature                             |
        ATSourceFile                            |
        ATSourceDebugExtension                  |
        ATLineNumberTable                       |
        ATLocalVariableTable                    |          
        ATLocalVariableTypeTable                |
        ATDeprecated                            |
        ATRuntimeVisibleAnnotations             |
        ATRuntimeInvisibleAnnotations           |
        ATRuntimeVisibleParameterAnnotations    |
        ATRuntimeInvisibleParameterAnnotations  |
        ATRuntimeVisibleTypeAnnotations         |
        ATRuntimeInvisibleTypeAnnotations       |
        ATAnnotationDefault                     |
        ATBootstrapMethods                      |
        ATMethodParameters                     
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
data MethodParameter = MethodParameter {
                                name            :: !String,
                                accessFlags     :: ![AccessFlag]
                        } deriving Show



-- | Attribute Info data
{- TODO: Some structures below are represented as just bytes :: [Word8], which are the raw unparsed bytes.
         Since this is not a general purpose library, but is intended to be used within the Purity checker,
         not all the attributes are of value. Please add parsing code for them, as and when required, and
         replace the appropriate structure with its parsed format (possibly adding more structure).

         Template structures for some of them (maybe incomplete) are written in the commented out parts.
         They should be uncommented out once the parsing code is written.

         Also see, the array 'parseable' defined later.

        **EDIT**: Added on 10/5/18. For all constructors AI<x> that has a AIParsed<x> counterpart, the AI<x> part is there
                only for 'parseAttribute' in Reader.hs, and for debugging helps.
-}

data AInfo = 
        AIConstantValue 
                {      
                        bytes                   :: ![Word8]
                        -- constantValueIndex      :: Word16
                } |
        AICode 
                {      
                        bytes                   :: ![Word8]
                        --maxStack                :: Word16,
                        --maxLocals               :: Word16,
                        --code                    :: [Instruction],               -- not defined yet
                        --exceptionTable          :: [ExceptionTableEntry], 
                        --attributes              :: [AttributeInfo]
                } |
        AIParsedCode 
                {
                        maxStack                :: !Int,
                        maxLocals               :: !Int,
                        codeLength              :: !Int,
                        code                    :: ![CodeAtom]    -- contains opcodes paired with their arguments
                } |
        AIStackMapTable 
                {
                        bytes                   :: ![Word8]
                        --entries                 :: [StackMapFrameEntry]         -- not defined yet
                } |

        AIExceptions 
                {
                        bytes                   :: ![Word8]
                        --exceptionIndexTable     :: [Word16] 
                } |
        AIInnerClasses
                {
                        bytes                   :: ![Word8]
                        --innerClassInfoIndex     :: Word16,
                        --outerClassInfoIndex     :: Word16,
                        --innerNameIndex          :: Word16,
                        --innerClassAccessFlags   :: [AccessFlag]
                } |
        AIEnclosingSingleMethod
                {
                        bytes                   :: ![Word8]
                        --classIndex              :: Word16,
                        --methodIndex             :: Word16
                } |
        AISynthetic 
                {
                        bytes                   :: ![Word8]
                } |
        AISignature
                {
                        bytes                   :: ![Word8]
                        --signatureIndex          :: Word16
                } |
        AISourceFile
                {
                        bytes                   :: ![Word8]
                        --sourceFileIndex         :: Word16
                } |
        AISourceDebugExtension
                {
                        bytes                   :: ![Word8]
                        --debugExtension          :: [Word8]
                } |
        AILineNumberTable
                {
                        bytes                   :: ![Word8]
                        --lineNumberTable         :: [LineNumberTableEntry]
                } |
        AILocalVariableTable
                {
                        bytes                   :: ![Word8]
                        --localVariableTable      :: [LocalVariableTableEntry]
                } |
        AILocalVariableTypeTable
                {
                        bytes                   :: ![Word8]
                        --localVariableTypeTable  :: [LocalVariableTypeTableEntry]
                } |
        AIDeprecated 
                {
                        bytes                   :: ![Word8]
                } |
        AIRuntimeVisibleAnnotations
                {
                        bytes                   :: ![Word8]
                        --annotations             :: [Annotation]                 -- not defined yet
                } |
        AIRuntimeInvisibleAnnotations 
                {
                        bytes                   :: ![Word8]
                        --annotations             :: [Annotation]
                } |
        AIRuntimeVisibleParameterAnnotations 
                {
                        bytes                   :: ![Word8]
                        --parameterAnnotations    :: [ParameterAnnotations]
                } |
        AIRuntimeInvisibleParameterAnnotations 
                {
                        bytes                   :: ![Word8]
                        --parameterAnnotations    :: [ParameterAnnotations]
                } |
        AIRuntimeVisibleTypeAnnotations 
                {
                        bytes                   :: ![Word8]
                        --annotations             :: [TypeAnnotation]             -- not defined yet
                } |
        AIRuntimeInvisibleTypeAnnotations 
                {
                        bytes                   :: ![Word8]
                        --annotations             :: [TypeAnnotation]
                } |
        AIAnnotationDefault                     
                {
                        bytes                   :: ![Word8]                      
                } |
        AIBootstrapMethods
                {
                        bytes                   :: ![Word8]
                        --bootstrapMethods        :: [BootstrapMethod]
                } |
        AIMethodParameters 
                {
                        bytes                   :: ![Word8]
                        --parameters              :: [MethodParameter]
                } |
        AIParsedMethodParameters 
                {
                        parameters              :: ![MethodParameter]
                } |
        AIDummy
                        -- added just for `parseParseableAttribute' in Reader.hs
        deriving Show        

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
data AttributeInfo = AttributeInfo {
                        attributeType :: !AttributeType,
                        attributeInfo :: !AInfo
                } deriving Show


-- Converts String to represented Attribute Type
toAttributeType :: String -> Either Error AttributeType
toAttributeType s = case s of 
                      "ConstantValue"                           -> Right ATConstantValue
                      "Code"                                    -> Right ATCode
                      "StackMapTable"                           -> Right ATStackMapTable
                      "Exceptions"                              -> Right ATExceptions
                      "InnerClasses"                            -> Right ATInnerClasses
                      "EnclosingMethod"                         -> Right ATEnclosingMethod
                      "Synthetic"                               -> Right ATSynthetic
                      "Signature"                               -> Right ATSignature
                      "SourceFile"                              -> Right ATSourceFile
                      "SourceDebugExtension"                    -> Right ATSourceDebugExtension
                      "LineNumberTable"                         -> Right ATLineNumberTable
                      "LocalVariableTypeTable"                  -> Right ATLocalVariableTypeTable
                      "Deprecated"                              -> Right ATDeprecated
                      "RuntimeVisibleAnnotations"               -> Right ATRuntimeVisibleAnnotations
                      "RuntimeInvisibleAnnotations"             -> Right ATRuntimeInvisibleAnnotations
                      "RuntimeVisibleParameterAnnotations"      -> Right ATRuntimeVisibleParameterAnnotations
                      "RuntimeInvisibleParameterAnnotations"    -> Right ATRuntimeInvisibleParameterAnnotations
                      "AnnotationDefault"                       -> Right ATAnnotationDefault
                      "BootstrapMethods"                        -> Right ATBootstrapMethods
                      "MethodParameters"                        -> Right ATMethodParameters
                      _                                         -> Left $ produceError $ "Invalid Attribute Type" ++ s
                        

-- | Field Info structure
data FieldInfo = FieldInfo {
                        accessFlags     :: ![AccessFlag],
                        name            :: !String,
                        descriptor      :: !String,
                        attributes      :: ![AttributeInfo]
                } deriving Show


data MethodInfo = MethodInfo {
                        accessFlags     :: ![AccessFlag],
                        name            :: !String,
                        descriptor      :: ![(Int, Bool)],       -- see comments on 'descriptorIndices' in Reader.hs for info on what these values mean
                        descriptorString:: !String,
                        attributes      :: ![AttributeInfo]
                } deriving Show
