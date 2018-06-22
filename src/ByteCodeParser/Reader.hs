{-# LANGUAGE OverloadedStrings, DuplicateRecordFields,
  ScopedTypeVariables, BangPatterns #-}

module ByteCodeParser.Reader
    ( readRawClassFile
    , descriptorIndices
    , readRawByteString
    ) where

import ByteCodeParser.BasicTypes
import Control.Applicative ((<*>), pure)
import Control.Monad (forM, replicateM, when)
import Data.Binary
import Data.Binary.Get (Get, getWord16be, getWord32be, getWord8, runGet)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Char (chr)
import Data.Either
import Data.Word (Word16, Word32, Word8)
import System.IO (FilePath, Handle, IOMode, hGetContents, withFile)

import qualified Data.Text as T

import EtanolTools.Unsafe 

import ByteCodeParser.Instructions (readInstructions)

-- | Gives the Lazy ByteString stream of the input from the class file
getClassFileStream ::
       ClassName -- ^ The input class
    -> IO BL.ByteString -- ^ The output bytestring stream, wrapped in IO
getClassFileStream className =
    pure BL.fromStrict <*> B.readFile classFilePath -- strict reading
  where
    classFilePath = getClassFilePath className

getRawBytes :: Int ->  Get [Word8]
getRawBytes how_many = do
    result <- forM [1 .. how_many] (const getWord8)
    return $! result

-- | Reads the magic number, and checks if its okay.
readMagicNumber :: Get Word32
readMagicNumber = do
    magic <- getWord32be
    if magic == mAGIC
        then return magic
        else error $! produceError "Magic Number is incorrect."

-- Read the major and minor versions and return
readVersions :: Get (Word16, MajorVersion)
readVersions = do
    minor <- getWord16be
    major <- getWord16be
    let maybeMajorVersion = toMajorVersion major
    case maybeMajorVersion of
        Right majorVersion -> return $! (minor, majorVersion)
        Left errorMessage -> error errorMessage

baseType = ['B', 'C', 'D', 'F', 'I', 'J', 'S', 'Z'] :: [Char]

twoPlaces = ['J', 'D'] :: [Char] 
-- base types that take up two consecutive locations on the stack

objType = 'L'

arrayType = '['

-- | Given a method descriptor this function will return the positions in the 
-- | local variable array which are of parameters, 
-- | and if the indexed parameter is a reference (Object/Array), 
-- | the second argument is true
descriptorIndices :: T.Text -> [(Int, Bool)]
descriptorIndices descriptor = recursiveCalc desc2 1
  where
    desc2 = T.takeWhile (')' /=) $! T.drop 1 descriptor -- Convert (xxx)yyy -> xxx
    recursiveCalc :: T.Text -> Int -> [(Int, Bool)]
    recursiveCalc ("") x = []
    recursiveCalc des x =
        if c `elem` baseType
            then if c `elem` twoPlaces
                     then (x, False) : recursiveCalc left (x + 2)
                     else (x, False) : recursiveCalc left (x + 1)
            else if c == objType
                     then (x, True) :
                          recursiveCalc
                              (T.drop 1 $! T.dropWhile (';' /=) left)
                              (x + 1)
                     else if c == arrayType
                              then let arrTypeAnd = T.dropWhile ('[' ==) left
                                       arrType = T.head arrTypeAnd
                                    in if arrType == objType
                                           then (x, True) :
                                                recursiveCalc
                                                    (T.drop 1 $!
                                                     T.dropWhile
                                                         (';' /=)
                                                         arrTypeAnd)
                                                    (x + 1)
                                           else (x, True) :
                                                recursiveCalc
                                                    (T.drop 1 arrTypeAnd)
                                                    (x + 1)
                              else []
        where 
              c     = T.head des
              left  = T.tail des 

-- Read a Constant from the pool, see 'readConstantPool'
readConstFromPool :: Get (ConstantInfo, Int)
readConstFromPool = do
    tag <- getWord8
    constType <-
        case toConstType tag of
            Right cType -> return cType
            Left err -> error err
    cinfo <-
        case constType of
            CClass -> do
                nameIndex <- getWord16be
                return $! CClassI (nameIndex - 1)
            CFieldRef -> do
                classIndex <- getWord16be
                nameAndTypeIndex <- getWord16be
                return $! CFieldRefI (classIndex - 1) (nameAndTypeIndex - 1)
            CMethodRef -> do
                classIndex <- getWord16be
                nameAndTypeIndex <- getWord16be
                return $! CMethodRefI (classIndex - 1) (nameAndTypeIndex - 1)
            CInterfaceMethodRef -> do
                classIndex <- getWord16be
                nameAndTypeIndex <- getWord16be
                return $!
                    CInterfaceMethodRefI (classIndex - 1) (nameAndTypeIndex - 1)
            CString -> do
                stringIndex <- getWord16be
                return $! CStringI (stringIndex - 1)
            CInteger -> do
                bytei <- getWord32be
                return $! CIntegerI bytei
            CFloat -> do
                bytef <- getWord32be
                return $! CFloatI bytef
            CLong -> do
                high <- getWord32be
                low <- getWord32be
                return $! CLongI high low
            CDouble -> do
                high <- getWord32be
                low <- getWord32be
                return $! CDoubleI high low
            CNameAndType -> do
                nameIndex <- getWord16be
                descriptorIndex <- getWord16be
                return $! CNameAndTypeI (nameIndex - 1) (descriptorIndex - 1)
            CUtf8 -> do
                len <- getWord16be
                bytes <- getBytes len
                return $! CUtf8I len bytes
            CMethodHandle -> do
                refkind <- getWord8
                referenceIndex <- getWord16be
                referenceKind <-
                    case toReferenceKind refkind of
                        Right refKind -> return refKind
                        Left err -> error err
                return $! CMethodHandleI referenceKind (referenceIndex - 1)
            CMethodType -> do
                descriptorIndex <- getWord16be
                return $! CMethodTypeI (descriptorIndex - 1)
            CInvokeDynamic -> do
                bootstrapMethodAttrIndex <- getWord16be
                nameAndTypeIndex <- getWord16be
                return $!
                    CInvokeDynamicI
                        (bootstrapMethodAttrIndex - 1)
                        (nameAndTypeIndex - 1)
    return $!
        ( ConstantInfo constType cinfo
        , if constType == CLong || constType == CDouble
              then 2
              else 1)
                -- | getBytes gets len bytes from the input
  where
    getBytes :: Word16 -> Get T.Text
    getBytes len =
        fmap T.pack $! (fmap . fmap) (chr . fromIntegral) $! forM [1 .. len] $! const getWord8

-- | Read the Constant Pool
readConstantPool :: Get [ConstantInfo]
readConstantPool = do
    cpsize <- getWord16be
    when (cpsize == 0) $
        error $! produceError "Constant pool size is 0, should be atleast 1."
        -- cpsize - 1 because of ConstantPool size convention
    readPool $! fromIntegral (cpsize - 1)
                -- reads elements from the pool according to their byte size, specifically, CLong and CDouble are 2 places each
                -- Note that this duplicates the same value for fields which take up two consecutive locations in the constant pool
                -- like longs and doubles to keep the constantPool indices okay.
  where
    readPool :: Int -> Get [ConstantInfo]
    readPool rem =
        if rem == 0
            then return []
            else do
                (!c_elem, !c_positions) <- readConstFromPool
                q <-
                    if c_positions == 1
                        then pure (c_elem :) <*> readPool (rem - c_positions)
                        else pure ([c_elem, c_elem] ++) <*>
                             readPool (rem - c_positions)
                return $! q

-- | Reads the access flags
readAccessFlags :: Get [ClassAccessFlag]
readAccessFlags = do
    flags <- getWord16be
    return $! toClassAccessFlags flags

-- | Get the name of this class
readThisClass :: [ConstantInfo] -> Get T.Text
readThisClass constPool = do
    index <- getWord16be
    let classNameIndex = nameIndex . info $! constPool !@ (index - 1)
        className = string . info $! constPool !@ classNameIndex
    return className

-- | Get the name of the super class
readSuperClass :: [ConstantInfo] -> Get (Maybe T.Text)
readSuperClass constPool = do
    index <- getWord16be
    if index == 0
        then return Nothing
        else (return . Just) $!
             let classNameIndex = nameIndex . info $! constPool !@ (index - 1)
              in string . info $! constPool !@ classNameIndex

-- | Interfaces and interface count
readInterfaces :: Get [Word16]
readInterfaces = do
    interfacesCount <- getWord16be
    forM [1 .. interfacesCount] (\_ -> pure pred <*> getWord16be)

codeParser :: Get AInfo
-- returns a AIParsedCode 
codeParser = do
    maxStack :: Int <- pure fromIntegral <*> getWord16be
    maxLocals :: Int <- pure fromIntegral <*> getWord16be
    codeLength :: Int <- pure fromIntegral <*> getWord32be
    code :: [Word8] <- replicateM codeLength getWord8
        --traceM ("codeParser, it has code :: " ++ show code)
    return $!
        AIParsedCode
            maxStack
            maxLocals
            codeLength
            (runGet readInstructions (BL.pack code))

getStringFromConstPool constPool x = (string . info) $ constPool !@ (x - 1)

readParameter :: [ConstantInfo] -> Get MethodParameter
readParameter constPool = do
    name :: T.Text <- pure (getStringFromConstPool constPool) <*> getWord16be
    accessFlags <- pure toMethodParameterAccessFlags <*> getWord16be
    return $! MethodParameter name accessFlags

methodParametersParser :: [ConstantInfo] -> Get AInfo
-- returns the parsed methodParameters
methodParametersParser constPool = do
    paramCount :: Int <- pure fromIntegral <*> getWord8
    params :: [MethodParameter] <-
        replicateM paramCount (readParameter constPool)
    return $! AIParsedMethodParameters params

-- | Parse attributes for which there are special methods
parseParseableAttribute :: [ConstantInfo] -> AttributeType -> [Word8] -> AInfo
parseParseableAttribute constPool attrType bytes =
    case attrType of
        ATCode -> runGet codeParser (BL.pack bytes)
        ATMethodParameters ->
            runGet (methodParametersParser constPool) (BL.pack bytes)
        _ -> AIDummy -- added just so that this typechecks 

-- | Parse the raw bytes info for attributes if possible
parseAttribute :: [ConstantInfo] -> AttributeType -> [Word8] -> AInfo
parseAttribute constPool attributeType byteInfo =
    case attributeType of
        ATConstantValue ->
            if attributeType `elem` parseable
                then parseParseableAttribute constPool attributeType byteInfo
                else AIConstantValue byteInfo
        ATCode ->
            if attributeType `elem` parseable
                then parseParseableAttribute constPool attributeType byteInfo
                else AICode byteInfo
        ATStackMapTable ->
            if attributeType `elem` parseable
                then parseParseableAttribute constPool attributeType byteInfo
                else AIStackMapTable byteInfo
        ATExceptions ->
            if attributeType `elem` parseable
                then parseParseableAttribute constPool attributeType byteInfo
                else AIExceptions byteInfo
        ATInnerClasses ->
            if attributeType `elem` parseable
                then parseParseableAttribute constPool attributeType byteInfo
                else AIInnerClasses byteInfo
        ATEnclosingMethod ->
            if attributeType `elem` parseable
                then parseParseableAttribute constPool attributeType byteInfo
                else AIEnclosingSingleMethod byteInfo
        ATSynthetic ->
            if attributeType `elem` parseable
                then parseParseableAttribute constPool attributeType byteInfo
                else AISynthetic byteInfo
        ATSignature ->
            if attributeType `elem` parseable
                then parseParseableAttribute constPool attributeType byteInfo
                else AISignature byteInfo
        ATSourceFile ->
            if attributeType `elem` parseable
                then parseParseableAttribute constPool attributeType byteInfo
                else AISourceFile byteInfo
        ATSourceDebugExtension ->
            if attributeType `elem` parseable
                then parseParseableAttribute constPool attributeType byteInfo
                else AISourceDebugExtension byteInfo
        ATLineNumberTable ->
            if attributeType `elem` parseable
                then parseParseableAttribute constPool attributeType byteInfo
                else AILineNumberTable byteInfo
        ATLocalVariableTable ->
            if attributeType `elem` parseable
                then parseParseableAttribute constPool attributeType byteInfo
                else AILocalVariableTable byteInfo
        ATLocalVariableTypeTable ->
            if attributeType `elem` parseable
                then parseParseableAttribute constPool attributeType byteInfo
                else AILocalVariableTypeTable byteInfo
        ATDeprecated ->
            if attributeType `elem` parseable
                then parseParseableAttribute constPool attributeType byteInfo
                else AIDeprecated byteInfo
        ATRuntimeVisibleAnnotations ->
            if attributeType `elem` parseable
                then parseParseableAttribute constPool attributeType byteInfo
                else AIRuntimeVisibleAnnotations byteInfo
        ATRuntimeInvisibleAnnotations ->
            if attributeType `elem` parseable
                then parseParseableAttribute constPool attributeType byteInfo
                else AIRuntimeInvisibleAnnotations byteInfo
        ATRuntimeVisibleParameterAnnotations ->
            if attributeType `elem` parseable
                then parseParseableAttribute constPool attributeType byteInfo
                else AIRuntimeVisibleParameterAnnotations byteInfo
        ATRuntimeInvisibleParameterAnnotations ->
            if attributeType `elem` parseable
                then parseParseableAttribute constPool attributeType byteInfo
                else AIRuntimeInvisibleParameterAnnotations byteInfo
        ATRuntimeVisibleTypeAnnotations ->
            if attributeType `elem` parseable
                then parseParseableAttribute constPool attributeType byteInfo
                else AIRuntimeVisibleTypeAnnotations byteInfo
        ATRuntimeInvisibleTypeAnnotations ->
            if attributeType `elem` parseable
                then parseParseableAttribute constPool attributeType byteInfo
                else AIRuntimeInvisibleTypeAnnotations byteInfo
        ATAnnotationDefault ->
            if attributeType `elem` parseable
                then parseParseableAttribute constPool attributeType byteInfo
                else AIAnnotationDefault byteInfo
        ATBootstrapMethods ->
            if attributeType `elem` parseable
                then parseParseableAttribute constPool attributeType byteInfo
                else AIBootstrapMethods byteInfo
        ATMethodParameters ->
            if attributeType `elem` parseable
                then parseParseableAttribute constPool attributeType byteInfo
                else AIMethodParameters byteInfo

readAttribute :: [ConstantInfo] -> Get AttributeInfo
readAttribute constPool = do
    attributeNameIndex <- getWord16be
    let attributeName =
            string . info $!
            (constPool !@ (attributeNameIndex - 1) :: ConstantInfo)
        attributeType = toAttributeType attributeName
    case attributeType of
        Left errorMessage -> error errorMessage
        Right attrType -> do
            attributeLength :: Int <- pure fromIntegral <*> getWord32be
            byteInfo <- getRawBytes attributeLength
            return $!
                AttributeInfo
                    attrType
                    (parseAttribute constPool attrType byteInfo)

-- | Read a field
readFieldInfo :: [ConstantInfo] -> Get FieldInfo
readFieldInfo pool = do
    accessFlags :: [FieldAccessFlag] <-
        pure toFieldAccessFlags <*> getWord16be
    name :: T.Text <- pure (getStringFromConstPool pool) <*> getWord16be
    descriptor :: T.Text <-
        pure (getStringFromConstPool pool) <*> getWord16be
    attributeCount :: Int <- pure fromIntegral <*> getWord16be
    attributes :: [AttributeInfo] <-
        replicateM attributeCount (readAttribute pool)
    return $! FieldInfo accessFlags name descriptor attributes

-- | Reads the fields from the bytecode
readFields :: [ConstantInfo] -> Get [FieldInfo]
readFields pool = do
    fieldCount :: Int <- pure fromIntegral <*> getWord16be
    replicateM fieldCount (readFieldInfo pool)

-- | Read a method
readMethodInfo :: [ConstantInfo] -> Get MethodInfo
readMethodInfo pool = do
    accessFlags :: [MethodAccessFlag] <-
        pure toMethodAccessFlags <*> getWord16be
    name :: T.Text <- pure (getStringFromConstPool pool) <*> getWord16be
    descriptor :: T.Text <-
        pure (getStringFromConstPool pool) <*> getWord16be
    attributeCount :: Int <- pure fromIntegral <*> getWord16be
        --traceM ("name: " ++ name ++ "###################### Attributes ######################### " ++ show attributeCount ++ "\n\n\n\n")
    attributes :: [AttributeInfo] <-
        replicateM attributeCount (readAttribute pool)
    return $!
        MethodInfo
            accessFlags
            name
            (descriptorIndices descriptor)
            descriptor
            attributes

-- | Read the methods from the bytecode
readMethods :: [ConstantInfo] -> Get [MethodInfo]
readMethods pool = do
    methodCount :: Int <- pure fromIntegral <*> getWord16be
    replicateM methodCount (readMethodInfo pool)

-- | The main reader. This calls many other other sub readers, and produces a RawClassFile structure
reader :: Get RawClassFile
reader = do
    magic <- readMagicNumber
    (minor, major) <- readVersions
    constPool <- readConstantPool
    accessFlags <- readAccessFlags
    thisClass <- readThisClass constPool
    superClass <- readSuperClass constPool
    interfaces <- readInterfaces
    fields <- readFields constPool
    methods <- readMethods constPool
    return $!
        RawClassFile
            magic
            minor
            major
            constPool
            accessFlags
            thisClass
            superClass
            interfaces
            fields
            methods

{--
infoAboutClass ::
       ClassName -> Either Error RawClassFile -> Either Error RawClassFile
infoAboutClass path (Left error) = Left ("In class: " ++ path ++ ", " ++ error)
infoAboutClass _ (Right x) = Right x
--}

-- | Read from byte-string
readRawByteString :: BL.ByteString -> RawClassFile
readRawByteString !bs = runGet reader bs

-- | Reads the class file and forms a parsed RawClassFile structure
readRawClassFile :: ClassName -> IO RawClassFile
readRawClassFile className = do
    let unpacked = T.unpack className
    debugLoggerM $ "Reading " ++ unpacked
    classFileStream <- getClassFileStream className
    let result = readRawByteString classFileStream
    debugLoggerM "Completed"
    return result
