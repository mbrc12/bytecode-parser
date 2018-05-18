{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, ScopedTypeVariables #-}

module ByteCodeParser.Reader (
        readRawClassFile
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Debug.Trace(trace)
import Data.Binary
import Data.Binary.Get (Get, runGet, getWord8, getWord16be, getWord32be)
import System.IO (FilePath, Handle, IOMode, withFile, hGetContents)
import Data.Word (Word8, Word16, Word32)
import Data.Either
import Data.Char (chr)
import Control.Monad (when, forM, replicateM)
import Control.Applicative (pure, (<*>))
import Control.Monad.Trans.Except (ExceptT, runExceptT, except, throwE)
import Control.Monad.Trans.Class  (lift)
import ByteCodeParser.BasicTypes (
        (!@),
        mAGIC, ClassName, 
        getClassFilePath, RawClassFile(..), 
        Error, produceError,
        MajorVersion(..), toMajorVersion,
        ConstType(..), toConstType, 
        ConstantInfo(..), CInfo(..),
        ReferenceKind(..), toReferenceKind,
        AccessFlag(..), toAccessFlags,
        parseable, AttributeType(..),
        AInfo(..), AttributeInfo(..),
        toAttributeType, parseable,
        FieldInfo(..), MethodInfo(..),
        MethodParameter(..), CodeAtom)

import ByteCodeParser.Instructions (
        readInstructions)

-- | Gives the Lazy ByteString stream of the input from the class file
getClassFileStream :: ClassName                 -- ^ The input class
                   -> IO BL.ByteString          -- ^ The output bytestring stream, wrapped in IO
getClassFileStream className = 
        pure BL.fromStrict <*> B.readFile classFilePath -- strict reading
        where classFilePath = getClassFilePath className

getRawBytes :: Int -> ExceptT Error Get [Word8]
getRawBytes how_many = forM [1 .. how_many] (\_ -> lift getWord8) 

-- | Reads the magic number, and checks if its okay.
readMagicNumber :: ExceptT Error Get Word32
readMagicNumber = do 
        magic <- lift getWord32be
        if magic == mAGIC
           then return magic
           else throwE $ produceError "Magic Number is incorrect."

-- Read the major and minor versions and return
readVersions :: ExceptT Error Get (Word16, MajorVersion)
readVersions = do
        minor <- lift getWord16be
        major <- lift getWord16be
        let maybeMajorVersion = toMajorVersion major
        case maybeMajorVersion of
          Right majorVersion -> return (minor, majorVersion)
          Left  errorMessage -> throwE errorMessage


baseType        = ['B', 'C', 'D', 'F', 'I', 'J', 'S', 'Z'] :: [Char]
twoPlaces       = ['J', 'D'] :: [Char]  -- base types that take up two consecutive locations on the stack
objType         = 'L'
arrayType       = '['


-- | Given a method descriptor this function will return the positions in the local variable array which are of parameters, 
-- | and if the indexed parameter is a reference (Object/Array), the second argument is true
descriptorIndices :: String -> [(Int, Bool)]
descriptorIndices descriptor = recursiveCalc desc2 1
                                where
                                        desc2 = takeWhile (')' /=) $ drop 1 descriptor          -- Convert (xxx)yyy -> xxx
                                        
                                        recursiveCalc :: String -> Int -> [(Int, Bool)]
                                        recursiveCalc ("") x = []
                                        recursiveCalc (c:left) x = if c `elem` baseType
                                                                    then if c `elem` twoPlaces
                                                                            then (x, False) : recursiveCalc left (x + 2)
                                                                            else (x, False) : recursiveCalc left (x + 1)
                                                                    else if c == objType
                                                                            then (x, True) : recursiveCalc (drop 1 $ dropWhile (';' /=) left) (x + 1)
                                                                            else if c == arrayType
                                                                                    then let arrTypeAnd = dropWhile ('[' ==) left
                                                                                             arrType    = head arrTypeAnd
                                                                                         in if arrType == objType
                                                                                               then (x, True) : recursiveCalc (drop 1 $ dropWhile (';' /=) arrTypeAnd) (x + 1)
                                                                                               else (x, True) : recursiveCalc (drop 1 arrTypeAnd) (x + 1)
                                                                                    else []

                                                                         


-- Read a Constant from the pool, see 'readConstantPool'
readConstFromPool :: ExceptT Error Get (ConstantInfo, Int)
readConstFromPool = do
        tag <- lift getWord8
        constType <- case toConstType tag of
                       Right cType      -> return cType
                       Left  err        -> throwE err
        
        cinfo <- case constType of 
                        CClass          -> do   
                                                nameIndex <- lift getWord16be
                                                return $ CClassI (nameIndex - 1)
                        CFieldRef       -> do
                                                classIndex <- lift getWord16be
                                                nameAndTypeIndex <- lift getWord16be
                                                return $ CFieldRefI (classIndex - 1) (nameAndTypeIndex - 1)
                        CMethodRef      -> do
                                                classIndex <- lift getWord16be
                                                nameAndTypeIndex <- lift getWord16be
                                                return $ CMethodRefI (classIndex - 1) (nameAndTypeIndex - 1)
                        CInterfaceMethodRef 
                                        -> do 
                                                classIndex <- lift getWord16be
                                                nameAndTypeIndex <- lift getWord16be
                                                return $ CInterfaceMethodRefI (classIndex - 1) (nameAndTypeIndex - 1)
                        CString         -> do 
                                                stringIndex <- lift getWord16be
                                                return $ CStringI (stringIndex - 1)
                        CInteger        -> do
                                                bytei <- lift getWord32be
                                                return $ CIntegerI bytei
                        CFloat          -> do 
                                                bytef <- lift getWord32be
                                                return $ CFloatI bytef
                        CLong           -> do
                                                high <- lift getWord32be
                                                low <- lift getWord32be
                                                return $ CLongI high low
                        CDouble         -> do
                                                high <- lift getWord32be
                                                low <- lift getWord32be
                                                return $ CDoubleI high low
                        CNameAndType    -> do
                                                nameIndex <- lift getWord16be
                                                descriptorIndex <- lift getWord16be
                                                return $ CNameAndTypeI (nameIndex - 1) (descriptorIndex - 1)
                        CUtf8           -> do
                                                len <- lift getWord16be
                                                bytes <- lift $ getBytes len
                                                return $ CUtf8I len bytes
                        CMethodHandle   -> do
                                                refkind <- lift getWord8
                                                referenceIndex <- lift getWord16be
                                                referenceKind <- case toReferenceKind refkind of
                                                                   Right refKind        -> return refKind
                                                                   Left  err            -> throwE err
                                                    
                                                return $ CMethodHandleI referenceKind (referenceIndex - 1)
                        CMethodType     -> do
                                                descriptorIndex <- lift getWord16be
                                                return $ CMethodTypeI (descriptorIndex - 1)
                        CInvokeDynamic  -> do                                                  
                                                bootstrapMethodAttrIndex <- lift getWord16be
                                                nameAndTypeIndex <- lift getWord16be
                                                return $ CInvokeDynamicI (bootstrapMethodAttrIndex - 1) (nameAndTypeIndex - 1)

        return (ConstantInfo constType cinfo, if constType == CLong || constType == CDouble then 2 else 1)
        
        where
                -- | getBytes gets len bytes from the input
                getBytes :: Word16 -> Get String
                getBytes len =  (fmap.fmap) (chr.fromIntegral) $ forM [1..len] $ const getWord8  

-- | Read the Constant Pool
readConstantPool :: ExceptT Error Get [ConstantInfo]
readConstantPool = do
        cpsize <- lift getWord16be
        when (cpsize == 0) $ throwE $ produceError "Constant pool size is 0, should be atleast 1."

        -- cpsize - 1 because of ConstantPool size convention
        
        readPool $ fromIntegral (cpsize - 1)
        
        where
                -- reads elements from the pool according to their byte size, specifically, CLong and CDouble are 2 places each
                -- Note that this duplicates the same value for fields which take up two consecutive locations in the constant pool
                -- like longs and doubles to keep the constantPool indices okay.
                readPool :: Int -> ExceptT Error Get [ConstantInfo]
                readPool rem = if rem == 0 
                                  then return []
                                  else do 
                                          (c_elem, c_positions) <- readConstFromPool
                                          if c_positions == 1 
                                             then pure (c_elem :) <*> readPool (rem - c_positions)
                                             else pure ([c_elem, c_elem] ++) <*> readPool (rem - c_positions)
                                                                

-- | Reads the access flags
readAccessFlags :: ExceptT Error Get [AccessFlag]
readAccessFlags = do
        flags <- lift getWord16be
        return $ toAccessFlags flags


-- | Get the name of this class
readThisClass :: [ConstantInfo] -> ExceptT Error Get String
readThisClass constPool = do
        index <- lift getWord16be
        let classNameIndex = nameIndex.info $ constPool!@(index - 1)
            className = string.info $ constPool!@classNameIndex
        return className

-- | Get the name of the super class
readSuperClass :: [ConstantInfo] -> ExceptT Error Get (Maybe String)
readSuperClass constPool = do
        index <- lift getWord16be
        if index == 0 
           then return Nothing
           else (return.Just) $
                   let classNameIndex = nameIndex.info $ constPool!@(index - 1)
                    in string.info $ constPool!@classNameIndex

-- | Interfaces and interface count
readInterfaces :: ExceptT Error Get [Word16]
readInterfaces = do
        interfacesCount <- lift getWord16be
        forM [1..interfacesCount] (\_ -> pure pred <*> lift getWord16be) 



codeParser :: Get AInfo
-- returns a AIParsedCode 
codeParser = do
        maxStack :: Int         <- pure fromIntegral <*> getWord16be
        maxLocals :: Int        <- pure fromIntegral <*> getWord16be
        codeLength :: Int       <- pure fromIntegral <*> getWord32be
        code :: [Word8]         <- replicateM codeLength getWord8
        return $ AIParsedCode maxStack maxLocals codeLength (runGet readInstructions (BL.pack code))

getStringFromConstPool constPool x = (string.info) $ constPool !@ (x - 1)

readParameter :: [ConstantInfo] -> Get MethodParameter
readParameter constPool = do
        name :: String <- pure (getStringFromConstPool constPool) <*> getWord16be
        accessFlags    <- pure toAccessFlags <*> getWord16be
        return $ MethodParameter name accessFlags

methodParametersParser :: [ConstantInfo] -> Get AInfo
-- returns the parsed methodParameters
methodParametersParser constPool = do
        paramCount :: Int               <- pure fromIntegral <*> getWord8
        params     :: [MethodParameter] <- replicateM paramCount (readParameter constPool)
        return $ AIParsedMethodParameters params

-- | Parse attributes for which there are special methods
parseParseableAttribute :: [ConstantInfo] -> AttributeType -> [Word8] -> AInfo
parseParseableAttribute constPool attrType bytes =
        case attrType of
                ATCode                                  -> runGet codeParser (BL.pack bytes)

                ATMethodParameters                      -> runGet (methodParametersParser constPool) (BL.pack bytes)

                _                                       -> AIDummy                               -- added just so that this typechecks 
                
-- | Parse the raw bytes info for attributes if possible
parseAttribute :: [ConstantInfo] -> AttributeType -> [Word8] -> AInfo
parseAttribute constPool attributeType byteInfo =
        case attributeType of 
                ATConstantValue                         -> if attributeType `elem` parseable
                                                              then parseParseableAttribute constPool attributeType byteInfo
                                                              else AIConstantValue byteInfo
                ATCode                                  -> if attributeType `elem` parseable
                                                              then parseParseableAttribute constPool attributeType byteInfo
                                                              else AICode byteInfo
                ATStackMapTable                         -> if attributeType `elem` parseable
                                                              then parseParseableAttribute constPool attributeType byteInfo
                                                              else AIStackMapTable byteInfo
                ATExceptions                            -> if attributeType `elem` parseable
                                                              then parseParseableAttribute constPool attributeType byteInfo
                                                              else AIExceptions byteInfo
                ATInnerClasses                          -> if attributeType `elem` parseable
                                                              then parseParseableAttribute constPool attributeType byteInfo
                                                              else AIInnerClasses byteInfo
                ATEnclosingMethod                       -> if attributeType `elem` parseable
                                                              then parseParseableAttribute constPool attributeType byteInfo
                                                              else AIEnclosingSingleMethod byteInfo
                ATSynthetic                             -> if attributeType `elem` parseable
                                                              then parseParseableAttribute constPool attributeType byteInfo
                                                              else AISynthetic byteInfo
                ATSignature                             -> if attributeType `elem` parseable
                                                              then parseParseableAttribute constPool attributeType byteInfo
                                                              else AISignature byteInfo
                ATSourceFile                            -> if attributeType `elem` parseable
                                                              then parseParseableAttribute constPool attributeType byteInfo
                                                              else AISourceFile byteInfo
                ATSourceDebugExtension                  -> if attributeType `elem` parseable
                                                              then parseParseableAttribute constPool attributeType byteInfo
                                                              else AISourceDebugExtension byteInfo
                ATLineNumberTable                       -> if attributeType `elem` parseable
                                                              then parseParseableAttribute constPool attributeType byteInfo
                                                              else AILineNumberTable byteInfo
                ATLocalVariableTable                    -> if attributeType `elem` parseable
                                                              then parseParseableAttribute constPool attributeType byteInfo
                                                              else AILocalVariableTable byteInfo
                ATLocalVariableTypeTable                -> if attributeType `elem` parseable
                                                              then parseParseableAttribute constPool attributeType byteInfo
                                                              else AILocalVariableTypeTable byteInfo
                ATDeprecated                            -> if attributeType `elem` parseable
                                                              then parseParseableAttribute constPool attributeType byteInfo
                                                              else AIDeprecated byteInfo
                ATRuntimeVisibleAnnotations             -> if attributeType `elem` parseable
                                                              then parseParseableAttribute constPool attributeType byteInfo
                                                              else AIRuntimeVisibleAnnotations byteInfo
                ATRuntimeInvisibleAnnotations           -> if attributeType `elem` parseable
                                                              then parseParseableAttribute constPool attributeType byteInfo
                                                              else AIRuntimeInvisibleAnnotations byteInfo
                ATRuntimeVisibleParameterAnnotations    -> if attributeType `elem` parseable 
                                                              then parseParseableAttribute constPool attributeType byteInfo
                                                              else AIRuntimeVisibleParameterAnnotations byteInfo
                ATRuntimeInvisibleParameterAnnotations  -> if attributeType `elem` parseable
                                                              then parseParseableAttribute constPool attributeType byteInfo
                                                              else AIRuntimeInvisibleParameterAnnotations byteInfo
                ATRuntimeVisibleTypeAnnotations         -> if attributeType `elem` parseable
                                                              then parseParseableAttribute constPool attributeType byteInfo
                                                              else AIRuntimeVisibleTypeAnnotations byteInfo
                ATRuntimeInvisibleTypeAnnotations       -> if attributeType `elem` parseable
                                                              then parseParseableAttribute constPool attributeType byteInfo
                                                              else AIRuntimeInvisibleTypeAnnotations byteInfo
                ATAnnotationDefault                     -> if attributeType `elem` parseable
                                                              then parseParseableAttribute constPool attributeType byteInfo
                                                              else AIAnnotationDefault byteInfo
                ATBootstrapMethods                      -> if attributeType `elem` parseable
                                                              then parseParseableAttribute constPool attributeType byteInfo
                                                              else AIBootstrapMethods byteInfo
                ATMethodParameters                      -> if attributeType `elem` parseable
                                                              then parseParseableAttribute constPool attributeType byteInfo
                                                              else AIMethodParameters byteInfo

readAttribute :: [ConstantInfo] -> ExceptT Error Get AttributeInfo
readAttribute constPool = do
        attributeNameIndex <- lift getWord16be
        
        let attributeName = string.info $ (constPool !@ (attributeNameIndex - 1) :: ConstantInfo) 
            attributeType = toAttributeType attributeName

        case attributeType of 
                Left errorMessage       -> throwE errorMessage
                Right attrType          -> do 
                                                attributeLength :: Int  <- pure fromIntegral <*> lift getWord32be
                                                byteInfo <- getRawBytes attributeLength
                                                return $ AttributeInfo attrType (parseAttribute constPool attrType byteInfo)
                                                
-- | Read a field
readFieldInfo :: [ConstantInfo] -> ExceptT Error Get FieldInfo
readFieldInfo pool = do
        accessFlags :: [AccessFlag]     <- pure toAccessFlags <*> lift getWord16be
        name :: String                  <- pure (getStringFromConstPool pool) <*> lift getWord16be
        descriptor :: String            <- pure (getStringFromConstPool pool) <*> lift getWord16be
        attributeCount :: Int           <- pure fromIntegral <*> lift getWord16be
        attributes :: [AttributeInfo]   <- replicateM attributeCount (readAttribute pool) 
        return $ FieldInfo accessFlags name descriptor attributes        

-- | Reads the fields from the bytecode
readFields :: [ConstantInfo] -> ExceptT Error Get [FieldInfo]
readFields pool = do
        fieldCount :: Int <- pure fromIntegral <*> lift getWord16be
        replicateM fieldCount (readFieldInfo pool)
       

-- | Read a method
readMethodInfo :: [ConstantInfo] -> ExceptT Error Get MethodInfo
readMethodInfo pool = do
        accessFlags :: [AccessFlag]     <- pure toAccessFlags <*> lift getWord16be
        name :: String                  <- pure (getStringFromConstPool pool) <*> lift getWord16be
        descriptor :: String            <- pure (getStringFromConstPool pool) <*> lift getWord16be
        attributeCount :: Int           <- pure fromIntegral <*> lift getWord16be
        attributes :: [AttributeInfo]   <- replicateM attributeCount (readAttribute pool) 
        trace ("Method: "++name++" AttributeC: "++show attributeCount) $ return $ MethodInfo accessFlags name (descriptorIndices descriptor) descriptor attributes 

-- | Read the methods from the bytecode
readMethods :: [ConstantInfo] -> ExceptT Error Get [MethodInfo]
readMethods pool = do
        methodCount :: Int <- pure fromIntegral <*> lift getWord16be 
        replicateM methodCount (readMethodInfo pool)

-- | The main reader. This calls many other other sub readers, and produces a RawClassFile structure
reader :: ExceptT Error Get RawClassFile
reader = do  
        magic           <- readMagicNumber
        (minor, major)  <- readVersions
        constPool       <- readConstantPool
        accessFlags     <- readAccessFlags
        thisClass       <- readThisClass constPool
        superClass      <- readSuperClass constPool
        interfaces      <- readInterfaces
        fields          <- readFields constPool
        methods         <- readMethods constPool
        return $ 
                RawClassFile    magic 
                                minor 
                                major 
                                constPool
                                accessFlags
                                thisClass
                                superClass
                                interfaces
                                fields
                                methods

-- | Reads the class file and forms a parsed RawClassFile structure
readRawClassFile :: ClassName
                 -> IO (Either Error RawClassFile)
readRawClassFile className = do
        classFileStream <- getClassFileStream className
        return $ runGet (runExceptT reader)  classFileStream

