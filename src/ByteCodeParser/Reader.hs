{-# LANGUAGE OverloadedStrings #-}

module ByteCodeParser.Reader (
        readRawClassFile
) where

import qualified Data.ByteString.Lazy as BL
import Data.Binary
import Data.Binary.Get (Get, runGet, getWord8, getWord16be, getWord32be)
import System.IO (FilePath, Handle, IOMode, withFile, hGetContents)
import Data.Word (Word8, Word16, Word32)
import Data.Either
import Data.Char (chr)
import Control.Monad (when, forM)
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
        AccessFlag(..), toAccessFlags)

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

-- Read a Constant from the pool, see 'readConstantPool'
readConstFromPool :: ExceptT Error Get ConstantInfo
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

        return $ ConstantInfo constType cinfo
        
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
        forM [1..cpsize - 1] $ const readConstFromPool

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
            className = bytes.info $ constPool!@classNameIndex
        return className

-- | Get the name of the super class
readSuperClass :: [ConstantInfo] -> ExceptT Error Get (Maybe String)
readSuperClass constPool = do
        index <- lift getWord16be
        if index == 0 
           then return Nothing
           else (return.Just) $
                   let classNameIndex = nameIndex.info $ constPool!@(index - 1)
                    in bytes.info $ constPool!@classNameIndex

-- | Interfaces and interface count
readInterfaces :: ExceptT Error Get [Word16]
readInterfaces = do
        interfacesCount <- lift getWord16be
        forM [1..interfacesCount] (\_ -> pure pred <*> lift getWord16be) 


-- | Get all the fields
readFields :: ExceptT Error Get [FieldInfo]
readFields 

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
        return $ 
                RawClassFile    magic 
                                minor 
                                major 
                                constPool
                                accessFlags
                                thisClass
                                superClass
                                interfaces

-- | Reads the class file and forms a parsed RawClassFile structure
readRawClassFile :: ClassName
                 -> IO (Either Error RawClassFile)
readRawClassFile className = do
        classFileStream <- getClassFileStream className
        return $ runGet (runExceptT reader) classFileStream

