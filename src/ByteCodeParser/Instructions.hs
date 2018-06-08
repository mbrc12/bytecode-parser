{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, ScopedTypeVariables#-}

module ByteCodeParser.Instructions (
        readInstructions,
        opIfs,
        opIfNonNull,
        opIfNull,
        opLookupSwitch,
        opTableSwitch,
        opGoto,
        opGotoW,
        opReturns,
        opAConstNull,
        opGetField,
        opPutField,
        opGetStatic,
        opPutStatic
) where
import Data.Int (Int32)
import Debug.Trace (trace, traceM)
import Data.Word (Word8, Word16, Word32)
import Data.List (zip)
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import Control.Monad 
import ByteCodeParser.BasicTypes (
        CInfo, CodeAtom) 

opPutField, opIfNonNull, opIfNull, opLookupSwitch, opTableSwitch, opGoto, opGotoW, opAConstNull, opGetField, opGetStatic :: Word8
opIfs, opReturns :: [Word8]

opAConstNull = 1
opIfs = [165, 166, 159, 160, 161, 162, 163, 164, 153, 154, 155, 156, 157, 158, 199]
opIfNonNull = 199
opIfNull = 198
opLookupSwitch = 171
opTableSwitch = 170
opGoto = 167
opGotoW = 200
opGetField = 180
opPutField = 181
opGetStatic = 178
opPutStatic = 179
opReturns = [176, 175, 174, 172, 173, 169, 177]  
 
readInstructions :: Get [CodeAtom]
readInstructions = readInstructionsIntoWords 0

-- | Given current position returns how many to read to go to a multiple of 4
toFour :: Int -> Int
toFour x = let p = x `rem` 4 
            in 3 - p

-- | Convert [x1, x2, x3, x4] -> x1 << 24 + x2 << 16 + x3 << 8 + x4
-- | uses Int32 to get the effect of signed-ness.
convertToInt :: [Word8] -> Int
convertToInt xs = let signedval :: Int32 = sum $ map (\(x, y) -> x * y) $ zip [2^24, 2^16, 2^8, 1] $ map fromIntegral xs
                  in fromIntegral signedval

-- | Given a bytestring, this reads the instructions and organizes them, so that each element consists of
-- | its opcode and the arguments passes to it. Assumed valid bytecode, so no error
readInstructionsIntoWords :: Int -> Get [CodeAtom] 
readInstructionsIntoWords pos = do
        empty <- isEmpty
        if empty
           then return []
           else do
                   (instruction, pos') <- readInstruction pos
                   others <- readInstructionsIntoWords pos'
                   return (instruction : others)

readInstruction :: Int -> Get (CodeAtom, Int) 
readInstruction pos = do
        opcode <- getWord8
        let howManyMore = consumeBytes opcode
        if howManyMore == -1
                then case opcode of
                        196             -> do                                   -- wide
                                                op <- getWord8
                                                if op == 132    -- iinc
                                                        then do
                                                                others <- replicateM 4 getWord8
                                                                return ((pos, [opcode, op] ++ others), pos + 6)
                                                        else do
                                                                others <- replicateM 2 getWord8
                                                                return ((pos, [opcode, op] ++ others), pos + 4)
                        170             -> do                                   -- tableswitch
                                                let padding = toFour pos
                                                pad <- replicateM_ padding getWord8
                                                [d1, d2, d3, d4] <- replicateM 4 getWord8
                                                [l1, l2, l3, l4] <- replicateM 4 getWord8
                                                [h1, h2, h3, h4] <- replicateM 4 getWord8
                                                let low = convertToInt [l1, l2, l3, l4]
                                                    high = convertToInt [h1, h2, h3, h4]
                                                    toRead = (high - low + 1) * 4
                                                --traceM $ "The low is : " ++ show low
                                                --traceM $ "The high is : " ++ show high
                                                manyMore <- replicateM toRead getWord8
                                                return ((pos, [opcode, d1, d2, d3, d4, l1, l2, l3, l4, h1, h2, h3, h4] ++ manyMore), pos + 1 + padding + 12 + toRead)
                        171             -> do                                   -- lookupswitch
                                                let padding = toFour pos
                                                 
                                                replicateM_ padding getWord8
                                                [d1, d2, d3, d4] <- replicateM 4 getWord8
                                                [n1, n2, n3, n4] <- replicateM 4 getWord8
                                                let nPairs = convertToInt [n1, n2, n3, n4]
                                                    toRead = nPairs * 4 * 2
                                                manyMore <- replicateM toRead getWord8
                                                return ((pos, [opcode, d1, d2, d3, d4, n1, n2, n3, n4] ++ manyMore), pos + 1 + padding + 8 + toRead)
                else do
                        bytes <- replicateM howManyMore getWord8
                        return ((pos, opcode : bytes), pos + 1 + howManyMore)
                        
                
-- | assumes verified bytecodes, so no error
-- | returns the number of more bytes to be consumed by this instruction
consumeBytes :: Word8 -> Int
consumeBytes code = case code of 
                      50        -> 0
                      83        -> 0
                      1         -> 0
                      25        -> 1
                      42        -> 0
                      43        -> 0
                      44        -> 0
                      45        -> 0
                      189       -> 2
                      176       -> 0
                      190       -> 0
                      58        -> 1
                      75        -> 0
                      76        -> 0
                      77        -> 0
                      78        -> 0
                      191       -> 0
                      51        -> 0
                      84        -> 0
                      16        -> 1
                      52        -> 0
                      85        -> 0
                      192       -> 2
                      144       -> 0
                      142       -> 0
                      143       -> 0
                      99        -> 0
                      49        -> 0
                      82        -> 0
                      152       -> 0
                      151       -> 0
                      14        -> 0
                      15        -> 0
                      111       -> 0
                      24        -> 1
                      38        -> 0
                      39        -> 0
                      40        -> 0
                      41        -> 0
                      107       -> 0
                      119       -> 0
                      115       -> 0
                      175       -> 0
                      57        -> 1
                      71        -> 0
                      72        -> 0
                      73        -> 0
                      74        -> 0
                      103       -> 0
                      89        -> 0
                      90        -> 0
                      91        -> 0
                      92        -> 0
                      93        -> 0
                      94        -> 0
                      141       -> 0
                      139       -> 0
                      140       -> 0
                      98        -> 0
                      48        -> 0
                      81        -> 0
                      150       -> 0
                      149       -> 0
                      11        -> 0
                      12        -> 0
                      13        -> 0
                      110       -> 0
                      23        -> 1
                      34        -> 0
                      35        -> 0
                      36        -> 0
                      37        -> 0
                      106       -> 0
                      118       -> 0
                      114       -> 0
                      174       -> 0
                      56        -> 1
                      67        -> 0
                      68        -> 0
                      69        -> 0
                      70        -> 0
                      102       -> 0
                      180       -> 2
                      178       -> 2
                      167       -> 2
                      200       -> 4
                      145       -> 0
                      146       -> 0
                      135       -> 0
                      134       -> 0
                      133       -> 0
                      147       -> 0
                      96        -> 0
                      46        -> 0
                      126       -> 0
                      79        -> 0
                      2         -> 0
                      3         -> 0
                      4         -> 0
                      5         -> 0
                      6         -> 0
                      7         -> 0
                      8         -> 0
                      108       -> 0
                      165       -> 2
                      166       -> 2
                      159       -> 2
                      160       -> 2
                      161       -> 2
                      162       -> 2
                      163       -> 2
                      164       -> 2
                      153       -> 2
                      154       -> 2
                      155       -> 2
                      156       -> 2
                      157       -> 2
                      158       -> 2
                      199       -> 2
                      198       -> 2
                      132       -> 2
                      21        -> 1
                      26        -> 0
                      27        -> 0
                      28        -> 0
                      29        -> 0
                      104       -> 0
                      116       -> 0
                      193       -> 2
                      186       -> 4
                      185       -> 4
                      183       -> 2
                      184       -> 2
                      182       -> 2
                      128       -> 0
                      112       -> 0
                      172       -> 0
                      120       -> 0
                      122       -> 0
                      54        -> 1
                      59        -> 0
                      60        -> 0
                      61        -> 0
                      62        -> 0
                      100       -> 0
                      124       -> 0
                      130       -> 0
                      168       -> 2
                      201       -> 4
                      138       -> 0
                      137       -> 0
                      136       -> 0
                      97        -> 0
                      47        -> 0
                      127       -> 0
                      80        -> 0
                      148       -> 0
                      9         -> 0
                      10        -> 0
                      18        -> 1
                      19        -> 2
                      20        -> 2
                      109       -> 0
                      22        -> 1
                      30        -> 0
                      31        -> 0
                      32        -> 0
                      33        -> 0
                      105       -> 0
                      117       -> 0
                      171       -> -1 -- lookup switch : see code
                      129       -> 0
                      113       -> 0
                      173       -> 0
                      121       -> 0
                      123       -> 0
                      55        -> 1
                      63        -> 0
                      64        -> 0
                      65        -> 0
                      66        -> 0
                      101       -> 0
                      125       -> 0
                      131       -> 0
                      194       -> 0
                      195       -> 0
                      197       -> 3
                      187       -> 2
                      188       -> 1
                      0         -> 0
                      87        -> 0
                      88        -> 0
                      181       -> 2
                      179       -> 2
                      169       -> 1
                      177       -> 0
                      53        -> 0
                      86        -> 0
                      17        -> 2
                      95        -> 0
                      170       -> -1 -- table switch : see code
                      196       -> -1 -- wide : see code
                      _         -> -2 -- error 





{-
data Instruction = 
        Iaaload  |
        Iaastore |
        Iaconst_null |
        Iaload { index :: Int } |
        Ianewarray { objType :: CInfo } |
        Iareturn |
        Iarraylength |
        Iastore { index :: Int } |
        Iathrow |
        Ibaload |
        Ibastore |
        Ibipush { byte :: Int } |
        Icaload |
        Icastore |
        Icheckcast { objType :: CInfo } |
        Id2f |
        Id2i |
        Id2l |
        Idadd |
        Idaload |
        Idastore |
        Idcmpg |
        Idcmpl |
        Idconst { value :: Double }
        Iddiv
-}

