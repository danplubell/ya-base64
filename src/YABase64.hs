module YABase64 (encodeBase64,stringToByteString, decodeBase64) where

import qualified Data.ByteString as BC
import Data.Bits
import Data.Char
import Data.Word
import Data.List.Split
import Data.Array (Array,array, (!))


{-
Encodes a ByteString into a list of base64 encoded char values
convert the bytestring into a list of [Word8] values
convert the list of Word8 values into 3-Tuples
convert the 3-Tuples into integers based on the 24 bits in the 3-Tuple
convert the 24 bits into 6 bit chunks
convert the 6 bit chunks into integers
lookup the 6 bit chunks in the alphabet array an convert to a char
add the char to the list
pad the encoded string based on the base64 padding specifications

-}
encodeBase64 :: BC.ByteString -> String
encodeBase64 x = padEncodedString                       --pad the encoded string with the needed characters
                (padCharsNeeded (byteStringToList x)) $ --calculate the padding characters needed
                map (encodeArray!)                      --get the encoded character for the list of coded characters
                (concatMap                              --concat the list of lists of integers    
                    (buildIntList.chunksOfSixBool)      --chunk tuples into chunks of six bits represented by list of list of bools
                    (toTuples $ byteStringToList x))    --convert list of Word8 values into 3-tuples


--convert a ByteString to a list of Word8 values
byteStringToList:: BC.ByteString -> [Word8]
byteStringToList x = case BC.uncons x of
                        Just n -> fst n: byteStringToList (snd n)
                        Nothing -> []

--convert the list of Word8 values to 3-tuples
toTuples :: [Word8] -> [(Word8, Word8, Word8)]
toTuples  l = Prelude.map unSafeConvert (chunksOf 3 l) 
        where unSafeConvert [x,y,z] = (x,y,z)
              unSafeConvert [x,y] = (x, y, 0)
              unSafeConvert [x] = (x, 0, 0)
              unSafeConvert [] = (0,0,0)

--convert the Word8 3-tuple to a list of Bools based on chunks of 6 bits
chunksOfSixBool :: (Word8, Word8, Word8) -> [[Bool]]
chunksOfSixBool x = chunksOf 6 $ snd $ splitAt 40 $ bitListBool $ make24Bit x

--Build list of boolean values from a numeric value
bitListBool::(Bits a) => a -> [Bool]
bitListBool x = reverse [testBit x j | let k = bitSize x - 1, j <- [0..k]]

                                                     
--make a Word8 3-Tuple into an integer
make24Bit::(Word8,Word8,Word8) -> Int
make24Bit (x,y,z) = fromIntegral$(shiftL (fromIntegral x::Int) 16) + shiftL (fromIntegral y::Int) 8  + (fromIntegral z::Int)

--build list of integers from a list of lists of Bools
buildIntList::[[Bool]]->[Int]
buildIntList [] = []
buildIntList xs = map buildFromBool xs
                where buildFromBool = foldl (\base bool -> shiftL base 1 + if bool then 1 else 0) 0
buildWordList::[[Bool]]->[Word8]
buildWordList [] = []
buildWordList xs =  map buildFromBool xs
                where buildFromBool = foldl (\base bool -> shiftL base 1 + if bool then 1 else 0) 0

--get the number of pad characters that are needed
--the number of pad characters is based on the length of the source that is encoded to base64
padCharsNeeded :: [a] -> Int
padCharsNeeded source = let p = (length source `mod` 3) 
                in if p > 0 then 3 - p else 0

--pad the encoded string with the appropriate number of pad characters
padEncodedString :: Int -> String -> String
padEncodedString nbrOfPad target = take (length target - nbrOfPad  ) target ++ replicate nbrOfPad '=' 


encodeArray :: Array Int Char
encodeArray = array (0,65) 
          [ (0,'A'),  (1,'B'),  (2,'C'),  (3,'D'),  (4,'E'),  (5,'F')                    
          , (6,'G'),  (7,'H'),  (8,'I'),  (9,'J'),  (10,'K'), (11,'L')                    
          , (12,'M'), (13,'N'), (14,'O'), (15,'P'), (16,'Q'), (17,'R')
          , (18,'S'), (19,'T'), (20,'U'), (21,'V'), (22,'W'), (23,'X')
          , (24,'Y'), (25,'Z'), (26,'a'), (27,'b'), (28,'c'), (29,'d')
          , (30,'e'), (31,'f'), (32,'g'), (33,'h'), (34,'i'), (35,'j')
          , (36,'k'), (37,'l'), (38,'m'), (39,'n'), (40,'o'), (41,'p')
          , (42,'q'), (43,'r'), (44,'s'), (45,'t'), (46,'u'), (47,'v')
          , (48,'w'), (49,'x'), (50,'y'), (51,'z'), (52,'0'), (53,'1')
          , (54,'2'), (55,'3'), (56,'4'), (57,'5'), (58,'6'), (59,'7')
          , (60,'8'), (61,'9'), (62,'+'), (63,'/')]

--a sparse array with only the values for the base64 alphabet included
decodeArray:: Array Char Int
decodeArray = array ('\0','~')
          [('A',0),('B',1),('C',2),('D',3),('E',4),('F',5)
          ,('G',6),('H',7),('I',8),('J',9),('K',10),('L',11)
          ,('M',12),('N',13),('O',14),('P',15),('Q',16),('R',17)
          ,('S',18),('T',19),('U',20),('V',21),('W',22),('X',23)
          ,('Y',24),('Z',25),('a',26),('b',27),('c',28),('d',29)
          ,('e',30),('f',31),('g',32),('h',33),('i',34),('j',35)
          ,('k',36),('l',37),('m',38),('n',39),('o',40),('p',41)
          ,('q',42),('r',43),('s',44),('t',45),('u',46),('v',47)
          ,('w',48),('x',49),('y',50),('z',51),('0',52),('1',53)
          ,('2',54),('3',55),('4',56),('5',57),('6',58),('7',59)
          ,('8',60),('9',61),('+',62),('/',63), ('=',0)]

decodeBase64 ::String -> BC.ByteString
decodeBase64 x =   BC.pack 
                        $ buildWordList                                         --build the Word8 list from 8 bit chunks
                        $ takeWhile (\a -> length a == 8)                       --remove the trailing zeroes left over from padding
                        $ concatMap (chunksOf 8.concat)                         --concat and chunk into 8 bit chunks
                        $ chunksOf 4                                            --chunk the 6 bit parts into 24 bit chunks
                        $ map (snd.splitAt 58.bitListBool) (decodeToList        --decode the characters by alphabet, create bool list, split into 6 bit parts 
                        $ takeWhile (/= '=') x)                                 --remove padding characters

decodeToList :: String -> [Int]
decodeToList = map (\ x -> decodeArray ! x) 


stringToByteString :: String -> BC.ByteString
stringToByteString  x = BC.pack $ map (\c -> fromIntegral(ord c)::Word8) x