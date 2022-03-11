module Lib
    ( someFunc
    , translateMorse
    , Morse
    ) where

import Data.Maybe (fromMaybe)
import Data.WAVE
import Data.List
import qualified Data.Ord

someFunc :: IO ()
someFunc = do
    (WAVE (WAVEHeader channels framerate bits frames) samples) <- getWAVEFile "message.wav"
    let monoStream = concat samples
    let monoRms = rms monoStream
    let rmsSlices = map (round . rms) (nSlices 10000 monoStream)

    let quantizedSlices = quantize 1000 rmsSlices
    let onezero = map (\a -> if a`div` 5 > 2 then 1 else 0) quantizedSlices
    let filteredOZ = filterXs onezero
    let lengths = init . tail $ map (\a -> (head a, length a)) $ group filteredOZ
    let (signalLengths, silenceLengths) = partition ((==1) . fst) lengths
    let sigLens = sort . map snd $ signalLengths
    let silLens = sort . map snd $ silenceLengths
    let sigDist' = findBiggestLeapBetween2 sigLens
    let sigDist = leaps sigLens
    let sdLB = take 1 $ map snd . sortOn (Data.Ord.Down . fst) $ sigDist
    let daaMinLength = head sdLB

    let silDist = leaps silLens
    let sildLB = take 2 $ map snd . sortOn (Data.Ord.Down . fst) $ silDist
    let [wordSpaceMinLength, letterSpaceMinLength] = sildLB

    let morses = lengthsToMorses daaMinLength wordSpaceMinLength letterSpaceMinLength lengths
    putStrLn $ concatMap show $ filter (/=Space) morses
    putStrLn $ translateMorse morses

lengthsToMorses :: Int -> Int -> Int -> [(Int, Int)] -> [Morse]
lengthsToMorses sigLength wordSpaceMinLength letterSpaceMinLength [] = []
lengthsToMorses sigLength wordSpaceMinLength letterSpaceMinLength ((1,l):ls)
    | l < sigLength = Di : lengthsToMorses sigLength wordSpaceMinLength letterSpaceMinLength ls
    | otherwise = Daa : lengthsToMorses sigLength wordSpaceMinLength letterSpaceMinLength ls
lengthsToMorses sigLength wordSpaceMinLength letterSpaceMinLength ((0,l):ls)
    | l < letterSpaceMinLength = Space : lengthsToMorses sigLength wordSpaceMinLength letterSpaceMinLength ls
    | l < wordSpaceMinLength = LetterSpace : lengthsToMorses sigLength wordSpaceMinLength letterSpaceMinLength ls
    | otherwise = WordSpace : lengthsToMorses sigLength wordSpaceMinLength letterSpaceMinLength ls
lengthsToMorses _ _ _ _ = []

findBiggestLeapBetween2 :: [Int] -> (Int, Int)
findBiggestLeapBetween2 = findBiggestLeapBetween2' (0,0)
 where
  findBiggestLeapBetween2' biggest [] = (0,0)
  findBiggestLeapBetween2' biggest [x] = biggest
  findBiggestLeapBetween2' biggest (x:x':xs)
      | x'-x > fst biggest = findBiggestLeapBetween2' (x'-x, x) (x':xs)
      | otherwise = findBiggestLeapBetween2' biggest (x':xs)

leaps :: (Eq b, Num b) => [b] -> [(b, b)]
leaps (x:x':xs) = foldl (\a@((n,i):acc) nxt -> if nxt == i then a else (nxt-i, nxt):a) [(x-x',x')] xs
leaps _ = []

-- filterXs :: [a0] -> t
filterXs [a,b] = [a]
filterXs (a:b:c:xs)
    | a == c && a /= b = a : filterXs (a:c:xs)
    | otherwise = a : filterXs (b:c:xs)
filterXs _ = []

pieceLengths :: [Int] -> [(Int, Int)]
pieceLengths xs = do
    let ma = maximum xs
    let mi = minimum xs
    let border = ((ma - mi) `div` 2) + mi
    let xs' = map (`div` (border*10`div`2)) xs
    let groupedXs = group xs'
    zip (map head groupedXs) (map length groupedXs)

pp (1:cs) = 'X' : pp cs
pp (0:cs) = ' ' : pp cs
pp _ = ""

prettyPrint :: [(Int, Int)] -> String 
prettyPrint ((0,n):xs) = replicate n ' ' ++ prettyPrint xs
prettyPrint ((1,n):xs) = replicate n 'X' ++ prettyPrint xs
prettyPrint _ = ""

quantize :: Int -> [Int] -> [Int]
quantize stages xs = let max = maximum xs
                     in map (quantize' max stages) xs
quantize' :: Int -> Int -> Int -> Int
quantize' max stages n = n `div` (max `div` stages)

nSlices n samples =
    let sliceLength = length samples `div` n
    in slice sliceLength samples

slice len [] = []
slice len smpls = let (this, rest) = splitAt len smpls
                  in this : slice len rest

rms :: (Integral a) => [a] -> Double
rms samples = fromIntegral (rms' samples) / fromIntegral (length samples)
  where rms' samples = foldl' (\acc nxt -> (acc`div`10000) + abs nxt) 0 samples

morseRawMessageWithSpacesAtStartAndEnd = [Space, Space, Space, Space, Space, Space, Space, Space, Space, Space, Space, Space, Di, Space, Daa, Space, Space, Space, Daa, Space, Di, Space, Di, Space, Di, Space, Space, Space, Daa, Space, Di, Space, Daa, Space, Di, Space, Space, Space, Space, Space, Space, Space, Daa, Space, Di, Space, Daa, Space, Space, Space, Di, Space, Di, Space, Space, Space, Di, Space, Di, Space, Di, Space, Space, Space, Di, Space, Di, Space, Di, Space, Space, Space, Di, Space, Daa, Space, Space, Space, Space, Space, Space, Space, Space, Space, Space, Space, Space, Space, Space, Space, Space, Space, Space, Space, Space, Space, Space, Space, Space, Space, Space, Space, Space, Space, Space, Space, Space, Space, Space, Space]

morseRawMessage = [Di, Space, Daa, Space, Space, Space, Daa, Space, Di, Space, Di, Space, Di, Space, Space, Space, Daa, Space, Di, Space, Daa, Space, Di, Space, Space, Space, Space, Space, Space, Space, Daa, Space, Di, Space, Daa, Space, Space, Space, Di, Space, Di, Space, Space, Space, Di, Space, Di, Space, Di, Space, Space, Space, Di, Space, Di, Space, Di, Space, Space, Space, Di, Space, Daa]

convertSpaces :: [Morse] -> [Morse]
convertSpaces [] = []
convertSpaces (Space:Space:Space:Space:Space:Space:Space:ms) = WordSpace : convertSpaces ms
convertSpaces (Space:Space:Space:ms) = LetterSpace : convertSpaces ms
convertSpaces (m:ms) = m : convertSpaces ms

stripStartAndEndSpaces :: [Morse] -> [Morse]
stripStartAndEndSpaces ms = reverse . dropWhile (==Space) . reverse $ dropWhile (==Space) ms

stripSpacesBetween :: [Morse] -> [Morse]
stripSpacesBetween [] = []
stripSpacesBetween [a] = [a]
stripSpacesBetween [a,b] = [a,b]
stripSpacesBetween (a:b:c:ms)
    | b == Space && a /= Space && c /= Space = a: stripSpacesBetween (c:ms)
    | otherwise = a: stripSpacesBetween (b:c:ms)

morsemessage1 :: [Morse]
morsemessage1 = [Di, Daa, Space, Space, Space, Daa, Di, Di, Di, Space, Space, Space, Daa, Di, Daa, Di, Space, Space, Space, Space, Space, Space, Space, Daa, Di, Daa, Space, Space, Space, Di, Di, Space, Space, Space, Di, Di, Di, Space, Space, Space, Di, Di, Di, Space, Space, Space, Di, Daa]

data Morse = Di | Daa | Space | LetterSpace | WordSpace
    deriving (Eq)

instance Show Morse where
    show Di = "•"
    show Daa = "-"
    show Space = ""
    show LetterSpace = " "
    show WordSpace = "   "

morseLength :: Num a => Morse -> a
morseLength Di = 1
morseLength Daa = 3
morseLength Space = 1
morseLength LetterSpace = 3 * morseLength Space
morseLength WordSpace = 7 * morseLength Space

morseAlphabet :: [([Morse], Char)]
morseAlphabet =
    [ ([Di, Daa],'A')
    , ([Daa, Di, Di, Di],'B')
    , ([Daa, Di, Daa, Di],'C')
    , ([Daa, Di, Di],'D')
    , ([Di],'E')
    , ([Di, Di, Daa, Di],'F')
    , ([Daa, Daa, Di],'G')
    , ([Di, Di, Di, Di],'H')
    , ([Di, Di],'I')
    , ([Di, Daa, Daa, Daa],'J')
    , ([Daa, Di, Daa],'K')
    , ([Di, Daa, Di, Di],'L')
    , ([Daa, Daa],'M')
    , ([Daa, Di],'N')
    , ([Daa, Daa, Daa],'O')
    , ([Di, Daa, Daa, Di],'P')
    , ([Daa, Daa, Di, Daa],'Q')
    , ([Di, Daa, Di],'R')
    , ([Di, Di, Di],'S')
    , ([Daa],'T')
    , ([Di, Di, Daa],'U')
    , ([Di, Di, Di, Daa],'V')
    , ([Di, Daa, Daa],'W')
    , ([Daa, Di, Di, Daa],'X')
    , ([Daa, Di, Daa, Daa],'Y')
    , ([Daa, Daa, Di, Di],'Z')
    , ([Di, Daa, Daa, Daa, Daa],'1')
    , ([Di, Di, Daa, Daa, Daa],'2')
    , ([Di, Di, Di, Daa, Daa],'3')
    , ([Di, Di, Di, Di, Daa],'4')
    , ([Di, Di, Di, Di, Di],'5')
    , ([Daa, Di, Di, Di, Di],'6')
    , ([Daa, Daa, Di, Di, Di],'7')
    , ([Daa, Daa, Daa, Di, Di],'8')
    , ([Daa, Daa, Daa, Daa, Di],'9')
    , ([Daa, Daa, Daa, Daa, Daa],'0')
    , ([Di, Daa, Di, Daa, Di, Daa],'.')
    ]

parseMorse2 :: [Morse] -> String 
parseMorse2 [] = ""
parseMorse2 (WordSpace:ms) = ' ': parseMorse2 ms
parseMorse2 (LetterSpace:ms) = parseMorse2 ms
parseMorse2 ms = let (letter, ms') = parseChar ms
                           in letter : parseMorse2 ms'

parseChar :: [Morse] -> (Char, [Morse])
parseChar [] = ('.', [])
parseChar ms = let (didaas, ms') = span (\m -> m==Di || m==Daa) ms
               in (fromMaybe '*' (lookup didaas morseAlphabet), ms')

translateMorse :: [Morse] -> [Char]
translateMorse = parseMorse2 . convertSpaces . stripSpacesBetween . stripStartAndEndSpaces