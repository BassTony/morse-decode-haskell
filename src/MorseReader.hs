module MorseReader
  ( readMorseWAVEfile
  ) where

import           Data.List  (foldl', group, partition, sort, sortOn)
import           Data.Maybe (fromMaybe)
import qualified Data.Ord
import           Data.WAVE  (WAVE (WAVE), WAVEHeader (WAVEHeader), WAVESample,
                             getWAVEFile)

data Morse
  = Dit
  | Dah
  | Space
  | LetterSpace
  | WordSpace
  deriving (Eq)

instance Show Morse where
  show Dit         = "•"
  show Dah         = "-"
  show Space       = ""
  show LetterSpace = " "
  show WordSpace   = "   "

morseAlphabet :: [([Morse], Char)]
morseAlphabet =
  [ ([Dit, Dah], 'A')
  , ([Dah, Dit, Dit, Dit], 'B')
  , ([Dah, Dit, Dah, Dit], 'C')
  , ([Dah, Dit, Dit], 'D')
  , ([Dit], 'E')
  , ([Dit, Dit, Dah, Dit], 'F')
  , ([Dah, Dah, Dit], 'G')
  , ([Dit, Dit, Dit, Dit], 'H')
  , ([Dit, Dit], 'I')
  , ([Dit, Dah, Dah, Dah], 'J')
  , ([Dah, Dit, Dah], 'K')
  , ([Dit, Dah, Dit, Dit], 'L')
  , ([Dah, Dah], 'M')
  , ([Dah, Dit], 'N')
  , ([Dah, Dah, Dah], 'O')
  , ([Dit, Dah, Dah, Dit], 'P')
  , ([Dah, Dah, Dit, Dah], 'Q')
  , ([Dit, Dah, Dit], 'R')
  , ([Dit, Dit, Dit], 'S')
  , ([Dah], 'T')
  , ([Dit, Dit, Dah], 'U')
  , ([Dit, Dit, Dit, Dah], 'V')
  , ([Dit, Dah, Dah], 'W')
  , ([Dah, Dit, Dit, Dah], 'X')
  , ([Dah, Dit, Dah, Dah], 'Y')
  , ([Dah, Dah, Dit, Dit], 'Z')
  , ([Dit, Dah, Dah, Dah, Dah], '1')
  , ([Dit, Dit, Dah, Dah, Dah], '2')
  , ([Dit, Dit, Dit, Dah, Dah], '3')
  , ([Dit, Dit, Dit, Dit, Dah], '4')
  , ([Dit, Dit, Dit, Dit, Dit], '5')
  , ([Dah, Dit, Dit, Dit, Dit], '6')
  , ([Dah, Dah, Dit, Dit, Dit], '7')
  , ([Dah, Dah, Dah, Dit, Dit], '8')
  , ([Dah, Dah, Dah, Dah, Dit], '9')
  , ([Dah, Dah, Dah, Dah, Dah], '0')
  , ([Dit, Dah, Dit, Dah, Dit, Dah], '.')
  , ([Dah, Dah, Dit, Dit, Dah, Dah], ',')
  , ([Dah, Dit, Dah, Dit, Dah, Dah], '!')
  , ([Dit, Dah, Dit, Dit, Dit], '&')
  ]

readMorseWAVEfile :: FilePath -> IO (String, String)
readMorseWAVEfile filepath = do
  (WAVE (WAVEHeader channels framerate bits frames) samples) <-
    getWAVEFile filepath
  let monoStream = concat samples
  let rmsSlices = map (round . avg) (slice10ms framerate monoStream)
  let quantizedSlices = quantize 1000 rmsSlices
  let onesAndZeroes =
        map
          (\a ->
             if a `div` 5 > 2
               then 1
               else 0)
          quantizedSlices
  let filteredOZ = fillGaps onesAndZeroes
  let lengths = init . tail $ map (\a -> (head a, length a)) $ group filteredOZ
  let (signalLengths, silenceLengths) = partition ((== 1) . fst) lengths
  let signalLengthsSorted = sort . map snd $ signalLengths
  let spaceLengthsSorted = sort . map snd $ silenceLengths
  let sigLength = distancesBetweenNeighbouringValues signalLengthsSorted
  let dahMinLength = head . map snd . sortOn (Data.Ord.Down . fst) $ sigLength
  let [wordSpaceMinLength, letterSpaceMinLength] =
        take 2 $
        map snd . sortOn (Data.Ord.Down . fst) $
        distancesBetweenNeighbouringValues spaceLengthsSorted
  let morses =
        sequencesToMorses
          dahMinLength
          wordSpaceMinLength
          letterSpaceMinLength
          lengths
  return (translateMorse morses, concatMap show . filter (/= Space) $ morses)

sequencesToMorses :: Int -> Int -> Int -> [(Int, Int)] -> [Morse]
sequencesToMorses sigLength wordSpaceMinLength letterSpaceMinLength [] = []
sequencesToMorses sigLength wordSpaceMinLength letterSpaceMinLength ((1, l):ls)
  | l < sigLength =
    Dit : sequencesToMorses sigLength wordSpaceMinLength letterSpaceMinLength ls
  | otherwise =
    Dah : sequencesToMorses sigLength wordSpaceMinLength letterSpaceMinLength ls
sequencesToMorses sigLength wordSpaceMinLength letterSpaceMinLength ((0, l):ls)
  | l < letterSpaceMinLength =
    Space :
    sequencesToMorses sigLength wordSpaceMinLength letterSpaceMinLength ls
  | l < wordSpaceMinLength =
    LetterSpace :
    sequencesToMorses sigLength wordSpaceMinLength letterSpaceMinLength ls
  | otherwise =
    WordSpace :
    sequencesToMorses sigLength wordSpaceMinLength letterSpaceMinLength ls
sequencesToMorses _ _ _ _ = []

distancesBetweenNeighbouringValues :: (Eq b, Num b) => [b] -> [(b, b)]
distancesBetweenNeighbouringValues (x:x':xs) =
  foldl
    (\a@((n, i):acc) nxt ->
       if nxt == i
         then a
         else (nxt - i, nxt) : a)
    [(x - x', x')]
    xs
distancesBetweenNeighbouringValues _ = []

fillGaps :: Eq a => [a] -> [a]
fillGaps [a, b] = [a]
fillGaps (a:b:c:xs)
  | a == c && a /= b = a : fillGaps (a : c : xs)
  | otherwise = a : fillGaps (b : c : xs)
fillGaps _ = []

quantize :: Int -> [Int] -> [Int]
quantize stages xs =
  let max = maximum xs
   in map (quantize' max stages) xs

quantize' :: Int -> Int -> Int -> Int
quantize' max stages n = n `div` (max `div` stages)

slice10ms samplerate = slice (samplerate `div` 100)

slice len [] = []
slice len smpls =
  let (this, rest) = splitAt len smpls
   in this : slice len rest

avg :: (Integral a) => [a] -> Double
avg samples = fromIntegral (absAverage' samples) / fromIntegral (length samples)
  where
    absAverage' samples =
      foldl' (\acc nxt -> (acc `div` 10000) + abs nxt) 0 samples

-- Should be using this instead of avg, but ran out of time
rms' :: (Integral a) => [a] -> Double
rms' [] = 123456789.0
rms' xs =
  sqrt $
  (fromIntegral $ (sum . map ((^ 2) . (`div` 10000))) xs) /
  fromIntegral (length xs)

convertSpaces :: [Morse] -> [Morse]
convertSpaces [] = []
convertSpaces (Space:Space:Space:Space:Space:Space:Space:ms) =
  WordSpace : convertSpaces ms
convertSpaces (Space:Space:Space:ms) = LetterSpace : convertSpaces ms
convertSpaces (m:ms) = m : convertSpaces ms

stripStartAndEndSpaces :: [Morse] -> [Morse]
stripStartAndEndSpaces ms =
  reverse . dropWhile (== Space) . reverse $ dropWhile (== Space) ms

stripSpacesBetween :: [Morse] -> [Morse]
stripSpacesBetween [] = []
stripSpacesBetween [a] = [a]
stripSpacesBetween [a, b] = [a, b]
stripSpacesBetween (a:b:c:ms)
  | b == Space && a /= Space && c /= Space = a : stripSpacesBetween (c : ms)
  | otherwise = a : stripSpacesBetween (b : c : ms)

parseMorse :: [Morse] -> String
parseMorse [] = ""
parseMorse (WordSpace:ms) = ' ' : parseMorse ms
parseMorse (LetterSpace:ms) = parseMorse ms
parseMorse ms =
  let (letter, ms') = parseMorseChar ms
   in letter : parseMorse ms'

parseMorseChar :: [Morse] -> (Char, [Morse])
parseMorseChar [] = ('.', [])
parseMorseChar ms =
  let (didaas, ms') = span (\m -> m == Dit || m == Dah) ms
   in (fromMaybe '*' (lookup didaas morseAlphabet), ms')

translateMorse :: [Morse] -> [Char]
translateMorse =
  parseMorse . convertSpaces . stripSpacesBetween . stripStartAndEndSpaces
-- morseLength :: Num a => Morse -> a
-- morseLength Dit = 1
-- morseLength Dah = 3
-- morseLength Space = 1
-- morseLength LetterSpace = 3 * morseLength Space
-- morseLength WordSpace = 7 * morseLength Space
