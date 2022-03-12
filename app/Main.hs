module Main where

import           MorseReader (readMorseWAVEfile)

main :: IO ()
main = do
  (morseStringAsLetters, morseStringAsCodes) <- readMorseWAVEfile "message.wav"
  putStrLn morseStringAsLetters
  putStrLn $ "(" ++ morseStringAsCodes ++ ")"
