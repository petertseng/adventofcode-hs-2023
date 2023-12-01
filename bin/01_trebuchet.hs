import AdventOfCode (readInputFileAndFlags)

import Control.Monad (unless)
import Data.Char (intToDigit, isDigit)
import Data.Function (on)
import Data.List (elemIndex, findIndex, isPrefixOf, minimumBy, tails)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)

name :: [String]
name = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

calib1 :: String -> Int
calib1 s = let ds = filter isDigit s in read (head ds : [last ds])

calib2 :: String -> Int
calib2 s = firstDigit id * 10 + firstDigit reverse
  where firstDigit :: (String -> String) -> Int
        firstDigit f = minimumBy (comparing (digitIndex f)) [1..9]
        digitIndex :: (String -> String) -> Int -> Int
        digitIndex f d = minMaybe (elemIndex (intToDigit d) (f s)) (findIndex (isPrefixOf (f (name !! d))) (tails (f s)))
        minMaybe :: Maybe Int -> Maybe Int -> Int
        minMaybe = min `on` fromMaybe (length s)

sumBy :: (a -> Int) -> [a] -> Int
sumBy f = sum . map f

main :: IO ()
main = do
  (s, f) <- readInputFileAndFlags
  let ls = lines s
  unless (any ((== '2') . fst) f) $ print (sumBy calib1 ls)
  print (sumBy calib2 ls)
