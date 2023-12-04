import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

import Control.Arrow (first)
import Data.Bits (shiftL)
import Data.Char (isDigit)
import qualified Data.IntSet as IntSet
import Data.List (mapAccumL)

score :: Int -> Int
score 0 = 0
score n = 1 `shiftL` (n - 1)

numCards :: [Int] -> Int -> ([Int], Int)
numCards upcomingMults wins = (upcomingMults', myMult)
  where myMult = case upcomingMults of
                   [] -> 1
                   m:_ -> 1 + m
        upcomingMults' = zipWithDefault 0 0 (+) (replicate wins myMult) (drop 1 upcomingMults)

card :: String -> Int
card = countWins . discardNumber

countWins :: String -> Int
countWins s = count (`IntSet.member` winners) mine
  where winners = IntSet.fromList (map read (words l))
        mine = map read (words r) :: [Int]
        (l, r) = splitOnOne '|' s

discardNumber :: String -> String
discardNumber s = case first words (splitOnOne ':' s) of
  (["Card", d], c) | all isDigit d -> c
  _ -> error ("bad card " ++ s)

zipWithDefault :: a -> b -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithDefault dx dy f = zwd
  where zwd [] [] = []
        zwd [] ys = map (f dx) ys
        zwd xs [] = map (`f` dy) xs
        zwd (x:xs) (y:ys) = f x y : zwd xs ys

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

sumBy :: (a -> Int) -> [a] -> Int
sumBy f = sum . map f

main :: IO ()
main = do
  s <- readInputFile
  let cards = map card (lines s)
  print (sumBy score cards)
  print (sum (snd (mapAccumL numCards [] cards)))
