{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)

import Control.Arrow (first)
import Data.Array.Unboxed ((!), UArray, bounds, inRange, listArray)
import Data.Char (isDigit)
import Data.Function (on)
import Data.List (groupBy, mapAccumL)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)

type Pos = (Int, Int)
type Engine = UArray Pos Char

nextToSymbol :: Engine -> (Pos, Int) -> Bool
nextToSymbol e = any sym . neigh e
  where sym :: Pos -> Bool
        sym p = let c = e ! p in not (isDigit c) && c /= '.'

gearsNextTo :: Engine -> (Pos, Int) -> [Pos]
gearsNextTo e = filter ((== '*') . (e !)) . neigh e

numsByGears :: Engine -> [(Pos, Int)] -> [[Int]]
numsByGears e nums = Map.elems (Map.fromListWith (++) pairs)
  where pairs = concatMap pairWithGears nums
        pairWithGears :: (Pos, Int) -> [(Pos, [Int])]
        pairWithGears np@(_, n) = map (, [n]) (gearsNextTo e np)

gearRatio :: [Int] -> Int
gearRatio [a, b] = a * b
gearRatio _ = 0

neigh :: Engine -> (Pos, Int) -> [Pos]
neigh e ((y, x), n) = filter (inRange (bounds e)) [(cy, cx) | cy <- [y - 1 .. y + 1], cx <- [x - 1 .. x + nDigits n]]

numsAndPoses :: String -> [(Pos, Int)]
numsAndPoses = concat . zipWith (\y row -> map (first (y, )) (rowNums row)) [1..] . lines

rowNums :: String -> [(Int, Int)]
rowNums = catMaybes . snd . mapAccumL pos 1 . groupBy ((==) `on` isDigit)
  where pos x n@(d:_) | isDigit d = (x + length n, Just (x, read n))
        pos x s = (x + length s, Nothing)

nDigits :: Int -> Int
nDigits n | n < 10 = 1
nDigits n | n < 100 = 2
nDigits n | n < 1000 = 3
nDigits n | n < 10000 = 4
nDigits n = 4 + n `quot` 10000

uniform :: Eq b => (a -> b) -> [a] -> b
uniform _ [] = error "empty uniform"
uniform f (x:xs) | any ((/= f x) . f) xs = error "inconsistent uniform"
uniform f (x:_) = f x

grid :: String -> Engine
grid s = listArray ((1, 1), (h, w)) (concat ls)
  where ls = lines s
        h = length ls
        w = uniform length ls

sumBy :: (a -> Int) -> [a] -> Int
sumBy f = sum . map f

main :: IO ()
main = do
  s <- readInputFile
  let engine = grid s
      nums = numsAndPoses s
  print (sumBy snd (filter (nextToSymbol engine) nums))
  print (sumBy gearRatio (numsByGears engine nums))
