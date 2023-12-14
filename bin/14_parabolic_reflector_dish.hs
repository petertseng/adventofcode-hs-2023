import AdventOfCode (readInputFile)

import Data.Bits ((.|.), (.&.), complement, popCount, shiftL, shiftR)
import Data.List (foldl')
import qualified Data.Map as Map

tilt :: (Integer -> Integer) -> (Integer -> Integer) -> Integer -> Integer -> Integer -> Integer
tilt forwardMove reverseMove cantMove blocks = until ((== 0) . canMove) stepMove
  where canMove rocks = rocks .&. complement (reverseMove (blocks .|. rocks)) .&. complement cantMove
        stepMove rocks = let moving = canMove rocks in rocks .&. complement moving .|. forwardMove moving

load :: Int -> Integer -> Int
load w = addRow 1
  where addRow _ 0 = 0
        addRow y n = y * popCount (n .&. row) + addRow (y + 1) (n `shiftR` w)
        row = (1 `shiftL` w) - 1

platform :: (Integer, Integer) -> Char -> (Integer, Integer)
platform (blocks, rocks) '#' = (blocks `shiftL` 1 .|. 1, rocks `shiftL` 1)
platform (blocks, rocks) 'O' = (blocks `shiftL` 1, rocks `shiftL` 1 .|. 1)
platform (blocks, rocks) '.' = (blocks `shiftL` 1, rocks `shiftL` 1)
platform (_, _) c = error (c : " bad")

firstRepeat :: Ord a => (a -> a) -> a -> (Int, Int, a)
firstRepeat f = step 0 Map.empty
  where step t seen s = case Map.insertLookupWithKey (\_ a _ -> a) s t seen of
          (Nothing, seen') -> step (t + 1) seen' (f s)
          (Just t', _) -> (t, t - t', s)

uniform :: Eq b => (a -> b) -> [a] -> b
uniform _ [] = error "empty uniform"
uniform f (x:xs) | any ((/= f x) . f) xs = error "inconsistent uniform"
uniform f (x:_) = f x

main :: IO ()
main = do
  s <- readInputFile
  let grid = lines s
      height = length grid
      width = uniform length grid
      size = height * width

      (blocks, rocks) = foldl' platform (0, 0) (concat grid)

      eachRow = foldl' (\a _ -> a `shiftL` width .|. 1) 0 (replicate height ())
      eachCol = (1 `shiftL` width) - 1

      leftCol = (1 `shiftL` (width - 1)) * eachRow
      rightCol = 1 * eachRow
      topRow = eachCol `shiftL` (size - width)
      bottomRow = 1 * eachCol

      tiltUp = tilt (`shiftL` width) (`shiftR` width) topRow blocks
  print (load width (tiltUp rocks))

  let tiltLeft = tilt (`shiftL` 1) (`shiftR` 1) leftCol blocks
      tiltDown = tilt (`shiftR` width) (`shiftL` width) bottomRow blocks
      tiltRight = tilt (`shiftR` 1) (`shiftL` 1) rightCol blocks
      spincycle = tiltRight . tiltDown . tiltLeft . tiltUp

      (cycleAt, cycleLen, rocks') = firstRepeat spincycle rocks
      target = 1000000000
      remain = (target - cycleAt) `rem` cycleLen
  print (load width (iterate spincycle rocks' !! remain))
