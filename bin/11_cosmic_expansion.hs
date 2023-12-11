{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFileAndFlags)

import Data.Foldable (for_)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (mapAccumL)

type Pos = (Int, Int)

sumDists :: Int -> IntMap Int -> Int
sumDists expandFactor = sum . snd . mapAccumL dist (0, 0, 0) . IntMap.assocs
  where dist :: (Int, Int, Int) -> (Int, Int) -> ((Int, Int, Int), Int)
        dist (galasToLeft, distsToLeft, prev) (x, freqx) = ((galasToLeft + freqx, distsToLeft', x), distsToLeft' * freqx)
          where distsToLeft' = distsToLeft + galasToLeft * (galaxySteps + nonGalaxySteps * expandFactor)
                galaxySteps = 1
                nonGalaxySteps = x - prev - galaxySteps

enumGrid :: [[a]] -> [(Pos, a)]
enumGrid = concat . zipWith enumRow [1..]
  where enumRow y = zipWith (\x cell -> ((y, x), cell)) [1..]

freq :: [Int] -> IntMap Int
freq = IntMap.fromListWith (+) . map (, 1)

main :: IO ()
main = do
  (s, flags) <- readInputFileAndFlags
  let grid = enumGrid (lines s)
      galaxies = map fst (filter ((== '#') . snd) grid)
      ys = freq (map fst galaxies)
      xs = freq (map snd galaxies)
      expandFactor = maybe 1000000 read (lookup 'n' flags)
  for_ [2, expandFactor] $ \n -> print (sumDists n ys + sumDists n xs)
