import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn)

import Control.Arrow (second)
import Data.Foldable (for_)
import Data.List (transpose)

reflectScores :: Eq a => [[a]] -> [(Int, Int)]
reflectScores x = map (second (* 100)) (summary x) ++ summary (transpose x)
  where summary xs = map (\p@(l, _) -> (reflectErrors p, length l)) (reflectPairs xs)

reflectErrors :: Eq a => ([[a]], [[a]]) -> Int
reflectErrors = sum . uncurry (zipWith errors)
  where errors a b = count id (zipWith (/=) a b)

reflectPairs :: [a] -> [([a], [a])]
reflectPairs [] = error "reflect empty"
reflectPairs (x:xs) = takeWhile (not . null . snd) (iterate move ([x], xs))
  where move (ls, r:rs) = (r:ls, rs)
        move (_, []) = error "reflect right empty"

lookups :: Eq a => a -> [(a, b)] -> [b]
lookups x = map snd . filter ((== x) . fst)

one :: [a] -> a
one [x] = x
one [] = error "none"
one (_:_) = error "too many"

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  s <- readInputFile
  let patterns = splitOn "" (lines s)
      scores = map reflectScores patterns
  for_ [0, 1] $ \d -> print (sum (map (one . lookups d) scores))
