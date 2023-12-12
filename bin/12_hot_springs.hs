{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn)

import Data.Foldable (for_)
import Data.List (foldl', intercalate)
import qualified Data.Map as Map

-- Apparently it's faster to pass around the index (gi) and index in with gs !! gi
-- than it is to pass the [Int] instead and match on it.
-- A little surprising, but okay.
ways :: (String, [Int]) -> Int
ways (s, gs) = sumBy snd (filter (correct . fst) (foldl' waysForChar [((0, 0), 1)] s))
  where waysForChar xs c = Map.assocs (Map.fromListWith (+) (concatMapFirst (consume c) xs))
        consume '.' (gi, 0) = [(gi, 0)]
        consume '.' (gi, sz) | sz == gs !! gi = [(gi + 1, 0)]
        consume '.' (_, _) = []
        consume '#' (gi, _) | gi >= length gs = []
        consume '#' (gi, sz) | sz >= gs !! gi = []
        consume '#' (gi, sz) = [(gi, sz + 1)]
        consume '?' x = consume '.' x ++ consume '#' x
        consume c _ = error ("bad char " ++ [c] ++ " in " ++ s)
        correct (gi, 0) = gi == length gs
        correct (gi, sz) = gi == length gs - 1 && sz == gs !! gi

expand :: (String, [Int]) -> (String, [Int])
expand (s, i) = (intercalate "?" (replicate 5 s), concat (replicate 5 i))

spring :: String -> (String, [Int])
spring s = case words s of
  [a, b] -> (a, map read (splitOn ',' b))
  _ -> error ("bad spring " ++ s)

concatMapFirst :: (a -> [b]) -> [(a, c)] -> [(b, c)]
concatMapFirst f = cmf
  where cmf [] = []
        cmf ((a, c):xs) = map (, c) (f a) ++ cmf xs

sumBy :: (a -> Int) -> [a] -> Int
sumBy f = sum . map f

main :: IO ()
main = do
  s <- readInputFile
  let springs = map spring (lines s)
  for_ [id, expand] $ \f -> print (sumBy (ways . f) springs)
