{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)

import Data.Foldable (for_)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (partition, sortBy, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

-- natural sort order already sorts hand types correctly, so no need to create `data Hand`.
-- [5], [4, 1], [3, 2], [3, 1, 1], [2, 2, 1], [2, 1, 1, 1], [1, 1, 1, 1, 1]

rankHands :: (a -> [Int]) -> Int -> [a] -> [a]
rankHands cards joker = sortOn (strength . cards)
  where strength hand = (addJokers (length jokers) (groupSizes notJokers), ranks')
          where ranks' = map (\c -> if c == joker then minBound else c) hand
                (jokers, notJokers) = partition (== joker) hand

groupSizes :: [Int] -> [Int]
groupSizes = sortBy (flip compare) . IntMap.elems . freq

-- joker become a copy of the hand's most frequent card
addJokers :: Int -> [Int] -> [Int]
addJokers n [] = [n]
addJokers n (x:xs) = (x + n) : xs

handAndBid :: String -> ([Int], String, Int)
handAndBid s = case words s of
  -- original never used again, but keep for ease of understanding
  [h, b] -> (map (ranks Map.!) h, h, read b)
  _ -> error ("bad hand " ++ s)

ranks :: Map Char Int
ranks = Map.fromList (zip "23456789TJQKA" [0..])

freq :: [Int] -> IntMap Int
freq = IntMap.fromListWith (+) . map (, 1)

main :: IO ()
main = do
  s <- readInputFile
  let hands = map handAndBid (lines s)
      winnings = sum . zipWith (*) [1..] . map (\(_, _, bid) -> bid)
  for_ [-1, ranks Map.! 'J'] (\joker -> print (winnings (rankHands (\(rs, _, _) -> rs) joker hands)))
