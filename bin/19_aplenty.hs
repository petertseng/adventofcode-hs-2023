{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn, splitOnOne)

import Control.Arrow ((***), second)
import Data.List (mapAccumL, sort)
import Data.Map (Map)
import qualified Data.Map as Map

type Interval = (Int, Int)
type Part = (Interval, Interval, Interval, Interval)
type Rule = ((Interval -> Maybe Interval) -> Part -> Maybe Part, (Interval -> [Interval]) -> Part -> [Part], Interval, String)

maxPartVal :: Int
maxPartVal = 4000

numAccept :: Map String [Rule] -> Part -> Int
numAccept wfs = numAccept' "in"
  where numAccept' "R" _ = 0
        numAccept' "A" pt = numParts pt
        numAccept' wf pt = rules (wfs Map.! wf) pt
        rules [] pt = error ("no rule for " ++ show pt)
        rules ((update, adjusts, matchInter, ifMatch) : rs) pt = matched + unmatched
          where matched = maybe 0 (numAccept' ifMatch) (update (interNonempty . intersect matchInter) pt)
                unmatched = sum [rules rs pt' | pt' <- adjusts (`intervalMinus` [matchInter]) pt]

numParts :: Part -> Int
numParts (a, b, c, d) = interSize a * interSize b * interSize c * interSize d

part :: String -> Part
part s = case second (map (splitOnOne '=')) (bracket s) of
  ("", [("x", x), ("m", m), ("a", a), ("s", s')]) -> dup (read x, read m, read a, read s')
    where dup (a', b, c, d) = ((a', a'), (b, b), (c, c), (d, d))
  _ -> error ("bad part " ++ s)

rule :: String -> Rule
rule s0 = case splitOnOne ':' s0 of
  -- if we wanted to use the same function for intersect and non-intersect
  -- (perhaps via maybeToList on the intersect),
  -- we'd still need to enforce correct behaviour for an unconditional rule,
  -- where it matches everything and leaves nothing unmatched.
  -- So far this code can't do that, so update has to be used for intersect,
  -- and adjusts for non-intersect.
  (r, "") -> (const Just, const (const []), (1, maxPartVal), r)
  (v:o:arg, r) -> (update v, adjusts v, op o (read arg), r)
  _ -> error ("bad rule " ++ s0)
  where update 'x' f (x, m, a, s) = fmap (, m, a, s) (f x)
        update 'm' f (x, m, a, s) = fmap (x, , a, s) (f m)
        update 'a' f (x, m, a, s) = fmap (x, m, , s) (f a)
        update 's' f (x, m, a, s) = fmap (x, m, a, ) (f s)
        update c _ _ = error ("bad update " ++ [c])
        adjusts 'x' f (x, m, a, s) = [(x', m, a, s) | x' <- f x]
        adjusts 'm' f (x, m, a, s) = [(x, m', a, s) | m' <- f m]
        adjusts 'a' f (x, m, a, s) = [(x, m, a', s) | a' <- f a]
        adjusts 's' f (x, m, a, s) = [(x, m, a, s') | s' <- f s]
        adjusts c _ _ = error ("bad adjust " ++ [c])
        op '<' v = (1, v - 1)
        op '>' v = (v + 1, maxPartVal)
        op c _ = error ("bad op " ++ [c])

bracket :: String -> (String, [String])
bracket s = let (l, mr) = splitOnOne '{' s in case splitOnOne '}' mr of
  (m, "") -> (l, splitOn ',' m)
  (_, r@(_:_)) -> error ("unknown " ++ r ++ " after closing bracket")

intervalMinus :: Interval -> [Interval] -> [Interval]
intervalMinus (ll, lr) rs = addLast (filter (not . interEmpty) maybeUnintersected)
  where (lastUnintersected, maybeUnintersected) = mapAccumL advanceUnintersected ll (sort rs)
        advanceUnintersected :: Int -> Interval -> (Int, Interval)
        advanceUnintersected ul (l, r) = (max (r + 1) ul, (ul, min lr (l - 1)))
        addLast = if lastUnintersected <= lr then ((lastUnintersected, lr) :) else id

interNonempty :: Interval -> Maybe Interval
interNonempty i | interEmpty i = Nothing
interNonempty i = Just i

intersect :: Interval -> Interval -> Interval
intersect (a, b) (c, d) = (max a c, min b d)

interSize :: Interval -> Int
interSize (a, b) = if b >= a then b - a + 1 else 0

interEmpty :: Interval -> Bool
interEmpty (a, b) = a > b

main :: IO ()
main = do
  s <- readInputFile
  let (wfs, parts) = (Map.fromList . map (second (map rule) . bracket) *** map part) (splitOnOne "" (lines s))
  print (sum (map (\pt@((a, _), (b, _), (c, _), (d, _)) -> (a + b + c + d) * numAccept wfs pt) parts))
  let partVals = (1, maxPartVal)
  print (numAccept wfs (partVals, partVals, partVals, partVals))
