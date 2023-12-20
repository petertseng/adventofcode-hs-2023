import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn)

import Control.Arrow (first, second)
import Data.List (findIndices, foldl', mapAccumL)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)

data Module = Broadcast | FlipFlop | Conj deriving (Show)
type Modname = String

type Network = Map Modname (Module, [Modname])

type ConjMem = Map Modname (Map Modname Bool)
type FFMem = Map Modname Bool
type NetMem = (ConjMem, FFMem)

type Sig = Bool
type SentSig = (Modname, Modname, Sig)

button :: Network -> Maybe Modname -> (NetMem, (Int, Int), a) -> (NetMem, (Int, Int), [Modname])
button net traced (cm0, ffm0, _) = fst (until (null . snd) dequeue ((cm0, ffm0, []), q0))
  where q0 = [("broadcaster", undefined, False)]
        dequeue :: ((NetMem, (Int, Int), [Modname]), [SentSig]) -> ((NetMem, (Int, Int), [Modname]), [SentSig])
        dequeue (mems, q) = second concat (mapAccumL send mems q)
        send :: (NetMem, (Int, Int), [Modname]) -> SentSig -> ((NetMem, (Int, Int), [Modname]), [SentSig])
        send (mem@(conjMem, ffMem), counts, trace) (to, from, sig) = ((mem', (if sig then first else second) succ counts, trace'), sigs)
          where (mem', sigs) = case (Map.lookup to net, sig) of
                  (Just (Broadcast, dests), _) -> (mem, [(dest, to, sig) | dest <- dests])
                  (Just (FlipFlop, _), True) -> (mem, [])
                  (Just (FlipFlop, dests), False) ->
                    let (oldState, ffMem') = Map.insertLookupWithKey (const (/=)) to True ffMem
                    in ((conjMem, ffMem'), [(dest, to, not (fromMaybe False oldState)) | dest <- dests])
                  (Just (Conj, dests), _) ->
                    let (mySrcs, conjMem') = Map.updateLookupWithKey (const (Just . Map.insert from sig)) to conjMem
                        sig' = not (and (Map.elems (fromJust mySrcs)))
                    in ((conjMem', ffMem), [(dest, to, sig') | dest <- dests])
                  (Nothing, _) | to == "rx" -> (mem, [])
                  (Nothing, _) | to == "output" -> (mem, [])
                  (Nothing, _) -> error ("no module named " ++ to)
                trace' = if sig && traced == Just to then from : trace else trace

-- can't call it module since that's a keyword
modul :: String -> (Module, Modname, [Modname])
modul s = case words s of
  ((t:name):"->":dests) -> (typ, name', splitOn ',' (concat dests))
    where (typ, name') = case (t, name) of
            ('b', "roadcaster") -> (Broadcast, "broadcaster")
            ('%', n) -> (FlipFlop, n)
            ('&', n) -> (Conj, n)
            _ -> error ("bad module type/name " ++ s)
  _ -> error ("bad module " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let net = map modul (lines s)
      net' = Map.fromList [(src, (t, dsts)) | (t, src, dsts) <- net]
      sources = Map.fromListWith (++) [(dst, [src]) | (_, src, dsts) <- net, dst <- dsts]
      conjSources = Map.filterWithKey (\k _ -> case Map.lookup k net' of Just (Conj, _) -> True; _ -> False) sources
      conjMem = Map.map (\srcs -> Map.fromList [(src, False) | src <- srcs]) conjSources
      -- can't just examine conjMem because the input isn't high at the end of the cycle!!!
      -- have to pass in the component we want to trace and have button trace it
      traced = case Map.lookup "rx" sources of
        Nothing -> Nothing
        Just [v] -> Just v
        Just v -> error ("bad rx has inputs " ++ show v)
      nets = iterate (button net' traced) ((conjMem, Map.empty), (0, 0), [])
      ((_, _), (los, his), _) = nets !! 1000
  print (los * his)

  case traced of
    Nothing -> putStrLn "no rx"
    Just t -> do
      let srcs = conjSources Map.! t
          period [a, b, c] | b == a * 2 && c == a * 3 = a
          period l = error ("bad cycle " ++ show l)
          srcWasHi src (_, _, c) = src `elem` c
          periods = map (\src -> period (take 3 (findIndices (srcWasHi src) nets))) srcs
      print (foldl' lcm 1 periods)
