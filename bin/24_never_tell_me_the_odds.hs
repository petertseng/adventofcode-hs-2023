import AdventOfCode (readInputFileAndFlags)
import AdventOfCode.Split (splitOn, splitOnOne)

import Control.Arrow ((***))
import Data.List (tails)
import Data.Ratio (denominator, numerator)

type Hailstone = ((Int, Int, Int), (Int, Int, Int))
type SlopeIntercept = (Rational, Rational, Int, Int)

newtype Vec3 a = Vec3 (a, a, a) deriving Show

intersectIn :: (Int, Int) -> SlopeIntercept -> SlopeIntercept -> Bool
intersectIn range (slope1, yinter1, px1, vx1) (slope2, yinter2, px2, vx2) =
  slope1 /= slope2 && range `contains` x && range `contains` y && t1 > 0 && t2 > 0
  where (l, r) `contains` v = fromIntegral l <= v && v <= fromIntegral r
        x = (yinter2 - yinter1) / (slope1 - slope2)
        y = slope1 * x + yinter1
        t1 = (x - fromIntegral px1) / fromIntegral vx1
        t2 = (x - fromIntegral px2) / fromIntegral vx2

slopeIntercept :: Hailstone -> SlopeIntercept
slopeIntercept ((px, py, _), (vx, vy, _)) = (slope, yinter, px, vx)
  where slope = fromIntegral vy / fromIntegral vx
        yinter = fromIntegral py - slope * fromIntegral px

interPlane :: Integral a => (Vec3 a, Vec3 a) -> (Vec3 a, a) -> (Vec3 Rational, Rational)
(pos, vel) `interPlane` (normal, d) = (vint pos + vmap ((* t) . fromIntegral) vel, t)
  where t = fromIntegral tnum / fromIntegral tdenom
        -- NB: These exceed Int range!
        -- Must use Integer, since the fraction will be wrong if numerator/denominator are wrapped
        tnum = fromIntegral d - vint pos `dot` vint normal :: Integer
        tdenom = vint vel `dot` vint normal :: Integer

hailstone :: String -> Hailstone
hailstone s = case words s of
  [px, py, pz, "@", vx, vy, vz] -> (int3 (px ++ py ++ pz), int3 (vx ++ vy ++ vz))
  _ -> error ("bad hailstone " ++ s)
  where int3 s' = case splitOn ',' s' of
          [x, y, z] -> (read x, read y, read z)
          _ -> error ("bad coord " ++ s')

instance Num a => Num (Vec3 a) where
  (+) = vzip (+)
  (-) = vzip (-)
  -- cross product
  Vec3 (x1, y1, z1) * Vec3 (x2, y2, z2) = Vec3 (y1 * z2 - z1 * y2, z1 * x2 - x1 * z2, x1 * y2 - y1 * x2)
  -- would like this to be norm or something, but Num abs requires same type
  abs = vmap abs
  signum = vmap signum
  negate = vmap negate
  fromInteger _ = error "don't know how to make a vec from one number"

dot :: Num a => Vec3 a -> Vec3 a -> a
Vec3 (x1, y1, z1) `dot` Vec3 (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2

vmap :: (a -> b) -> Vec3 a -> Vec3 b
vmap f (Vec3 (x, y, z)) = Vec3 (f x, f y, f z)

vzip :: (a -> b -> c) -> Vec3 a -> Vec3 b -> Vec3 c
vzip f (Vec3 (x1, y1, z1)) ((Vec3 (x2, y2, z2))) = Vec3 (f x1 x2, f y1 y2, f z1 z2)

vint :: (Integral a, Num b) => Vec3 a -> Vec3 b
vint = vmap fromIntegral

norm1 :: Num a => Vec3 a -> a
norm1 = (`dot` vint (Vec3 (1, 1, 1 :: Int)))

rationalMustInt :: Rational -> Integer
rationalMustInt n
  | denominator n == 1 = numerator n
  | otherwise = error (show n ++ " not integer")

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x : ys) <- tails l, y <- ys]

main :: IO ()
main = do
  (s, flags) <- readInputFileAndFlags
  let hails = map hailstone (lines s)
      area = maybe (200000000000000, 400000000000000) ((read *** read) . splitOnOne ',') (lookup 'n' flags)
  print (count (uncurry (intersectIn area)) (pairs (map slopeIntercept hails)))

  -- Can't get the matrix package to work (says vector is broken),
  -- so can't use the three hailstones method.
  -- Will try the four hailstones method here instead.
  -- https://www.reddit.com/r/adventofcode/comments/18q0kfc/2023_day_24_part_2_simplifying_assumption/kes6ywf/
  -- https://www.reddit.com/r/adventofcode/comments/18qexvu/2023_day_24_part_2_3d_vector_interpretation_and/
  -- (Both posts describe the same solution)
  -- Make the first hailstone the reference frame
  -- (subtract its pos and vel from all others).
  -- rock must pass through origin.
  let ((pref, vref), (pa, va), (pb, vb), (pc, vc)) = case map (Vec3 *** Vec3) hails of
        (r:a:b:c:_) -> (r, a, b, c)
        _ -> error "need four hailstones"
      -- rock must be in the plane formed by the origin and the second hailstone's path
      -- find this plane by cross product of two basis vectors.
      -- basis 1: origin to position at t=0
      basis1 = pa - pref
      -- basis 2: position at t=0 to position at t=1, which is just its velocity
      basis2 = va - vref
      -- Don't the multiplications in here overflow Int?
      -- but I guess the subtractions keep it in Int range and wrapping means the result comes out correct.
      normal = basis1 * basis2
      -- equation for plane is n_x * x + n_y * y + n_z * z = d
      -- in this case, it passes through the origin, so d = 0
      d = 0
      -- positions and times where hailstones 3 and 4 intersect the plane
      (pr1, t1) = (pb - pref, vb - vref) `interPlane` (normal, d)
      (pr2, t2) = (pc - pref, vc - vref) `interPlane` (normal, d)
      vr = vmap (/ (t2 - t1)) (pr2 - pr1)
      pr = pr1 - vmap (* t1) vr
  print (norm1 (vint pref + vmap rationalMustInt pr))
