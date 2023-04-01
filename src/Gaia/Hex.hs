module Gaia.Hex
  ( Hex(..)
  , hexScreenPoints
  , byCoord
  ) where

import Solitude
import qualified Data.Vector.Sized as VSU
import Data.Finite (Finite, finite)
import Linear

data Hex coord t = Hex
  { coords :: V2 coord
  , hexData :: t
  } deriving stock (Show)

byCoord ::
  (coord -> coord2)
  -> (coord -> coord2)
  -> Hex coord t
  -> Hex coord2 t
byCoord fq fr (Hex (V2 q r) t) = Hex (V2 (fq q) (fr r)) t

instance Eq coord => Eq (Hex coord t) where
  (==) t1 t2 = coords t1 == coords t2

class Num a => Divisible a where
  (/*/) :: a -> a -> a

instance Divisible Int where
  (/*/) = div

instance Divisible Float where
  (/*/) = (/)

instance Divisible Double where
  (/*/) = (/)

s ::
  Num coord
  => Hex coord t
  -> coord
s (Hex (V2 q r) _) = (-q) - r

hexSubtract ::
  Num coord
  => Hex coord t1
  -> Hex coord t2
  -> HexVec coord
hexSubtract t1 t2 = Hex (coords t1 ^-^ coords t2) ()

hexAdd ::
  Num coord
  => Hex coord t1
  -> Hex coord t2
  -> HexVec coord
hexAdd t1 t2 = Hex (coords t1 ^+^ coords t2) ()

hexMultiply ::
  Num coord
  => Hex coord t1
  -> Hex coord t2
  -> HexVec coord
hexMultiply t1 t2 = Hex (liftA2 (*) (coords t1) (coords t2)) ()

hexLength ::
  Divisible coord
  => Hex coord t
  -> coord
hexLength t@(Hex (V2 q r) _) = (abs q + abs r + abs (s t)) /*/ 2

hexDistance ::
  Divisible coord
  => Hex coord t1
  -> Hex coord t2
  -> coord
hexDistance t1 t2 = hexLength $ hexSubtract t1 t2

hexDirectionVector :: Num c => VSU.Vector 6 (HexVec c)
hexDirectionVector = VSU.generate' (Proxy @6) go
  where
    go :: Num c => Finite 6 -> HexVec c
    go 0 = Hex (V2 1 0) ()
    go 1 = Hex (V2 1 (-1)) ()
    go 2 = Hex (V2 0 (-1)) ()
    go 3 = Hex (V2 (-1) 0) ()
    go 4 = Hex (V2 (-1) 1) ()
    go 5 = Hex (V2 0 1) ()
    go _ = error "impossible"

hexDirection ::
  Num coord
  => Integral index
  => index
  -> HexVec coord
hexDirection = VSU.index hexDirectionVector . finite . fromIntegral . flip mod 6

hexNeighbour ::
  Num coord
  => Integral index
  => Hex coord t1
  -> index
  -> HexVec coord
hexNeighbour t = hexAdd t . hexDirection

-- pointy top matrices!
pointyHexScreenMatrix :: V2 (V2 Double)
pointyHexScreenMatrix = V2 (V2 (sqrt 3) (sqrt 3/ 2)) (V2 0 (3/2))

pointyScreenHexMatrix :: V2 (V2 Double)
pointyScreenHexMatrix = V2 (V2 (sqrt 3 / 3) (-1 / 3)) (V2 0 (2/3))

hexScreenMatrix :: V2 (V2 Double)
hexScreenMatrix = V2 (V2 (3/2) 0) (V2 (sqrt 3 / 2) (sqrt 3))

screenHexMatrix :: V2 (V2 Double)
screenHexMatrix = V2 (V2 (2/3) 0) (V2 (-(1/3)) (sqrt 3 / 3))

hexToScreen ::
  Real coord
  => Real coord2
  => Real coord3
  => V2 coord
  -> V2 coord2
  -> V2 coord3
  -> V2 Double
hexToScreen size origin v = (toDouble <$> origin) ^+^ ((toDouble <$> size) * (hexScreenMatrix !* (toDouble <$> v)))

screenToHex ::
  Real coord
  => Real coord2
  => Real coord3
  => V2 coord
  -> V2 coord2
  -> Hex coord3 t
  -> V2 Double
screenToHex size offset (Hex v _) = screenHexMatrix !*
  liftA2 (/*/)
    ((toDouble <$> v) ^-^ (toDouble <$> offset))
    (toDouble <$> size)

hexScreenPoints ::
  Real coord
  => V2 Double
  -> V2 Double
  -> Hex coord t
  -> VSU.Vector 6 (V2 Double)
hexScreenPoints scale origin (Hex c _) = VSU.generate ((^+^) (hexToScreen scale origin c) . hexCornerOffset)

hexCornerOffset :: Finite 6 -> V2 Double
hexCornerOffset i = V2 (10 * cos angle') (10 * sin angle')
  where
    angle' = 2.0 * pi * ((0.0 + toDouble i) / 6)

toDouble :: (Real n) => n -> Double
toDouble = fromRational . toRational

type HexInt t = Hex Int t

type HexVec coord = Hex coord ()
type HexVecInt = HexVec Int
type FractionalHex t = HexVec Float