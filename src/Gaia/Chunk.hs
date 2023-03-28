{-# LANGUAGE UndecidableInstances #-}

module Gaia.Chunk
  ( makeChunk, HexChunk, Chunk(..)

  ) where

import Solitude hiding (natVal)
import Data.TypeNums
import Gaia.Hex
import qualified Data.Vector.Sized as VSU
import Linear

type family CentredHexagonalNumber (n :: Nat) :: Nat where
  CentredHexagonalNumber n = (n + 1) ^ 3 - (n^3)

type Timestamp = Word64

data Chunk (chunkSize :: Nat) coord hexData chunkData = Chunk
  { chunkHexes :: VSU.Vector (CentredHexagonalNumber chunkSize) (Hex coord hexData)
  , chunkData :: chunkData
  , timestamp :: Timestamp
  } deriving stock (Show)

type HexChunk (chunkSize :: Nat) hexData = Chunk chunkSize Int () hexData

makeChunk ::
  forall chunkSize hexData chunkData.
  KnownNat (CentredHexagonalNumber chunkSize)
  => KnownNat chunkSize
  => (V2 Int -> hexData)
  -> chunkData
  -> V2 Int -- ^ offset
  -> Chunk chunkSize Int hexData chunkData
makeChunk generator cd (V2 offQ offR) = Chunk
  { chunkHexes = VSU.unfoldrN hexGenF hexIndices
  , chunkData = cd
  , timestamp = 0
  }
  where
    minIndex :: Int
    minIndex = fromIntegral $ -1 * natVal @chunkSize Proxy
    maxIndex :: Int
    maxIndex = fromIntegral $ natVal @chunkSize Proxy
    hexIndices = mconcat $ map (\q ->
      let
        r1 :: Int
        r1 = max minIndex ((-q) - maxIndex)
        r2 :: Int
        r2 = min maxIndex ((-q) + maxIndex)
      in
        map (\r -> let c = V2 (offQ+q) (offR+r) in Hex c (generator c) ) [r1..r2]) [minIndex .. maxIndex]
    hexGenF :: [Hex Int hexData] -> (Hex Int hexData, [Hex Int hexData])
    hexGenF [] = error "didn't have enough hexes in the list"
    hexGenF (x:xs) = (x, xs)