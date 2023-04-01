{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Solitude
import qualified SDL

import           Foreign.C.Types        (CInt)
import           SDL                    (($=))
import qualified SDL.Image
import Gaia.Hex
import Linear
import qualified Data.Vector.Sized as VSU
import Data.Finite (finite)
import Gaia.Chunk
import Gaia.SDL

loadTextureWithInfo :: (MonadIO m) => SDL.Renderer -> FilePath -> m (SDL.Texture, SDL.TextureInfo)
loadTextureWithInfo r p = do
  t <- SDL.Image.loadTexture r p
  i <- SDL.queryTexture t
  pure (t, i)

mkPoint :: a -> a -> SDL.Point SDL.V2 a
mkPoint x y = SDL.P (SDL.V2 x y)


mkRect :: a -> a -> a -> a -> SDL.Rectangle a
mkRect x y w h = SDL.Rectangle o z
  where
    o = SDL.P (SDL.V2 x y)
    z = SDL.V2 w h


moveTo :: SDL.Rectangle a -> (a, a) -> SDL.Rectangle a
moveTo (SDL.Rectangle _ d) (x, y) = SDL.Rectangle (mkPoint x y) d


centerWithin :: (Fractional a) => SDL.Rectangle a -> SDL.Rectangle a -> SDL.Rectangle a
centerWithin (SDL.Rectangle _ iz) (SDL.Rectangle (SDL.P op) oz)
  = SDL.Rectangle p iz
  where
    p = SDL.P $ op + (oz - iz) / 2

data Colour = Red | Blue | Green | Yellow | White deriving stock (Show, Enum)


type HasRenderer m = ( MonadIO m, MonadReader SDL.Renderer m)

setColour :: (HasRenderer m) => Colour -> m ()
setColour c = do
  r <- ask
  SDL.rendererDrawColor r $= getColour c
  where
    getColour :: Colour -> SDL.V4 Word8
    getColour White  = SDL.V4 0 0 0 maxBound
    getColour Red    = SDL.V4 maxBound 0 0 maxBound
    getColour Green  = SDL.V4 0 maxBound 0 maxBound
    getColour Blue   = SDL.V4 0 0 maxBound maxBound
    getColour Yellow = SDL.V4 maxBound maxBound 0 maxBound


clearScreen :: (HasRenderer m) => m ()
clearScreen = do
  r <- ask
  setColour White
  SDL.clear r


drawRectangle :: (HasRenderer m) => SDL.Rectangle CInt -> m ()
drawRectangle s = ask >>= \r -> SDL.drawRect r (Just s)


fillRectangle :: (HasRenderer m) => SDL.Rectangle CInt -> m ()
fillRectangle s = ask >>= \r -> SDL.fillRect r (Just s)


drawLine :: (HasRenderer m) => V2 Int -> V2 Int -> m ()
drawLine p1 p2 =
  ask >>= \r -> SDL.drawLine r ( SDL.P $ fromIntegral <$> p1) ( SDL.P $ fromIntegral <$> p2)


drawDot :: (HasRenderer m) => (CInt, CInt) -> m ()
drawDot (x, y) = ask >>= \r -> SDL.drawPoint r (SDL.P (SDL.V2 x y))


screenWidth :: Int
screenWidth = 640


screenHeight :: Int
screenHeight = 480

{-# INLINE fromV2 #-}
fromV2 :: V2 Int -> (CInt, CInt)
fromV2 (V2 x y) = (fromIntegral x, fromIntegral y)

draw :: (HasRenderer m) => m ()
draw = do
  clearScreen
  let
    h :: [(Colour, [VSU.Vector 7 (V2 Double)])]
    h = map (\(Chunk l c _) -> (c, toList $ VSU.map (\h' -> hexScreenPoints (V2 10 10) (V2 0 0) h' `VSU.snoc` (hexScreenPoints (V2 10 10) (V2 0 0) h' `VSU.index` 0)) l) ) makeChunks
  mapM_ (\(c, h') -> do
    setColour c
    mapM_
      (\ha -> mapM_ (\p -> drawLine (round <$> VSU.index ha (finite p)) (round <$> VSU.index ha (finite (p+1)))) [0..5])
        h'
      ) h

  ask >>= SDL.present

whileM :: Monad m => m Bool -> m ()
whileM act = do
    b <- act
    when b $ whileM act

makeHexes :: [Hex Int ()]
makeHexes = mconcat $ map (\q -> map (\r -> Hex (V2 q r) ()) [(-100)..100] ) [(-100)..100]

makeChunks :: [HexChunk 4 Colour]
makeChunks = [makeChunk (const ()) (mkCol x) (mkCoordCentre x y) | x <- [-10..10], y <- [-10..10]]

mkCoordCentre :: Int -> Int -> V2 Int
mkCoordCentre x y = x *^ V2 4 5 ^+^ y *^ V2 (-1) 14
mkCol :: Int -> Colour
mkCol = toEnum . abs . flip mod 4

main :: IO ()
main = withSDL $ do
  setHintQuality
  withWindow "Lesson 08" (V2 screenWidth screenHeight) $ \w ->
    withRenderer w $ \r -> do

      runReaderT (setColour White) r
      runReaderT draw r

      --whileM $ not . hasQuitEvent <$> SDL.pollEvents
