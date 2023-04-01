module Gaia.SDL
  ( withSDL
  , setHintQuality
  , withWindow
  , withRenderer

  ) where

import Solitude

import qualified SDL
import Linear
import Control.Monad.Catch

withSDL ::
  MonadIO m
  => MonadMask m
  => m a
  -> m ()
withSDL f = bracket_ SDL.initializeAll f SDL.quit

withWindow ::
  MonadIO m
  => MonadMask m
  => Integral i
  => Text
  -> V2 i
  -> (SDL.Window -> m a)
  -> m ()
withWindow title size op = bracket
  (SDL.createWindow title $ SDL.defaultWindow { SDL.windowInitialSize = fromIntegral <$> size })
  (\w -> do
    SDL.showWindow w
    op w
  )
  SDL.destroyWindow

withRenderer ::
  MonadIO m
  => MonadMask m
  => SDL.Window
  -> (SDL.Renderer -> m a)
  -> m ()
withRenderer w m = bracket (SDL.createRenderer w (-1) rendererConfig) m SDL.destroyRenderer

rendererConfig :: SDL.RendererConfig
rendererConfig = SDL.RendererConfig
  { SDL.rendererType = SDL.AcceleratedVSyncRenderer
  , SDL.rendererTargetTexture = False
  }

hasQuitEvent :: [SDL.Event] -> Bool
hasQuitEvent = elem SDL.QuitEvent . map SDL.eventPayload

setHintQuality :: MonadIO m => m ()
setHintQuality = SDL.HintRenderScaleQuality SDL.$= SDL.ScaleNearest