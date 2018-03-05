module YampaSDL2.Backend.Output
  (outputAction) where

import qualified SDL
import qualified SDL.Primitive as GFX
import Data.Colour.SRGB
import Control.Monad
import Linear.V4
import Linear.V2
import Data.Maybe
import Control.Concurrent.MVar
import Data.StateVar (($=), get)
import Data.List
import Data.Colour.Names
import Data.Colour
import Debug.Trace

import YampaSDL2.AppOutput

-- changed bool variable does not do anything
outputAction :: Cache -> Double -> MVar Double -> MVar Bool -> MVar (Maybe Graphics) -> SDL.Window -> SDL.Renderer -> Bool -> AppOutput -> IO Bool
outputAction mvarCache fps mvarFPS mvarReady mvarG window renderer _ ao = do
  lastTime <- readMVar mvarFPS
  currentTime <- SDL.time
  ensureFPS <- if currentTime - lastTime > 1/fps
    then modifyMVar_ mvarFPS (return . const currentTime) >> return True
    else return False
  ready <- readMVar mvarReady
  when (ensureFPS && ready) $ do
    modifyMVar_ mvarReady (\_ -> return False)
    renderGraphics mvarCache window renderer (graphics ao)
    modifyMVar_ mvarReady (\_ -> return True)
  return (shouldExit ao)


renderGraphics :: Cache -> SDL.Window -> SDL.Renderer -> Graphics -> IO ()
renderGraphics mvarCache window renderer gra = do
  let newGraphics =
         adjustToCamera $
          removeOutOfBounds gra
  (V2 wW wH) <- fmap (fromIntegral . fromEnum) <$> get (SDL.windowSize window)
  (V2 cW cH) <- return (cSize $ camera gra)
  SDL.rendererScale renderer $= realToFrac <$> (V2 (wW/cW) (wH/cH))
  renderObjects mvarCache renderer newGraphics

-- Preprocessing rendershapes for rendering

renderObjects :: Cache -> SDL.Renderer -> Graphics -> IO ()
renderObjects mvarCache renderer gra = do
  mapM_ (\r -> (render r) mvarCache (center r) renderer) $
      sortBy (\r1 r2 -> zIndex r1 `compare` zIndex r2) (objects gra)
  SDL.present renderer


removeOutOfBounds :: Graphics -> Graphics
removeOutOfBounds graphics =
  let cam = camera graphics
      objs = objects graphics
      (V2 bR bT) =  cPos cam + cSize cam/2
      (V2 bL bB) = cPos cam - cSize cam/2
      notOutOfBounds s = not $
        let (V4 u r d l) = bounds s
        in r < bL || l > bR || u < bB || d > bT
  in graphics{objects=filter (notOutOfBounds) objs}

adjustToCamera :: Graphics -> Graphics
adjustToCamera gra =
  let cam = camera gra
      obs = objects gra
  in gra{objects = adjustToCamera' cam <$> obs}


adjustToCamera' :: Camera -> RenderObject -> RenderObject
adjustToCamera' c rs =
  let (V2 cx cy) = cPos c
      (V2 w h) = cSize c
      adjustPoint (V2 x y) = V2 (x+w/2-cx) (h/2-(y+cy))
  in modifyCenter adjustPoint rs

-- helper functions
-- shapeToBorders :: RenderShape -> V4 Double
-- shapeToBorders rs =
--   case rs of
--     Image {shapeCentre=V2 x y, size=V2 w h} ->
--       V4 (x+w/2) (x-w/2) (y+h/2) (y-h/2)
--     Object {shapeCentre=V2 x y} ->
--       let s = shape rs
--           (V2 x y) = shapeCentre rs
--       in case s of
--         Rectangle {rectSize=V2 w h} ->
--           V4 (x+w/2) (x-w/2) (y+h/2) (y-h/2)
--         Circle {radius=r} ->
--           V4 (x+r) (x-r) (y+r) (y-r)
--         Triangle {pointA=V2 xa ya, pointB=V2 xb yb, pointC=V2 xc yc} ->
--           V4 (x+maximum [xa, xb, xc]) (x+minimum [xa, xb, xc]) (y+maximum [ya,yb,yc]) (y-maximum [ya,yb,yc])


