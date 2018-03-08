module YampaSDL2.Internal.SDL.Output
  ( outputAction
  ) where

import Control.Concurrent.MVar
import Control.Monad
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Data.List
import Data.Maybe
import Data.StateVar (($=), get)
import Debug.Trace
import Linear.V2
import Linear.V4
import qualified SDL

import YampaSDL2.Internal.AppOutput

-- changed bool variable does not do anything
outputAction ::
     Cache
  -> Double
  -> MVar Double
  -> MVar Bool
  -> MVar (Maybe Scene)
  -> SDL.Window
  -> SDL.Renderer
  -> Bool
  -> AppOutput
  -> IO Bool
outputAction mvarCache fps mvarFPS mvarReady mvarG window renderer _ ao = do
  lastTime <- readMVar mvarFPS
  currentTime <- SDL.time
  ensureFPS <-
    if currentTime - lastTime > 1 / fps
      then modifyMVar_ mvarFPS (return . const currentTime) >> return True
      else return False
  ready <- readMVar mvarReady
  when (ensureFPS && ready) $ do
    modifyMVar_ mvarReady (\_ -> return False)
    renderScene mvarCache window renderer (scene ao)
    modifyMVar_ mvarReady (\_ -> return True)
  return (shouldExit ao)

renderScene :: Cache -> SDL.Window -> SDL.Renderer -> Scene -> IO ()
renderScene mvarCache window renderer gra = do
  let newScene = adjustToCamera $ removeOutOfBounds gra
  (V2 wW wH) <- fmap (fromIntegral . fromEnum) <$> get (SDL.windowSize window)
  (V2 cW cH) <- return (cSize $ cam gra)
  SDL.rendererScale renderer $= realToFrac <$> (V2 (wW / cW) (wH / cH))
  renderObjects mvarCache renderer newScene

-- Preprocessing rendershapes for rendering
renderObjects :: Cache -> SDL.Renderer -> Scene -> IO ()
renderObjects mvarCache renderer gra = do
  mapM_ (\r -> (draw r) mvarCache (center r) renderer) $
    sortBy (\r1 r2 -> zIndex r1 `compare` zIndex r2) (objects gra)
  SDL.present renderer

removeOutOfBounds :: Scene -> Scene
removeOutOfBounds scene =
  let camera = cam scene
      objs = objects scene
      (V2 bR bT) = cPos camera + cSize camera / 2
      (V2 bL bB) = cPos camera - cSize camera / 2
      notOutOfBounds s =
        not $
        let (V4 u r d l) = bounds s
        in r < bL || l > bR || u < bB || d > bT
  in scene {objects = filter (notOutOfBounds) objs}

adjustToCamera :: Scene -> Scene
adjustToCamera gra =
  let camera = cam gra
      obs = objects gra
  in gra {objects = adjustToCamera' camera <$> obs}

adjustToCamera' :: Camera -> RenderObject -> RenderObject
adjustToCamera' c rs =
  let (V2 cx cy) = cPos c
      (V2 w h) = cSize c
      adjustPoint (V2 x y) = V2 (x + w / 2 - cx) (h / 2 - (y + cy))
  in translate adjustPoint rs
