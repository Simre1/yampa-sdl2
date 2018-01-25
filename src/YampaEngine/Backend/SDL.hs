{-# Language OverloadedStrings #-}

module YampaEngine.Backend.SDL where

import qualified SDL
import SDL.Vect
import FRP.Yampa
import Control.Concurrent
import Control.Monad
import Data.List (sortBy)
import Data.SG
import Data.Colour.SRGB
import Data.StateVar (($=))

import YampaEngine.Backend (Backend(..))
import YampaEngine.AppInput (AppInput(..), initAppInput)
import YampaEngine.AppOutput
import YampaEngine.Geometry

sdlBackend :: IO (Backend (Event SDL.EventPayload) AppOutput)
sdlBackend = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "a" windowConf
  SDL.showWindow window
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
  let closeAction' = do
        SDL.destroyRenderer renderer
        SDL.destroyWindow window
        SDL.quit
  lastInteraction <- newMVar =<< SDL.time
  return $ Backend
    { initAction = initAction'
    , inputAction = inputAction' lastInteraction
    , outputAction = outputAction' renderer
    , parseInput = parseInput'
    , closeAction = closeAction'
    }
  where
    windowConf =
      SDL.defaultWindow
      { SDL.windowInitialSize =
          V2 (fromIntegral 800) (fromIntegral 600)
      }

initAction' :: IO (Event SDL.EventPayload)
initAction' = return NoEvent

inputAction' :: MVar DTime -> Bool -> IO (DTime, Maybe (Event SDL.EventPayload))
inputAction' lastInteraction _canBlock = do
  currentTime <- SDL.time
  dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
  threadDelay $ round (10000-dt) -- 10ms pause for roughly 100 FPS
  mEvent <- SDL.pollEvent
  return (dt, Event . SDL.eventPayload <$> mEvent)

outputAction' :: SDL.Renderer -> Bool -> AppOutput -> IO Bool
outputAction' renderer changed ao = do
  when changed $ do
    let os = sortBy (\r1 r2 -> zIndex r1 `compare` zIndex r2) $ objects (graphics ao)
        c = camera (graphics ao)
    SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
    SDL.clear renderer
    mapM_ (renderView renderer c) os
    SDL.present renderer
  return (shouldExit ao)

parseInput' :: SF (Event SDL.EventPayload) AppInput
parseInput' = accumHoldBy onSDLInput initAppInput

onSDLInput :: AppInput -> SDL.EventPayload -> AppInput
onSDLInput ai SDL.QuitEvent = ai {inpQuit = True}
onSDLInput ai (SDL.KeyboardEvent key) = ai {inpKey = Just ()}
onSDLInput ai _ = initAppInput

renderView :: SDL.Renderer -> Camera -> RenderShape -> IO ()
renderView renderer camera rs = do
  let cameraPoint = shapeCentre (view camera)
      (cameraW, cameraH) = rectSize (view camera)
      shapePoint = shapeCentre (shape rs)
      adjustedShape = (shape rs) {shapeCentre = shapePoint `plusDir` (iso ((*(-1)) <$> cameraPoint)) `plusDir` (makeRel2 (cameraW/2, cameraH/2))}
  case adjustedShape of
    Rectangle {shapeCentre=Point2 (x, y), rectSize=(w, h)} -> do
      let (RGB r g b) = toSRGB24 (colour rs)
      SDL.rendererDrawColor renderer $= V4 r g b maxBound
      SDL.fillRect renderer $
        Just $
          SDL.Rectangle (round <$> P (V2 (x-w/2) (y-h/2)) ) (round <$> V2 w h )

