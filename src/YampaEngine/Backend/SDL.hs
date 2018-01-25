{-# LANGUAGE OverloadedStrings #-}

module YampaEngine.Backend.SDL where

import Control.Concurrent
import Control.Monad
import Data.Colour.SRGB
import Data.List (sortBy)
import Data.SG
import Data.StateVar (($=))
import Data.Text (pack)
import FRP.Yampa
import qualified SDL
import SDL.Vect

import YampaEngine.AppInput (AppInput(..), initAppInput)
import YampaEngine.AppOutput
import YampaEngine.Backend
import YampaEngine.Geometry

sdlBackend ::
     BackendConfiguration -> IO (Backend (Event SDL.EventPayload) AppOutput)
sdlBackend bc = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow (pack $ windowName bc) windowConf
  SDL.showWindow window
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
  let closeAction' = do
        SDL.destroyRenderer renderer
        SDL.destroyWindow window
        SDL.quit
  lastInteraction <- newMVar =<< SDL.time
  return $
    Backend
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
          V2 (fromIntegral (windowWidth bc)) (fromIntegral (windowHeight bc))
      }

initAction' :: IO (Event SDL.EventPayload)
initAction' = return NoEvent

inputAction' :: MVar DTime -> Bool -> IO (DTime, Maybe (Event SDL.EventPayload))
inputAction' lastInteraction _canBlock = do
  currentTime <- SDL.time
  dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
  threadDelay $ round (10000 - dt) -- 10ms pause for roughly 100 FPS
  mEvent <- SDL.pollEvent
  return (dt, Event . SDL.eventPayload <$> mEvent)

outputAction' :: SDL.Renderer -> Bool -> AppOutput -> IO Bool
outputAction' renderer changed ao = do
  let os =
        sortBy (\r1 r2 -> zIndex r1 `compare` zIndex r2) $ objects (graphics ao)
      c = camera (graphics ao)

  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
  texture <-
    SDL.createTexture
      renderer
      SDL.RGB24
      SDL.TextureAccessTarget
      ((fmap round . uncurry V2 . rectSize . view) c)
  SDL.rendererRenderTarget renderer $= return texture
  SDL.clear renderer
  mapM_ (renderView renderer c) os
  SDL.rendererRenderTarget renderer $= Nothing
  SDL.clear renderer
  SDL.copy renderer texture Nothing Nothing
  SDL.present renderer
  SDL.destroyTexture texture
  return (shouldExit ao)

parseInput' :: SF (Event SDL.EventPayload) AppInput
parseInput' = accumHoldBy onSDLInput initAppInput

onSDLInput :: AppInput -> SDL.EventPayload -> AppInput
onSDLInput ai SDL.QuitEvent = ai {inpQuit = True}
onSDLInput ai (SDL.KeyboardEvent ev)
  | SDL.keyboardEventKeyMotion ev == SDL.Pressed =
    ai {inpKey = Just $ getKeyCode ev}
  | SDL.keyboardEventKeyMotion ev == SDL.Released =
    ai
    { inpKey =
        if inpKey ai == return (getKeyCode ev)
          then Nothing
          else inpKey ai
    }
  where
    getKeyCode =
      fromIntegral .
      SDL.unwrapScancode . SDL.keysymScancode . SDL.keyboardEventKeysym
onSDLInput ai (SDL.MouseMotionEvent ev) =
  ai {inpMousePos = (fromIntegral x, fromIntegral y)}
  where
    P (V2 x y) = SDL.mouseMotionEventPos ev
onSDLInput ai (SDL.MouseButtonEvent ev) =
  ai {inpMouseLeft = lmb, inpMouseRight = rmb}
  where
    motion = SDL.mouseButtonEventMotion ev
    button = SDL.mouseButtonEventButton ev
    pos = inpMousePos ai
    inpMod =
      case (motion, button) of
        (SDL.Released, SDL.ButtonLeft) -> first (const Nothing)
        (SDL.Pressed, SDL.ButtonLeft) -> first (const (Just pos))
        (SDL.Released, SDL.ButtonRight) -> second (const Nothing)
        (SDL.Pressed, SDL.ButtonRight) -> second (const (Just pos))
        _ -> id
    (lmb, rmb) = inpMod $ (inpMouseLeft &&& inpMouseRight) ai
onSDLInput ai _ = ai

renderView :: SDL.Renderer -> Camera -> RenderShape -> IO ()
renderView renderer camera rs = do
  let cameraPoint = shapeCentre (view camera)
      (cameraW, cameraH) = rectSize (view camera)
      shapePoint = shapeCentre (shape rs)
      adjustedShape =
        (shape rs)
        { shapeCentre =
            shapePoint `plusDir` (iso ((* (-1)) <$> cameraPoint)) `plusDir`
            (makeRel2 (cameraW / 2, cameraH / 2))
        }
  case adjustedShape of
    Rectangle {shapeCentre = Point2 (x, y), rectSize = (w, h)} -> do
      let (RGB r g b) = toSRGB24 (colour rs)
      SDL.rendererDrawColor renderer $= V4 r g b maxBound
      SDL.fillRect
        renderer
        (Just $
         SDL.Rectangle
           (round <$> P (V2 (x - w / 2) (cameraH - (y + h / 2))))
           (round <$> V2 w h))
  return ()
