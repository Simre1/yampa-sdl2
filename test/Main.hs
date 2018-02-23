{-# Language Arrows #-}

import FRP.Yampa
import YampaSDL2
import Debug.Trace
import Data.Maybe

main :: IO ()
main = do
  backend <- sdlBackend defaultBackendConfiguration
  mainLoop backend sf

sf :: SF AppInput AppOutput
sf = proc input -> do
  anyKeyE <- anyKeyActive -< input
  point <- accumHoldBy
    (\p int -> p + direction int)
    (V2 0 0) -< anyKeyE
  shouldQuit <- quit -< input

  objAnimated <- animate animation -< ()
  returnA -< AppOutput
    { graphics = Graphics
      { camera = camera
      , objects = [background] ++ container (V2 100 100) [obj1 point, fromMaybe obj5 
objAnimated]
      }
    , sound = []
    , shouldExit = isEvent shouldQuit
    }
  where camera = Camera (V2 0 0) (V2 800 600)
        shape1 colour = Triangle (V2 0 50) (V2 50 (-50)) (V2 (-50) (-50)) (Filled colour)
        shape2 = Rectangle (V2 50 50) (Filled blue)
        obj1 point = RS point shape2 2
        obj2 = RS (V2 0 0) (shape1 orange) 1
        obj3 = RS (V2 0 0) (shape1 white) 1
        obj4 = RS (V2 0 0) (shape1 green) 1
        obj5 = RS (V2 0 0) (shape1 yellow) 1
        animation = newAnimation [(0.5,obj2), (0.5,obj3), (0.5,obj4), (0.5, obj5)] Endless
        direction ScancodeRight = V2 2 0
        direction ScancodeLeft = V2 (-2) 0
        direction ScancodeDown = V2 0 (-2)
        direction ScancodeUp = V2 0 2
        direction _ = V2 0 0
        background = RS (V2 0 0) (Image (V2 800 600) Nothing "./test/MARBLES.BMP") 0
