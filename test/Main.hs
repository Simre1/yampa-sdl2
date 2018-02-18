{-# Language Arrows #-}

import FRP.Yampa
import YampaSDL2

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
      , objects = [Container (V2 200 200) [obj1 point, fromMaybe obj5 objAnimated]]
      }
    , sound = []
    , shouldExit = isEvent shouldQuit
    }
  where camera = Camera (V2 0 0) (V2 800 600)
        shape1 = Triangle (V2 0 0) (V2 0 50) (V2 50 (-50)) (V2 (-50) (-50))
        shape2 point = Rectangle point (V2 50 50)
        obj1 point = RS (shape2 point) (Filled blue) 2
        obj2 = RS shape1 (Filled yellow) 1
        obj3 = RS shape1 (Filled violet) 1
        obj4 = RS shape1 (Filled green) 1
        obj5 = RS shape1 (Filled orange) 1
        animation = newAnimation [(0.5,obj2), (0.5,obj3), (0.5,obj4), (0.5, obj5)] Endless
        direction ScancodeRight = V2 1 0
        direction ScancodeLeft = V2 (-1) 0
        direction ScancodeDown = V2 0 (-1)
        direction ScancodeUp = V2 0 1
        direction _ = V2 0 0

