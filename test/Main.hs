{-# Language Arrows #-}

import FRP.Yampa
import YampaEngine
import YampaEngine.Backend.SDL

import Data.Maybe
import Debug.Trace

main :: IO ()
main = do
  backend <- sdlBackend defaultBackendConfiguration
  mainLoop backend sf


sf :: SF AppInput AppOutput
sf = proc input -> do
  anyKeyE <- anyKeyActiveEvent -< input
  point <- accumHoldBy
    (\p int -> p `plusDir` direction int)
    (Point2 (0,0)) -< anyKeyE
  shouldQuit <- quitEvent -< input

  objAnimated <- animate animation -< ()
  returnA -< AppOutput
    { graphics = Graphics
      { camera = camera
      , objects = [obj1 point, fromMaybe obj5 objAnimated]
      }
    , sound = []
    , shouldExit = isEvent shouldQuit
    }
  where camera = Camera $ Rectangle (Point2 (0,0)) (800,600)
        shape1 =  Rectangle (Point2 (0,0)) (100,100)
        shape2 point = Rectangle point (50,50)
        obj1 point = R (shape2 point) blue 2
        obj2 = R shape1 yellow 1
        obj3 = R shape1 violet 1
        obj4 = R shape1 green 1
        obj5 = R shape1 orange 1
        animation = newAnimation [(0.5,obj2), (0.5,obj3), (0.5,obj4), (0.5, obj5)] Endless
        direction 79 = makeRel2 (1,0)
        direction 80 = makeRel2 (-1,0)
        direction 81 = makeRel2 (0,-1)
        direction 82 = makeRel2 (0,1)
        direction _ = makeRel2 (0,0)

