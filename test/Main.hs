{-# Language Arrows #-}

import FRP.Yampa
import YampaEngine
import YampaEngine.Backend.SDL

import Debug.Trace

main :: IO ()
main = do
  backend <- sdlBackend
  mainLoop backend sf


sf :: SF AppInput AppOutput
sf = proc input -> do
  anyKeyE <- anyKeyActiveEvent -< input
  point <- accumHoldBy (\p int -> p `plusDir` direction int) (Point2 (0,0)) -< anyKeyE
  shouldQuit <- quitEvent -< input
  let camera = Camera $ Rectangle (Point2 (0,0)) (800,600)
      obj1 =  Rectangle (Point2 (0,0)) (100,100)
      obj2 = Rectangle point (50,50)
  returnA -< AppOutput
    { graphics = Graphics
      { camera = camera
      , objects = [R obj1 blue 0, R obj2 orange 1]
      }
    , sound = []
    , shouldExit = isEvent shouldQuit
    }
  where direction 79 = makeRel2 (1,0)
        direction 80 = makeRel2 (-1,0)
        direction 81 = makeRel2 (0,-1)
        direction 82 = makeRel2 (0,1)
        direction _ = makeRel2 (0,0)

