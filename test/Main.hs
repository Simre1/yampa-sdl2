{-# Language Arrows #-}

import FRP.Yampa
import YampaEngine
import YampaEngine.Backend.SDL

main :: IO ()
main = do
  backend <- sdlBackend
  mainLoop backend sf


sf :: SF AppInput AppOutput
sf = proc input -> do
  let camera = Camera $ Rectangle (Point2 (0,0)) (800,600)
      obj1 =  Rectangle (Point2 (0,0)) (100,100)
      obj2 = Rectangle (Point2 (50,50)) (100,100)
  returnA -< AppOutput
    { graphics = Graphics
      { camera = camera
      , objects = [R obj1 blue 0, R obj2 orange 1]
      }
    , sound = []
    , shouldExit = False
    }

