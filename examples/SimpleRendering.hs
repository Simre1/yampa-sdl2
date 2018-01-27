{-# Language Arrows #-}

import FRP.Yampa
import Linear.V2
import YampaEngine
import YampaEngine.Backend.SDL


main :: IO ()
main = do
  backend <- sdlBackend defaultBackendConfiguration
    { windowWidth = 600
    , windowHeight = 600
    }
    
  mainLoop backend sf


sf :: SF AppInput AppOutput
sf = proc i -> do
  shouldQuit <- isEvent ^<< quitEvent -< i
  returnA -< AppOutput
    { graphics = Graphics
      { camera = Camera $ Rectangle (V2 0 (-15)) (V2 200 200)
      , objects =
        [rBottom, rMiddle, rTopLeft, rTopRight] 
      }
    , sound = []
    , shouldExit = shouldQuit
    }

  where
    rColor = orange
    rBottom = R (Rectangle (V2 0 (-82)) (V2 100 50)) rColor 0
    rMiddle = R (Rectangle (V2 0 0) (V2 70 115)) rColor 0
    rTopLeft = R (Rectangle (V2 (-33) 60) (V2 33 35)) rColor 0
    rTopRight = R (Rectangle (V2 33 60) (V2 33 35)) rColor 0
