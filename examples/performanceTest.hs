{-# LANGUAGE Arrows #-}

import FRP.Yampa
import YampaSDL2

main :: IO ()
main = do
  backend <- sdlBackend defaultBackendConfiguration {fps=60}
  mainLoop backend mainSF

mainSF :: SF AppInput AppOutput
mainSF = proc i -> do
  anyKeyE <- anyKeyActive -< i
  point <- accumHoldBy
    (\p int -> p + direction int)
    (V2 0 0) -< anyKeyE
  shouldQuit <- quit -< i
  returnA -< AppOutput
               { graphics = Graphics
                 { camera = myCamera
                 , objects = [movingRectangle point, whiteBackground] ++ createShapes 1000 ++ container (V2 0 (-50)) (createShapes 1000)
                 }
               , sound = []
               , shouldExit = False
               }
  where direction ScancodeRight = V2 2 0
        direction ScancodeLeft = V2 (-2) 0
        direction ScancodeDown = V2 0 (-2)
        direction ScancodeUp = V2 0 2
        direction _ = V2 0 0

myCamera :: Camera
myCamera = Camera
  { cPos = V2 0 0
  , cSize = V2 800 600
  }

movingRectangle point = RS point (Rectangle (V2 50 50) (Filled orange)) 2

createShapes x =
  fmap blueCircle [1..x]

blueCircle :: Double -> RenderShape
blueCircle point = RS
  { shapeCentre = V2 point point - V2 400 300
  , shape = Rectangle (V2 50 50) (Filled blue)
  , zIndex = 1
  }

whiteBackground :: RenderShape
whiteBackground = RS
  { shapeCentre = V2 0 0
  , shape = Rectangle (V2 800 600) (Filled white)
  , zIndex = 0
  }
