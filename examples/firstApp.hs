{-# LANGUAGE Arrows #-}

import FRP.Yampa
import YampaSDL2

main :: IO ()
main = do
  backend <- sdlBackend defaultBackendConfiguration
  mainLoop backend mainSF

mainSF :: SF AppInput AppOutput
mainSF = proc _ -> do
  returnA -< AppOutput
               { graphics = Graphics
                 { camera = myCamera
                 , objects = [blueCircle]
                 }
               , sound = []
               , shouldExit = False
               }

myCamera :: Camera
myCamera = Camera
  { cPos = V2 0 0
  , cSize = V2 800 600
  }

blueCircle :: RenderShape
blueCircle = RS
  { shape = Circle (V2 0 0) 100
  , colour = Filled blue
  , zIndex = 0
  }
