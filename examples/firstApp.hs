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
               { scene = Scene
                 { camera = myCamera
                 , objects = [whiteBackground, blueCircle]
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
  { shapeCentre = V2 0 0
  , shape = Circle 100 (Filled blue)
  , zIndex = 1
  }

whiteBackground :: RenderShape
whiteBackground = RS
  { shapeCentre = V2 0 0
  , shape = Rectangle (V2 800 600) (Filled white)
  , zIndex = 0
  }
