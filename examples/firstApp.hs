{-# LANGUAGE Arrows #-}

import FRP.Yampa
import YampaSDL2

main :: IO ()
main = defaultLoop mainSF

mainSF :: SF AppInput AppOutput
mainSF = proc i -> do
  shouldQuit <- isEvent ^<< quit -< i
  returnA -< output
    (render myCamera [blueCircle, whiteBackground])
    []
    shouldQuit

myCamera :: Camera
myCamera = camera (V2 0 0) (V2 800 600)

blueCircle :: RenderObject
blueCircle = circle (V2 0 0) 100 (Filled $ blue `withOpacity` 1) 1

whiteBackground :: RenderObject
whiteBackground = rectangle (V2 0 0) (V2 800 600) (Filled $ white `withOpacity` 1) 0
