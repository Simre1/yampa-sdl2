# YampaEngine

YampaSDL2 exports some SDL2 bindings to use with the FRP library [Yampa](https://github.com/ivanperez-keera/Yampa).

Screenshot of the example below:
![Screenshot](screenshot.png)

Planned features:
- Display 2D graphics
- Play sound

## How to Use

Here is the example you see at the top. You can also find it in ./examples/SimpleRendering.hs.

```haskell
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
      { camera = Camera $ (V2 0 (-15)) (V2 200 200)
      , objects =
        [rBottom, rMiddle, rTopLeft, rTopRight] 
      }
    , sound = []
    , shouldExit = shouldQuit
    }

  where
    rColor = Filled orange
    rBottom = RS (Rectangle (V2 0 (-82)) (V2 100 50)) rColor 0
    rMiddle = RS (Rectangle (V2 0 0) (V2 70 115)) rColor 0
    rTopLeft = RS (Rectangle (V2 (-33) 60) (V2 33 35)) rColor 0
    rTopRight = RS (Rectangle (V2 33 60) (V2 33 35)) rColor 0
```
