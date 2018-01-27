# YampaEngine

YampaEngine aims to speed up game developement with the FRP library [Yampa](https://github.com/ivanperez-keera/Yampa).

Screenshot of the example below:
![Screenshot](screenshot.png)

Planned features:
- Display 2D graphics
- Play sound
- Collision detection
- Physics engine

## How to Use

By importing YampaEngine, you automatically import:
- YampaEngine.MainLoop
- YampaEngine.Geometry
- YampaEngine.AppInput
- YampaEngine.AppOutput
- YampaEngine.Backend
- YampaEngine.Animation
- [Data.Colour.SRGB](https://hackage.haskell.org/package/colour-2.3.4/docs/Data-Colour-SRGB.html)
- [Data.Colour.Names](https://hackage.haskell.org/package/colour-2.3.4/docs/Data-Colour-Names.html)

In addition to importing YampaEngine, you also need to import a graphics backend. The only option right now is SDL, which you can find in YampaEngine.Backend.SDL. That means you need the library [sdl](https://www.libsdl.org/) installed on your pc.

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
```
