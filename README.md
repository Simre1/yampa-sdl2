# YampaEngine

YampaEngine aims to speed up game developement with the FRP library [Yampa](https://github.com/ivanperez-keera/Yampa).

Screenshot of the example below:
![Screenshot](test/screenshot.png)

Planned features:
- Display 2D graphics
- Play sound
- Collision detection
- Physics engine

Currently the only thing you can do is displaying Rectangles.

## How to Use

By importing YampaEngine, you automatically import:
- YampaEngine.MainLoop
- YampaEngine.Geometry
- YampaEngine.AppInput
- YampaEngine.AppOutput
- YampaEngine.Backend
- [Data.Colour.SRGB](https://hackage.haskell.org/package/colour-2.3.4/docs/Data-Colour-SRGB.html)
- [Data.Colour.Names](https://hackage.haskell.org/package/colour-2.3.4/docs/Data-Colour-Names.html)
- [Data.SG](https://hackage.haskell.org/package/SGplus-1.1)

In addition to importing YampaEngine, you also need to import a graphics backend. The only option right now is SDL, which you can find in YampaEngine.Backend.SDL. That means you need the library [sdl](https://www.libsdl.org/) installed on your pc.

Here is a working example that you can also find in the test folder. You can move the orange square with the arrow keys.

```haskell
{-# Language Arrows #-}

import FRP.Yampa
import YampaEngine
import YampaEngine.Backend.SDL
  
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
```
