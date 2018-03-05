{-# Language Arrows #-}

import FRP.Yampa
import YampaSDL2
import Debug.Trace
import Data.Maybe
import Data.Foldable

main :: IO ()
main = do
  backend <- sdlBackend defaultBackendConfiguration
  mainLoop backend sf

sf :: SF AppInput AppOutput
sf = proc input -> do
  anyKeyE <- anyKeyActive -< input
  point <- accumHoldBy
    (\p keys -> p + foldl (\acc key -> acc + direction key) (V2 0 0) keys)
    (V2 0 0) -< anyKeyE
  shouldQuit <- quit -< input
  returnA -< AppOutput
    { graphics = Graphics
      { camera = camera
      , objects =
        [ rectangle (V2 0 0) (V2 100 100) (Filled (blue `withOpacity` 1)) 1
        , circle (V2 0 0) 150 (Filled (orange `withOpacity` 1)) 0
        , image (V2 100 0) (V2 100 100) Nothing "./test/MARBLES.BMP" 2
        ]
      }
    , sound = []
    , shouldExit = isEvent shouldQuit
    }
  where camera = Camera (V2 0 0) (V2 800 600)
        direction ScancodeRight = V2 2 0
        direction ScancodeLeft = V2 (-2) 0
        direction ScancodeDown = V2 0 (-2)
        direction ScancodeUp = V2 0 2
        direction _ = V2 0 0
