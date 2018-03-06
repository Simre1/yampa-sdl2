{-# Language Arrows #-}

import FRP.Yampa
import YampaSDL2
import Debug.Trace
import Data.Maybe
import Data.Foldable

main :: IO ()
main = do
  backend <- initSDL defaultSDLConfiguration
  mainLoop backend sf

sf :: SF AppInput AppOutput
sf = proc input -> do
  anyKeyE <- anyKeyActive -< input
  point <- accumHoldBy
    (\p keys -> p + foldl (\acc key -> acc + direction key) (V2 0 0) keys)
    (V2 0 0) -< anyKeyE
  shouldQuit <- quit -< input
  returnA -< output
    (render cam
        [ rectangle (V2 0 0) (V2 100 100) (Filled (blue `withOpacity` 1)) 2
        , circle (V2 0 0) 150 (Filled (orange `withOpacity` 0.5)) 1
        , image (V2 0 0) (V2 800 600) Nothing "./test/MARBLES.BMP" 0
        ]
    )
    []
    (isEvent shouldQuit)

  where cam = camera (V2 0 0) (V2 800 600)
        direction ScancodeRight = V2 2 0
        direction ScancodeLeft = V2 (-2) 0
        direction ScancodeDown = V2 0 (-2)
        direction ScancodeUp = V2 0 2
        direction _ = V2 0 0
