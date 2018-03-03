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

  objAnimated <- animate animation (Object (V2 0 0) shape1 (Filled (blue `withOpacity` 1)) 1) -< ()
  returnA -< AppOutput
    { graphics = Graphics
      { camera = camera
      , objects = [background] ++ container (V2 100 100) [obj1 point, objAnimated]
      }
    , sound = []
    , shouldExit = isEvent shouldQuit
    }
  where camera = Camera (V2 0 0) (V2 800 600)
        shape1 = Circle 50
        shape2 = Rectangle (V2 50 50)
        obj1 point = Object point shape2 (Filled (orange `withOpacity` 0.5)) 2
        animation = [xAnimation 50 easeIn 3 `mappend` yAnimation 100 easeOut 3, yAnimation (-100) easeIn 2]
        direction ScancodeRight = V2 2 0
        direction ScancodeLeft = V2 (-2) 0
        direction ScancodeDown = V2 0 (-2)
        direction ScancodeUp = V2 0 2
        direction _ = V2 0 0
        background = Image (V2 0 0) (V2 800 600) Nothing "./test/MARBLES.BMP" 0
