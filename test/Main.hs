{-# Language Arrows #-}

import FRP.Yampa

import YampaEngine.MainLoop
import YampaEngine.Backend.SDL
import YampaEngine.AppInput
import YampaEngine.AppOutput


main :: IO ()
main = do
  backend <- sdlBackend
  mainLoop backend sf


sf :: SF AppInput AppOutput
sf = proc input -> do
  returnA -< AppOutput {shouldExit=False, i=5}
