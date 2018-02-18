module YampaSDL2.MainLoop
  (mainLoop) where

import FRP.Yampa

import YampaSDL2.AppInput
import YampaSDL2.AppOutput
import YampaSDL2.Backend

mainLoop :: Backend a AppOutput -> SF AppInput AppOutput -> IO ()
mainLoop backend sf = do
  reactimate
    (initAction backend)
    (inputAction backend)
    (outputAction backend)
    (parseInput backend >>> sf)
  closeAction backend
