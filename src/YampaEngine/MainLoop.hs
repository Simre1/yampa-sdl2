module YampaEngine.MainLoop where

import FRP.Yampa


import YampaEngine.AppInput
import YampaEngine.AppOutput
import YampaEngine.Backend

mainLoop :: Backend a AppOutput -> SF AppInput AppOutput -> IO ()
mainLoop backend sf = do
  reactimate
    (initAction backend)
    (inputAction backend)
    (outputAction backend)
    (parseInput backend >>> sf)
  closeAction backend
