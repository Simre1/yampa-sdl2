{-|
Module      : MainLoop
-}


module YampaSDL2.MainLoop
  ( -- * MainLoop
    mainLoop
  ) where

import FRP.Yampa

import YampaSDL2.AppInput
import YampaSDL2.AppOutput
import YampaSDL2.Backend

-- | Starts the Yampa loop
mainLoop :: Backend a AppOutput -> SF AppInput AppOutput -> IO ()
mainLoop backend sf = do
  reactimate
    (initAction backend)
    (inputAction backend)
    (outputAction backend)
    (parseInput backend >>> sf)
  closeAction backend
