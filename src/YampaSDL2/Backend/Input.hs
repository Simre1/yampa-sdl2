module YampaSDL2.Backend.Input
  (inputAction) where

import qualified SDL
import Control.Concurrent
import FRP.Yampa
import Debug.Trace

inputAction :: MVar DTime -> Bool -> IO (DTime, Maybe (Event SDL.EventPayload))
inputAction lastInteraction _canBlock = do
  maybeEvent <- SDL.waitEventTimeout maximumWaitTime
  currentTime <- SDL.time
  dt <- (currentTime -) <$> swapMVar lastInteraction currentTime

  return (dt, Event . traceShowId . SDL.eventPayload <$> maybeEvent)

minIPS = 30

maximumWaitTime = round $ 1000/fromIntegral minIPS
