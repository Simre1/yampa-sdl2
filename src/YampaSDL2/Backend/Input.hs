module YampaSDL2.Backend.Input
  (inputAction) where

import qualified SDL
import Control.Concurrent
import FRP.Yampa

inputAction :: MVar DTime -> Bool -> IO (DTime, Maybe (Event SDL.EventPayload))
inputAction lastInteraction _canBlock = do
  currentTime <- SDL.time
  dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
  let waitTime = if dt < delayTime then delayTime - dt else 0
  --threadDelay $ round waitTime -- 10ms pause for roughly 100 FPS
  mEvent <- SDL.pollEvent
  return (dt, Event . SDL.eventPayload <$> mEvent)

maxFPS = 100

delayTime = 1000000/maxFPS
