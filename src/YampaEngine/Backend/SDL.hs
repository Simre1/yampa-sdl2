module YampaEngine.Backend.SDL where

import qualified SDL as SDL
import FRP.Yampa
import Control.Concurrent
import Control.Monad

import YampaEngine.Backend (Backend(..))
import YampaEngine.AppInput (AppInput(..), initAppInput)
import YampaEngine.AppOutput (AppOutput(..))

sdlBackend :: IO (Backend (Event SDL.EventPayload) AppOutput)
sdlBackend = do 
  lastInteraction <- newMVar =<< SDL.time
  return $ Backend
    { initAction = initAction'
    , inputAction = inputAction' lastInteraction
    , outputAction = outputAction'
    , parseInput = parseInput'
    }

initAction' :: IO (Event SDL.EventPayload)
initAction' = return NoEvent

inputAction' :: MVar DTime -> Bool -> IO (DTime, Maybe (Event SDL.EventPayload))
inputAction' lastInteraction _canBlock = do
  currentTime <- SDL.time
  dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
  mEvent <- SDL.pollEvent
  return (dt, Event . SDL.eventPayload <$> mEvent)

outputAction' :: Bool -> AppOutput -> IO Bool
outputAction' changed ao =do
  when changed $ print (i ao)
  return (shouldExit ao)

parseInput' :: SF (Event SDL.EventPayload) AppInput
parseInput' = accumHoldBy onSDLInput initAppInput

onSDLInput :: AppInput -> SDL.EventPayload -> AppInput
onSDLInput ai SDL.QuitEvent = ai {inpQuit = True}
onSDLInput ai (SDL.KeyboardEvent key) = ai {inpKey = Just ()}
