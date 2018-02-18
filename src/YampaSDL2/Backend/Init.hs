module YampaSDL2.Backend.Init
  (initAction) where

import FRP.Yampa
import qualified SDL

initAction :: IO (Event SDL.EventPayload)
initAction = return NoEvent

