module YampaEngine.AppInput where

import FRP.Yampa
  
data AppInput = AppInput
  { inpQuit :: Bool
  , inpKey :: Maybe ()
  }

initAppInput :: AppInput
initAppInput = AppInput False Nothing

quitEvent :: SF AppInput (Event ())
quitEvent = inpQuit ^>> edge

