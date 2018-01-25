{-# Language Arrows #-}

module YampaEngine.AppInput where

import Debug.Trace
import FRP.Yampa
import FRP.Yampa.Event (maybeToEvent)
import Data.Maybe (isJust)
 
data AppInput = AppInput
  { inpQuit :: Bool
  , inpKey :: Maybe Int
  , inpMousePos :: (Double, Double)
  , inpMouseLeft :: Maybe (Double, Double)
  , inpMouseRight :: Maybe (Double, Double)
  }

initAppInput :: AppInput
initAppInput = AppInput
  { inpQuit = False
  , inpKey = Nothing
  , inpMousePos = (0, 0)
  , inpMouseLeft = Nothing
  , inpMouseRight = Nothing
  }
quitEvent :: SF AppInput (Event ())
quitEvent = inpQuit ^>> edge

-- This will fire events as long as the key is pressed
anyKeyActiveEvent :: SF AppInput (Event Int)
anyKeyActiveEvent = inpKey ^>> arr maybeToEvent

-- This will fire events only once when a key is pressed. Pressing the key while another is already pressed will not work.
anyKeyPressEvent :: SF AppInput (Event Int)
anyKeyPressEvent = inpKey ^>> edgeJust 

