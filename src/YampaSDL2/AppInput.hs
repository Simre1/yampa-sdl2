{-# Language Arrows #-}

module YampaSDL2.AppInput

  ( AppInput(..)
  , module SDL.Input.Keyboard.Codes
  , initAppInput
  , quit
  , anyKeyActive
  , anyKeyPress
  , mouseLeftActive
  , mouseLeftPress
  , mouseRightActive
  , mouseRightPress
  , mousePosition
  ) where

import FRP.Yampa
import FRP.Yampa.Event (maybeToEvent)
import Data.Maybe (isJust)
import SDL.Input.Keyboard.Codes
import Linear.V2

data AppInput = AppInput
  { inpQuit :: Bool
  , inpKey :: Maybe Scancode
  , inpMousePos :: V2 Double
  , inpMouseLeft :: Maybe (V2 Double)
  , inpMouseRight :: Maybe (V2 Double)
  }

initAppInput :: AppInput
initAppInput = AppInput
  { inpQuit = False
  , inpKey = Nothing
  , inpMousePos = V2 0 0
  , inpMouseLeft = Nothing
  , inpMouseRight = Nothing
  }

quit :: SF AppInput (Event ())
quit = inpQuit ^>> edge

-- This will fire events as long as the key is pressed
anyKeyActive :: SF AppInput (Event Scancode)
anyKeyActive = inpKey ^>> arr maybeToEvent

-- This will fire events only once when a key is pressed. Pressing the key while another is already pressed will not work.
anyKeyPress :: SF AppInput (Event Scancode)
anyKeyPress = inpKey ^>> edgeJust

mouseLeftActive :: SF AppInput (Event (V2 Double))
mouseLeftActive = inpMouseLeft ^>> arr maybeToEvent

mouseLeftPress :: SF AppInput (Event (V2 Double))
mouseLeftPress = inpMouseLeft ^>> edgeJust

mouseRightActive :: SF AppInput (Event (V2 Double))
mouseRightActive = inpMouseRight ^>> arr maybeToEvent

mouseRightPress :: SF AppInput (Event (V2 Double))
mouseRightPress = inpMouseRight ^>> edgeJust

mousePosition :: SF AppInput (V2 Double)
mousePosition = arr inpMousePos
