{-|
Module      : Input
Description : Contains all input SF functions
-}

{-# Language Arrows #-}

module YampaSDL2.AppInput
  ( -- * Input
    AppInput(..)
  , initAppInput
    -- ** SFs
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

-- | Your main SF receives AppInput as input
data AppInput = AppInput
  { inpQuit :: Bool
  , inpKey :: [Scancode]
  , inpMousePos :: V2 Double
  , inpMouseLeft :: Maybe (V2 Double)
  , inpMouseRight :: Maybe (V2 Double)
  } deriving Show

initAppInput :: AppInput
initAppInput = AppInput
  { inpQuit = False
  , inpKey = []
  , inpMousePos = V2 0 0
  , inpMouseLeft = Nothing
  , inpMouseRight = Nothing
  }

quit :: SF AppInput (Event ())
quit = inpQuit ^>> edge

anyKeyActive :: SF AppInput (Event [Scancode])
anyKeyActive = inpKey ^>> (\e -> if e == [] then Nothing else return e) ^>> arr maybeToEvent

anyKeyPress :: SF AppInput (Event [Scancode])
anyKeyPress = inpKey ^>> (\e -> if e == [] then Nothing else return e) ^>> edgeJust

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
