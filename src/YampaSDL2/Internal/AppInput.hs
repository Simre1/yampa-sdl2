module YampaSDL2.Internal.AppInput
  ( initAppInput
  , AppInput(..)
  ) where

import Linear.V2
import SDL.Input.Keyboard.Codes

-- | Data type which stores all user inputs at the current time. You should not interact with this type directly, but use signal functions like 'anyKeyActive' to get events.
data AppInput = AppInput
  { inpQuit :: Bool -- ^ If the app receives a quit event
  , inpKey :: [Scancode] -- ^ All pressed keys
  , inpMousePos :: V2 Double -- ^ The current mouse position
  , inpMouseLeft :: Maybe (V2 Double) -- ^ User presses the left mouse button
  , inpMouseRight :: Maybe (V2 Double) -- ^ User presses the right mouse button
  }

initAppInput :: AppInput
initAppInput =
  AppInput
  { inpQuit = False
  , inpKey = []
  , inpMousePos = V2 0 0
  , inpMouseLeft = Nothing
  , inpMouseRight = Nothing
  }
