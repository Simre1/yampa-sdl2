module YampaSDL2.InputOutput
  ( -- * Input
    AppInput
    -- ** Input Signals
  , mouseLeftActive
  , mouseLeftPress
  , mouseRightActive
  , mouseRightPress
  , mousePosition
  , anyKeyActive
  , anyKeyPress
  , quit
  -- * Output
  , AppOutput (..)
  , output
  -- ** Scene
  , Scene (..)
  , render
  -- *** Camera
  , Camera (..)
  , camera
  -- *** RenderObject
  , Center
  , Bounds
  , Cache
  , RenderObject (..)
  , translate
  , ShapeColour (..)
  -- ** Sound
  , Sound (..)
  ) where

import FRP.Yampa
import FRP.Yampa.Event (maybeToEvent)
import Data.Maybe (isJust)
import SDL.Input.Keyboard.Codes
import Linear.V2

import YampaSDL2.Internal.AppInput
import YampaSDL2.Internal.AppOutput

-- Output

output :: Scene -> [Sound] -> Bool -> AppOutput
output = AppOutput

camera :: V2 Double -> V2 Double -> Camera
camera = Camera

render :: Camera -> [RenderObject] -> Scene
render = Scene

-- Input

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
