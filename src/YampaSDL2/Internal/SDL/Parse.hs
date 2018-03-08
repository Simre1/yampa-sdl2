module YampaSDL2.Internal.SDL.Parse
  ( parseInput
  ) where

import FRP.Yampa
import qualified SDL
import SDL.Vect

import YampaSDL2.Internal.AppInput (AppInput(..), initAppInput)

parseInput :: SF (Event SDL.EventPayload) AppInput
parseInput = accumHoldBy onSDLInput initAppInput

onSDLInput :: AppInput -> SDL.EventPayload -> AppInput
onSDLInput ai SDL.QuitEvent = ai {inpQuit = True}
onSDLInput ai (SDL.KeyboardEvent ev)
  | SDL.keyboardEventKeyMotion ev == SDL.Pressed =
    ai
    {inpKey = filter (/= getKeyCode ev) (inpKey ai) `mappend` [getKeyCode ev]}
  | SDL.keyboardEventKeyMotion ev == SDL.Released =
    ai {inpKey = filter (/= getKeyCode ev) (inpKey ai)}
  where
    getKeyCode = SDL.keysymScancode . SDL.keyboardEventKeysym
onSDLInput ai (SDL.MouseMotionEvent ev) =
  ai {inpMousePos = fromIntegral <$> V2 x y}
  where
    P (V2 x y) = SDL.mouseMotionEventPos ev
onSDLInput ai (SDL.MouseButtonEvent ev) =
  ai {inpMouseLeft = lmb, inpMouseRight = rmb}
  where
    motion = SDL.mouseButtonEventMotion ev
    button = SDL.mouseButtonEventButton ev
    pos = inpMousePos ai
    inpMod =
      case (motion, button) of
        (SDL.Released, SDL.ButtonLeft) -> first (const Nothing)
        (SDL.Pressed, SDL.ButtonLeft) -> first (const (Just pos))
        (SDL.Released, SDL.ButtonRight) -> second (const Nothing)
        (SDL.Pressed, SDL.ButtonRight) -> second (const (Just pos))
        _ -> id
    (lmb, rmb) = inpMod $ (inpMouseLeft &&& inpMouseRight) ai
onSDLInput ai _ = ai
