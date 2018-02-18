module YampaSDL2
  ( module YampaSDL2.MainLoop
  , module YampaSDL2.Geometry
  , module YampaSDL2.AppInput
  , module YampaSDL2.AppOutput
  , module YampaSDL2.Backend
  , module YampaSDL2.Animation
  , module YampaSDL2.Backend.SDL
  , module Data.Colour.Names
  , module Data.Colour.SRGB
  , module Linear.V2
  , module SDL.Input.Keyboard.Codes
  ) where

import Linear.V2
import Data.Colour.Names
import Data.Colour.SRGB
import SDL.Input.Keyboard.Codes

import YampaSDL2.AppInput
  ( AppInput
  , quit
  , anyKeyActive
  , anyKeyPress
  , mouseLeftActive
  , mouseLeftPress
  , mouseRightActive
  , mouseRightPress
  , mousePosition
  )
import YampaSDL2.AppOutput
  ( AppOutput(..)
  , Graphics(..)
  , Sound(..)
  , RenderShape(..)
  , Camera(..)
  , ShapeColour(..)
  )
import YampaSDL2.Backend (defaultBackendConfiguration, BackendConfiguration(..))
import YampaSDL2.Geometry (Shape(..))
import YampaSDL2.MainLoop (mainLoop)
import YampaSDL2.Animation
  ( Animation
  , AnimationType(..)
  , animate
  , newAnimation
  )
import YampaSDL2.Backend.SDL (sdlBackend)
