module YampaSDL2.Internal.SDL
  ( SDLInit(..)
  , SDLConfiguration(..)
  , defaultSDLConfiguration
  , initSDL
  ) where

import YampaSDL2.Internal.SDL.Init
       (SDLConfiguration(..), SDLInit(..), defaultSDLConfiguration)
import YampaSDL2.Internal.SDL.Start (initSDL)
