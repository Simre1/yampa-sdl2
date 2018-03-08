module YampaSDL2.Init
  ( -- * Initialization
    initSDL
  , SDLConfiguration(..)
  , defaultSDLConfiguration
  , SDLInit
  , mainLoop
  , defaultLoop
  ) where

import FRP.Yampa

import YampaSDL2.Internal.AppInput (AppInput)
import YampaSDL2.Internal.AppOutput (AppOutput)
import YampaSDL2.Internal.MainLoop (mainLoop)
import YampaSDL2.Internal.SDL
       (SDLConfiguration(..), SDLInit, defaultSDLConfiguration, initSDL)

-- | 'mainLoop' with default configurations
defaultLoop :: SF AppInput AppOutput -> IO ()
defaultLoop sf = do
  sdl <- initSDL defaultSDLConfiguration
  mainLoop sdl sf
