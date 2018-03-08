module YampaSDL2.Internal.SDL.Init
  ( firstEvent
  , SDLInit(..)
  , SDLConfiguration(..)
  , defaultSDLConfiguration
  ) where

import FRP.Yampa
import qualified SDL

import YampaSDL2.Internal.AppInput (AppInput)

firstEvent :: IO (Event SDL.EventPayload)
firstEvent = return NoEvent

data SDLInit a b = SDLInit
  { initAction :: IO a
  , inputAction :: Bool -> IO (DTime, Maybe a)
  , outputAction :: Bool -> b -> IO Bool
  , parseInput :: SF a AppInput
  , closeAction :: IO ()
  }

-- | Configurations regarding the window.
data SDLConfiguration = SDLConfiguration
  { windowWidth :: Int
  , windowHeight :: Int
  , windowName :: String
  , windowResizable :: Bool
  , fps :: Double
  }


defaultSDLConfiguration :: SDLConfiguration
defaultSDLConfiguration =
  SDLConfiguration
  { windowWidth = 800
  , windowHeight = 600
  , windowName = "App"
  , windowResizable = True
  , fps = 60
  }
