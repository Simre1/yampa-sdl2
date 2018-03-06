{-# LANGUAGE OverloadedStrings #-}

module YampaSDL2.Internal.SDL.Start
  ( initSDL
  ) where

import Control.Concurrent.MVar
import Data.StateVar (($=))
import Data.Text (pack)
import FRP.Yampa
import Linear.V2
import qualified SDL

import qualified YampaSDL2.Internal.SDL.Close as Close
import qualified YampaSDL2.Internal.SDL.Init as Init
import qualified YampaSDL2.Internal.SDL.Input as Input
import qualified YampaSDL2.Internal.SDL.Output as Output
import qualified YampaSDL2.Internal.SDL.Parse as Parse

import YampaSDL2.Internal.AppOutput (AppOutput)
import YampaSDL2.Internal.SDL.Init
       (SDLConfiguration(..), SDLInit(..))

-- | Set up SDL
initSDL :: SDLConfiguration -> IO (SDLInit (Event SDL.EventPayload) AppOutput)
initSDL bc = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow (pack $ windowName bc) windowConf
  SDL.showWindow window
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  SDL.rendererDrawBlendMode renderer $= SDL.BlendAlphaBlend
  lastInteraction <- newMVar =<< SDL.time
  lastScene <- newMVar Nothing
  lastRender <- newMVar 0
  cache <- newMVar []
  ready <- newMVar True
  return $
    SDLInit
    { initAction = Init.firstEvent
    , inputAction = Input.inputAction lastInteraction
    , outputAction =
        Output.outputAction
          cache
          (fps bc)
          lastRender
          ready
          lastScene
          window
          renderer
    , parseInput = Parse.parseInput
    , closeAction = Close.closeAction renderer window
    }
  where
    windowConf =
      SDL.defaultWindow
      { SDL.windowInitialSize =
          V2 (fromIntegral (windowWidth bc)) (fromIntegral (windowHeight bc))
      , SDL.windowResizable = windowResizable (bc)
      }
