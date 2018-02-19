{-# LANGUAGE OverloadedStrings #-}

module YampaSDL2.Backend.SDL
  (sdlBackend) where

import qualified SDL
import SDL.Vect
import Control.Concurrent.MVar
import FRP.Yampa
import Data.Text (pack)

import qualified YampaSDL2.Backend.Init as Init
import qualified YampaSDL2.Backend.Input as Input
import qualified YampaSDL2.Backend.Output as Output
import qualified YampaSDL2.Backend.Parse as Parse
import qualified YampaSDL2.Backend.Close as Close

import YampaSDL2.AppOutput (AppOutput)
import YampaSDL2.Backend

-- | Set up SDL
sdlBackend ::
     BackendConfiguration -> IO (Backend (Event SDL.EventPayload) AppOutput)
sdlBackend bc = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow (pack $ windowName bc) windowConf
  SDL.showWindow window
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  lastInteraction <- newMVar =<< SDL.time
  return $
    Backend
    { initAction = Init.initAction
    , inputAction = Input.inputAction lastInteraction
    , outputAction = Output.outputAction renderer
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

