{-|
Module      : Backend
-}
module YampaSDL2.Backend
  ( -- * Backend
    Backend(..)
  , BackendConfiguration(..)
  , defaultBackendConfiguration
  ) where

import FRP.Yampa

import YampaSDL2.AppInput (AppInput)


data Backend a b = Backend
  { initAction :: IO a
  , inputAction :: Bool -> IO (DTime, Maybe a)
  , outputAction :: Bool -> b -> IO Bool
  , parseInput :: SF a AppInput
  , closeAction :: IO ()
  }

data BackendConfiguration = BackendConfiguration
  { windowWidth :: Int
  , windowHeight :: Int
  , windowName :: String
  , windowResizable :: Bool
  }

defaultBackendConfiguration = BackendConfiguration
  { windowWidth = 800
  , windowHeight = 600
  , windowName = "App"
  , windowResizable = True
  }
