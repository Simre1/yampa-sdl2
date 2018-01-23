module YampaEngine.Backend where

import FRP.Yampa
import YampaEngine.AppInput

data Backend a b = Backend
  { initAction :: IO a
  , inputAction :: Bool -> IO (DTime, Maybe a)
  , outputAction :: Bool -> b -> IO Bool
  , parseInput :: SF a AppInput
  }
