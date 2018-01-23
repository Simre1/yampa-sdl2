module YampaEngine.AppOutput
  (AppOutput(..))where

import FRP.Yampa

import YampaEngine.Backend (Backend(..))

data AppOutput = AppOutput
  { i :: Int
  , shouldExit :: Bool
  }
