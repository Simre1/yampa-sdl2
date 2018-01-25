{-# Language Arrows #-}

module YampaEngine.AppInput where

import Debug.Trace
import FRP.Yampa
import Data.Maybe (isJust)
  
data AppInput = AppInput
  { inpQuit :: Bool
  , inpKey :: Maybe ()
  }

initAppInput :: AppInput
initAppInput = AppInput False Nothing

quitEvent :: SF AppInput (Event ())
quitEvent = inpQuit ^>> edge

anyKeyEvent :: SF AppInput (Event ())
anyKeyEvent = isJust . inpKey ^>> edge
