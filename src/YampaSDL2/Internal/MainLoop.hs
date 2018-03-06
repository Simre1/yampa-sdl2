module YampaSDL2.Internal.MainLoop
  ( mainLoop
  ) where

import FRP.Yampa

import YampaSDL2.Internal.AppInput (AppInput)
import YampaSDL2.Internal.AppOutput (AppOutput)
import YampaSDL2.Internal.SDL (SDLInit(..))

-- | Starts the game loop
mainLoop ::
     SDLInit a AppOutput
  -> SF AppInput AppOutput -- ^ The main signal function
  -> IO ()
mainLoop backend sf = do
  reactimate
    (initAction backend)
    (inputAction backend)
    (outputAction backend)
    (parseInput backend >>> sf)
  closeAction backend
