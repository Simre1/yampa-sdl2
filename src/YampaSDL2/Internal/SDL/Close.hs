module YampaSDL2.Internal.SDL.Close
  (closeAction) where

import qualified SDL

closeAction :: SDL.Renderer -> SDL.Window -> IO ()
closeAction r w = do
  SDL.destroyRenderer r
  SDL.destroyWindow w
  SDL.quit
