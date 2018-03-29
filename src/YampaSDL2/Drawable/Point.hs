module YampaSDL2.Drawable.Point
  ( point
  ) where

import Data.StateVar (($=))
import Linear.V2
import Linear.V4 hiding (point)
import qualified SDL

import YampaSDL2.Internal.AppOutput


-- | Draw a point
--
-- Example:
--
-- > point (V2 0 0) (Filled $ blue `withOpacity` 1) 3
point :: Center -- ^ center of the rectangle
          -> ShapeColour -- ^ colour
          -> Int -- ^ zIndex
          -> RenderObject
point center colour zIndex =
  let draw _ c renderer = do
        SDL.rendererDrawColor renderer $= colourToV4 colour
        let newCenter = SDL.P c
        SDL.drawPoint renderer (ceiling <$> newCenter)
      (V2 x y) = center
  in RO center (V4 y x y x) zIndex draw
