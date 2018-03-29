module YampaSDL2.Drawable.Rectangle
  ( rectangle
  ) where

import Data.StateVar (($=))
import Linear.V2
import Linear.V4
import qualified SDL

import YampaSDL2.Internal.AppOutput


-- | Draw a rectangle
--
-- Example:
--
-- > rectangle (V2 0 0) (V2 100 200) (Filled blue) 3
rectangle :: Center -- ^ center of the rectangle
          -> V2 Double -- ^ size
          -> ShapeColour -- ^ colour
          -> Int -- ^ zIndex
          -> RenderObject
rectangle center dimensions colour zIndex =
  let (V2 r t) = center + dimensions / 2
      (V2 l b) = center - dimensions / 2
      draw _ c renderer = do
        SDL.rendererDrawColor renderer $= colourToV4 colour
        let draw' =
              if isFilled colour
                then SDL.fillRect
                else SDL.drawRect
            newCenter = SDL.P $ (c - dimensions / 2)
        draw' renderer (return $ ceiling <$> SDL.Rectangle newCenter dimensions)
  in RO center (V4 t r b l) zIndex draw
