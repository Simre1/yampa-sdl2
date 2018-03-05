module YampaSDL2.RenderObjects.Rectangle
  (rectangle) where

import Linear.V2
import Linear.V4
import qualified SDL
import Data.StateVar (($=))
import YampaSDL2.AppOutput

rectangle :: Center -> V2 Double -> ShapeColour -> Int -> RenderObject
rectangle center dimensions colour zIndex =
  let (V2 r t) = center + dimensions/2
      (V2 l b) = center - dimensions/2
      draw _ c renderer = do
        SDL.rendererDrawColor renderer $= colourToV4 colour
        let draw' = if isFilled colour then SDL.fillRect else SDL.drawRect
            newCenter = SDL.P $ (c-dimensions/2)
        draw'
          renderer
          (return $ round <$> SDL.Rectangle newCenter dimensions)
  in RO center (V4 t r b l) zIndex draw
