module YampaSDL2.Backend.Output
  (outputAction) where

import qualified SDL
import qualified SDL.Primitive as GFX
import Data.Colour.SRGB
import Data.List (find, sortBy)
import Control.Monad
import Control.Exception
import Linear.V4
import Linear.V2
import Data.Maybe
import Control.Concurrent.MVar

import YampaSDL2.AppOutput ( AppOutput(..)
                           , Graphics (..)
                           , Camera (..)
                           , RenderShape (..)
                           )
import YampaSDL2.Geometry




outputAction :: MVar Graphics -> SDL.Renderer -> Bool -> AppOutput -> IO Bool
outputAction mvar renderer changed ao = when changed (renderObjects mvar renderer (graphics ao)) >> return (shouldExit ao)

renderGraphics :: MVar Graphics -> SDL.Renderer -> Graphics -> IO ()
renderGraphics mvar renderer ao = do
  oldGraphics <- readMVar mvar
  return ()

removeOutOfBounds :: Graphics -> Graphics
removeOutOfBounds graphics =
  let cam = camera graphics
      objs = objects graphics
      (V2 bR bT) = cPos cam + cSize cam
      (V2 bL bB) = cPos cam - cSize cam
      notOutOfBounds s = not $
        let (V4 r l u d) = shapeToBorders s
        in r < bL || l > bR || u < bB || d > bT
  in graphics{objects=filter (notOutOfBounds . shape) objs} 


-- TODO: More efficient algorhytm -> delete oldRS elements that are already found
checkChanged :: [RenderShape] -> [RenderShape] -> [(Bool, RenderShape)]
checkChanged oldRS newRS =
  let oldSortedRS = sortBy (\rs1 rs2 -> key rs1 `compare` key rs2) oldRS
      newSortedRS = sortBy (\rs1 rs2 -> key rs1 `compare` key rs2) newRS
  in fmap (\rs -> (all ((==key rs).key) oldSortedRS, rs)) newSortedRS

needsRerender :: [(Bool, RenderShape)] -> (Bool, RenderShape) -> Bool
needsRerender rss (changed, rs) =
  let lowerZIndex = (zIndex rs >).zIndex
      colliding = isColliding (shape rs).shape
      doesNotCover cover = not $
        let (V4 rr rl rt rb) = shapeToBorders $ shape rs
            (V4 cr cl ct cb) = shapeToBorders $ shape cover
        in rr < cr && rl > cl && rt < ct && rb > cb
      notTransparent' = not . isTransparent . shape
      preFilteredRS = filter (\(_, r) -> colliding r) rss
  in fmap (\(c, r) -> lowerZIndex r || doesNotCover r || notTransparent' r) preFilteredRS
  where isSelf = ((key rs/=).key)


-- TODO: Need to properly implement this; Color and Image can be transparent!
isTransparent :: Shape -> Bool
isTransparent = const False

shapeToBorders :: Shape -> V4 Double
shapeToBorders s =
  case s of
    Rectangle {shapeCentre=V2 x y, rectSize=V2 w h} ->
      V4 (x+w/2) (x-w/2) (y+h/2) (y-h/2)
    Circle {shapeCentre=V2 x y, radius=r} ->
      V4 (x+r) (x-r) (y+r) (y-r)
    Triangle {shapeCentre=V2 x y, pointA=V2 xa ya, pointB=V2 xb yb, pointC=V2 xc yc} ->
      V4 (x+maximum [xa, xb, xc]) (x+minimum [xa, xb, xc]) (y+maximum [ya,yb,yc]) (y-maximum [ya,yb,yc])
    Image {shapeCentre=V2 x y, size=V2 w h} ->
      V4 (x+w/2) (x-w/2) (y+h/2) (y-h/2)

isColliding :: Shape -> Shape -> Bool
isColliding s1 s2 = not $
  let (V4 r1 l1 t1 b1) = shapeToBorders s1
      (V4 r2 l2 t2 b2) = shapeToBorders s2
  in r1 < l2 || l1 > r2 || t1 < b2 || b1 > t2
