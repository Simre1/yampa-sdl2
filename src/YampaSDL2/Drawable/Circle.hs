{-# LANGUAGE BangPatterns #-}
module YampaSDL2.Drawable.Circle
  ( circle
  ) where

import Data.StateVar (($=))
import qualified Data.Vector.Storable as Vector
import Linear.V2
import Linear.V4
import qualified SDL
import Debug.Trace
import qualified Data.MemoCombinators as Memo

import YampaSDL2.Internal.AppOutput

-- | Draw a circle
--
-- Example:
--
-- > circle (V2 0 0) 100 (Filled orange) 2
circle ::
     Center -- ^ center of the circle
  -> Double -- ^ radius
  -> ShapeColour -- ^ colour
  -> Int -- ^ zIndex
  -> RenderObject
circle center r colour zIndex =
  let (V2 x y) = center
      bounds = V4 (y + r) (x + r) (y - r) (x - r)
      draw _ (V2 xNew yNew) renderer = do
        SDL.rendererDrawColor renderer $= colourToV4 colour
        let sdlRect = calculateRectangle (round xNew) (round yNew) (round r)
            points =
              if isFilled colour
                then fullPoints (round xNew) (round yNew) (round r)
                else linePoints (round xNew) (round yNew) (round r)
        SDL.drawPoints renderer points
        if isFilled colour
          then SDL.fillRect renderer . return $ toEnum <$> sdlRect
          else return ()
  in RO center bounds zIndex draw

calculateRectangle :: Int -> Int -> Int -> SDL.Rectangle Int
calculateRectangle = (Memo.integral . Memo.integral . Memo.integral) f
  where
    f x y r =
      let a = 0.7071067811865476 * (fromIntegral r)
      in SDL.Rectangle
           (SDL.P $ V2 x y - (truncate <$> V2 a a))
           (ceiling <$> V2 (2 * a) (2 * a))

fullPoints = (memoize3 f)
  where
    f x y r =
      Vector.fromList $
      SDL.P . fmap (toEnum) <$> (+ (V2 x y)) <$> rasterCircleFull r

linePoints = memoize3 f
  where
    f x y r =
      Vector.fromList $
      SDL.P . fmap (toEnum) <$> (+ (V2 x y)) <$> rasterCircleLine r

-- Get octant points for a circle of given radius.
octantLine :: Int -> [V2 Int]
octantLine r =
  takeWhile inOctant . map (toV2 . fst) $ iterate step ((r, 0), 1 - r)
  where
    inOctant (V2 x y) = x >= y
    step ((x, y), e)
      | e < 0 = ((x, y + 1), e + 2 * (y + 1) + 1)
      | otherwise = ((x - 1, y + 1), e + 2 * (y - x + 2) + 1)

octantFull :: Int -> [V2 Int]
octantFull r =
  fmap round <$> filter inOctant [V2 x y | x <- [0 .. l], y <- [0 .. h]]
  where
    l = fromIntegral r
    h = 0.7071067811865476 * (fromIntegral r)
    inOctant (V2 x y) =
      x ^ 2 + y ^ 2 <= (fromIntegral r) ^ 2 &&
      y ^ 2 <= (fromIntegral r) ^ 2 - x ^ 2 &&
      (ceiling x) + 1 > ceiling ((fromIntegral r) * 0.7071067811865476)

-- Get quadrant points for a circle of given radius.
quadrant :: [V2 Int] -> [V2 Int]
quadrant octantPoints = octantPoints >>= mirror
  where
    mirror (V2 x y) = [(V2 x y), (V2 y x)]

-- Get points of a circle of given radius.
fcircle :: [V2 Int] -> [V2 Int]
fcircle quadrantPoints = quadrantPoints >>= mirror
  where
    mirror (V2 0 y) = [V2 0 y, V2 0 (-y)]
    mirror (V2 x 0) = [V2 x 0, V2 (-x) 0]
    mirror (V2 x y) = [(V2 u v) | u <- [x, -x], v <- [y, -y]]

rasterCircleLine :: Int -> [V2 Int]
rasterCircleLine =  memoize (fcircle . quadrant . octantLine)

rasterCircleFull :: Int -> [V2 Int]
rasterCircleFull = memoize (fcircle . quadrant . octantFull)

-- Helper functions
toV2 :: (a, a) -> V2 a
toV2 (a, b) = V2 a b

fromV2 :: V2 a -> (a, a)
fromV2 (V2 a b) = (a, b)

memoize = Memo.integral

memoize3 = Memo.memo3 memoize memoize memoize
