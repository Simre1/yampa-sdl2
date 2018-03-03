module YampaSDL2.Backend.RenderObject
  (renderObject, octantFull) where

import Control.Concurrent.MVar
import qualified SDL
import Data.Colour
import Data.Colour.SRGB
import Linear.V2
import Linear.V4
import Data.StateVar(($=))
import Control.Exception
import Data.Maybe
import qualified Data.Vector.Storable as Vector
import Debug.Trace
import Data.Function

import YampaSDL2.AppOutput
import YampaSDL2.Geometry

renderObject :: MVar [(String, (SDL.Texture, V2 Int))] -> SDL.Renderer -> RenderShape -> IO ()
renderObject mvarTextures renderer renderShape =
    case renderShape of
      Object{shape=shape', shapeCentre=centre', colour=colour'} -> do
        let (RGB r g b) = toSRGB24 (colourChannel (sColour renderShape))
            drawColour = (V4 r g b (truncate . (*255) $ alphaChannel (sColour renderShape)))
        SDL.rendererDrawColor renderer $= drawColour
        case shape' of
          Rectangle {rectSize = rectSize'} -> do
            let draw = if sFilled renderShape then SDL.fillRect else SDL.drawRect
            draw renderer
               (return $ round <$> SDL.Rectangle (SDL.P (centre'-rectSize'/2)) (rectSize'))
          Circle {radius=rad'} ->
            let points = if sFilled renderShape
                  then SDL.P . fmap (toEnum.round) <$> (+centre') <$> rasterCircleFull rad'
                  else SDL.P . fmap (toEnum.round) <$> (+centre') <$> rasterCircleLine rad'
            in SDL.drawPoints renderer (Vector.fromList points)

          Triangle {pointA=V2 pax pay, pointB=V2 pbx pby, pointC=V2 pcx pcy} -> return ()
            -- let (V2 x y) = centre'
            --     draw = if sFilled renderShape then GFX.fillTriangle else GFX.smoothTriangle
            -- draw renderer
            --   (round <$> V2 (x + pax) (y + pay))
            --   (round <$> V2 (x + pbx) (y + pby))
            --   (round <$> V2 (x + pcx) (y + pcy))
            --   drawColour
      Image {shapeCentre=centre', size=size', sourceRect=maybeRect, imgPath=path} -> do
        textures <- readMVar mvarTextures
        case lookup path textures of
          (Just (t,size)) ->
            let newSize = fromMaybe ((fromIntegral<$>size)/2, fromIntegral <$> size) maybeRect
            in drawImage renderer t (return newSize) centre' size'
          Nothing -> do
            eitherSurface <- try $ SDL.loadBMP path :: IO (Either SomeException SDL.Surface)
            case eitherSurface of
              Left ex -> putStrLn $ "IMG Loading failed: " ++ show ex
              Right val -> do
                newTexture <- SDL.createTextureFromSurface renderer val
                attrs <- SDL.queryTexture newTexture
                let w = SDL.textureWidth attrs
                    h = SDL.textureHeight attrs
                modifyMVar_ mvarTextures $ return . ((path,(newTexture, fromEnum <$> V2 w h)):)
                drawImage renderer newTexture maybeRect centre' size'
  where drawImage renderer texture source position size = do
          let toSDLRect (V2 x y, V2 w h) =
                SDL.Rectangle (round <$> SDL.P (V2 (x-w/2) (y-h/2))) (round <$> V2 w h)
          SDL.copy renderer texture (toSDLRect <$> source) (return $ toSDLRect (position,size))









-- Draw a circle, inspiration from yampy-cube(https://github.com/helsinki-frp/yampy-cube/blob/master/src/Graphics.hs)

-- Get octant points for a circle of given radius.
octantLine :: (Num a, Ord a) => a -> [V2 a]
octantLine r = takeWhile inOctant . map (toV2 . fst) $ iterate step ((r, 0), 1 - r)
    where inOctant (V2 x y) = x >= y
          step ((x, y), e)
              | e < 0     = ((x,     y + 1), e + 2 * (y + 1) + 1)
              | otherwise = ((x - 1, y + 1), e + 2 * (y - x + 2) + 1)

octantFull' :: (Num a, Ord a, Enum a, Floating a) => a -> [V2 a]
octantFull' r = filter inOctant [V2 x y | x <- [0..l], y <- [0..h]]
  where l = 1.1 * r
        h = sin (toRadian 45) * r
        toRadian x = x * (180/pi)
        inOctant (V2 x y) = x^2+y^2 <= r^2 && y^2 <= r^2 - x^2

-- TODO: It seems memoization does not work. Fix necessary!

octantFull :: (Num a, Ord a, Enum a, Floating a) => a -> [V2 a]
octantFull a = table!!(fromEnum a)

table :: (Num a, Ord a, Enum a, Floating a) => [[V2 a]]
table = fmap octantFull' [0..]

-- Get quadrant points for a circle of given radius.
quadrant :: (Num a, Ord a) => [V2 a] -> [V2 a]
quadrant octantPoints = octantPoints >>= mirror
    where mirror (V2 x y) = [ (V2 x y), (V2 y x) ]

-- Get points of a circle of given radius.
circle :: (Num a, Ord a) => [V2 a] -> [V2 a]
circle quadrantPoints = quadrantPoints >>= mirror
    where mirror (V2 x y) = [ (V2 u v) | u <- [x, -x], v <- [y, -y] ]

rasterCircleLine :: (Num a, Ord a) => a -> [V2 a]
rasterCircleLine r = circle $ quadrant $ octantLine r

rasterCircleFull :: (Num a, Ord a, Enum a, Floating a) => a -> [V2 a]
rasterCircleFull r = circle $ quadrant $ octantFull r

-- Helper functions

toV2 :: (a,a) -> V2 a
toV2 (a,b) = V2 a b

fromV2 :: V2 a -> (a,a)
fromV2 (V2 a b) = (a,b)

sColour :: RenderShape -> AlphaColour Double
sColour s =
  case s of
    Image {} -> transparent
    Object {colour=c} ->
      case c of
        (Filled a) -> a
        (Unfilled a) -> a

sFilled :: RenderShape -> Bool
sFilled s =
  case s of
    Image {} -> False
    Object {colour=c} ->
      case c of
        (Filled _) -> True
        (Unfilled _) -> False

