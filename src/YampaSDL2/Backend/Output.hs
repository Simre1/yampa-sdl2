module YampaSDL2.Backend.Output
  (outputAction) where

import qualified SDL
import qualified SDL.Primitive as GFX
import SDL.Vect
import Data.Colour.SRGB
import Data.List (sortBy)
import Data.StateVar (($=))

import YampaSDL2.AppOutput ( AppOutput(..)
                           , Graphics (..)
                           , Camera (..)
                           , RenderShape (..)
                           , ShapeColour (..)
                           )
import YampaSDL2.Geometry

outputAction :: SDL.Renderer -> Bool -> AppOutput -> IO Bool
outputAction renderer changed ao = do
  let os =
        sortBy (\r1 r2 -> zIndex r1 `compare` zIndex r2) $ objects (graphics ao)
      c = camera (graphics ao)

  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
  texture <-
    SDL.createTexture
      renderer
      SDL.RGB24
      SDL.TextureAccessTarget
      ((fmap round . cSize) c)
  SDL.rendererRenderTarget renderer $= return texture
  SDL.clear renderer
  mapM_ (renderView renderer c) os
  SDL.rendererRenderTarget renderer $= Nothing
  SDL.clear renderer
  SDL.copy renderer texture Nothing Nothing
  SDL.present renderer
  SDL.destroyTexture texture
  return (shouldExit ao)

renderView :: SDL.Renderer -> Camera -> RenderShape -> IO ()
renderView renderer camera rs = do
  let cameraPoint = (cPos camera)
      cameraView = (cSize camera)

  case rs of
    Container {containerCentre=centre, children=rs } -> do
      mapM_ (renderView renderer (Camera (cameraPoint - centre) cameraView)) (sortBy (\r1 r2 -> zIndex r1 `compare` zIndex r2) rs)
    RS {shape=s, colour=c} -> do
      let shapePoint = shapeCentre (shape rs)
          adjustedShape =
            s { shapeCentre =
              (shapePoint - cameraPoint) + cameraView/2
            }
      case adjustedShape of
        Rectangle {shapeCentre = V2 x y, rectSize = V2 w h} -> do
          let (RGB r g b) = toSRGB24 (sColour rs)
          let draw = if sFilled rs then GFX.fillRectangle else GFX.rectangle
          draw renderer
            (round <$> V2 (x - w / 2) (getY cameraView - (y + h / 2)))
            (round <$> V2 (x + w / 2) (getY cameraView - (y - h / 2)))
            (V4 r g b maxBound)
        Circle {shapeCentre = V2 x y, radius=radius} -> do
          let (RGB r g b) = toSRGB24 (sColour rs)
          let draw = if sFilled rs then GFX.fillCircle else GFX.circle
          draw renderer
            (round <$> V2 x y)
            (round radius)
            (V4 r g b maxBound)
        Triangle {shapeCentre=V2 x y, pointA=V2 pax pay, pointB=V2 pbx pby, pointC=V2 pcx pcy} -> do
          let (RGB r g b) = toSRGB24 (sColour rs)
          let draw = if sFilled rs then GFX.fillTriangle else GFX.smoothTriangle
          draw renderer
            (round <$> V2 (x + pax) (getY cameraView - (y + pay)))
            (round <$> V2 (x + pbx) (getY cameraView - (y + pby)))
            (round <$> V2 (x + pcx) (getY cameraView - (y + pcy)))
            (V4 r g b maxBound)
  return ()

sColour :: RenderShape -> Colour Double
sColour rs =
  case colour rs of
    (Filled a) -> a
    (Unfilled a) -> a

sFilled :: RenderShape -> Bool
sFilled rs =
  case colour rs of
    (Filled _) -> True
    otherwise -> False

getY :: V2 a -> a
getY (V2 x y) = y
