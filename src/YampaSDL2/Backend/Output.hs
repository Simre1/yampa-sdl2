module YampaSDL2.Backend.Output
  (outputAction) where

import qualified SDL
import qualified SDL.Primitive as GFX
import SDL.Vect
import Data.Colour.SRGB
import Data.List (sortBy)
import Data.StateVar (($=))
import Control.Monad

import YampaSDL2.AppOutput ( AppOutput(..)
                           , Graphics (..)
                           , Camera (..)
                           , RenderShape (..)
                           , ShapeColour (..)
                           )
import YampaSDL2.Geometry

outputAction :: SDL.Renderer -> Bool -> AppOutput -> IO Bool
outputAction renderer changed ao =
  when changed ( do
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
    )
  >> return (shouldExit ao)

renderView :: SDL.Renderer -> Camera -> RenderShape -> IO ()
renderView renderer cam rs = do
  let cameraPoint = (cPos cam)
      cameraView = (cSize cam)

  case rs of
    Container {containerCentre=centre, children=cs } -> do
      mapM_ (renderView renderer (Camera (cameraPoint - centre) cameraView)) (sortBy (\r1 r2 -> zIndex r1 `compare` zIndex r2) cs)
    RS {shape=s} -> do
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
        Circle {shapeCentre = V2 x y, radius=rad} -> do
          let (RGB r g b) = toSRGB24 (sColour rs)
          let draw = if sFilled rs then GFX.fillCircle else GFX.circle
          draw renderer
            (round <$> V2 x y)
            (round rad)
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
    (Unfilled _) -> False

getY :: V2 a -> a
getY (V2 _ y) = y
