module YampaSDL2.Drawable.Image
  ( image
  ) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Dynamic
import Data.List
import Data.Maybe
import Data.StateVar (($=))
import Linear.V2
import Linear.V4
import qualified SDL

import YampaSDL2.Internal.AppOutput

-- | Draw an image
--
-- Example:
--
-- > image (V2 0 0) (V2 800 600) Nothing "./path/to/image.bmp" 0
image ::
     Center -- ^ set the center point of the image
  -> V2 Double -- ^ set the size of the image (V2 length height)
  -> Maybe (V2 Double) -- ^ you can only use a part of the image, Nothing for the whole image
  -> String -- ^ the file name, image must be in BMP format
  -> Int -- ^ zIndex
  -> RenderObject
image center size source path zIndex =
  let (V2 r t) = center + size / 2
      (V2 l b) = center - size / 2
  in RO center (V4 t r b l) zIndex (drawImage size source path)

drawImage ::
     V2 Double
  -> Maybe (V2 Double)
  -> String
  -> Cache
  -> Center
  -> SDL.Renderer
  -> IO ()
drawImage size source path mvarCache center renderer = do
  (eitherTexture) <- loadTexture mvarCache path renderer
  either
    handleError
    (\t -> drawImage renderer t Nothing center size)
    eitherTexture
  where
    drawImage renderer texture source position size = do
      let toSDLRect (V2 x y, V2 w h) =
            SDL.Rectangle
              (round <$> SDL.P (V2 (x - w / 2) (y - h / 2)))
              (round <$> V2 w h)
      SDL.copy
        renderer
        texture
        (toSDLRect <$> source)
        (return $ toSDLRect (position, size))

loadTexture ::
     Cache -> String -> SDL.Renderer -> IO (Either SomeException SDL.Texture)
loadTexture mvarCache path renderer = do
  cache <- readMVar mvarCache
  maybe
    (loadAndCache mvarCache path renderer)
    (return . Right)
    (lookup path cache >>= fromDynamic)
  where
    loadAndCache mvarCache path renderer = do
      loadedTexture <- loadImage path renderer
      either
        (return . Left)
        (\t -> do
           modifyMVar_ mvarCache (return . ((path, toDyn t) :))
           return (Right t))
        (loadedTexture)

loadImage :: String -> SDL.Renderer -> IO (Either SomeException SDL.Texture)
loadImage p renderer = do
  eitherSurface <-
    (try (SDL.loadBMP p) :: IO (Either SomeException SDL.Surface))
  let eitherTexture =
        case eitherSurface of
          Left err -> return $ Left err
          Right val -> surfaceToTexture renderer val >>= return . Right
      surfaceToTexture renderer s = do
        texture <- SDL.createTextureFromSurface renderer s
        SDL.textureBlendMode texture $= SDL.BlendAlphaBlend
        SDL.freeSurface s
        return texture
  eitherTexture

handleError :: SomeException -> IO ()
handleError e = print e
