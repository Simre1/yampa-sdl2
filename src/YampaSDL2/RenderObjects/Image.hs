module YampaSDL2.RenderObjects.Image
  (image) where

import Control.Exception
import qualified SDL
import qualified SDL.Raw
import Linear.V2
import Linear.V4
import Control.Monad
import Data.Function.Memoize
import Data.StateVar (($=))
import Data.Dynamic
import Data.List
import Data.Maybe
import Debug.Trace

import YampaSDL2.AppOutput
import Control.Concurrent.MVar

image :: Center -> V2 Double -> Maybe (V2 Double) -> String -> Int -> RenderObject
image center size source path zIndex =
  let (V2 r t) = center + size/2
      (V2 l b) = center - size/2
  in RO center (V4 t r b l) zIndex (drawImage size source path)

drawImage :: V2 Double -> Maybe (V2 Double) -> String -> Cache -> Center -> SDL.Renderer -> IO ()
drawImage size source path mvarCache center renderer = do
  (eitherTexture) <- loadTexture mvarCache path renderer
  either handleError
    (\t -> drawImage renderer t Nothing center size)
    eitherTexture
  where drawImage renderer texture source position size = do
          let toSDLRect (V2 x y, V2 w h) =
                SDL.Rectangle (round <$> SDL.P (V2 (x-w/2) (y-h/2))) (round <$> V2 w h)
          SDL.copy renderer texture (toSDLRect <$> source) (return $ toSDLRect (position,size))

loadTexture :: Cache -> String -> SDL.Renderer -> IO (Either SomeException SDL.Texture)
loadTexture mvarCache path renderer = do
  cache <- readMVar mvarCache
  maybe
    (loadAndCache mvarCache path renderer)
    (return . Right)
    (lookup path cache >>= fromDynamic)
  where loadAndCache mvarCache path renderer = do
          loadedTexture <- loadImage path renderer
          either
            (return . Left)
            (\t -> do
                modifyMVar_ mvarCache (return . ((path, toDyn t):))
                return (Right t)
            )
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
--Image {shapeCentre=centre', size=size', sourceRect=maybeRect, imgPath=path} -> do
--         textures <- readMVar mvarTextures
--         case lookup path textures of
--           (Just (t,size)) ->
--             let newSize = fromMaybe ((fromIntegral<$>size)/2, fromIntegral <$> size) maybeRect
--             in drawImage renderer t (return newSize) centre' size'
--           Nothing -> do
--             eitherSurface <- try $ SDL.loadBMP path :: IO (Either SomeException SDL.Surface)
--             case eitherSurface of
--               Left ex -> putStrLn $ "IMG Loading failed: " ++ show ex
--               Right val -> do
--                 newTexture <- SDL.createTextureFromSurface renderer val
--                 attrs <- SDL.queryTexture newTexture
--                 let w = SDL.textureWidth attrs
--                     h = SDL.textureHeight attrs
--                 modifyMVar_ mvarTextures $ return . ((path,(newTexture, fromEnum <$> V2 w h)):)
--                 drawImage renderer newTexture maybeRect centre' size'
--   where drawImage renderer texture source position size = do
--           let toSDLRect (V2 x y, V2 w h) =
--                 SDL.Rectangle (round <$> SDL.P (V2 (x-w/2) (y-h/2))) (round <$> V2 w h)
--           SDL.copy renderer texture (toSDLRect <$> source) (return $ toSDLRect (position,size))


