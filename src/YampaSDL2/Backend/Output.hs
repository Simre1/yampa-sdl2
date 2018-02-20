module YampaSDL2.Backend.Output
  (outputAction) where

import qualified SDL
import qualified SDL.Primitive as GFX
import Data.Colour.SRGB
import Control.Monad
import Control.Exception
import Linear.V4
import Linear.V2
import Data.Maybe
import Control.Concurrent.MVar
import Data.StateVar (($=), get)
import Data.List

import YampaSDL2.AppOutput ( AppOutput(..)
                           , Graphics (..)
                           , Camera (..)
                           , RenderShape (..)
                           )
import YampaSDL2.Geometry

-- changed bool variable does not do anything
outputAction :: Double -> MVar [(String, SDL.Texture)]-> MVar Double -> MVar (Maybe Graphics) -> SDL.Window -> SDL.Renderer -> Bool -> AppOutput -> IO Bool
outputAction fps mvarTextures mvarFPS mvarG window renderer _ ao = do
  lastTime <- readMVar mvarFPS
  currentTime <- SDL.time
  ensureFPS <- if currentTime - lastTime > 1/fps
    then modifyMVar_ mvarFPS (return . const currentTime) >> return True
    else return False

  when ensureFPS (renderGraphics mvarTextures mvarG window renderer (graphics ao)) >> return (shouldExit ao)

renderGraphics :: MVar [(String, SDL.Texture)] -> MVar (Maybe Graphics) -> SDL.Window -> SDL.Renderer -> Graphics -> IO ()
renderGraphics mvarTextures mvarG window renderer gra = do
  let newGraphics =
        removeOutOfBounds $
          adjustToCamera gra
  oldObjects <- fromMaybe [] <$> fmap objects <$> swapMVar mvarG (return newGraphics)
  (V2 wW wH) <- fmap (fromIntegral . fromEnum) <$> get (SDL.windowSize window)
  (V2 cW cH) <- return (cSize $ camera gra)
  SDL.rendererScale renderer $= realToFrac <$> (V2 (wW/cW) (wH/cH))
  render mvarTextures renderer (oldObjects) newGraphics

render :: MVar [(String, SDL.Texture)] -> SDL.Renderer -> [RenderShape] -> Graphics -> IO ()
render mvarTextures renderer oldRS gra = do
  let oldSortedRS = sortBy (\rs1 rs2 -> zIndex rs1 `compare` zIndex rs2) oldRS
      newSortedRS = sortBy (\rs1 rs2 -> zIndex rs1 `compare` zIndex rs2) (objects gra)
      obsChanged = checkChanged oldSortedRS newSortedRS
      rerenderList = render' obsChanged
  mapM_ (renderShape mvarTextures renderer) $
      sortBy (\r1 r2 -> zIndex r1 `compare` zIndex r2) rerenderList
  SDL.present renderer

adjustToCamera :: Graphics -> Graphics
adjustToCamera gra =
  let cam = camera gra
      obs = objects gra
  in gra{objects = (\rs -> rs {shape=adjustToCamera' cam (shape rs)}) <$> obs}


adjustToCamera' :: Camera -> Shape -> Shape
adjustToCamera' c s =
  let (V2 cx cy) = cPos c
      (V2 w h) = cSize c
      adjustPoint (V2 x y) = V2 (x+w/2-cx) (h/2-(y+cy))
      inverseY (V2 x y) = V2 x (-y)
  in case s of
       Rectangle{shapeCentre=centre} ->
         s{shapeCentre=adjustPoint centre}
       Circle{shapeCentre=centre} ->
         s{shapeCentre=adjustPoint centre}
       Triangle{shapeCentre=centre, pointA=pA, pointB=pB, pointC=pC} ->
         s{shapeCentre=adjustPoint centre, pointA=inverseY pA, pointB = inverseY pB, pointC=inverseY pC}
       Image{shapeCentre=centre} ->
         s{shapeCentre=adjustPoint centre}

render' :: [(Bool, RenderShape)] -> [RenderShape]
render' rs = fmap snd $
  filter fst $
    foldl createRenderlist [] $
    -- highest zIndex first
      sortBy (\(_,a) (_,b) -> compare (zIndex b) (zIndex a)) rs

createRenderlist :: [(Bool,RenderShape)] -> (Bool,RenderShape) -> [(Bool,RenderShape)]
createRenderlist renderlist (changed,rs) =
  let isCoveredBy a b =
        let (V4 rr rl rt rb) = a
            (V4 cr cl ct cb) = b
        in rr < cr && rl > cl && rt < ct && rb > cb
      rsToV4 = shapeToBorders . shape
      v4RS = rsToV4 rs
      v4List = (\(_,a) -> rsToV4 a) <$> renderlist
      needRender =
           (any (\v4 -> not (v4 `isColliding` v4RS) && changed) v4List)
        || (any (\(c,rs) -> rsToV4 rs `isColliding` v4RS && not (v4RS `isCoveredBy` rsToV4 rs) && (changed || c)) renderlist)
        || (null renderlist && changed)
  in if needRender
    then (True,rs):fmap (\(a,b) -> (a || v4RS `isColliding` rsToV4 b,b)) renderlist
    else (False,rs):renderlist

-- TODO: More efficient algorhytm -> delete oldRS elements that are already found;
checkChanged :: [RenderShape] -> [RenderShape] -> [(Bool, RenderShape)]
checkChanged oldRS newRS =
 fmap (\rs -> (all (/=rs) oldRS, rs)) newRS
 

-- TODO: Need to properly implement this; Color and Image can be transparent!
isTransparent :: Shape -> Bool
isTransparent = const False

renderShape :: MVar [(String, SDL.Texture)] -> SDL.Renderer -> RenderShape -> IO ()
renderShape mvarTextures renderer renderShape =
      let shape' = shape renderShape in
      case shape' of
        Rectangle {shapeCentre = centre', rectSize = rectSize'} -> do
          let (RGB r g b) = toSRGB24 (sColour shape')
          let draw = if sFilled shape' then GFX.fillRectangle else GFX.rectangle
          draw renderer
            (round <$> centre'-rectSize'/2)
            (round <$> centre'+rectSize'/2)
            (V4 r g b maxBound)
        Circle {shapeCentre = centre', radius=rad'} -> do
          let (RGB r g b) = toSRGB24 (sColour shape')
          let draw = if sFilled shape' then GFX.fillCircle else GFX.circle
          draw renderer
            (round <$> centre')
            (round rad')
            (V4 r g b maxBound)
        Triangle {shapeCentre=V2 x y, pointA=V2 pax pay, pointB=V2 pbx pby, pointC=V2 pcx pcy, colour=c'} -> do
          let (RGB r g b) = toSRGB24 (sColour shape')
              draw = if sFilled shape' then GFX.fillTriangle else GFX.smoothTriangle
          draw renderer
            (round <$> V2 (x + pax) (y + pay))
            (round <$> V2 (x + pbx) (y + pby))
            (round <$> V2 (x + pcx) (y + pcy))
            (V4 r g b maxBound)
        Image {shapeCentre=centre', size=size', sourceRect=maybeRect, imgPath=path} -> do
          textures <- readMVar mvarTextures
          case lookup path textures of
              (Just t) -> drawImage renderer t maybeRect centre' size'
              Nothing -> do
                eitherSurface <- try $ SDL.loadBMP path :: IO (Either SomeException SDL.Surface)
                case eitherSurface of
                  Left ex -> putStrLn $ "IMG Loading failed: " ++ show ex
                  Right val -> do

                    newTexture <- SDL.createTextureFromSurface renderer val
                    modifyMVar_ mvarTextures $ return . ((path,newTexture):)
                    drawImage renderer newTexture maybeRect centre' size'


          return ()
  where drawImage renderer texture source position size = do
          let toSDLRect (V2 x y, V2 w h) =
                SDL.Rectangle (round <$> SDL.P (V2 (x-w/2) (y-h/2))) (round <$> V2 w h)
          SDL.copy renderer texture (toSDLRect <$> source) (return $ toSDLRect (position,size))

shapeToBorders :: Shape -> V4 Int
shapeToBorders s =
  case s of
    Rectangle {shapeCentre=V2 x y, rectSize=V2 w h} ->
      round <$> V4 (x+w/2) (x-w/2) (y+h/2) (y-h/2)
    Circle {shapeCentre=V2 x y, radius=r} ->
      round <$> V4 (x+r) (x-r) (y+r) (y-r)
    Triangle {shapeCentre=V2 x y, pointA=V2 xa ya, pointB=V2 xb yb, pointC=V2 xc yc} ->
      round <$> V4 (x+maximum [xa, xb, xc]) (x+minimum [xa, xb, xc]) (y+maximum [ya,yb,yc]) (y-maximum [ya,yb,yc])
    Image {shapeCentre=V2 x y, size=V2 w h} ->
      round <$> V4 (x+w/2) (x-w/2) (y+h/2) (y-h/2)
      
removeOutOfBounds :: Graphics -> Graphics
removeOutOfBounds graphics =
  let cam = camera graphics
      objs = objects graphics
      (V2 bR bT) = round <$> cPos cam + cSize cam
      (V2 bL bB) = round <$> cPos cam - cSize cam
      notOutOfBounds s = not $
        let (V4 r l u d) = shapeToBorders s
        in r < bL || l > bR || u < bB || d > bT
  in graphics{objects=filter (notOutOfBounds . shape) objs}


isColliding :: V4 Int -> V4 Int -> Bool
isColliding s1 s2 = not $
  let (V4 r1 l1 t1 b1) = s1
      (V4 r2 l2 t2 b2) = s2
  in r1 < l2 || l1 > r2 || t1 < b2 || b1 > t2

sColour :: Shape -> Colour Double
sColour s =
  case colour s of
    (Filled a) -> a
    (Unfilled a) -> a

sFilled :: Shape -> Bool
sFilled s =
  case colour s of
    (Filled _) -> True
    (Unfilled _) -> False

getY :: V2 a -> a
getY (V2 _ y) = y
