module YampaSDL2.Backend.ExperimentalOutput
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
import Data.Colour.Names
import Debug.Trace

import YampaSDL2.AppOutput ( AppOutput(..)
                           , Graphics (..)
                           , Camera (..)
                           , RenderShape (..)
                           )
import YampaSDL2.Geometry


-- changed bool variable does not do anything
outputAction :: Double -> MVar [(String, (SDL.Texture, V2 Int))]-> MVar Double -> MVar Bool -> MVar (Maybe Graphics) -> SDL.Window -> SDL.Renderer -> Bool -> AppOutput -> IO Bool
outputAction fps mvarTextures mvarFPS mvarReady mvarG window renderer _ ao = do
  lastTime <- readMVar mvarFPS
  currentTime <- SDL.time
  ensureFPS <- if currentTime - lastTime > 1/fps
    then modifyMVar_ mvarFPS (return . const currentTime) >> return True
    else return False
  ready <- readMVar mvarReady
  when (ensureFPS && ready) $ do
    modifyMVar_ mvarReady (\_ -> return False)
    renderGraphics mvarTextures mvarG window renderer (graphics ao)
    modifyMVar_ mvarReady (\_ -> return True)
  return (shouldExit ao)

-- Experimental render, inefficient right now
-- Rendering of preprocessed shapes
renderGraphics :: MVar [(String, (SDL.Texture, V2 Int))] -> MVar (Maybe Graphics) -> SDL.Window -> SDL.Renderer -> Graphics -> IO ()
renderGraphics mvarTextures mvarG window renderer gra = do
  textures <- readMVar mvarTextures
  let newGraphics =
        splitObjects textures 100 $
          adjustToCamera $
            removeOutOfBounds gra
  oldObjects <- fromMaybe [] <$> fmap objects <$> swapMVar mvarG (return newGraphics)
  (V2 wW wH) <- fmap (fromIntegral . fromEnum) <$> get (SDL.windowSize window)
  (V2 cW cH) <- return (cSize $ camera gra)
  SDL.rendererScale renderer $= realToFrac <$> (V2 (wW/cW) (wH/cH))
  render mvarTextures renderer oldObjects gra

render :: MVar [(String, (SDL.Texture, V2 Int))] -> SDL.Renderer -> [RenderShape] -> Graphics -> IO ()
render mvarTextures renderer oldRS gra = do
  let oldSortedRS = sortBy (\rs1 rs2 -> zIndex rs1 `compare` zIndex rs2) oldRS
      newSortedRS = sortBy (\rs1 rs2 -> zIndex rs1 `compare` zIndex rs2) (objects gra)
      obsChanged = checkChanged oldSortedRS newSortedRS
      rerenderList = render' obsChanged
  mapM_ (renderShape mvarTextures renderer) $
      sortBy (\r1 r2 -> zIndex r1 `compare` zIndex r2) newSortedRS
  SDL.present renderer

  
removeOutOfBounds :: Graphics -> Graphics
removeOutOfBounds graphics =
  let cam = camera graphics
      objs = objects graphics
      (V2 bR bT) =  cPos cam + cSize cam/2
      (V2 bL bB) = cPos cam - cSize cam/2
      notOutOfBounds s = not $
        let (V4 r l u d) = shapeToBorders s
        in r < bL || l > bR || u < bB || d > bT
  in graphics{objects=filter (notOutOfBounds) objs}

adjustToCamera :: Graphics -> Graphics
adjustToCamera gra =
  let cam = camera gra
      obs = objects gra
  in gra{objects = (\rs -> adjustToCamera' cam rs) <$> obs}


adjustToCamera' :: Camera -> RenderShape -> RenderShape
adjustToCamera' c rs =
  let (V2 cx cy) = cPos c
      (V2 w h) = cSize c
      s = shape rs
      adjustPoint (V2 x y) = V2 (x+w/2-cx) (h/2-(y+cy))
      inverseY (V2 x y) = V2 x (-y)
      adjustedCentre = adjustPoint (shapeCentre rs)
      adjustedShape = case s of
       Triangle{ pointA=pA, pointB=pB, pointC=pC} ->
         s{pointA=inverseY pA, pointB = inverseY pB, pointC=inverseY pC}
       otherwise -> s
  in rs{shapeCentre=adjustedCentre, shape=adjustedShape}

-- actual renderfunction
renderShape :: MVar [(String, (SDL.Texture, V2 Int))] -> SDL.Renderer -> RenderShape -> IO ()
renderShape mvarTextures renderer renderShape =
      let shape' = shape renderShape
          centre' = shapeCentre renderShape
      in case shape' of
        Rectangle {rectSize = rectSize'} -> do
          let (RGB r g b) = toSRGB24 (sColour shape')
          let draw = if sFilled shape' then GFX.fillRectangle else GFX.rectangle
          draw renderer
            (round <$> centre'-rectSize'/2)
            (round <$> centre'+rectSize'/2)
            (V4 r g b maxBound)
        Circle {radius=rad'} -> do
          let (RGB r g b) = toSRGB24 (sColour shape')
          let draw = if sFilled shape' then GFX.fillCircle else GFX.circle
          draw renderer
            (round <$> centre')
            (round rad')
            (V4 r g b maxBound)
        Triangle {pointA=V2 pax pay, pointB=V2 pbx pby, pointC=V2 pcx pcy, colour=c'} -> do
          let (RGB r g b) = toSRGB24 (sColour shape')
              (V2 x y) = centre'
              draw = if sFilled shape' then GFX.fillTriangle else GFX.smoothTriangle
          draw renderer
            (round <$> V2 (x + pax) (y + pay))
            (round <$> V2 (x + pbx) (y + pby))
            (round <$> V2 (x + pcx) (y + pcy))
            (V4 r g b maxBound)
        Image {size=size', sourceRect=maybeRect, imgPath=path} -> do
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



-- wrapper for RenderShape to add shapes which are not drawn to decide if a rerender is necessary
data RenderAction = RealAction RenderShape | ShadowAction RenderShape deriving (Show, Eq)

getShape :: RenderAction -> RenderShape
getShape (RealAction s) = s
getShape (ShadowAction s) = s

isReal :: RenderAction -> Bool
isReal (RealAction _) = True
isReal _ = False

-- TODO: More efficient algorhytm -> delete oldRS elements that are already found;
checkChanged :: [RenderShape] -> [RenderShape] -> [(Bool, RenderAction)]
checkChanged oldRS newRS =
  let changedOldRS = filter (\rs -> rs `notElem` newRS) oldRS
  in fmap (\rs -> ((not (any (==rs) oldRS) || null oldRS), RealAction rs)) newRS ++ fmap (\rs -> (True, ShadowAction rs)) changedOldRS

-- decide which rendershapes need rendering
render' :: [(Bool, RenderAction)] -> [RenderShape]
render' rs = fmap getShape . filter isReal $
  let unchanged = snd <$> filter (not . fst) rs
      changed = snd <$> filter fst rs
  in areUnchangedSame [] unchanged changed

areUnchangedSame :: [RenderAction] -> [RenderAction] -> [RenderAction] -> [RenderAction]
areUnchangedSame compare unchanged changed
  | compare == unchanged = changed
  | otherwise = uncurry (areUnchangedSame unchanged) (checkRenders unchanged $ changed)


checkRenders :: [RenderAction] -> [RenderAction] -> ([RenderAction],[RenderAction])
checkRenders unchanged changed =
  if null unchanged
    then ([],changed)
    else let tests = fmap (\uc -> ((any (checkIfRenderIsNecessary uc) changed),uc)) unchanged
         in (snd <$> filter (not.fst) tests, changed++(snd <$> filter (fst) tests))
  where checkIfRenderIsNecessary unchangedObject changedObject =
             unchangedObject `isColliding` changedObject


hasHigherZIndex :: RenderAction -> RenderAction -> Bool
hasHigherZIndex rs1 rs2 = zIndex (getShape rs1) > zIndex (getShape rs2)

isCoveredBy :: RenderAction -> RenderAction -> Bool
isCoveredBy a b =
  let (V4 rr rl rt rb) = shapeToBorders (getShape a)
      (V4 cr cl ct cb) = shapeToBorders (getShape b)
  in rr <= cr && rl >= cl && rt <= ct && rb >= cb

-- FIXME: Not sure if this is right
isColliding :: RenderAction -> RenderAction -> Bool
isColliding s1 s2 =
  let (V4 r1 l1 t1 b1) = shapeToBorders (getShape s1)
      (V4 r2 l2 t2 b2) = shapeToBorders (getShape s2)
  in (((r2 > l1 && r2 < r1) || (l2 > l1 && l2 < r1)) && ((t2 > b1 && t2 < t1) || (b2 > b1 && b2 < t1)))
  || (((r1 > l2 && r1 < r2) || (l1 > l2 && l1 < r2)) && ((t1 > b2 && t1 < t2) || (b1 > b2 && b1 < t2)))

-- TODO: Need to properly implement this; Color and Image can be transparent!
isTransparent :: RenderAction -> Bool
isTransparent = const False

-- Split objects at every x/givenSize==0 to minimize necessary rendering for big shapes if just a part of them changed.

splitObjects :: [(String, (SDL.Texture, V2 Int))] -> Double -> Graphics -> Graphics
splitObjects textures givenSize gra =
  gra{objects=concatMap (splitObjects' textures givenSize) (objects gra)}

splitObjects' :: [(String, (SDL.Texture, V2 Int))] -> Double -> RenderShape -> [RenderShape]
splitObjects' textures xSize rs =
 let (V2 x y) = shapeCentre rs
 in case shape rs of
    Rectangle{rectSize=V2 w h, colour=c} ->
      let (firstPoint, sizes) = calculateSize xSize x w
      in snd $ mapAccumL
        (\curPos (s,nextS) -> (curPos+s/2+nextS/2,RS (V2 curPos y) (Rectangle (V2 s h) c) (zIndex rs)))
        firstPoint
        sizes
    Image{size=V2 w h,imgPath=path,sourceRect=maybeRect} ->
      let sourceR = (flip fromMaybe) maybeRect <$> (\(_,s) -> ((fromIntegral <$> s)/2, fromIntegral <$> s)) <$> lookup path textures
      in case sourceR of
        Just (V2 sX sY,V2 sW sH) ->
          let (firstPoint, sizes) = calculateSize xSize x w
          in snd $ mapAccumL
             (\curPos (s,nextS) -> (curPos+s/2+nextS/2,RS (V2 curPos y) (Image (V2 s h) (return (V2 (curPos*sW/w) sY, V2 ((sW/w)*s) sH)) path) (zIndex rs)))
             firstPoint
             sizes

        Nothing -> [rs]
    otherwise -> [rs]
  where calculateSize :: Double -> Double -> Double -> (Double, [(Double,Double)])
        calculateSize preferredSize screenPos actualWidth =
          let leftSide = screenPos - (actualWidth / 2)
              firstSize = preferredSize - fromIntegral (round leftSide `rem` round preferredSize)
          in if firstSize < actualWidth
                then let lastSize = fromIntegral $ round (actualWidth - firstSize) `rem` round preferredSize
                         howManyNormal = round $ (actualWidth - (lastSize + firstSize)) / preferredSize
                         firstPoint = leftSide + (firstSize / 2)

                         sizes = firstSize:replicate howManyNormal preferredSize++if lastSize /= 0 then [lastSize] else []
                     in (firstPoint, sizes `zip` (tail sizes ++ [0]))
                else (leftSide+actualWidth / 2,[(actualWidth,0)])


-- helper functions
shapeToBorders :: RenderShape -> V4 Double
shapeToBorders rs =
  let s = shape rs
      (V2 x y) = shapeCentre rs
  in case s of
    Rectangle {rectSize=V2 w h} ->
      V4 (x+w/2) (x-w/2) (y+h/2) (y-h/2)
    Circle {radius=r} ->
      V4 (x+r) (x-r) (y+r) (y-r)
    Triangle {pointA=V2 xa ya, pointB=V2 xb yb, pointC=V2 xc yc} ->
      V4 (x+maximum [xa, xb, xc]) (x+minimum [xa, xb, xc]) (y+maximum [ya,yb,yc]) (y-maximum [ya,yb,yc])
    Image {size=V2 w h} ->
      V4 (x+w/2) (x-w/2) (y+h/2) (y-h/2)


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
