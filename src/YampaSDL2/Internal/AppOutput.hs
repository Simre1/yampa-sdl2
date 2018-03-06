module YampaSDL2.Internal.AppOutput
  ( RenderObject(..)
  , modifyCenter
  , Scene(..)
  , AppOutput(..)
  , Camera(..)
  , ShapeColour(..)
  , Sound(..)
  , isFilled
  , getColour
  , colourToV4
  , Center
  , Bounds
  , Cache
  ) where

import Control.Concurrent.MVar
import Data.Colour
import Data.Colour.SRGB
import Data.Dynamic
import Data.Word
import Linear.V2
import Linear.V4
import qualified SDL

-- | The Main signal function needs to return an AppOutput which tells SDL what to do.
--
-- Use 'output' to create an AppOutput
data AppOutput = AppOutput
  { scene :: Scene -- ^ Sets what will be drawn
  , sound :: [Sound] -- ^ Sadly not working yet
  , shouldExit :: Bool -- ^ Set if the app should exit
  }

-- | Properties of the scene
data Scene = Scene
  { cam :: Camera -- ^ Sets the viewpoint
  , objects :: [RenderObject] -- ^ All objects that are drawn
  }

data Camera = Camera
  { cPos :: V2 Double -- ^ Moves the viewpoint
  , cSize :: V2 Double -- ^ Set the size of the viewpoint, for example to zoom.
  }

type Center = V2 Double

type Bounds = V4 Double

type Cache = MVar [(String, Dynamic)]

-- To draw something on the scene, you need to create RenderObjects. The easiest way is to use functions like 'rectangle'. If you have special needs or want direct access to SDL, you can also create your own RenderObjects from scratch.
data RenderObject = RO
  { center :: Center -- ^ center
  , bounds :: Bounds -- ^ rectangular bounds used for optimizations
  , zIndex :: Int -- ^ RenderObject with higher zIndex will be rendered atop ones with lower zIndex
  , draw :: Cache -> Center -> SDL.Renderer -> IO ()
  }

modifyCenter :: (Center -> Center) -> RenderObject -> RenderObject
modifyCenter f r = r {center = f $ center r}

-- | Used to set the colour of shapes like 'rectangle' or 'circle' and whether they are filled or not. This library uses "Data.Colour" to create colours.
data ShapeColour
  = Filled (AlphaColour Double)
  | Unfilled (AlphaColour Double)
  deriving (Show, Eq)

isFilled :: ShapeColour -> Bool
isFilled (Filled _) = True
isFilled _ = False

getColour :: ShapeColour -> AlphaColour Double
getColour (Filled c) = c
getColour (Unfilled c) = c

colourToV4 :: ShapeColour -> V4 Word8
colourToV4 sc =
  let c = getColour sc
      (RGB r g b) = toSRGB24 (colourChannel c)
      alpha = truncate $ alphaChannel c * 255
  in V4 r g b alpha

-- | Not implemented yet
data Sound =
  NotImplementedYet
