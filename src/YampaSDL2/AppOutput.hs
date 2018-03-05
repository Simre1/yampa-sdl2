{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : Output
Description : Datatypes to describe the output
-}


module YampaSDL2.AppOutput
   where

import qualified SDL
import Control.Concurrent.MVar
import Linear.V2
import Linear.V4
import Data.Colour
import Data.Colour.SRGB
import Data.Word
import Data.Dynamic


-- | Your main SF needs to create an AppOutput
data AppOutput = AppOutput
  { graphics :: Graphics
  , sound :: [Sound]
  , shouldExit :: Bool
  }

data Graphics = Graphics
  { camera :: Camera
  , objects :: [RenderObject]
  }

data Camera = Camera
  { cPos :: V2 Double -- ^Moves the viewpoint
  , cSize :: V2 Double -- ^Set the size of the viewpoint, for example to zoom.
  }

type Center = V2 Double

type Bounds = V4 Double

type Cache = MVar [(String, Dynamic)]

data RenderObject = RO
  { center :: Center
  , bounds :: Bounds
  , zIndex :: Int
  , render :: Cache -> Center -> SDL.Renderer -> IO ()
  }


modifyCenter :: (Center -> Center) -> RenderObject -> RenderObject
modifyCenter f r = r{center=f $ center r}
data Sound =
  NotImplementedYet


data ShapeColour
  = Filled (AlphaColour Double)
  | Unfilled (AlphaColour Double) deriving (Show,Eq)

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
