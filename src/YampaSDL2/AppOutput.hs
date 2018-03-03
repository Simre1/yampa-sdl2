{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : Output
Description : Datatypes to describe the output
-}


module YampaSDL2.AppOutput
  ( -- * Output
    AppOutput(..)
  , Graphics(..)
  , Camera(..)
  , Sound(..)
  --, container
  ) where


import qualified SDL
import Control.Concurrent.MVar
import Linear.V2
import Linear.V4
import Data.Colour

import YampaSDL2.Geometry (Shape(..))


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

data RenderObject = RO
  { render :: V2 Double -> SDL.Renderer -> MVar SDL.Texture -> IO ()
  , bounds :: V4 Int
  , center :: V2 Double
  , zIndex :: Int
  }


modifyCenter :: (V2 Double -> V2 Double) -> RenderObject -> RenderObject
modifyCenter f r = r{center=f $ center r}

-- data Shape =
--     Rectangle
--       { rectSize :: V2 Double
--       }
--   | Circle
--      { radius :: Double
--      }
--   | Triangle
--      { pointA :: V2 Double
--      , pointB :: V2 Double
--      , pointC :: V2 Double
--      }
--      -- ^ Think of shapeCentre as a vector which is applied to all 3 points of the triangle
--      deriving (Eq, Show)

-- container :: V2 Double -> [RenderShape] -> [RenderShape]
-- container translateV2 children =
--   fmap (\rs -> rs {shapeCentre=shapeCentre rs + translateV2}) children


-- data RenderShape
--   = Object
--       { shapeCentre :: V2 Double
--       , shape :: Shape
--       , colour :: ShapeColour
--       , zIndex :: Int -- ^ Higher zIndex means the RenderShape is in front of the others
--       }
--   | Image
--       { shapeCentre :: V2 Double
--       , size :: V2 Double
--       , sourceRect :: Maybe (V2 Double, V2 Double) -- ^ the section of the image that you want to render
--       , imgPath :: String
--       , zIndex :: Int
--       } deriving (Show, Eq)

-- changeColour :: ShapeColour -> RenderShape -> RenderShape
-- changeColour c rs =
--   case rs of
--     Object {} -> rs{colour=c}
--     otherwise -> rs

-- changeZIndex :: Int -> RenderShape -> RenderShape
-- changeZIndex i rs = rs {zIndex=i}

-- changeCentre :: V2 Double -> RenderShape -> RenderShape
-- changeCentre dest rs = rs{shapeCentre=dest}

-- changeShape :: Shape -> RenderShape -> RenderShape
-- changeShape s rs =
--   case rs of
--     Object{} -> rs{shape=s}
--     otherwise -> rs

data Sound =
  NotImplementedYet


-- data ShapeColour
--   = Filled (AlphaColour Double)
--   | Unfilled (AlphaColour Double) deriving (Show,Eq, Functor)


