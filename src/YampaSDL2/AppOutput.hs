{-|
Module      : Output
Description : Datatypes to describe the output
-}

module YampaSDL2.AppOutput
  ( -- * Output
    AppOutput(..)
  , Graphics(..)
  , Camera(..)
  , RenderShape(..)
  , Sound(..)
  , container
  ) where

import Linear.V2

import YampaSDL2.Geometry (Shape(..))



-- | Your main SF needs to create an AppOutput
data AppOutput = AppOutput
  { graphics :: Graphics
  , sound :: [Sound]
  , shouldExit :: Bool
  }

data Graphics = Graphics
  { camera :: Camera
  , objects :: [RenderShape]
  } deriving Show

data Camera = Camera
  { cPos :: V2 Double -- ^Moves the viewpoint
  , cSize :: V2 Double -- ^Set the size of the viewpoint, for example to zoom.
  } deriving Show

container :: V2 Double -> [RenderShape] -> [RenderShape]
container translateV2 children =
  fmap (\rs -> rs {
                 shape=(shape rs){shapeCentre=shapeCentre (shape rs) + translateV2}
               }) children

data RenderShape
  = RS { shape :: Shape
       , zIndex :: Int -- ^ Higher zIndex means the RenderShape is in front of the others
       } deriving (Show, Eq)
data Sound =
  NotImplementedYet
