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
  , ShapeColour(..)
  , container
  ) where

import Linear.V2
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
  , objects :: [RenderShape]
  } deriving Show

data Camera = Camera
  { cPos :: V2 Double -- ^Moves the viewpoint
  , cSize :: V2 Double -- ^Set the size of the viewpoint, for example to zoom.
  } deriving Show

container :: V2 Double -> [RenderShape] -> [RenderShape]
container translateV2 children =
  fmap (\rs -> rs {shapeCentre=shapeCentre rs + translateV2}) children

data RenderShape
  = Object
      { shapeCentre :: V2 Double
      , shape :: Shape
      , colour :: ShapeColour
      , zIndex :: Int -- ^ Higher zIndex means the RenderShape is in front of the others
      }
  | Image
      { shapeCentre :: V2 Double
      , size :: V2 Double
      , sourceRect :: Maybe (V2 Double, V2 Double) -- ^ the section of the image that you want to render
      , imgPath :: String
      , zIndex :: Int
      } deriving (Show, Eq)

data Sound =
  NotImplementedYet


data ShapeColour
  = Filled (AlphaColour Double)
  | Unfilled (AlphaColour Double) deriving (Show,Eq)


