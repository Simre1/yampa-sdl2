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
  , ShapeColour(..)
  , Sound(..)
  ) where

import Data.Colour
import Linear.V2

import YampaSDL2.Geometry (Shape)



-- | Your main SF needs to create an AppOutput
data AppOutput = AppOutput
  { graphics :: Graphics
  , sound :: [Sound]
  , shouldExit :: Bool
  }

data Graphics = Graphics
  { camera :: Camera
  , objects :: [RenderShape]
  }

data Camera = Camera
  { cPos :: V2 Double -- ^Moves the view point
  , cSize :: V2 Double -- ^Set the size of the view, for example to zoom.
  }

data RenderShape
  = RS { shape :: Shape
       , colour :: ShapeColour
       , zIndex :: Int -- ^ Higher zIndex means the RenderShape is in front of the others.
       }

  |  -- | Allows you to move multiple RenderShapes at once with the same vector
    Container { containerCentre :: V2 Double
              , children :: [RenderShape] }

data ShapeColour
  = Filled (Colour Double)
  | Unfilled (Colour Double)

data Sound =
  NotImplementedYet