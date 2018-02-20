{-|
Module      : Geometry
Description : Declares all shapes
-}

module YampaSDL2.Geometry
  ( -- ** Shapes
    Shape(..)
  , ShapeColour(..)
  ) where

import Linear.V2
import Data.Colour

data Shape =
    Rectangle
      { shapeCentre :: V2 Double
      , rectSize :: V2 Double
      , colour :: ShapeColour
      }
  | Circle
     { shapeCentre :: V2 Double
     , radius :: Double
     , colour :: ShapeColour
     }
  | Triangle
     { shapeCentre :: V2 Double
     , pointA :: V2 Double
     , pointB :: V2 Double
     , pointC :: V2 Double
     , colour :: ShapeColour
     }
     -- ^ Think of shapeCentre as a vector which is applied to all 3 points of the triangle
  | Image
    { shapeCentre :: V2 Double
    , size :: V2 Double
    , sourceRect :: Maybe (V2 Double, V2 Double) -- ^ the section of the image that you want to render
    , imgPath :: String
    } deriving (Eq, Show)

data ShapeColour
  = Filled (Colour Double)
  | Unfilled (Colour Double) deriving (Show,Eq)



