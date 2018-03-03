{-|
Module      : Geometry
Description : Declares all shapes
-}

module YampaSDL2.Geometry
  ( -- ** Shapes
    Shape(..)
  ) where

import Linear.V2
import Data.Colour

data Shape =
    Rectangle
      { rectSize :: V2 Double
      }
  | Circle
     { radius :: Double
     }
  | Triangle
     { pointA :: V2 Double
     , pointB :: V2 Double
     , pointC :: V2 Double
     }
     -- ^ Think of shapeCentre as a vector which is applied to all 3 points of the triangle
     deriving (Eq, Show)
