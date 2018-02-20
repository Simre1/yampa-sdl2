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
      , colour :: Colour Double
      }
  | Circle
     { shapeCentre :: V2 Double
     , radius :: Double
     , colour :: Colour Double
     }
  | Triangle
     { shapeCentre :: V2 Double
     , pointA :: V2 Double
     , pointB :: V2 Double
     , pointC :: V2 Double
     , colour :: Colour Double
     }
     -- ^ Think of shapeCentre as a vector which is applied to all 3 points of the triangle
  | Image
    { shapeCentre :: V2 Double
    , size :: V2 Double
    , sourceRect :: Maybe (V2 Double, V2 Double)
    ,  imgPath :: String
    }

data ShapeColour
  = Filled (Colour Double)
  | Unfilled (Colour Double)



