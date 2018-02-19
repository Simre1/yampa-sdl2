{-|
Module      : Geometry
Description : Declares all shapes
-}

module YampaSDL2.Geometry
  ( -- ** Shapes
    Shape(..)
  ) where

import Linear.V2




data Shape =
    Rectangle
      { shapeCentre :: V2 Double
      , rectSize :: V2 Double
      }
  | Circle
     { shapeCentre :: V2 Double
     , radius :: Double
     }
  | Triangle
     { shapeCentre :: V2 Double
     , pointA :: V2 Double
     , pointB :: V2 Double
     , pointC :: V2 Double
     }
     -- ^ Think of shapeCentre as a vector which is applied to all 3 points of the triangle
  | Image
    { sourceRect :: Maybe (V2 Double, V2 Double)
    , destRect :: Maybe (V2 Double, V2 Double)
    ,  imgPath :: String
    }
