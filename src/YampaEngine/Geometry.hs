module YampaEngine.Geometry
  (Shape(..)) where

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
