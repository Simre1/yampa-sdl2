module YampaEngine.Geometry
  (Shape(..)) where

import Linear.V2

data Shape =
  Rectangle
  { shapeCentre :: V2 Double
  , rectSize :: V2 Double
  }
