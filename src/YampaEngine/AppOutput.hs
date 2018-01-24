module YampaEngine.AppOutput where

import FRP.Yampa
import Data.Colour

import YampaEngine.Geometry
import YampaEngine.Backend (Backend(..))

data AppOutput = AppOutput
  { graphics :: Graphics
  , sound :: [Sound]
  , shouldExit :: Bool
  }

data Graphics = Graphics
  { camera :: Camera
  , objects :: [RenderShape]
  }

data Sound = NotImplementedYet

data RenderShape = R
  { shape :: Shape
  , colour :: Colour Double
  , zIndex :: Int
  }

data Camera = Camera
  { view :: Shape }

