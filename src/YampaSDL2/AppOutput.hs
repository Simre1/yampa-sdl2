module YampaSDL2.AppOutput
  ( AppOutput(..)
  , Graphics (..)
  , Sound(..)
  , RenderShape(..)
  , ShapeColour (..)
  , Camera(..)
  )where

import Data.Colour
import Linear.V2

import YampaSDL2.Geometry (Shape)

sColour :: RenderShape -> Colour Double
sColour rs =
  case colour rs of
    (Filled a) -> a
    (Unfilled a) -> a

sFilled :: RenderShape -> Bool
sFilled rs =
  case colour rs of
    (Filled _) -> True
    otherwise -> False


data AppOutput = AppOutput
  { graphics :: Graphics
  , sound :: [Sound]
  , shouldExit :: Bool
  }

data Graphics = Graphics
  { camera :: Camera
  , objects :: [RenderShape]
  }

data Sound =
  NotImplementedYet

data RenderShape
  = RS { shape :: Shape
       , colour :: ShapeColour
       , zIndex :: Int }
  | Container { containerCentre :: V2 Double
              , children :: [RenderShape] }

data ShapeColour
  = Filled (Colour Double)
  | Unfilled (Colour Double)

data Camera = Camera
  { cPos :: V2 Double
  , cSize :: V2 Double
  }
