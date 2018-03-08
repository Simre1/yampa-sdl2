{-# Language Arrows #-}

module YampaSDL2.Animation
  ( -- * Animation
    animate
  -- ** Animatable Properties
  , animateNumber
  , animateV2
  , animateColour
  -- ** Animation curves
  , linear
  , easeIn
  , easeOut
  ) where

import FRP.Yampa
import Linear.V2
import Data.Maybe
import FRP.Yampa.Event
import Data.List
import Data.Colour

type Curve = Double -> Extent
-- | Extent is always between 0 and 1 (including 0 and 1). Starts with 0 and is at 1 at the end of the duration.
type Extent = Double
-- | Animation duration in seconds
type Duration = Double

-- | Create an animation
--
-- Example:
--
-- > animate animateV2 (V2 0 0) [(2, easeOut, V2 0 100), (2, easeIn, V2 0 0)] 
animate ::
    (a -> a -> Extent -> a) -- ^ The property to animate
  -> a -- ^ The starting point
  -> [(Duration, Curve,a)] -- ^ The animations to execute one after another
  -> SF () a
animate _ a [] = constant a
animate f aB ((dur,curve,aE):rest) = switch (sf aB f dur) (cont f rest)
  where sf propertyB animateF duration = proc _ -> do
          currentRS <- f aB aE  ^<< (boundExtent curve) ^<< (/duration) ^<< time -< ()
          event <- after duration () -< ()
          returnA -< (currentRS,tag event currentRS)
        cont f rest propertyB = animate f propertyB rest

animateNumber :: Double -> Double -> Extent -> Double
animateNumber beginning end e = beginning + (end - beginning) * e

animateV2 :: V2 Double -> V2 Double -> Extent -> V2 Double
animateV2 beginning end e = beginning + (end - beginning)*V2 e e

animateColour :: AlphaColour Double -> AlphaColour Double -> Extent -> AlphaColour Double
animateColour beginning end e = blend e end beginning

linear = id

easeIn x = x*x

easeOut x = 1-(x-1)^2

boundExtent :: Curve -> Double -> Double
boundExtent f time
  | f time < 0 = 0
  | f time > 1 = 1
  | otherwise = f time

