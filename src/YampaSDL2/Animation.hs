{-|
Module      : Animation
Description : Provide an easy way to do animations
-}
{-# Language Arrows #-}

module YampaSDL2.Animation
  ( -- * Animation
  ) where

import FRP.Yampa
import Linear.V2
import Data.Maybe
import FRP.Yampa.Event
import Data.List
import Data.Colour

import YampaSDL2.AppOutput

-- | Apply animation to a RenderShape.
-- The animations in the list are executed on after another. You can combine animations with mappend-- .
-- animate :: [Animation] -> RenderShape -> SF a RenderShape
-- animate [] rs = constant rs
-- animate (a:as) rs = switch (sf rs a) (cont as)
--   where sf rs animation = proc _ -> do
--           currentRS <- (fun animation) rs ^<< time -< ()
--           event <- after (dur animation) () -< ()
--           returnA -< (currentRS,tag event currentRS)
--         cont animations rs = animate animations rs

-- type Duration = Double

--  -- | Animation which works on every RenderShape. Animation is an instance of Monoid. Therefore you can combine multiple animations with mappend.
-- data Animation = Animation
--   { fun :: (RenderShape -> Double -> RenderShape)
--   , dur :: Duration
--   }

-- instance Monoid Animation where
--   mempty = Animation const 0
--   a `mappend` b = Animation (\start t -> (fun b) ((fun a) start t) t) (max (dur a) (dur b))


-- -- Animation curves

-- type Curve = Double -> Double

-- linear :: Curve
-- linear = id

-- easeIn :: Curve
-- easeIn x = x * x

-- easeOut :: Curve
-- easeOut x = sqrt x


-- -- Animatable attributes

-- yAnimation :: Double -> Curve -> Duration -> Animation
-- yAnimation = generateAnimation changeAttr
--   where changeAttr attr extent rs =
--           let (V2 xP yP) = shapeCentre rs
--           in rs{shapeCentre=V2 xP (yP+attr*extent)}

-- xAnimation :: Double -> Curve -> Duration -> Animation
-- xAnimation = generateAnimation changeAttr
--   where changeAttr attr extent rs =
--           let (V2 xP yP) = shapeCentre rs
--           in rs{shapeCentre=V2 (xP+attr*extent) yP}

-- colorAnimation :: AlphaColour -> Curve -> Duration -> Animation
-- colorAnimation = generateAnimation changeAttr
--   where changeAttr attr extent rs =
--           let c = colour rs
--           in changeColour 

-- -- Helper functions

-- boundExtent :: (Double -> Double) -> Double -> Double
-- boundExtent f time
--   | f time < 0 = 0
--   | f time > 1 = 1
--   | otherwise = f time

-- generateAnimation :: (a -> Double -> RenderShape -> RenderShape) -> a -> (Double -> Double) -> Duration -> Animation
-- generateAnimation attribute end extent duration =
--   let animation start t = attribute end (boundExtent extent (t/duration)) start

--   in Animation animation duration
