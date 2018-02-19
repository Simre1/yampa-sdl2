{-|
Module      : Animation
Description : Provide an easy way to do animations
-}
{-# Language Arrows #-}

module YampaSDL2.Animation
  ( -- * Animation
    animate
  , Animation(..)
  , AnimationType(..)
  , newAnimation
  ) where

import FRP.Yampa

import YampaSDL2.AppOutput (RenderShape)

data Animation = Animation
  { frames :: [(Time, RenderShape)] -- ^ Time specifies how long the RenderShape is displayed, for example [(0.5, renderShape1), (0.5, renderShape2)]
  , type_ :: AnimationType
  }

newAnimation = Animation

data AnimationType = Once | Endless | Repeat Int | Alternate | AlternateEndless

animate :: Animation -> SF a (Maybe RenderShape)
animate animation
  | (null $ frames animation) = constant Nothing
  | otherwise = proc _ -> do
      frameEvent <- afterEach (getFrames animation) -< ()
      frame <- hold initialFrame -< frameEvent
      returnA -< return frame
        where initialFrame = let (_,frame) = head $ frames animation
                       in frame

getFrames :: Animation -> [(Time, RenderShape)]
getFrames (Animation{frames=frames,type_=type_}) =
  case type_ of
    Once -> frames
    Endless -> cycle frames
    Repeat times -> concat $ replicate times frames
    Alternate -> frames ++ reverse frames
    AlternateEndless -> cycle (frames ++ reverse frames)
