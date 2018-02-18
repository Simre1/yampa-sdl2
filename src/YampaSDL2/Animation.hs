{-# Language Arrows #-}

module YampaSDL2.Animation
  ( animate
  , Animation
  , AnimationType(..)
  , newAnimation
  ) where

import FRP.Yampa

import YampaSDL2.AppOutput (RenderShape)

data Animation = Animation
  { frames :: [(Time, RenderShape)]
  , type_ :: AnimationType
  }

data AnimationType = Once | Endless | Repeat Int | Alternate | AlternateEndless

newAnimation = Animation

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
