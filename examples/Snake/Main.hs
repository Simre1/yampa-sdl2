{-# Language Arrows #-}

import FRP.Yampa
import YampaSDL2
import System.Random

main :: IO ()
main = defaultLoop sf

sf :: SF AppInput AppOutput
sf = proc i -> do
  shouldQuit <- isEvent ^<< quit -< i
  let g = undefined
  returnA -< output
    (render myCamera $ renderGame g)
    []
    shouldQuit


myCamera :: Camera
myCamera = camera (V2 0 0) (V2 200 150)

data Game = Game
  { player :: Player
  , food :: Food
  }

renderGame :: Game -> [RenderObject]
renderGame = undefined

data Player = Player
  { head :: V2 Double
  , direction :: V2 Double
  , body :: [V2 Double]
  }

data Food = Food (V2 Double) StdGen | NoFood StdGen

getGen :: Food -> StdGen
getGen (Food _ a) = a
getGen (NoFood a) = a

getFoodPos :: Food -> Maybe (V2 Double)
getFoodPos (Food v2 _) = return v2
getFoodPos (NoFood _) = Nothing

foodSF :: SF Player Food
foodSF = loop (mkStdGen 3)
  where loop gen = loopPre (NoFood gen) foodSF'

foodSF' :: SF (Player, Food) (Food, Food)
foodSF' = proc (p,f) -> do
  let ()
  

