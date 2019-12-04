module Lib
  ( intersection
  , Cable
  , Step(..)
  , Direction(..)
  ) where

import Debug.Trace

import Data.Function (on, (&))
import Data.List (intersectBy)
import Control.Arrow (first, second)

type Cable = [Step]

data Direction = U | D | L | R
  deriving (Show, Eq)

data Step = Step Direction Int
  deriving (Show, Eq)

type Point = (Int, Int)

o :: Point
o = (0, 0)

intersection :: (Cable, Cable) -> Int
intersection (cable1, cable2) =
  fmap (zip [0..] . concat . fromCable o) [cable1, cable2]
  & toTuple
  & uncurry intersectBy (compare `on` snd)
  & fmap distanceFromOrigin
  & minimum



toTuple :: [a] -> (a, a)
toTuple [a, b] = (a, b)
toTuple _ = error "Invalid array"

fromCable :: Point -> Cable -> [[Point]]
fromCable _ [] = []
fromCable origin (step:steps) =
  let xs = fromStep origin step
   in (tail xs) : fromCable (calc step origin) steps

calc :: Step -> Point -> Point
calc (Step D i) = second (+(-i))
calc (Step L i) = first (+(-i))
calc (Step U i) = second (+i)
calc (Step R i) = first (+i)

fromStep :: Point -> Step -> [Point]
fromStep origin (Step dir i) =
  take (i+1) $ iterate (f dir) origin
    where
      f D = second (+(-1))
      f L = first (+(-1))
      f U = second (+1)
      f R = first (+1)


distance :: Point -> Point -> Int
distance (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))

distanceFromOrigin :: Point -> Int
distanceFromOrigin = distance (0, 0)
