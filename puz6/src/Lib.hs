module Lib (orbits, transfers) where

import qualified Data.HashMap as H
import Data.List (intersect)

type Orbits = H.Map String String

orbits :: [String] -> Integer
orbits spec =
  let m = mkMap spec
   in H.foldWithKey (countParents m) 0 m

mkMap :: [String] -> Orbits
mkMap = H.fromList . fmap (reverseTuple . splitAt 3 . filter (/=')'))

transfers :: [String] -> Integer
transfers spec =
  let m = mkMap spec
   in findPath m (getParents "YOU" m) (getParents "SAN" m)

reverseTuple :: (a, b) -> (b, a)
reverseTuple (a, b) = (b, a)

countParents :: Orbits -> String -> String -> Integer -> Integer
countParents m key _ acum =
  case H.lookup key m of
    Nothing -> acum
    Just val -> countParents m val "" (acum + 1)

getParents :: String -> Orbits -> [String]
getParents k m =
  case H.lookup k m of
    Nothing -> []
    Just p  -> p : getParents p m

findPath :: Orbits -> [String] -> [String] -> Integer
findPath m you san =
  let common = head $ you `intersect` san
   in countUntil common "YOU" m 0 + countUntil common "SAN" m 0

countUntil :: String -> String -> Orbits -> Integer -> Integer
countUntil target key m acum =
  case H.lookup key m of
    Nothing -> acum
    Just val -> if val == target then acum else countUntil target val m (acum + 1)
