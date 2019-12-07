module Main where

import Lib (orbits, transfers)

test :: [String]
test =
  [ "COM)COB"
  , "COB)COC"
  , "COC)COD"
  , "COD)COE"
  , "COE)COF"
  , "COB)COG"
  , "COG)COH"
  , "COD)COI"
  , "COE)COJ"
  , "COJ)COK"
  , "COK)COL"
  , "COK)YOU"
  , "COI)SAN"
  ]

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  computeOrbits input >>= print
  computeTransfers input >>= print

computeOrbits :: [String] -> IO Integer
computeOrbits = return . orbits

computeTransfers :: [String] -> IO Integer
computeTransfers = return . transfers
