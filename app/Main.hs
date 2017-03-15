module Main where

import Lib (Tournament, Pairing, empty, step, pair, pairMinimising, players, corpDist, mispairDist, (//))

tournament :: Int -> Int -> Rational -> Pairing -> IO Tournament
tournament rounds players corpWin pfn = foldr (flip (>>=)) (return $ empty players corpWin) (replicate rounds (step pfn))

runSimulation :: Int -> Pairing -> IO ()
runSimulation n pfn = do
  ts <- sequence $ replicate n (tournament 8 32 (6/10) pfn)
  let ps = concatMap players ts
  putStrLn "corps:"
  corpDist ps // 3
  putStrLn "mispairs:"
  mispairDist ts // 3

main :: IO ()
main = do
  putStrLn "pairing naïvely"
  runSimulation 10000 pair
  putStrLn "pairing trying to match for side symmetry"
  runSimulation 10000 pairMinimising
