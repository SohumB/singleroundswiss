module Main where

import Lib (Tournament, Pairing, empty, step, pair, pairMinimising, players, corpDist, mispairDist)

tournament :: Int -> Int -> Pairing -> IO Tournament
tournament rounds players pfn = foldr (flip (>>=)) (return $ empty players) (replicate rounds (step pfn))

runSimulation :: Int -> Pairing -> IO ()
runSimulation n pfn = do
  ts <- sequence $ replicate n (tournament 8 32 pfn)
  let ps = concatMap players ts
  putStrLn (show (corpDist ps) ++ ", " ++ show (mispairDist ts) ++ " mispairs")

main :: IO ()
main = do
  runSimulation 10000 pair
  runSimulation 10000 pairMinimising
