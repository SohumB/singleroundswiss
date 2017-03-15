module Main where

import Lib (Tournament, Pairing, empty, step, pair, pairMinimising, players, corpDist, mispairDist)

tournament :: Int -> Pairing -> IO Tournament
tournament rounds pfn = foldr (flip (>>=)) (return $ empty 32) (replicate rounds (step pfn))

runSimulation :: Pairing -> IO ()
runSimulation pfn = do
  ts <- sequence $ replicate 10000 (tournament 8 pfn)
  let ps = concatMap players ts
  putStrLn (show (corpDist ps) ++ ", " ++ show (mispairDist ts) ++ " mispairs")

main :: IO ()
main = do
  runSimulation pair
  runSimulation pairMinimising
