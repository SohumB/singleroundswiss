{-# LANGUAGE ScopedTypeVariables #-}

module Lib ( Tournament, Pairing, rounds, players, mispairs, step, pair, pairMinimising, empty, winDist, corpDist, mispairDist) where

import Data.Function (on)
import Data.List (sortBy, sort, group, delete, maximumBy)
import Control.Monad.Random (MonadRandom, getRandom, fromList)
import Control.Monad (mapM)
import Numeric.Probability.Distribution (T, fromFreqs)

data Player = Player { wins :: Int, corps :: Int } deriving (Eq, Show, Ord)
data Tournament = Tournament { rounds :: Int, players :: [Player], mispairs :: [Int] }

type Pairing = [Player] -> [(Player, Player)]

-- drops any excess elements
split :: [a] -> [(a,a)]
split xs = split' xs []

split' :: [a] -> [(a,a)] -> [(a,a)]
split' [] acc = reverse acc
split' [_] acc = reverse acc
split' (x:y:xs) acc = split' xs ((x,y):acc)

pair :: Pairing
pair = split . sortBy (compare `on` wins)

score :: Player -> Player -> Int
score p1 p2 = abs (corps p1 - corps p2)

pairMinimising :: Pairing
pairMinimising [] = []
pairMinimising ps = if atStart then pair ps else pairMinimising' ps
  where atStart = all (\p -> wins p == 0) ps

pairMinimising' :: [Player] -> [(Player, Player)]
pairMinimising' [] = []
pairMinimising' ps = (p1, p2):pairMinimising' (delete p1 $ delete p2 ps)
  where picks = [ (x,y) |
                  x <- ps, wins x == maximum (map wins ps),
                  let rest = delete x ps,
                  y <- rest, wins y == maximum (map wins rest) ]
        (p1, p2) = maximumBy (compare `on` (uncurry score)) picks

scoreMismatches :: [(Player, Player)] -> [Int]
scoreMismatches ps = map (\(p1, p2) -> abs $ wins p1 - wins p2) ps

fight :: MonadRandom m => [(Player, Player)] -> m [Player]
fight ps = mapM winners ps >>= return . concat
  where winners :: MonadRandom m => (Player, Player) -> m [Player]
        winners (p1, p2) = do let c1 = corps p1
                              let c2 = corps p2
                              p1Corp :: Bool <- if (c1 == c2) then getRandom else (return $ c1 < c2)
                              corpWins :: Bool <- fromList [(True, 60), (False, 40)]
                              let p1Wins = p1Corp == corpWins
                              let p1' = if p1Wins then p1 { wins = (wins p1) + 1 } else p1
                              let p2' = if p1Wins then p2 else p2 { wins = (wins p2) + 1 }
                              let p1'' = if p1Corp then p1' { corps = (corps p1) + 1 } else p1'
                              let p2'' = if p1Corp then p2' else p2' { corps = (corps p2) + 1 }
                              return [p1'', p2'']

step :: MonadRandom m => Pairing -> Tournament -> m Tournament
step pairFn t = let pairing = pairFn $ players t in
  do ps' <- fight pairing
     return Tournament { rounds = rounds t + 1, players = ps', mispairs = mispairs t ++ scoreMismatches pairing }

empty :: Int -> Tournament
empty n = Tournament { rounds = 0, players = replicate n (Player 0 0), mispairs = [] }

freqs :: Ord a => [a] -> [(a, Int)]
freqs as = map (\ls -> (head ls, length ls)) (group (sort as))

dist :: Ord a => [a] -> T Double a
dist ps = fromFreqs (map (\(a, l) -> (a, fromIntegral l)) $ freqs ps)

winDist :: [Player] -> T Double Int
winDist = dist . map wins

corpDist :: [Player] -> T Double Int
corpDist = dist . map corps

mispairDist :: [Tournament] -> T Double Int
mispairDist = dist . concatMap mispairs

instance Show Tournament where
  show t = "rounds = " ++ show (rounds t) ++ "\nwins = " ++
    (show . winDist $ players t) ++ "\nsides = " ++ (show . corpDist $ players t)
