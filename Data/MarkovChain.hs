{-# LANGUAGE FlexibleContexts #-}

module Data.MarkovChain where

import           Control.Arrow
import           Control.Monad.State.Lazy
import qualified Data.Map.Strict          as M
import qualified Data.Vector              as V
import           System.Random (RandomGen, randomR)


type MarkovChain a = M.Map [a] (V.Vector a)

buildChain :: (Ord a) => Int -> [a] -> MarkovChain a
buildChain i xs = execState (process i xs) M.empty

process i [] = return ()
process i x@(_:xs)
        | length key < i = return ()
        | value == []    = return ()
        | otherwise      = do
                m <- get
                put $ M.insertWith (V.++) key (V.singleton $ head value) m
                process i xs
        where
        key = take i x
        value = drop i x


next :: (RandomGen g, Ord a) => MarkovChain a -> g -> [a] -> Maybe (a, g)
next c g k = fmap (flip randomItem g) $ M.lookup k c
        where
        randomIndex :: (RandomGen g) => V.Vector a -> g -> (Int, g)
        randomIndex f g = randomR (0, pred $ length f) g
        randomItem :: (RandomGen g) => V.Vector a -> g -> (a, g)
        randomItem f g = first (f V.!) $ randomIndex f g

generate c g k = k ++ generate' c g k
generate' :: (RandomGen g, Ord a) => MarkovChain a -> g -> [a] -> [a]
generate' c g k@(_:ks) = case (next c g k) of
        Nothing -> []
        Just (n, g') -> n : generate' c g' (ks ++ [n])
