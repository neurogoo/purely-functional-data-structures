{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall     #-}
module Main where

import Control.Monad
import Text.Pretty.Simple (pPrint)
import Control.Monad.Except
import Data.Maybe (fromMaybe)

import qualified Data.Set as Set
import qualified System.Random as Random
import qualified System.Random.Stateful as SRandom
import qualified LeftistHeap as LH


data (Ord e) => Set e = Nil | Tree (Set e) e (Set e) deriving (Show, Eq)

empty  :: Set e
empty = Nil

insert :: (Ord e) => e -> Set e -> Set e
insert x Nil = Tree Nil x Nil
insert x s@(Tree a y b) | x < y = Tree (insert x a) y b
                        | x > y = Tree a y (insert x b)
                        | otherwise = s

insert2 :: (Ord e) => e -> Set e -> Set e
insert2 e s = fromMaybe s (go e s)
  where
    go :: (Ord e) => e -> Set e -> Maybe (Set e)
    go x Nil = Just $ Tree Nil x Nil
    go x (Tree a y b) | x < y = Tree <$> go x a <*> Just y <*> Just b
                      | x > y = Tree <$> Just a <*> Just y <*> go x b
                      | otherwise = Nothing

member :: (Ord e) => e -> Set e -> Bool
member _ Nil = False
member x (Tree a y b) | x < y = member x a
                      | x > y = member x b
                      | otherwise = True

main :: IO ()
main = do
    testData <- replicateM 10 (SRandom.uniformRM (0 :: Int, 100 :: Int) SRandom.globalStdGen)
    pPrint (foldr insert Nil testData)
    pPrint (foldr insert2 Nil testData)
    print ((foldr insert Nil testData) == (foldr insert2 Nil testData))
