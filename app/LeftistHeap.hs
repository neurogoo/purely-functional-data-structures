{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall     #-}
module LeftistHeap where

data (Ord e) => Heap e = Nil | T Int e (Heap e) (Heap e)

rank :: Heap e -> Int
rank Nil = 0
rank (T r _ _ _) = r

makeT :: e -> Heap e -> Heap e -> Heap e
makeT x a b | rank a >= rank b = T (rank b + 1) x a b
            | otherwise        = T (rank a + 1) x b a

merge :: (Ord e) => Heap e -> Heap e -> Heap e
merge h Nil = h
merge Nil h = h
merge h1@(T _ x a1 b1) h2@(T _ y a2 b2) | x < y     = makeT x a1 (merge b1 h2)
                                        | otherwise = makeT y a2 (merge h1 b2)

insert :: (Ord e) => e -> Heap e -> Heap e
insert x h = merge (T 1 x Nil Nil) h

findMin :: (Ord e) => Heap e -> e
findMin Nil = error "No minimum element for empty heap"
findMin (T _ x _ _) = x

deleteMin :: (Ord e) => Heap e -> Heap e
deleteMin Nil = Nil
deleteMin (T _ x a b) = merge a b
