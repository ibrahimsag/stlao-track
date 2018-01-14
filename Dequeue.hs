{-# LANGUAGE CPP #-}
{- |
Module      :  Data.Dequeue
Description :  A typeclass and an implementation for double-ended queues.
Copyright   :  (c) Henry Bucklow 2009-2010
License     :  BSD3

Maintainer  :  henry@elsie.org.uk
Stability   :  provisional
Portability :  portable

A typeclass for double-ended queues, and an implementation of Banker's
Dequeues, as described in Chris Okasaki's Purely Functional Data Structures.
-}
module Dequeue (
    -- * The 'Dequeue' type class.
    Dequeue(..),
    -- * Banker's Dequeues
    BankersDequeue,
) where

import Prelude hiding (foldl, foldr, foldl1, foldr1, length, last)

import Control.Monad
import Data.Foldable
import qualified Data.List as List

-- | A typeclass for double-ended queues.
class Foldable q => Dequeue q where
    -- | Generates an empty queue.
    empty :: q a
    -- | Returns 'True' if this queue is empty.
    null :: q a -> Bool
    -- | Returns the number of elements in this queue.
    qlength :: q a -> Int
    -- | Returns the item on the front of the queue.
    first :: q a -> Maybe a
    -- | Returns the item on the end of the queue.
    last :: q a -> Maybe a
    -- | Returns the first n items from the front of the queue, in the order
    --   they would be popped.
    takeFront :: Int -> q a -> [a]
    -- | Returns the last n items from the end of the queue, in the order they
    --  would be popped.
    takeBack :: Int -> q a -> [a]
    -- | Pushes an item onto the front of the queue.
    pushFront :: q a -> a -> q a
    -- | Pops an item from the front of the queue.
    popFront :: q a -> Maybe (a, q a)
    -- | Pushes an item onto the back of the queue.
    pushBack :: q a -> a -> q a
    -- | Pops an item from the back of the queue.
    popBack :: q a -> Maybe (a, q a)
    -- | Converts a list into a queue.
    fromList :: [a] -> q a


-- | An implementation of Banker's Dequeues, as described in Chris Okasaki's
--   Purely Functional Data Structures. The functions for the 'Dequeue'
--   instance have the following complexities (where n is the 'length' of the
--   queue):
--
--    * 'qlength': O(1)
--
--    * 'first': O(1)
--
--    * 'last': O(1)
--
--    * 'takeFront': O(n)
--
--    * 'takeBack': O(n)
--
--    * 'pushFront': O(1) amortised
--
--    * 'popFront': O(1) amortised
--
--    * 'pushBack': O(1) amortised
--
--    * 'popBack': O(1) amortised
--
--    * 'fromList': O(n)
data BankersDequeue a = BankersDequeue Int [a] Int [a]

instance Functor BankersDequeue where
    fmap f (BankersDequeue sizeF front sizeR rear) =
        BankersDequeue sizeF (fmap f front) sizeR (fmap f rear)

instance Foldable BankersDequeue where
    fold (BankersDequeue _ front _ rear) = fold (front ++ reverse rear)
    foldMap f (BankersDequeue _ front _ rear) = foldMap f (front ++ reverse rear)
    foldr f a (BankersDequeue _ front _ rear) = foldr f a (front ++ reverse rear)
    foldl f a (BankersDequeue _ front _ rear) = foldl f a (front ++ reverse rear)
    foldr1 f (BankersDequeue _ front _ rear) = foldr1 f (front ++ reverse rear)
    foldl1 f (BankersDequeue _ front _ rear) = foldl1 f (front ++ reverse rear)


headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x : xs) = Just x

instance Dequeue BankersDequeue where
    empty = BankersDequeue 0 [] 0 []
    null (BankersDequeue 0 [] 0 []) = True
    null _ = False
    qlength (BankersDequeue sizeF _ sizeR _) = sizeF + sizeR
    first (BankersDequeue _ [] _ [x]) = Just x
    first (BankersDequeue _ front _ _) =  headMay front
    last (BankersDequeue _ [x] _ []) = Just x
    last (BankersDequeue _ _ _ rear) = headMay rear
    takeFront i (BankersDequeue sizeF front _ rear) =
        take i front ++ take (i - sizeF) (reverse rear)
    takeBack i (BankersDequeue _ front sizeR rear) =
        take i rear ++ take (i - sizeR) (reverse front)
    pushFront (BankersDequeue sizeF front sizeR rear) x =
        check $ BankersDequeue (sizeF + 1) (x : front) sizeR rear
    popFront (BankersDequeue _ [] _ []) = Nothing
    popFront (BankersDequeue _ [] _ [x]) = Just (x, empty)
    popFront (BankersDequeue _ [] _ _) = error "Queue is too far unbalanced."
    popFront (BankersDequeue sizeF (f : fs) sizeR rear) =
        Just (f, check $ BankersDequeue (sizeF - 1) fs sizeR rear)
    pushBack (BankersDequeue sizeF front sizeR rear) x =
        check $ BankersDequeue sizeF front (sizeR + 1) (x : rear)
    popBack (BankersDequeue _ [] _ []) = Nothing
    popBack (BankersDequeue _ [x] _ []) = Just (x, empty)
    popBack (BankersDequeue _ _ _ []) = error "Queue is too far unbalanced."
    popBack (BankersDequeue sizeF front sizeR (r : rs)) =
        Just (r, check $ BankersDequeue sizeF front (sizeR - 1) rs)
    fromList list = check $ BankersDequeue (List.length list) list 0 []

-- | The maximum number of times longer one half of a 'BankersDequeue' is
--   permitted to be relative to the other.
bqBalance :: Int
bqBalance = 4

-- | Checks to see if the queue is too far out of balance. If it is, it
--   rebalances it.
check :: BankersDequeue a -> BankersDequeue a
check q@(BankersDequeue sizeF front sizeR rear)
    | sizeF > c * sizeR + 1 =
        let front' = take size1 front
            rear' = rear ++ reverse (drop size1 front)
        in
        BankersDequeue size1 front' size2 rear'
    | sizeR > c * sizeF + 1 =
        let front' = front ++ reverse (drop size1 rear)
            rear' = take size1 rear
        in
        BankersDequeue size2 front' size1 rear'
    | otherwise = q
    where
        size1 = (sizeF + sizeR) `div` 2
        size2 = (sizeF + sizeR) - size1
        c = bqBalance

instance Eq a => Eq (BankersDequeue a) where
    queue1 == queue2 = toList queue1 == toList queue2

