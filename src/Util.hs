module Util
  ( hInitial
  , hAlloc
  , hFree
  , hLookup
  , hAddresses
  , hSize
  , hNull
  , hIsNull
  , showAddr
  , Heap
  , Addr

  , Assoc
  , aLookup
  , aDomain
  , aRange
  , aEmpty

  , getName
  , getNames
  , initialNameSupply
  , NameSupply

  , setFromList
  , setToList
  , setUnion
  , setIntersection
  , setSubtraction
  , setElementOf
  , setEmpty
  , setIsEmpty
  , setSingleton
  , setUnionList
  , Set

  , space
  )
where

import Data.List

-- |
-- 'shownum','hd', 'tl' and 'zip2' are omitted sesince
-- they are trivia in Haskell.

hInitial :: Heap a
hAlloc :: Heap a -> a -> (Heap a, Addr)
hUpdate :: Heap a -> Addr -> a -> Heap a
hFree :: Heap a -> Addr -> Heap a
hLookup :: Heap a -> Addr -> a
hAddresses :: Heap a -> [Addr]
hSize :: Heap a -> Int
hNull :: Addr
hIsNull :: Addr -> Bool
showAddr :: Addr -> String

type Heap a = (Int, [Addr], Assoc Addr a)
type Addr = Int

hInitial = (0, [1..], [])
hAlloc (size, next : free, cts) n = ((size + 1, free, (next, n) : cts), next)
hUpdate (size, free, cts) addr a = (size, free, (addr, a) : remove cts addr)
hFree (size, free, cts) addr = (size, free, remove cts addr)
hLookup (_, _, cts) addr = aLookup cts addr (error $ "can't find node" ++ showAddr addr ++ " in heap")
hAddresses (_, _, cts) = [ addr | (addr, _) <- cts ]
hSize (size, _, _) = size
hNull = 0
hIsNull = (== hNull)
showAddr addr = "#" ++ show addr

remove :: Assoc Addr a -> Addr -> Assoc Addr a
remove ((addr', a) : cts) addr
  | addr == addr' = cts
  | otherwise = (addr', a) : remove cts addr
remove [] addr = error $ "Attempt to update or free nonexistent address " ++ showAddr addr

type Assoc a b = [(a, b)]

aLookup :: (Eq a) => Assoc a b -> a -> b -> b
aLookup ((key', val) : bs) key def
  | key == key' = val
  | otherwise = aLookup bs key def
aLookup [] key def = def

aDomain :: Assoc a b -> [a]
aDomain alist = [ key | (key, _) <- alist ]

aRange :: Assoc a b -> [b]
aRange alist = [ val | (_, val) <- alist ]

aEmpty :: Assoc a b
aEmpty = []

getName :: NameSupply -> String -> (NameSupply, String)
getNames :: NameSupply -> [String] -> (NameSupply, [String])
initialNameSupply :: NameSupply

type NameSupply = Int

initialNameSupply = 0
getName nameSupply prefix = (nameSupply + 1, makeName prefix nameSupply)
getNames nameSupply prefixes
  = (nameSupply + length prefixes, zipWith makeName prefixes [nameSupply..])

makeName prefix ns = prefix ++ "_" ++ show ns

setFromList :: (Ord a) => [a] -> Set a
setToList :: (Ord a) => Set a -> [a]
setUnion :: (Ord a) => Set a -> Set a -> Set a
setIntersection :: (Ord a) => Set a -> Set a -> Set a
setSubtraction :: (Ord a) => Set a -> Set a -> Set a
setElementOf :: (Ord a) => a -> Set a -> Bool
setEmpty :: (Ord a) => Set a
setIsEmpty :: (Ord a) => Set a -> Bool
setSingleton :: (Ord a) => a -> Set a
setUnionList :: (Ord a) => [Set a] -> Set a

type Set a = [a]

setEmpty = []
setIsEmpty = null
setSingleton x = [x]
setFromList = rmDup . sort
  where
    rmDup [] = []
    rmDup [x] = [x]
    rmDup (x0 : x1: xs)
      | x0 == x1 = rmDup (x1 : xs)
      | otherwise = x0 : rmDup (x1 : xs)

setToList xs = xs

setUnion set1@(e1 : es1) set2@(e2 : es2)
  | e1 < e2 = e1 : setUnion es1 set2
  | e1 == e2 = e1 : setUnion es1 es2
  | e1 > e2 = e2 : setUnion set1 es2
setUnion set1@(_ : _) [] = set1
setUnion [] set2@(_ : _) = set2
setUnion [] [] = []

setIntersection set1@(e1 : es1) set2@(e2 : es2)
  | e1 < e2 = setIntersection es1 set2
  | e1 == e2 = e1 : setIntersection es1 es2
  | e1 > e2 = setIntersection set1 es2
setIntersection (_ : _) [] = []
setIntersection [] (_ : _) = []
setIntersection [] [] = []

setSubtraction set1@(e1 : es1) set2@(e2 : es2)
  | e1 < e2 = e1 : setSubtraction es1 set2
  | e1 == e2 = setSubtraction es1 es2
  | e1 > e2 = setSubtraction set1 es2
setSubtraction set1@(_ : _) [] = set1
setSubtraction [] (_ : _) = []
setSubtraction [] [] = []

setElementOf _ [] = False
setElementOf x (y : ys) = x == y || (x > y && setElementOf x ys)

setUnionList = foldl setUnion setEmpty

-- |
-- 'first','second', 'zipWith', 'foldll',
-- 'mapAccuml' and 'sort' are omitted sesince
-- they are trivia in Haskell.

space :: Int -> String
space indent = replicate indent ' '
