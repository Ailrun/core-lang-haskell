-- |
-- This module is for exercise 2.7
module Data.StatHeap
  ( statHInitial
  , statHAlloc
  , statHUpdate
  , statHFree
  , statHLookup
  , statHAddresses
  , statHSize
  , statHNull
  , statHIsNull
  , statHGetStats
  , StatHeap

  , statHSInitial
  , statHSIncHAlloc
  , statHSGetHAlloc
  , statHSIncHUpdate
  , statHSGetHUpdate
  , statHSIncHFree
  , statHSGetHFree
  , StatHeapStats

  , Addr
  )
where

import Util

statHInitial :: StatHeap a
statHInitial = (hInitial, statHSInitial)

statHAlloc :: StatHeap a -> a -> (StatHeap a, Addr)
statHAlloc (heap, stats) n = ((heap', statHSIncHAlloc stats), addr)
  where
    (heap', addr) = hAlloc heap n

statHUpdate :: StatHeap a -> Addr -> a -> StatHeap a
statHUpdate (heap, stats) addr a = (hUpdate heap addr a, statHSIncHUpdate stats)

statHFree :: StatHeap a -> Addr -> StatHeap a
statHFree (heap, stats) addr = (hFree heap addr, statHSIncHFree stats)

statHLookup :: StatHeap a -> Addr -> a
statHLookup (heap, _) = hLookup heap

statHAddresses :: StatHeap a -> [Addr]
statHAddresses (heap, _) = hAddresses heap

statHSize :: StatHeap a -> Int
statHSize (heap, _) = hSize heap

statHNull :: Addr
statHNull = 0

statHIsNull :: Addr -> Bool
statHIsNull = (== hNull)

statHGetStats :: StatHeap a -> StatHeapStats
statHGetStats (_, stats) = stats

type StatHeap a
  = (Heap a, StatHeapStats)

statHSInitial :: StatHeapStats
statHSInitial = (0, 0, 0)

statHSIncHAlloc :: StatHeapStats -> StatHeapStats
statHSIncHAlloc (a, u, f) = (a + 1, u, f)

statHSGetHAlloc :: StatHeapStats -> Int
statHSGetHAlloc (a, _, _) = a

statHSIncHUpdate :: StatHeapStats -> StatHeapStats
statHSIncHUpdate (a, u, f) = (a, u + 1, f)

statHSGetHUpdate :: StatHeapStats -> Int
statHSGetHUpdate (_, u, _) = u

statHSIncHFree :: StatHeapStats -> StatHeapStats
statHSIncHFree (a, u, f) = (a, u, f + 1)

statHSGetHFree :: StatHeapStats -> Int
statHSGetHFree (_, _, f) = f

type StatHeapStats
  = ( Int -- The number of heap allocations
    , Int -- The number of heap updates
    , Int -- The number of heap frees
    )
