---
categories:
- Challenges
- Functional Programming
- Haskell
comments: true
date: 2008-07-25T00:00:00Z
tags:
- challenge
- functional
title: Another Quick Coding Challenge
---

I was about to head to bed when I stumbled across another interesting coding challenge. Since I had another half hour or so to kill I thought I'd give it a shot!

<!--more-->

The challenge was first posted <a href="http://beust.com/weblog/archives/000491.html">here</a>. The problem is as follows:
<blockquote><p>Write a counter function that counts from 1 to max but only returns numbers whose digits don't repeat.

For example, part of the output would be:

    * 8, 9, 10, 12 (11 is not valid)
    * 98, 102, 103 (99, 100 and 101 are not valid)
    * 5432, 5436, 5437 (5433, 5434 and 5435 are not valid)

Also:

    * Display the biggest jump (in the sequences above, it's 4: 98 -&gt; 102)
    * Display the total count of numbers
    * Give these two values for max=10000
</p></blockquote>
So without further ado, here's my solution:
```
import Data.List

noDups :: String -> Bool
noDups l = length l == length (nub l)

items :: [Int]
items = filter (noDups . show) [1..10000]

main :: IO ()
main = print $ (maximum (zipWith (-) (tail items) items), length items)
```

It's probably not as concise as it could be, and I'm sure a more experienced Haskeller would be able to tidy it up a little bit. Despite that, it still performs quite well:

    Thu Jul 24 21:34 2008 Time and Allocation Profiling Report  (Final)

         main +RTS -p -RTS

      total time  =        0.00 secs   (0 ticks @ 20 ms)
      total alloc =   4,121,500 bytes  (excludes profiling overheads)

    COST CENTRE   MODULE   %time %alloc

    CAF           Main       0.0  100.0


                                                individual    inherited
    COST CENTRE   MODULE       no.    entries  %time %alloc   %time %alloc

    MAIN          MAIN           1           0   0.0    0.0     0.0  100.0
     CAF          Main         152          14   0.0  100.0     0.0  100.0
     CAF          GHC.Handle    88           4   0.0    0.0     0.0    0.0

Thoughts? How could I make this better/faster/more elegant? I'm keen to get some insight from a more experienced Haskell coder :) Cheers!
