---
categories:
- Functional Programming
- Haskell
- Project Euler
comments: true
date: 2008-07-24T00:00:00Z
tags:
- functional
- Haskell
- programming
- Project Euler
title: 'Project Euler #8'
---

<strong>WARNING!</strong> This post contains a spoiler for Problem #8 listed at <a href="http://projecteuler.net/" title="Project Euler">Project Euler</a>. Do not read the rest of this post if you're planning to attempt to solve the problem yourself.

<!--more-->

<a href="http://projecteuler.net/index.php?section=problems&id=8">Problem #8</a> was thankfully a great deal more interesting than <a href="/posts/project-euler-7/" title="Project Euler #7">the last problem</a>. It goes as follows..
<blockquote><p>Find the greatest product of five consecutive digits in the 1000-digit number.


       73167176531330624919225119674426574742355349194934
       96983520312774506326239578318016984801869478851843
       85861560789112949495459501737958331952853208805511
       12540698747158523863050715693290963295227443043557
       66896648950445244523161731856403098711121722383113
       62229893423380308135336276614282806444486645238749
       30358907296290491560440772390713810515859307960866
       70172427121883998797908792274921901699720888093776
       65727333001053367881220235421809751254540594752243
       52584907711670556013604839586446706324415722155397
       53697817977846174064955149290862569321978468622482
       83972241375657056057490261407972968652414535100474
       82166370484403199890008895243450658541227588666881
       16427171479924442928230863465674813919123162824586
       17866458359124566529476545682848912883142607690042
       24219022671055626321111109370544217506941658960408
       07198403850962455444362981230987879927244284909188
       84580156166097919133875499200524063689912560717606
       05886116467109405077541002256983155200055935729725
       71636269561882670428252483600823257530420752963450

</p></blockquote>
I'm keeping with my <a href="http://haskell.org/" title="Haskell">Haskell</a> theme and abusing some of the nice feature of Functional Programming to nail this one.

Here's my solution:
```
-- ord is required to convert chars to ints
import Data.Char (ord)

-- a string representation of the large number
number :: String
number =
     "73167176531330624919225119674426574742355349194934"
  ++ "96983520312774506326239578318016984801869478851843"
  ++ "85861560789112949495459501737958331952853208805511"
  ++ "12540698747158523863050715693290963295227443043557"
  ++ "66896648950445244523161731856403098711121722383113"
  ++ "62229893423380308135336276614282806444486645238749"
  ++ "30358907296290491560440772390713810515859307960866"
  ++ "70172427121883998797908792274921901699720888093776"
  ++ "65727333001053367881220235421809751254540594752243"
  ++ "52584907711670556013604839586446706324415722155397"
  ++ "53697817977846174064955149290862569321978468622482"
  ++ "83972241375657056057490261407972968652414535100474"
  ++ "82166370484403199890008895243450658541227588666881"
  ++ "16427171479924442928230863465674813919123162824586"
  ++ "17866458359124566529476545682848912883142607690042"
  ++ "24219022671055626321111109370544217506941658960408"
  ++ "07198403850962455444362981230987879927244284909188"
  ++ "84580156166097919133875499200524063689912560717606"
  ++ "05886116467109405077541002256983155200055935729725"
  ++ "71636269561882670428252483600823257530420752963450"

-- converts the number to a list of Int values
numberList :: [Int]
numberList = map (\n -> ord n - ord '0') number

-- breaks up the number list into chunks of 5 digits
chunks :: [[Int]]
chunks = c' numberList 5 995
  where
    c' _ _ 0 = []
    c' l n c = (take n l) : c' (tail l) n (c - 1)

-- main function iterates over the list of chunks,
-- calculates the product of each of the chunks and
-- then determines the biggest result
main :: IO ()
main = print $ foldr1 max (map product chunks)
```

The comments are pretty descriptive of what goes on. It turns out that the solution isn't that shabby in the performance stakes either:

    Thu Jul 24 20:46 2008 Time and Allocation Profiling Report  (Final)

         main +RTS -p -RTS

      total time  =        0.00 secs   (0 ticks @ 20 ms)
      total alloc =     571,224 bytes  (excludes profiling overheads)

    COST CENTRE   MODULE  %time %alloc

    CAF           Main      0.0   99.8

                                               individual    inherited
    COST CENTRE   MODULE        no.  entries  %time %alloc   %time %alloc

    MAIN          MAIN            1         0   0.0    0.0     0.0  100.0
     CAF          Main          152        15   0.0   99.8     0.0   99.8
     CAF          GHC.Handle     88         4   0.0    0.2     0.0    0.2

I still love how concise and easy the solutions to problems like this are in Haskell.
