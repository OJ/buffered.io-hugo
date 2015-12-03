---
categories:
- Functional Programming
- Haskell
- Project Euler
comments: true
date: 2008-03-22T00:00:00Z
tags:
- functional
- Haskell
- programming
- Project Euler
title: 'Project Euler #4 - Reprise'
---

After publishing the performance stats of my <a href="/posts/project-euler-4/" title="Project Euler #4">previous solution to Project Euler #4</a>, I got thinking about how I might improve things. I didn't want to be overly anal with regards to things such as memory allocations, because it's easy to get stuck in the perpetual loop of attempted optimisations. Instead I wanted to think of a method that wasn't as brute force as the previous one. If you're interested to see what I did, read on (<strong>Note:</strong> This is a spoiler for the problem, just like the last post was).

<!--more-->

Let's start by looking at the source, which I commented heavily:
```
-- A function that determines if a number is a palindromic number
isPalindrome :: Integer -> Bool
isPalindrome n = n == (read (reverse (show n))::Integer)

-- create a bunch of possible factors knowing what we know about the problem.
-- a palindrome of 6 digits is in the form: abccba (we'll call this x)
-- x = abccba
-- so therefore x = 100000a + 10000b + 1000c + 100c + 10b + 1a
-- hence x = 100001a + 10010b + 1100c
-- That being the case, we can factor out the greatest common factor of
-- 100001, 10010 and 1100 which will give us a number that the result
-- must be evenly divisible by. So we generate a list, starting with the
-- first number >= 100 which is a multiple of that value. We increment by
-- that value each time. This should give us a much smaller list to work
-- against and hence reduce the time to calculate the answer.
possibleFactors :: [Integer]
possibleFactors = [ s, o..999 ]
                where
                  d = (gcd (gcd 1100 10010) 100001) -- gives the magic value (11 ;))
                  s = d * ceiling (100.0 / (fromIntegral d)) -- the starting point (110 ;))
                  o = s + d -- the next value in the list (121 ;))

-- the solution to the question - this time using the list of possible factors
-- and all the other numbers from 100 to 999
main :: IO()
main = print $ maximum [ a * b | a <- possibleFactors, b <- [100..999], isPalindrome (a * b) ]
```

If that's not clear, then drop me a comment and I'll try and explain further. In a nutshell, using the properties we know of the target result, we can narrow down the set of values that we can use to test.

Proof is in the perf! Check out the stats:

```
  Sat Mar 22 13:43 2008 Time and Allocation Profiling Report  (Final)

     main +RTS -p -RTS

  total time  =        2.36 secs   (118 ticks @ 20 ms)
  total alloc = 327,270,176 bytes  (excludes profiling overheads)

COST CENTRE                    MODULE               %time %alloc

CAF                            Main                 100.0  100.0

                                                  individual    inherited
COST CENTRE    MODULE            no.    entries  %time %alloc   %time %alloc

MAIN           MAIN                1           0   0.0    0.0   100.0  100.0
 CAF           Main              152          17 100.0  100.0   100.0  100.0
 CAF           Text.Read.Lex     129           8   0.0    0.0     0.0    0.0
 CAF           GHC.Real          127           1   0.0    0.0     0.0    0.0
 CAF           GHC.Read          124           1   0.0    0.0     0.0    0.0
 CAF           GHC.Float         123           9   0.0    0.0     0.0    0.0
 CAF           GHC.Handle         88           4   0.0    0.0     0.0    0.0
```

So the total time dropped to <strong>2.26 seconds</strong>. Quite an improvement over the previous implementation (around 4x faster).

Thoughts on further improvements?
