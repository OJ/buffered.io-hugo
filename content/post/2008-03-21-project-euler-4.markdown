---
categories:
- Functional Programming
- Haskell
- Project Euler
comments: true
date: 2008-03-21T00:00:00Z
tags:
- functional
- Haskell
- programming
- Project Euler
title: 'Project Euler #4'
---

<strong>WARNING!</strong> This post contains a spoiler for Problem #4 listed at <a href="http://projecteuler.net/" title="Project Euler">Project Euler</a>. Do not read the rest of this post if you're planning to attempt to solve the problem yourself.

<!--more-->

The next in the series, <a href="http://projecteuler.net/index.php?section=problems&id=4">Problem #4</a>, isn't a particularly difficult one, but it could pose some interesting issues depending on which language you use as well as which method you use. It reads:<blockquote><p>A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91  99.

Find the largest palindrome made from the product of two 3-digit numbers.</p></blockquote>
Thankfully, the built-in features of our chosen language, Haskell, make this problem extremely simple. The obvious way to solve this problem is brute force. So that's exactly what I did with a bit of tweaking. For those following my Euler posts so far who are also new to Haskell, you may find there's some additional syntax and functions here that you haven't seen before.

```
-- A function that determines if a number is a palindromic number
isPalindrome :: Integer -> Bool
isPalindrome n = n == (read (reverse (show n))::Integer)

-- the solution to the question
main :: IO()
main = print $ maximum [ a * b | a <- [100..999], b <- [a..999], isPalindrome (a * b) ]
```


It is fairly easy to understand so long as you keep the following in mind:
<ol><li><strong>show</strong> - essentially converts a value to a string representation. So in our case, it converts the Integer to a String (or [Char] as it is in fact behind the scenes)</li><li><strong>reverse</strong> - reverses a list. So we take the list of characters from the previous call, and reverse it.</li><li><strong>read</strong> - reads a string of characters, and converts it into another type based on the one specified (in our case "::Integer" says to make the value an Integer)</li><li><strong>maximum</strong> - returns the largest numeric value found in a list.</li></ol>Yet another neat little solution. Essentially a two-liner! I can't see that happening in an imperative language :)

But what is the trade-off? Since this is the first brute force solution to a problem that might be faster solved by doing something smarter, let's see how long it takes. Here's a profile dump from GHC:

```
  Fri Mar 21 13:13 2008 Time and Allocation Profiling Report  (Final)

     main +RTS -p -RTS

  total time  =        8.24 secs   (412 ticks @ 20 ms)
  total alloc = 1,793,269,760 bytes  (excludes profiling overheads)

COST CENTRE                    MODULE               %time %alloc

CAF                            Main                 100.0  100.0

                                                individual    inherited
COST CENTRE    MODULE         no.    entries  %time %alloc   %time %alloc

MAIN           MAIN             1           0   0.0    0.0   100.0  100.0
 CAF           Main           152           6 100.0  100.0   100.0  100.0
 CAF           Text.Read.Lex  129           8   0.0    0.0     0.0    0.0
 CAF           GHC.Read       124           1   0.0    0.0     0.0    0.0
 CAF           GHC.Handle      88           4   0.0    0.0     0.0    0.0
```

Total time = <strong>8.24 secs</strong>. Not blindingly fast, but pretty darned good considering how long it took to write the solution!

Thoughts?
