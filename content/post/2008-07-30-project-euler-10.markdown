---
categories:
- Functional Programming
- Haskell
- Project Euler
comments: true
date: 2008-07-30T00:00:00Z
tags:
- functional
- Haskell
- programming
- Project Euler
title: 'Project Euler #10'
---

<strong>WARNING!</strong> This post contains a spoiler for Problem #10 listed at <a href="http://projecteuler.net/" title="Project Euler">Project Euler</a>. Do not read the rest of this post if you're planning to attempt to solve the problem yourself.

<!--more-->

<a title="Project Euler #10" href="http://projecteuler.net/index.php?section=problems&amp;id=10">Problem #10</a> takes us back into the realm of the relatively boring - prime numbers (again). The question is as follows:<blockquote><p>The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.</p></blockquote>
Exciting stuff! Since we've already got ourselves a relatively nifty prime number generator from a <a title="Project Euler #7" href="/posts/project-euler-7/">previous post</a>, we can simply reuse this, along with the sum function that comes in <a title="Prelude" href="http://www.haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html">Prelude</a>, to generate the answer.

Here is what the code looks like:
```
merge :: (Ord a) => [a] -> [a] -> [a]
merge xs@(x:xt) ys@(y:yt) =
  case compare x y of
    LT -> x : (merge xt ys)
    EQ -> x : (merge xt yt)
    GT -> y : (merge xs yt)

diff :: (Ord a) => [a] -> [a] -> [a]
diff xs@(x:xt) ys@(y:yt) =
  case compare x y of
    LT -> x : (diff xt ys)
    EQ -> diff xt yt
    GT -> diff xs yt

primes, nonprimes :: [Integer]
primes    = [2, 3, 5] ++ (diff [7, 9 ..] nonprimes)
nonprimes = foldr1 f $ map g $ tail primes
  where
    f (x:xt) ys = x : (merge xt ys)
    g p         = [ n * p | n <- [p, p + 2 ..]]

main :: IO ()
main = print $ sum (takeWhile (<2000000) primes)
```

I don't really have much else to say about this. I won't bother with the performance information because it's not really doing anything too exciting that hasn't already been discussed.

Cheers!
