---
categories:
- Functional Programming
- Haskell
- Project Euler
comments: true
date: 2008-06-15T00:00:00Z
tags:
- functional
- Haskell
- programming
- Project Euler
title: 'Project Euler #7'
---

<strong>WARNING!</strong> This post contains a spoiler for Problem #7 listed at <a href="http://projecteuler.net/" title="Project Euler">Project Euler</a>. Do not read the rest of this post if you're planning to attempt to solve the problem yourself.

<!--more-->

<a href="http://projecteuler.net/index.php?section=problems&id=7">Problem #7</a> was as boring as batshit (to be quite frank). The only reason I'm posting about it is to keep <a href="http://berkshirehunt.com/" title="Berkshire Hunt">The Head</a> happy ;)

The reason I thought it was boring was because it was just the age-old problem of "how fast can you generate primes":<blockquote><p>
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10001st prime number?</p></blockquote>
A simple search of the web reveals a million different ways of solving this problem -- mostly written by people smarter than me :)

So did I cheat? No I didn't. I wrote the standard <a href="http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes" title="Sieve of Eratosthenes">Sieve of Eratosthenes</a> implementation in Haskell and got the answer in a respectable amount of time. Exciting stuff :|

I browsed around the web a little bit after solving the problem because I was interested to see how others might have solved it using Haskell. Yes, I am aware that there is a <a href="" title="">whole page dedicated to it</a> but that wasn't enough.

After a bit of a search, I stumbled across this little doozy. If you can make heads or tails of it you're a better man than I ;) It's quite a mind job to get your head around how this code works. This is an exercise I leave to the reader.
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
main = print $ primes !! 10000
```

Enjoy :)

PS. Jon - are ya happy now? :D
