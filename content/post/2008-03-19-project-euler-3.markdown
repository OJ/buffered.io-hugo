---
categories:
- Functional Programming
- Haskell
- Project Euler
comments: true
date: 2008-03-19T00:00:00Z
tags:
- functional
- Haskell
- programming
- Project Euler
title: 'Project Euler #3'
---

<strong>WARNING!</strong> This post contains a spoiler for Problem #3 listed at <a href="http://projecteuler.net/" title="Project Euler">Project Euler</a>. Do not read the rest of this post if you're planning to attempt to solve the problem yourself.

<!--more-->

<a href="http://projecteuler.net/index.php?section=problems&id=3">Problem #3</a> in the series is the first one to bring in prime numbers. Here's the question:<blockquote><p>The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?</p></blockquote>
For this particular problem I decided to avoid generating prime numbers because that takes ages. I had a feeling that later on down the track I was going to encounter a problem where I would need to generate primes, but for this problem I didn't find it necessary.

Instead, I attempted to use my noggin'. Up there for thinkin', down there for dancin'. Here's my Haskell source:
```
biggestDivisor :: Integer -> Integer
biggestDivisor n
              | even n    = biggestDivisor (div n 2)
              | otherwise = m n 3
                where m n d
                          | d >= n        = d
                          | mod n d == 0  = m (div n d) d
                          | otherwise     = m n (d + 2)

main :: IO ()
main = print $ biggestDivisor 600851475143
```

Don't ya just love built-in Big Integer support? :)

This obviously works from the bottom up. It starts with a special case with an even number. It keeps dividing by two until it's no longer even. At this point, it begins factorising using odd numbers starting at 3. The loop finishes when the last found factor is bigger than the number that is being factored. This works because as we find the factors, we divide the number by the factor. This keeps reducing the size of the number we're factorising, while increasing the size of the factors.

Pretty simple really isn't it! Thoughts?

<strong>Note:</strong> I'm assuming that nobody is going to call this function that is a multiple of 2!
