---
categories:
- Functional Programming
- Haskell
- Project Euler
comments: true
date: 2008-07-25T00:00:00Z
tags:
- functional
- Haskell
- programming
- Project Euler
title: 'Project Euler #9'
---

<strong>WARNING!</strong> This post contains a spoiler for Problem #9 listed at <a href="http://projecteuler.net/" title="Project Euler">Project Euler</a>. Do not read the rest of this post if you're planning to attempt to solve the problem yourself.

<!--more-->

<a href="http://projecteuler.net/index.php?section=problems&id=9">Problem #9</a> takes a turn back into the math theory and forces you to think about Pythagorean triplets. It goes as follows:<blockquote><p>
A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,
&nbsp;&nbsp;&nbsp;&nbsp;a<sup>2</sup> + b<sup>2</sup> = c<sup>2</sup>

For example, 3<sup>2</sup> + 4<sup>2</sup> = 9 + 16 = 25 = 5<sup>2</sup>.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
</p></blockquote>
The first solution to this problem that I came up with was just atrocious :) It was long, bloated and extremely messy. But it worked. If you really want to see it then ask me nicely and I'll post it.

For now I'm going to show you my second attempt, which is a little nicer on the eyes.
```
main :: IO ()
main = print $ head [ a * b * c
                    | b <- [1..400],
                      a <- [b..400],
                      let c = 1000 - a - b,
                      c^2 == a^2 + b^2 ]
```

It's a mini brute-forcer which uses two variables to count up to 400. I chose 400 as the end number because it seemed like a stupid idea to go as far as 500 when brute forcing since the first of the numbers that fit the criteria wouldn't be anywhere near that high. For each combination of numbers I pick what's left over and check to see if it fits the rules of a Pythagorean triplet. In this case there is only one that does (as per the question).

We can see below that the solution isn't particularly fantastic as far as performance goes. It's not terrible, but approaching .2 seconds for a problem like this is starting to get on the sluggish side.

```
  Fri Jul 25 21:28 2008 Time and Allocation Profiling Report  (Final)

     main +RTS -p -RTS

  total time  =        0.18 secs   (9 ticks @ 20 ms)
  total alloc =  29,337,532 bytes  (excludes profiling overheads)

COST CENTRE   MODULE  %time %alloc

CAF           Main    100.0  100.0


                                           individual    inherited
COST CENTRE   MODULE      no.    entries  %time %alloc   %time %alloc

MAIN          MAIN          1           0   0.0    0.0   100.0  100.0
 CAF          Main        152          16 100.0  100.0   100.0  100.0
 CAF          GHC.Handle   88           4   0.0    0.0     0.0    0.0
```

Another way of brute-force solving this is to abuse a bit of math. We know that each Pythagorean triplet is representative of the sides of a triangle. Given that the sides of a triangle scale evenly when the entire triangle is scaled, we can assume that if we were able to find a triplet whose sum is a factor of 1000, then we could use this factor to generate a, b and c. It turns out that the 5th Pythagorean triplet (8, 15, 17) adds up to 40, and since 40 * 25 == 1000, we can simply multiple each of the values by 25 to get (200, 375, 425).

While this is interesting, brute-force methods aren't never really the nicest way of solving a given problem. In the case of Project Euler it's rare to find a problem that doesn't have some form of trickery that can be abused to get the answer in a much more efficient manner. The theory that should be used is that each Pythagorean triplet can be <a href="http://en.wikipedia.org/wiki/Pythagorean_triple#Generating_a_triple">written in the following form</a>:

```
k [ m^2 - n^2, 2mn, m^2 + n^2 ]
```

This info can be abused to come up with some code to generate the answer rather quickly. At this point, I become tired of the problem and decided not to implement a version that uses this theory. If you decide to write one, please ping me!
