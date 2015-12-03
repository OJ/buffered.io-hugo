---
categories:
- Functional Programming
- Haskell
- Project Euler
comments: true
date: 2008-03-28T00:00:00Z
tags:
- functional
- Haskell
- programming
- Project Euler
title: 'Project Euler #6'
---

<strong>WARNING!</strong> This post contains a spoiler for Problem #6 listed at <a href="http://projecteuler.net/" title="Project Euler">Project Euler</a>. Do not read the rest of this post if you're planning to attempt to solve the problem yourself.

<!--more-->

<a href="http://projecteuler.net/index.php?section=problems&id=6">Problem #6</a> was a <em>little</em> bit of a disappointment, but I think that's because the solution was so extremely simple using Haskell. Again, we'll check out the question first:<blockquote><p>The sum of the squares of the first ten natural numbers is,
1<sup>2</sup> + 2<sup>2</sup> + ... + 10<sup>2</sup> = 385

The square of the sum of the first ten natural numbers is,
(1 + 2 + ... + 10)<sup>2</sup> = 55<sup>2</sup> = 3025

Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 - 385 = 2640.

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.</p></blockquote>Now there are two main ways of doing this. The first requires knowledge of the properties of each of the two "bits" that we need to determine. If you Google enough, you'll find information which leads you to two different formulae; one which gives you the sum of the squares, the other the square of the sums. These will tell you how to solve each in O(1) time. Combine the two and you'll have a magic formula for solving the problem in a single step.

As for me, after doing the research I found no thrill in writing a function which took an integer and returned an integer that's the result of a basic equation. So instead, I wrote the following:
```
main :: IO ()
main = print $ (sum [1..100]) ^ 2 - sum [ x^2 | x <- [1..100] ]
```

Nice and concise. Sure it takes longer as the numbers increase, but what the hell! If I need to do it in the real world I'll use the method that I haven't posted :).

I'd ask for thoughts, but for this problem there really aren't too many that can be had! On to the next problem.
