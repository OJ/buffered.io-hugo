---
categories:
- Functional Programming
- Haskell
- Project Euler
comments: true
date: 2008-03-17T00:00:00Z
tags:
- functional
- Haskell
- programming
- Project Euler
title: 'Project Euler #1'
---

**WARNING!** This post contains a spoiler for Problem #1 listed at [Project Euler][]. Do not read the rest of this post if you're planning to attempt to solve the problem yourself.

<!--more-->

I decided to work my way through these problems using [Haskell][], as I felt it'd be a great way to learn the language (or at least start to get familiar with it).

[Problem #1][] in the series is a nice "starter" question. It's a simple one to get you going, which for me was great since I am a bit of a Haskell beginner:

> If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
> 
> Find the sum of all the multiples of 3 or 5 below 1000.

Here is my Haskell source that generates the required answer:

```
sum [ n | n <- [1..999], ((mod n 3) == 0) || ((mod n 5) == 0) ]
```

Don't you just love how concise the answer is? If you're like me and are new to Haskell, let me explain it a little.

1. **`[1..999]`** - this generates a list of values from 1 to 999 inclusive.
1. **`[ value | item &lt;- source list, condition ]`** - this is the basic [list comprehension][ListComp] syntax in Haskell (basically this is how you generate lists from other lists). Here is the breakdown.
    1. _source list_ is a reference to a list which is used as the source to generate the new list.
    1. _item_ is the variable that is an alias for each item in the source list. This is usually referenced in the _value_ expression.
    1. _condition_ is a boolean condition that indicates whether or not a value should be included in the list.
    1. _value_ is the final expression that gets evaluated and stored in the list. The expression is only evaluated and added to the list _if the condition is true_
1. **`mod n 3`** - this evaluates "n modulo 3", and hence gives the remainder when *n* is divided by 3.
1. **`((mod n 3) == 0) || ((mod n 5) == 0)`** - this is a condition that evaluates to true if n is evenly divisible by 3 or 5.
1. **`[ n | n &lt;- [1..999], ((mod n 3) == 0) || ((mod n 5) == 0) ]`** - for each item *n* in the list of numbers 1 through 999, if the number is evenly divisible by 3 or 5, then include it in the list.
1. **`sum`** - this function simply produces the sum of all the numbers in the given list.

Pretty simple really isn't it :) Enjoy.

  [Problem #1]: http://projecteuler.net/index.php?section=problems&id=1 "Problem #1"
  [Project Euler]: http://projecteuler.net/ "Project Euler"
  [Haskell]: http://www.haskell.org/ "Haskell"
  [ListComp]: http://en.wikipedia.org/wiki/List_comprehension "List Comprehension"
