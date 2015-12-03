---
categories:
- Functional Programming
- Haskell
- Project Euler
comments: true
date: 2008-03-23T00:00:00Z
tags:
- functional
- Haskell
- programming
- Project Euler
title: 'Project Euler #5'
---

<strong>WARNING!</strong> This post contains a spoiler for Problem #5 listed at <a href="http://projecteuler.net/" title="Project Euler">Project Euler</a>. Do not read the rest of this post if you're planning to attempt to solve the problem yourself.

<!--more-->

<a href="http://projecteuler.net/index.php?section=problems&id=5">Problem #5</a> is a great little problem that requires a little maths knowledge to be able to solve in a reasonable period of time. Let's take a look at the question:<blockquote><p>2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest number that is <u>evenly divisible</u> by all of the numbers from 1 to 20?</p></blockquote>
Brute force? Hrm, I don't think so :) It'd take an ice age. Nope, instead we need to do a bit of <a href="http://en.wikipedia.org/wiki/Least_common_multiple" title="Least common multiple">research</a> to help come up with a smarter solution.

In short, the best way is to find the lowest common multiple (LCM) for each of the numbers respectively. Thankfully, Haskell comes with a couple of handy functions which make it really easy to iterate over a list and find the LCM of each pair of items:<ol><li><strong>lcm</strong> - finds the LCM of two numbers.</li><li><strong>fold*</strong> - iterates over a list and applies a function to each item pair iteratively using the result on the next item. There are many flavours, but we'll use <a href="http://haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html#v%3Afoldl1" title="foldl1">foldl1</a>.</li></ol>
So here it is, the solution to the problem in all its glory:
```
main :: IO ()
main = print $ foldl1 (lcm) [2..20]
```

Genius isn't it :) We could actually avoid all the numbers from 2 to 10 if we wanted, because we know that multiples of those numbers appear from 11 to 20, so to reduce a bit of time we could do this:
```
main :: IO ()
main = print $ foldl1 (lcm) [11..20]
```

That gives the same result just a tiny bit quicker.

Now it's time for me to embarass myself a little. Before I came to the "neat" little conclusion above, I actually did a fair bit of work hand-coding my own solution. I was trying to be too tricky for my own good, and as a result I paid the price of wasting a lot of time. Having said that, I learned a lot about the language when I wrote this, so it wasn't a <em>complete</em> waste of time. The code below is what happens when you attempt a problem without actually doing any research on it :) Enjoy!
```
import List

-- Prime factor is a tuple where the first value is
-- the number, and the second value is the power that
-- the number is to be raised to. eg:
-- (2, 4) = 2 ** 4
type PrimeFactor = (Integer, Int)

-- Function that factorises a number into a list of
-- prime factors. eg:
-- factorise 24 = [2, 2, 2, 3]
factorise :: Integer -> [Integer]
factorise 1 = []
factorise n | even n    = 2 : factorise (div n 2)
            | otherwise = f n 3
          where
            f 1 _ = []
            f n d | mod n d == 0  = d : f (div n d) d
                  | otherwise     = f n (d + 2)

-- Convert a list of factors into a list of prime factors. eg:
-- power [2, 2, 2, 3] = [(2, 3), (3, 1)]
power :: [Integer] -> [PrimeFactor]
power l = map (\l -> (head l, length l)) $ group l

-- Convert a list of prime factors into a normalise list where
-- we keep the instance of a number with the highest power. eg:
-- normalise [(2, 1), (2, 2), (3, 3), (3, 2)] = [(2, 2), (3, 3)]
normalise :: [PrimeFactor] -> [PrimeFactor]
normalise []      = []
normalise (n:ns)  = [f n ns] ++ normalise (filter (\x -> fst x /= fst n) ns)
          where
            f n []          = n
            f (n, p) (l:ls) | n == fst l && p <= snd l  = f l ls
                            | otherwise                 = f (n, p) ls

-- expand the list of prime factors and multiply them all out. eg:
-- expand [(2, 2), (3, 3)] = 2 ** 2 * 3 ** 3 = 8 * 9 = 72
expand :: [PrimeFactor] -> Integer
expand []           = 0
expand ((n, p):[])  = n ^ p
expand ((n, p):ls)  = n ^ p * expand ls

-- helper function that takes a list of prime factor lists and
-- generates the resulting product number (as per the question)
eval :: [[PrimeFactor]] -> Integer
eval = expand . normalise . concat

-- standard IO monad to solve and print to screen
-- note, we don't need to include the numbers from 1 to 10
-- because the numbers from 11 to 20 cover those values as
-- factors!
main :: IO ()
main = print $ eval $ map (\l -> power (factorise l)) [11..20]
```

Isn't that just terrific :P Not only is it huge, but it just stinks of "n00b". You can tell that it wasn't written by an experienced Haskeller.

Thoughts?
