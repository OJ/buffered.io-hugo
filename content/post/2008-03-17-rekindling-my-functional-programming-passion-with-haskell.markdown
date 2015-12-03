---
categories:
- Functional Programming
- Haskell
comments: true
date: 2008-03-17T00:00:00Z
title: Rekindling my Functional Programming Passion with Haskell
---

Back in the day when I was studying at <a href="http://www.uts.edu.au/" title="UTS">University</a> I tutored a subject which taught students how to write functional code in <a href="" title="">Miranda</a>. I loved it! The whole <a href="http://en.wikipedia.org/wiki/Functional_programming" title="Functional Programming">functional programming</a> thing really did it for me. I was fascinated by it. Despite my interest, I never chased it up after finishing at University.

I recently stumbled on a random blog post that went into depth on some functional programming topics, and that resparked my interest.

<!--more-->

So after a few years off, I'm now back into toying with functional lanauges. Right now, the one I'm using is <a href="http://www.haskell.org/" title="Haskell">Haskell</a>, which is considered a pure functional programming language (by that I mean it has no <a href="http://en.wikipedia.org/wiki/Side_effect_%28computer_science%29" title="Side effect (computer science)">side effects</a>).

I'm still in the learning phase, and hence am not an expert. But I do love how concise the solutions are to some seriously crazy problems.

I decided to show you a really basic program which solves a really basic problem. Its job is to generate the <a href="" title="">Roman Numeral</a> equivalent of an integer. It's far from complete, and doesn't cover zero or numbers less than zero, but it does show how easy it can be to solve the problem with a very small amount of code. It's probably far from the best solution, and there may well be a more functional approach than what I've listed.

So for your scrutiny (and enjoyment :)) here's the source to that little proggy:
```
{- The 'map' of roman numerals and their string equivalents -}
romanMap :: [(Int,[Char])]
romanMap = [ (1000, "M"), (900, "CM"), (500, "D"), (400, "CD"), (100, "C"), (90, "XC"), (50, "L"), (40, "XL"), (10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1, "I")]

{- Get the roman numeral equivalent of a number using the given map -}
getRoman :: Int -> [(Int,[Char])] -> [Char]
getRoman _ [] = ""
getRoman 0 _ = ""
getRoman n ((d,r):rs) = (concat $ replicate (div n d) r) ++ (getRoman (mod n d) rs)

{- Helper function that calls the core function passing in the roman numeral map -}
roman :: Int -> [Char]
roman n = getRoman n romanMap
```

This should compile fine using <a href="http://www.haskell.org/ghc/" title="GHC">GHC</a>. If it doesn't, feel free to abuse me. Invoke the <em>roman</em> function and pass it a positive integer to get the result.

Over the coming weeks I'll start sharing my solutions to the <a href="http://projecteuler.net/" title="Project Euler">Project Euler</a> questions in the hope that you guys can help me improve them.

<strong>Be warned!</strong> Functional programming, once you've got a taste for it, can be very addictive. Start at your own risk!
