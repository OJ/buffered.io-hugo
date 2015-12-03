---
categories:
- Challenges
- F#
- Functional Programming
- Google
- Haskell
comments: true
date: 2008-06-14T00:00:00Z
tags:
- challenge
- F#
- functional
- Google
- Haskell
- interview
- programming
title: An Interesting Little Problem
---

This post was inspired by a recent interview question that was posted over at <a href="http://www.fsharp.it/posts/google-interview-question-product-of-other-elements-in-an-array-in-on/" title="Fsharp.it">fsharp.it</a>. It's one of those neat little questions which looks really simple on the surface but is quite tricky.

<!--more-->

The question apparently originates from an interview that someone had with Google, and goes something like this:<blockquote><p>
There is an array A[N] of N integers. You have to compose an array Output[N] such that Output[i] will be equal to the product of all the elements of A[] except A[i].

Example:
&nbsp;&nbsp;&nbsp;&nbsp;INPUT:[4, 3, 2, 1, 2]
&nbsp;&nbsp;&nbsp;&nbsp;OUTPUT:[12, 16, 24, 48, 24]

<strong>Note:</strong> Solve it <em>without</em> the division operator and in O(n).
</p></blockquote>
Since I had a spare 10 minutes, I decided to give it a shot ... in Haskell.

I'll cut to the chase, here's the source to my solution:
```
vals :: [Int]
vals = [ 4, 3, 2, 1, 2 ]

answers :: [Int]
answers = [ front !! (x-1) * back !! (x+1) | x <- [1..length vals] ]
  where
    front = scanl1 (*) temp
    back = scanr1 (*) temp
    temp = [1] ++ vals ++ [1]
```

I'm hoping that this is quite self-explanatory. But in case it's not, I'll cover some of the gory bits.

The core of the problem is coming up with a way of determining the value of the product of numbers from the start of the list up to a given index, and to do the same at the other end of the list from that given index.

I thought that the easiest way would be to create two lists: both of them containing the compounded products of the numbers in the list, but each of them in different directions. To generate those lists, I thought that I'd add the value of 1 to the list, both at the start and at the end, as it would allow me to do two things:<ol><li>Generate the lists using the <a href="http://haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html#v%3Ascanl1" title="scanl1">scanl1</a> and scanr1 functions.</li><li>Index into the list using a counter that's based on the size of the original values without having to worry about going past the bounds of the list.</li></ol>Yup, quite lazy, but very handy.

Here's the output when I execute <em>answers</em> in <a href="http://www.haskell.org/ghc/docs/latest/html/users_guide/ghci.html" title="GHCi">GHCi</a>:

    Prelude> :l google.hs
    [1 of 1] Compiling Main             ( google.hs, interpreted )
    Ok, modules loaded: Main.
    *Main> answers
    [12,16,24,48,24]
    *Main>


Problem solved in O(n). Neato! After feeling rather chuffed with myself I thought I'd go back to Fsharp.it and check out the answer posted there. The principle was similar, but the implementation listed was a little longer.

So I thought I'd have a go at writing up my solution using F#. It didn't seem like a stretch until I realised how little of the language I know (I'm currently reading through <a href="http://www.amazon.com/Expert-F-Experts-Voice-Net/dp/1590598504" title="Expert F#">Expert F#</a>, but I'm still far from being one myself). Here's what I came up with:
```
#light

let vals = [ 4; 3; 2; 1; 2; ]

let mul a b = a * b
let (++) = List.append

let answers =
  let temp = [1] ++ vals ++ [1]
  let front = List.scan1_left mul temp |> List.to_array
  let back = List.scan1_right mul temp |> List.to_array
  seq { for x in [1 .. vals.Length] -> front.[x-1] * back.[x+1] }
```

A few things you might notice:<ol><li>My syntax highlighter plugin doesn't currently support F# :)</li><li>I use a similar method to the Haskell solution, but ended up having to convert the <em>front</em> and <em>back</em> lists to arrays. The reason was because I need to be able to index into the resulting integer set, and I can't do that with lists (if I'm wrong, please let me know!)</li><li>I defined a function called <em>mul</em> which does a simple multiplication. I wanted to pass <em>(*)</em> as the first parameter to the scan1_* functions, but the interpreter took that as the start of a comment instead! So I had to resort to a dodgey hack. If you know a way around this, please let me know.</li><li>I wrote my own (++) operator because I didn't want to have to write List.append more than once :)</li></ol>
In other words, my F# version smells like n00b. I'm sure there are so many better ways to implement this using the built-in features of the language and supporting libraries, but I'm yet to get to the level when I can write it. I'd love for someone to show me how :)

I did enjoy having a dabble with F# for the first time in ages, though I have to admit I much prefer using <a href="http://www.vim.org/" title="VIM">VIM</a> and <a href="http://research.microsoft.com/fsharp/manual/compiler.aspx" title="F# Interactive">fsi.exe</a> instead of <a href="http://msdn.microsoft.com/en-us/vstudio/default.aspx" title="Visual Studio">Visual Studio</a> and the <a href="http://blogs.msdn.com/dsyme/archive/posts/534925.aspx" title="A Taste of F# Interactive in Visual Studio">interactive F# add-in</a>.

As always, feedback and criticism welcomed (and needed).

<h2>Update</h2>
After some great feedback (see below), I've come to realise that the !! operator in Haskell is actually O(n) itself. Hence it was a bad choice for inclusion. Back to the drawing board for me!

Here are a couple of submitted Haskell solutions.
```
-- by lf
scanm f z xs = zipWith f (scanl f z xs) (tail $ scanr f z xs)
main = print $ scanm (*) 1 [4,3,2,1,2]

-- by Henning
answers :: [Int] -> [Int]
answers vals = zipWith (*) front (drop 1 back)
	where
	front = scanl (*) 1 vals
	back = scanr (*) 1 vals

-- by desp
problem input = zipWith (*) front back where
  front = init (scanl (*) 1 input)
  back = tail (scanr (*) 1 input)

-- by foobar
foo [] _ = ([], 1)
foo (x:xs) acc = let (l, m) = foo xs (acc*x)
                 in ((m*acc):l , m*x)
```


Thanks for the submissions guys :) Sorry for not including the imperative versions in the update.
