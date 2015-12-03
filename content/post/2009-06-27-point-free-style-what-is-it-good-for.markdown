---
categories:
- Functional Programming
- Haskell
- Software Development
comments: true
date: 2009-06-27T00:00:00Z
tags:
- Haskell
- Point Free
- style
title: '''Point-Free style: What is it good for?'''
---

If you're not interested in what inspired this post, then skip this section and <a href="#MoreInteresting" title="The purpose of the post">jump</a> to the more interesting bits.

A Little Bit of History
-----------------------

Recently I've been delving into <a href="http://haskell.org/" title="Haskell">Haskell</a> quite a bit. It's part of my apparently never-ending quest to learn as much as I can about as many languages as I can (well, those that appeal to me at least :)). While I love playing around with a language, toying with ideas, writing small programs, reading books, blog posts, etc it's not really the same as having an on-call expert to help and guide you.

<!--more-->

 While I'm aware of, and frequently visit, the <a href=http://www.haskell.org/haskellwiki/IRC_channel" title="Haskell IRC">Haskell IRC channell</a> I find that the level of understanding there is so great that my piddly <a href="http://en.wikipedia.org/wiki/Newbie" title="Newbie">noob</a> questions tend to get lost amongst the bombardment of much more advanced & interesting discussions. In short, while I find it a great place to go, it's not a great place to find someone who's happy to help guide me through the maze and go over topics in quite a bit of detail while I annoy them with questions.

Haskell is one of those languages where having a mentor is really beneficial. So I set about finding myself one. Thankfully, I found a rather helpful chap based in the UK, <del>(he shall remain anonymous, as I don't want to violate the man's privacy)</del>, <a href="http://twitter.com/peter_marks" title="Peter Marks">Peter Marks</a>, who has been very forthcoming with information. He's humoured me and been incredibly patient so far, and I'm very grateful for his time.

One of the questions that I asked him was:<blockquote cite="OJ"><p>Why is it that everyone seems to strive to get their code into <a href="http://www.haskell.org/haskellwiki/Pointfree" title="Point Free">Point-Free</a> style? I can see how a lot of the implementations are more concise, but many of those lose readability.

What is so special about it?</p></blockquote>The discussion that followed was really insightful. That is what has inspired me to write this post.

Please note, any errors in this post are totally my own. They are not the fault of my mentor :)

<span id="MoreInteresting"/>

The purpose of the post: Why aim for Point-Free style?
------------------------------------------------------

So it is just me, or does anyone else out there feel that there's a bit of a "thing" going on for Point-Free style? Sometimes I share my terrible code with people and I get shunned when it's not Point-Free and it could be.

Anyone?

I hope it's not just me :) Let's start with taking a quick look at what Point-Free style actually is.

### So what is Point-Free style? ###

If we take a look at the <a href=http://en.wikipedia.org/wiki/Point-free_programming" title="Wikipedia">Wikipedia</a> we can see that..<blockquote><p>Tacit [point-free] programming is a programming paradigm in which a function definition does not include information regarding its arguments, using combinators and function composition (but not ?-abstraction) instead of variables.</p></blockquote>Simple eh? :) In essence, it basically means that your function definition doesn't reference any of its arguments/variables. For a crass definition, think <em>point == argument</em> and it should make sense.

So we're writing functions without directly referencing its arguments. For those not familiar with Haskell or other languages that support this, let's take a look at an example:
```
-- sum : take a list of numbers and add them all up to get a total
-- start with the base-case: an empty list
sum [] = 0
sum (x:xs) = x + sum xs
```

We can see that the above function is written in such a way that the arguments passed into the function are actually referenced in the body of the code. This is how a standard imperative programmer would write this function if he/she was new to Haskell. If we instead used a <a href="http://www.haskell.org/haskellwiki/Fold" title="Fold">fold</a>, we could define it like so:
```
sum xs = foldr (+) 0 xs
```

This does exactly the same thing as the previous definition, but as you can see the grunt work is done by the fold function. Now that we have this definition, we can easily move to Point-Free style:
```
sum = foldr (+) 0
```

Here we can see that no reference is made to the arguments of the function. Since we haven't referenced any "points", we have a Point-Free implementation.

Awesome. Cool. Sweet. Nifty.

But what does it give me? Why is it better?

### Why use Point-Free style? ###

This section is based totally on my own opinion and is not an official definition :) I think that Point-Free style fits in the same category as many other coding patterns and styles, and that it's usually down to the individual programmer to determine the when and the why. So take my view with a pinch of salt.

In short, Point-Free style let's you <em>focus on the <strong>how</strong> rather than the <strong>what</strong></em>.

It might be just me, but imperative programs seem to have a large focus on the data. The code which operates on the data is lost within a plethora of code that isn't necessarily specific to what the program needs to do. I've found that functional programming tends to be very different, at least in Haskell. Haskell lets me focus on what it is I need to do, and I feel that Point-Free is another step in the same direction. Is this good? I think so :) But I'll let you decide.

As well as the focus on the <strong>what</strong> I've found that aiming for a Point-Free solution can aid in helping you understand your problem better. This claim sounds like fluff, so let's go through an example and hopefully you'll see what I mean.
### Where Point-Free helped me get a better understanding ###
Haskell developers use the <a href="http://www.haskell.org/haskellwiki/Function_composition" title="Function composition">composition operator</a> <a href="http://haskell.org/ghc/docs/latest/html/libraries/base/src/GHC-Base.html#." title="(.) implementation in Prelude">(.)</a> a lot. It actually aids in creating Point-Free style. I love the irony here. We add points (.) to remove points (arguments).

Anyway, the composition operator's definition, according to <a href="http://www.haskell.org/onlinereport/standard-prelude.html" title="Prelude">Prelude</a> is:
```
(.)       :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = f (g x)
```

This operator takes two functions and produces a function which is composed of the two. This function takes the output of one function and sends it through as the input to another function, returning the result of that call.

This is really handy. We can do some interesting things like:

    ghci> let doubleAndAdd5 = (+5) . (*2)
    ghci> doubleAndAdd5 20
    45
    ghci> :m +Data.List
    ghci> let sumColumns = map sum . transpose
    ghci> sumColumns [[1,2,3],[4,5,6],[7,8,9]]
    [12,15,18]

Very handy indeed. While handy, it doesn't necessarily allow us to do everything we might want to do. One example is handling cases where we want to compose a function where the right-hand function takes two arguments instead of one.

So if we wanted to create a function that would take two values, add them together and double the result, all while using Point-Free style, we'd like to do something like this:

    ghci> let addAndDouble = (*2) . (+)

    <interactive>:1:20:
        No instance for (Num (a -> a))
          arising from a use of `*' at <interactive>:1:20-21
        Possible fix: add an instance declaration for (Num (a -> a))
        In the first argument of `(.)', namely `(* 2)'
        In the expression: (* 2) . (+)
        In the definition of `addAndDouble': addAndDouble = (* 2) . (+)
    ghci>

As you can see, ghci doesn't like it. And rightly so! This is because the composition operator's signature is:
```
(.) :: (b -> c) -> (a -> b) -> a -> c
```

That is:
<ol><li>It takes an argument which is a function which takes a value of type <em>b</em> which returns a value of type <em>c</em></li><li>It takes a function which takes a value of type <em>a</em> which returns returns a value of type <em>b</em></li><li>It returns a new function which takes a value of type <em>a</em> and returns a value of type <em>c</em></li></ol>
This doesn't work in our case, as we want our right-hand function to take two arguments. That is, we want a type signature that looks like this:
```
foo :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
```

So everywhere we had <em>(a -> b)</em>, what we really want is <em>(a -> a1 -> b)</em>. Given that we don't have an operator that does that for us, let's define one. We'll start with the definition of the (.) operator and work towards a function that does what we need.

```
-- here is the composition operator again
(.) f g x = f (g x)
-- here's our new operator's definition
(.^) f g x y = f (g x y)
```

Simple! Let's see what ghci says about it:

    ghci> let (.^) f g x y = f (g x y)
    ghci> :t (.^)
    a :: (t2 -> t3) -> (t -> t1 -> t2) -> t -> t1 -> t3

Looks good! This is exactly what we need. Now that we have a working function, let's aim to write this in Point-Free style. We do this by breaking down the function slowly and eliminating arguments by moving them to the far right hand side of the function definition.

The first argument to get rid of is 'y', as this is the last argument passed in:
```
-- start by moving y to the right
(.^) f g x y = f (g x y)
-- becomes
(.^) f g x y = f . (g x) $ y
```

These are functionally equivalent, and now that 'y' is out on it's own, we can drop it from our function definition:

    ghci> let (.^) f g x = f . (g x)
    ghci> :t (.^)
    (.^) :: (b -> c) -> (t -> a -> b) -> t -> a -> c
    ghci>

Excellent, we're a step closer. Now we need to do the same with 'x'. This takes a little more fiddling:
```
-- we need to take the original definition
(.^) f g x = f . (g x)
-- and change it so that it uses prefix notation instead of infix
(.^) f g x = (.) f (g x)
-- which is the same as
(.^) f g x = ((.) f) (g x)
-- what we're doing is calling g with x and applying the result to f
-- and hence we can compose the composition of f with the call to g
-- giving us the following
(.^) f g x = ((.) f) . g $ x
-- finally leaving us with
(.^) f g = ((.) f) . g
```

Phew :) I hope you can see the progression. We've managed to move x out of the picture, so now we're down to a fairly crazy looking definition. Let's see what ghci has to say:

    ghci> let (.^) f g = ((.) f) . g
    ghci> :t (.^)
    (.^) :: (b -> c) -> (a1 -> a -> b) -> a1 -> a -> c

So we've got rid of two variables, but there are still two more to go! Remember, 'f' and 'g' are still variables, they just carry functions. So let's get rid of 'g':
```
-- start with what we had before
(.^) f g = ((.) f) . g
-- change to prefix notation again
(.^) f g = (.) ((.) f) g
-- and drop g
(.^) f = (.) ((.) f)
```

This is looking rather crazy :) Again, let's make sure we haven't done anything stupid:

    ghci> let (.^) f = (.) ((.) f)
    ghci> :t (.^)
    (.^) :: (b -> c) -> (a1 -> a -> b) -> a1 -> a -> c

Excellent. We've still got what we need. One more point needs to be dropped, so let's get rid of 'f':
```
-- start with what we had before
(.^) f = (.) ((.) f)
-- which means that we're composing a composition with a
-- function composed of f and something else
(.^) f = (.) . (.) $ f
-- and we finally drop f
(.^) = (.) . (.)
```

Isn't that just crazy! Let's again check ghci:

```
ghci> let (.^) = (.) . (.)
ghci> :t (.^)
(.^) :: (b -> c) -> (a1 -> a -> b) -> a1 -> a -> c
```

So there we have it, our operator completely in Point-Free style.

This is where the penny dropped for me. The whole exercise of moving through to Point-Free made me really understand what it was I was doing in the first place. The final definition makes it very clear. We're composing two separate compositions.
Let's see if our function behaves itself using the example we listed above.

```
ghci> let addAndDouble = (*2) .^ (+)
ghci> :t addAndDouble
addAndDouble :: Integer -> Integer -> Integer
ghci> addAndDouble 10 15
50
ghci> addAndDouble 21 3
48
```

It does exactly what we need it too.

Conclusion
----------

To sum up, Point-Free helps you tidy your code into more concise implementations which tend to aid you in understanding what it is you are trying to do. I feel it really helps you focus on what you're doing as opposed to what you're doing it to. It's down to you to determine whether you feel this is a good thing or not!

It is ultimately down to the developer to dictate when it should be used. There are definitely cases where the resulting function might not actually help in making things clearer. But on the whole, Point-Free style seems to help me understand what it is I'm doing (or, arguably, not doing).

Of course, you could just get sick and tired of trying to come up with variable names, in which case Point-Free is the bomb :)

Feedback of any kind is always welcome. Cheers!
