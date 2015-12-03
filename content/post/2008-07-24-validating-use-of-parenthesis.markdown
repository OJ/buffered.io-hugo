---
categories:
- Challenges
- F#
- Functional Programming
comments: true
date: 2008-07-24T00:00:00Z
tags:
- challenge
- fsharp
- functional
title: Validating use of Parenthesis
---

Yet another <a href="http://www.dev102.com/posts/a-programming-job-interview-challenge-13-brackets/" title="Programming Challenge">programming challenge appeared on dev102</a> the other day, and I thought that this time I'd post my solution here in the blog rather than letting it get lost in the depths of the comment thread!

<!--more-->

The problem is as follows:<blockquote><p>Your input is a string which is composed from bracket characters. The allowed characters are:(', ), [', '], {, }, &lt; and &gt;. Your mission is to determine whether the brackets structure is legal or not.

Example of a legal expression: ([](&lt;{}&gt;)).

Example of an illegal expression: ({&lt;)&gt;}.</p></blockquote>Is what I'm about to show the most efficient? No. Is it the most elegant? Hell no! But it works :)

It's surprisingly similar to <a href="http://www.fsharp.it/posts/balanced-parenthesis/" title="Balanced Parenthesis">this solution</a> over at Fsharp.it. It is different in that it allows non-bracket characters to be entered into the string as well.
```
#light

let parens = [| ('(',')');('[',']');('{','}');('<','>') |]
let isOpen l = Array.exists (fun(o,c) -> o = l) parens
let isClose l = Array.exists (fun(o,c) -> c = l) parens
let isPair p = Array.exists (fun l -> p = l) parens

let validate inp =
  let rec v' str stack =
    match str with
    | [] -> stack = []
    | c :: cs when isOpen c -> v' cs (c :: stack)
    | c :: cs when isClose c ->
        if isPair ((List.hd stack), c)
            then v' cs (List.tl stack)
            else false
    | c :: cs -> v' cs stack
  v' (List.of_seq inp) []
```


Call the function like so:
```
validate "thisIsAFunction(int[] foo, delegate{}, bar()); // < testing >"
```

In short, the function uses an internal "stack" (which is actually a list) to keep track of open brackets. When a closed bracket is found, it's validated against the open bracket at the top of the stack.

Fairly simple stuff. There are optimisations that can be made around the searching for items in the <em>parens</em> array, but I couldn't be bothered changing it :) For me at the moment it's more about playing with F#.

Thoughts?
