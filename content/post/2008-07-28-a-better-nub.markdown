---
categories:
- Haskell
comments: true
date: 2008-07-28T00:00:00Z
tags:
- Haskell
- performance
- tip
title: A Better 'nub'
---

During my Haskell travels I have found myself using the <a href="http://haskell.org/ghc/docs/latest/html/libraries/base/Data-List.html#v%3Anub" title="Data.List nub">nub</a> function quite regularly. For those too lazy to click the link: <em>nub</em> removes duplicates from a list of items. eg:
```
Prelude> nub [1,1,3,3,5,5,6,6,6,1]
[1,3,5,6]
```

Fairly simple stuff. Until recently I hadn't bothered pondering the internal implementation of this function because I hadn't really been too worried about performance. That's no longer the case. I recently cracked open the hood of <em>nub</em>, and was rather surprised by what I saw.

<!--more-->

Since the source of <a href="http://www.haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html" title="Prelude">Prelude</a> is in the public domain, I'm safe to show you the content:
```
nub                     :: (Eq a) => [a] -> [a]
nub                      = nubBy (==)

nubBy                   :: (a -> a -> Bool) -> [a] -> [a]
nubBy eq []              = []
nubBy eq (x:xs)          = x : nubBy eq (filter (\y -> not (eq x y)) xs)
```

So <em>nub</em> uses <em>nubBy</em> and passes <em>(==)</em> as the filter for the items. This is all well and good, but the result is an O(n<sup>2</sup>) level of complexity! In this day and age that's pretty poor for a simple filtering mechanism, especially when we have more powerful mechanisms built-in to most libraries.

Since seeing this I had added a "write my own nub" task to my TODO list, but I haven't yet got round to it. So you can imagine my delight when I found <a href="http://www.hvergi.net/2008/07/playing-with-haskells-lazy-lists/#comment-50" title="Playing with Haskell's lazy lists - Comment by Jedai">this comment</a> by Jedai over at <a href="http://www.hvergi.net/" title="hvergi.net">hvergi.net</a>. He'd pointed out that <em>nub</em> was indeed inefficient, and showed an alternative implementation:
<blockquote cite="Jedai"><p>Note that nub only demands that the list element be part of the Eq typeclass. As a result it is very inefficient and a better solution must always be prefered whenever the nature of the elements allows it.

```
import qualified Data.Set as S
nub' :: (Ord a) => [a] -> [a]
nub' = go S.empty
  where go _ [] = []
        go s (x:xs) | S.member x s = go s xs
                    | otherwise    = x : go (S.insert x s) xs
```

La diffrence de performance est norme.</p></blockquote>
You got that right! Thanks to Jedai (and Arnar via hvergi.net) I now have a much faster <em>nub</em> implementation. Cheers guys!

It just goes to show that even these days you can't always trust the libraries. There is no harm is taking a look under the hood to see what's going on! You don't have to reinvent the wheel, but there's nothing wrong with making the wheel smoother :)
