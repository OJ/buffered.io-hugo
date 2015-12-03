---
categories:
- Algorithms
- Functional Programming
- Haskell
- Software Development
comments: true
date: 2009-06-25T00:00:00Z
title: Data Crunching in Haskell
---

A few days ago I was having a chat to a <a href="http://shiftperception.com/blog" title="Shifty">friend of mine</a> about a little data parsing problem. He had the need to parse a multi-dimensional array to pull out some values. That array was guaranteed to be square, but not necessarily in contiguous memory. He needed to parse each "column" of the array, calculate a total, and then determine the biggest and smallest of those totals.

A sample of the data might look something like this:

```
data = ({150,200,45,57,95,2,45,32,15,10,5,2,2,4},
        {12,20,45,37,10,5,2,2,10,95,2,45,32,7},
        {32,15,10,5,2,23,24,15,20,45,57,95,0,45})
```

So the first step would be to add 150, 12 and 32 and store the value. Then 200, 20 and 15, and store the value. Do this for all of the columns, then get a maximum and a minimum.

<!--more-->

This little algorithm was going to be part of his project, and hence needed to be implemented in <a href="http://www.adobe.com/devnet/actionscript/articles/actionscript3_overview.html" title="ActionScript 3.0">AS3</a>. So I picked his brains about the AS3 syntax, because I have absolutely no clue given that I've never worked with any version of ActionScript in the past.

Together, we came up with the following solution:

```
var columnTotal:Number;
var biggest:Number;
var smallest:Number;
biggest = smallest = sum(vData, 0);

for(var i = 1; i < _scope.period_mcs.length; ++i)
{
  columnTotal = sum(vData, i);
  biggest = Math.max(biggest, columnTotal);
  smallest = Math.min(smallest, columnTotal);
}

// helper function 
function sum(var data:Array, var index:Integer):Number
{
  var total:Number = 0;
  for(int i = 0; i < data.length; ++i)
  {
    total += data[i][index];
  }
  return total;
}
```

Does it work? Yes, it sure does. Is it optimal? Yes, and no :) I was lazy and used the Math.max and Math.min functions instead of doing the obvious...

```
if(biggest < columnTotal) biggest = columnTotal;
```

So if we did that, to reduce the need for function calls and unnecessary assignments, we end up with this:

```
var columnTotal:Number;
var biggest:Number;
var smallest:Number;
biggest = smallest = sum(vData, 0);

for(var i = 1; i < _scope.period_mcs.length; ++i)
{
  columnTotal = sum(vData, i);
  if(biggest < columnTotal)
  {
    biggest = columnTotal;
  }
  else if(smallest > columnTotal)
  {
    smallest = columnTotal;
  }
}

// helper function 
function sum(var data:Array, var index:Integer):Number
{
  var total:Number = 0;
  for(int i = 0; i < data.length; ++i)
  {
    total += data[i][index];
  }
  return total;
}
```

I can't see many ways to improve on this without going overboard with optimisation. Any AS3 guru's are more than welcome to prove me wrong!

So after thinking about this in an imperative language, I couldn't help but have a look at what the functional version might look like. Of course, my current chosen Functional toy is <a href="http://haskell.org/" title="Haskell">Haskell</a> and so I fired up <a href="http://vim.org/" title="VIM">VIM</a> and <a href="http://www.haskell.org/ghc/" title="GHC">GHCI</a> and had a bit of a play.

First, I put the data into Haskell's list format:

```
vals = [[150,200,45,57,95,2,45,32,15,10,5,2,2,4],
        [12,20,45,37,10,5,2,2,10,95,2,45,32,7],
        [32,15,10,5,2,23,24,15,20,45,57,95,0,45]]
```

That was easy enough. The next step was to break the problem down so that I could use some of the built in functions of Haskell's <a href="http://haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html" title="Prelude">Prelude</a> libraries. My thought processes were:
<ol>
<li>Summing the column of the array could be done using <a href="http://haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html#v:sum" title="sum">sum</a>, but I'd need to change the list so that rows become columns, and vice-versa.</li>
<li>To switch rows and columns, I could use the <a href="http://haskell.org/ghc/docs/latest/html/libraries/base/Data-List.html#v:transpose" title="Data.List.transpose">transpose</a> function.</li>
<li>Then all I'd need to do is use <a href="http://haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html#v:maximum" title="maximum">maximum</a> and <a href="http://haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html#v:minimum" title="minimum">minimum</a> to get the right values out.</li>
</ol>
Being a bit of a primitive Haskeller, my first pass was something like this:
```
extremes n = (b, s)
  where
    l = map sum $ transpose n
    b = maximum l
    s = minimum l
```

The function <em>extremes</em> is the function which takes the data (list of lists) and spits out a tuple of <em>(max, min)</em>.

Before the end of the conversation with my designer colleague in arms, I pinged him my version of the solution in Haskell and needless to say he was a little surprised as how concise it was. I was sure to point out that there is no doubt a better way of representing this solution with regards to speed and conciseness.

The first thing that I thought could be improved would be using a custom <a href="http://www.haskell.org/haskellwiki/Fold" title="Fold">fold</a> to get the max and min while parsing the transposed list. This would allow us to calculate the values in a single pass and hence be a little better with regards to performance. That would obviously sacrifice a little bit of the conciseness we're looking for.

When the conversation ended, I jumped onto IRC and spoke to some more seasoned Haskellers. The first suggested improvement that popped out of that chat was to use <a href="http://www.haskell.org/arrows/" title="Arrows">arrows</a> to remove the need for the where clause. That solution looks like this:
```
extremes = (maximum &&& minimum) . map sum . transpose
```

Nifty :) Of all the other options, this proved to be the most readable and concise, though not the best performing.

The next most notable solution included the fold which calculated the min and max in a single parse, not a double parse:
```
extremes = foldl1 (\(a, b) -> max a *** min b) . join zip . map sum . transpose
```

Folds really are fantastic aren't they. Again we're using arrows here to do a bit of heavy lifting and that keeps things looking a little nicer.

So after this little session, my designer friend was aware of how easy it can be to crunch certain types of data using a functional language, like Haskell. It made me think again about how it'd be nice to just be able to plug in whichever language we wanted whenever we felt it would do the job better than whatever the current tool is.

So how would you improve it? :)
