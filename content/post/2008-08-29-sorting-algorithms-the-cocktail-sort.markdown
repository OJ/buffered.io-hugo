---
categories:
- Algorithms
- Software Development
- Sorting
comments: true
date: 2008-08-29T00:00:00Z
tags:
- Cocktail Sort
- Sorting
title: 'Sorting Algorithms: The Cocktail Sort'
---

<img src="/uploads/2008/08/cocktail.jpg" alt="Cocktail" title="Cocktail" style="float: right; margin-left: 5px; margin-bottom: 5px" />Welcome to the second post in my series on <a href="/category/sorting/" title="Sorting @ OJ's rants">sorting</a> algorithms. This time we're going to talk about a sort that most people haven't heard a great deal about: the <strong>Cocktail Sort</strong>.

This algorithm was the next logical choice in the series because it is very similar to the <a href="/posts/sorting-algorithms-the-bubble-sort/" title="Sorting Algorithms: The Bubble Sort">Bubble Sort</a> in the way that it operates. If you're yet to read the first in the series, head <a href="/posts/sorting-algorithms-the-bubble-sort/" title="Sorting Algorithms: The Bubble Sort">over there now</a> as it will make this algorithm easier to understand.

<!--more-->

Fundamentally the algorithm is the same. The difference is that the Cocktail Sort iterates through a given <em>data set</em> in <strong>both</strong> directions when sorting. So let's break it down.

<h2>Common Terms</h2>
<ul>
<li><em>Data set</em> - the array or list of items that is to be sorted.</li>
<li><em>n</em> - the number of elements in the <em>data set</em></li>
</ul>

<h2>The Algorithm</h2>
Each iteration of the algorithm is broken up into two stages.

The first stage loops through the <em>data set</em> from bottom to top, just like the Bubble Sort. During the loop, adjacent items are compared. If at any point the value on the left is greater than the value on the right, the items are swapped. At the end of the first iteration, the largest number will reside at the end of the set.

The second stage loops through the <em>data set</em> in the <strong>opposite</strong> direction - starting from the item just before the most recently sorted item, and moving back towards the start of the list. Again, adjacent items are swapped if required.

The Cocktail Sort also fits in the category of <strong>Exchange Sorts</strong> due to the manner in which elements are moved inside the <em>data set</em> during the sorting process.

<h2>The Example</h2>
<div>
<style type="text/css">
span.eg { font-family: Courier new; font-size: 12px; display: block; }
span.eg b { color: Red; }
span.eg u { color: Green; }
span.eg i { color: Blue; }
</style>
</div>

We will use the same initial <em>data set</em> that we used for the Bubble Sort to aid in highlighting the differences.

The initial set looks like this:

<span class="eg">33 98 74 13 55 20 77 45 64 83</span>

The first iteration starts at the beginning of the list, comparing the first two items (marked in red):

<span class="eg"><b>33 98</b> 74 13 55 20 77 45 64 83</span>

Since 33 is less than 98, no swapping needs to be done as they're already in the correct order. So we move on to the next comparison:

<span class="eg">33 <b>98 74</b> 13 55 20 77 45 64 83</span>

This time a swap is required as 98 is greater than 74:

<span class="eg">33 <i>74 98</i> 13 55 20 77 45 64 83</span>

We do the same again, this time starting at the third item:

<span class="eg">33 74 <b>98 13</b> 55 20 77 45 64 83</span>

Again, we need to swap the items since they're not in order:

<span class="eg">33 74 <i>13 98</i> 55 20 77 45 64 83</span>

We repeat this process until we get to the end of the list (marked in green):

<span class="eg">33 74 13 55 20 77 45 64 83 <u>98</u></span>

As was mentioned earlier, the result is that the largest number, <em>98</em> is placed at the end of the list. The next stage of the first iteration requires us to loop in the opposite direction. Since we know that <em>98</em> is in its rightful place, we start at the items immediately to the left:

<span class="eg">33 74 13 55 20 77 45 <b>64 83</b> <u>98</u></span>

We perform the same comparison as normal, and in this case we can see that we don't have to swap the items because 64 is less than 83. We move on to the next pair:

<span class="eg">33 74 13 55 20 77 <b>45 64</b> 83 <u>98</u></span>

Again, we find that a swap is not necessary because 45 is less than 64. Moving down the list again we compare the previous pair:

<span class="eg">33 74 13 55 20 <b>77 45</b> 64 83 <u>98</u></span>

This time we <strong>do</strong> want to swap the items, because 45 is less than 77, and hence the items are in the wrong order.

<span class="eg">33 74 13 55 20 <i>45 77</i> 64 83 <u>98</u></span>

With the swap complete we again move to the previous pair:

<span class="eg">33 74 13 55 <b>20 45</b> 77 64 83 <u>98</u></span>

Again, no swap needed, look at the previous pair:

<span class="eg">33 74 13 <b>55 20</b> 45 77 64 83 <u>98</u></span>

These two are not in the right order, so swap them:

<span class="eg">33 74 13 <i>20 55</i> 45 77 64 83 <u>98</u></span>

With the swap performed, we again move to the previous pair:

<span class="eg">33 74 <b>13 20</b> 55 45 77 64 83 <u>98</u></span>

No swap needed, go to the previous pair:

<span class="eg">33 <b>74 13</b> 20 55 45 77 64 83 <u>98</u></span>

These are out of order, so swap:

<span class="eg">33 <i>13 74</i> 20 55 45 77 64 83 <u>98</u></span>

Finally we look at the last pair in this stage:

<span class="eg"><b>33 13</b> 74 20 55 45 77 64 83 <u>98</u></span>

Again a swap is required:

<span class="eg"><i>13 33</i> 74 20 55 45 77 64 83 <u>98</u></span>

We're now done with the second stage, and as we can see we have the highest and lowest values at the end and start of the set (respectively):

<span class="eg"><u>13</u> 33 74 20 55 45 77 64 83 <u>98</u></span>

So at this point we're ready to iterate again, but we don't want to include the items that have already been sorted because we know they're in the right spot. Here's a short hand demo of both stages of the next iteration (remember, comparisons are in red, swaps are in blue, stores are in green):

<span class="eg"><u>13</u> <b>33 74</b> 20 55 45 77 64 83 <u>98</u></span>
<span class="eg"><u>13</u> 33 <b>74 20</b> 55 45 77 64 83 <u>98</u></span>
<span class="eg"><u>13</u> 33 <i>20 74</i> 55 45 77 64 83 <u>98</u></span>
<span class="eg"><u>13</u> 33 20 <b>74 55</b> 45 77 64 83 <u>98</u></span>
<span class="eg"><u>13</u> 33 20 <i>55 74</i> 45 77 64 83 <u>98</u></span>
<span class="eg"><u>13</u> 33 20 55 <b>74 45</b> 77 64 83 <u>98</u></span>
<span class="eg"><u>13</u> 33 20 55 <i>45 74</i> 77 64 83 <u>98</u></span>
<span class="eg"><u>13</u> 33 20 55 45 <b>74 77</b> 64 83 <u>98</u></span>
<span class="eg"><u>13</u> 33 20 55 45 74 <b>77 64</b> 83 <u>98</u></span>
<span class="eg"><u>13</u> 33 20 55 45 74 <i>77 64</i> 83 <u>98</u></span>
<span class="eg"><u>13</u> 33 20 55 45 74 77 <b>64 83</b> <u>98</u></span>
<span class="eg"><u>13</u> 33 20 55 45 74 <b>77 64</b> <u>83</u> <u>98</u></span>
<span class="eg"><u>13</u> 33 20 55 45 74 <i>64 77</i> <u>83</u> <u>98</u></span>
<span class="eg"><u>13</u> 33 20 55 45 <b>74 64</b> 77 <u>83</u> <u>98</u></span>
<span class="eg"><u>13</u> 33 20 55 45 <i>64 74</i> 77 <u>83</u> <u>98</u></span>
<span class="eg"><u>13</u> 33 20 55 <b>45 64</b> 74 77 <u>83</u> <u>98</u></span>
<span class="eg"><u>13</u> 33 20 <b>55 45</b> 64 74 77 <u>83</u> <u>98</u></span>
<span class="eg"><u>13</u> 33 20 <i>45 55</i> 64 74 77 <u>83</u> <u>98</u></span>
<span class="eg"><u>13</u> 33 <b>20 45</b> 55 64 74 77 <u>83</u> <u>98</u></span>
<span class="eg"><u>13</u> <b>33 20</b> 45 55 64 74 77 <u>83</u> <u>98</u></span>
<span class="eg"><u>13</u> <i>20 33</i> 45 55 64 74 77 <u>83</u> <u>98</u></span>
<span class="eg"><u>13</u> <u>20</u> 33 45 55 64 74 77 <u>83</u> <u>98</u></span>

The process repeats again. But this time as we iterate through, we can see that no swaps are required. The result after the next iterate is simple:

<span class="eg"><u>13</u> <u>20</u> <u>33</u> <u>45</u> <u>55</u> <u>64</u> <u>74</u> <u>77</u> <u>83</u> <u>98</u></span>

As we can see, using this algorithm to sort this particular data set results in less work than when using the Bubble Sort.

<h2>The Implementation</h2>
Here is a sample implementation written in <a href="http://en.wikipedia.org/wiki/C_Sharp" title="C Sharp">C#</a>. It is heavily commented in the hope that it will aid in understanding how the algorithm works:

```
/// <summary>
/// Performs a cocktail sort on an array of integers.
/// </summary>
/// <param name="dataSet">An array of ints to be sorted.</param>
static void CocktailSortBasic(int[] dataSet)
{
    bool swapped = false;
    int start = 0;
    int end = dataSet.Length - 1;

    do
    {
        // make sure we reset the swapped flag on entering
        // the loop, because it might be true from a previous
        // iteration.
        swapped = false;

        // loop from bottom to top just like we do with
        // the bubble sort
        for (int i = start; i < end; ++i)
        {
            if (dataSet[i] > dataSet[i + 1])
            {
                Swap(dataSet, i, i + 1);
                swapped = true;
            }
        }

        // if nothing moved, then we're sorted.
        if (!swapped)
        {
            break;
        }

        // otherwise, reset the swapped flag so that it
        // can be used in the next stage
        swapped = false;

        // move the end point back by one, because we know
        // that the item at the end is in its rightful spot
        --end;

        // this time we loop from top to bottom, doing the
        // same comparison as in the previous stage
        for (int i = end - 1; i >= start; --i)
        {
            if (dataSet[i] > dataSet[i + 1])
            {
                Swap(dataSet, i, i + 1);
                swapped = true;
            }
        }

        // this time we increase the starting point, because
        // the last stage would have moved the next smallest
        // number to its rightful spot.
        ++start;
    } while (swapped);
}
```

<h2>The Complexity</h2>
Both space and time complexity are the same as that of the <a href="/posts/sorting-algorithms-the-bubble-sort/" title="Sorting Algorithms: The Bubble Sort">Bubble Sort</a> for exactly the same reasons. That is, time complexity is O(n<sup>2</sup>), and space complexity for in-place sorting is O(1).

<h2>A Note on Performance</h2>
The Cocktail Sort can actually prove to be faster than the Bubble Sort in a fair few cases. This is due to the fact that we sort in both directions each iteration instead of just one.

Here's an example <em>data set</em> which would require 9 iterations with a Bubble Sort, but only 1 iteration (of two stages) with a Cocktail Sort:

<span class="eg">20 33 45 55 64 74 77 83 98 13</span>

The second stage of the Cocktail Sort would simply move the number <em>13</em> all the way down to the start of the list, at which point the list is then sorted. The Bubble Sort would move the number <em>13</em> left one place for each iteration.

In general, the Cocktail Sort will perform, at worst, the same as the Bubble Sort.

<a name="CocktailSortBitBucket"></a>
<h2>Other Implementations</h2>
I'm slowly gathering a collection of implementations of all the sorting algorithms, including the ones listed above, that I'm covering in this series and posting them up on <a href="http://www.bitbucket.org/OJ/sorting/overview/" title="Sorting @ OJ's BitBucket">BitBucket</a>. The Cocktail Sort implementations can be found <a href="http://www.bitbucket.org/OJ/sorting/src/acf7fe9d7127/02-CocktailSort/" title="CocktailSort @ OJ's BitBucket">here</a>.

<em>Note: You'll need <a href="http://www.selenic.com/mercurial/" title="Mercurial">Mercurial</a> if you want to pull directly from the repository, otherwise you'll have to copy and paste from the web.</em>

<h2>References and Other Information</h2>
<ol>
<li><a href="http://en.wikipedia.org/wiki/Cocktail_sort" title="Cocktail sort">Wikipedia - Cocktail sort</a></li>
</ol>
Next up, we'll be looking at the <strong>Comb Sort</strong>.

<h2>Disclaimer</h2>
I'm no expert, nor an authority. The post above is based on my experience and a bit of research. If you find something wrong with anything I've said please let me know. Comments and feedback are greatly appreciated.

<em>Note: For those reading this article in an RSS reader, you may find the colours do not appear in the examples properly. For some reason the feed is stripping out some of the formatting. I will do my best to fix this up soon.</em>
