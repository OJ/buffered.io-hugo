---
categories:
- Algorithms
- Software Development
- Sorting
comments: true
date: 2008-10-16T00:00:00Z
tags:
- Gnome Sort
- Sorting
title: 'Sorting Algorithms: The Gnome Sort'
---

<img src="/uploads/2008/10/gnome.jpg" alt="Gnome Sort" title="Gnome Sort" width="240" style="float: right; margin-left: 5px; margin-bottom: 5px;"/>After a bit of down time for personal reasons, here is the fourth post in the series on <a href="/category/sorting/" title="Sorting @ OJ's rants">sorting</a> algorithms. This time round we're taking a good look at the <strong>Gnome Sort</strong>.

People often do a double-take when hearing the term "Gnome Sort" because it's not that common. The <strong>Gnome Sort</strong> is extremely simple, and is very similar to the principle behind the <strong>Insertion Sort</strong> (which we'll be covering soon).

The Gnome Sort is yet another <em>comparison</em> and <em>exchange</em> sort which has elements that are similar to the <a href="/posts/sorting-algorithms-the-bubble-sort/" title="Sorting Algorithms: The Bubble Sort">Bubble Sort</a>. If you haven't read up on the Bubble Sort, be sure to do that before reading this article as it may help with your understanding.

<!--more-->

Let's get straight into it!

<h2>Common Terms</h2>
<ul>
<li><em>Data set</em> - the array or list of items that is to be sorted.</li>
<li><em>n</em> - the number of elements in the <em>data set</em>.</li>
<li><em>Bubble</em> - perform a <strong>Bubble Sort-like</strong> shuffle of an item in a given direction.</li>
</ul>

<h2>The Algorithm</h2>
The first principle to bear in mind when doing a <strong>Gnome Sort</strong> is that as the algorithm progresses, all items to the left of the <em>current</em> item are always in order. Each iteration of the algorithm moves the current item to the correct spot with respect to the previously sorted items.

The first part of the algorithm looks at the first two items in the <em>data set</em> and swaps them around if they are in the wrong order. Then, for each iteration thereafter, the next item is <em>bubbled backwards</em> towards to head of the list until a value is encountered which is less than the current item (or the head of the list is reached). At this point, the current item is left alone, and the sort continues from the next item.

<h2>The Example</h2>
<div>
<style type="text/css">
span.eg { font-family: Courier new; font-size: 12px; display: block; }
span.eg b { color: Red; }
span.eg u { color: Green; }
span.eg i { color: Blue; }
</style>
</div>

As per usual we will use the same initial <em>data set</em> that we used for the Bubble Sort and Cocktail Sort to aid in highlighting the differences.

The initial set looks like this:

<span class="eg">33 98 74 13 55 20 77 45 64 83</span>

The first step is to compare the firs two values:

<span class="eg"><b>33</b> <b>98</b> 74 13 55 20 77 45 64 83</span>

Given that they are in the correct order we leave this two values alone. We can say that two items in the <em>data set</em> are <strong>in order</strong>.

Next we look at the item immediately after the <strong>in order</strong> items and compare that to the last sorted item:

<span class="eg">33 <b>98</b> <b>74</b> 13 55 20 77 45 64 83</span>

We can see that these two are not in the correct order, so we swap them:

<span class="eg">33 <i>74</i> <i>98</i> 13 55 20 77 45 64 83</span>

Given that we had to swap the values in the previous step, we can't assume that the current item is in the right location with respect to the other <strong>in order</strong> items, and hence we need to continue to work backwards. We compare the current item to the preceeding item in the <em>data set</em>:

<span class="eg"><b>33</b> <b>74</b> 98 13 55 20 77 45 64 83</span>

These two values are in the correct order, so we can now stop this iteration. At this point, the first three items in the set are sorted with respect to each other. Next, we move to the fourth item, and compare that with the last of the <strong>in order</strong> items:

<span class="eg">33 74 <b>98</b> <b>13</b> 55 20 77 45 64 83</span>

They're out of order, so we swap them:

<span class="eg">33 74 <i>13</i> <i>98</i> 55 20 77 45 64 83</span>

... and we keep <em>bubbling backwards</em> until we reach either a value that is smaller or the head of the list:

<span class="eg">33 <b>74</b> <b>13</b> 98 55 20 77 45 64 83</span>
<span class="eg">33 <i>13</i> <i>74</i> 98 55 20 77 45 64 83</span>
<span class="eg"><b>33</b> <b>13</b> 74 98 55 20 77 45 64 83</span>
<span class="eg"><i>13</i> <i>33</i> 74 98 55 20 77 45 64 83</span>

Here we can see that 13 was the smallest number, so it was bubbled all the way to the head of the list. We then move on to the next iteration where we again compare the next item with the head of the list, and again <em>bubble backwards</em> until we reach the correct location. Here is a summary of the steps for the next iteration:

<span class="eg">13 33 74 <b>98</b> <b>55</b> 20 77 45 64 83</span>
<span class="eg">13 33 74 <i>55</i> <i>98</i> 20 77 45 64 83</span>
<span class="eg">13 33 <b>74</b> <b>55</b> 98 20 77 45 64 83</span>
<span class="eg">13 33 <i>55</i> <i>74</i> 98 20 77 45 64 83</span>
<span class="eg">13 <b>33</b> <b>55</b> 74 98 20 77 45 64 83</span>

55 is now in the correct spot with regards to the other pre-sorted items, so we move onto the next item. At this point it should be fairly obvious what's going on, so I shall show a summary for the rest of the example:

<span class="eg">13 33 55 74 <b>98</b> <b>20</b> 77 45 64 83</span>
<span class="eg">13 33 55 74 <i>20</i> <i>98</i> 77 45 64 83</span>
<span class="eg">13 33 55 <b>74</b> <b>20</b> 98 77 45 64 83</span>
<span class="eg">13 33 55 <i>20</i> <i>74</i> 98 77 45 64 83</span>
<span class="eg">13 33 <b>55</b> <b>20</b> 74 98 77 45 64 83</span>
<span class="eg">13 33 <i>20</i> <i>55</i> 74 98 77 45 64 83</span>
<span class="eg">13 <b>33</b> <b>20</b> 55 74 98 77 45 64 83</span>
<span class="eg">13 <i>20</i> <i>33</i> 55 74 98 77 45 64 83</span>
<span class="eg"><b>13</b> <b>20</b> 33 55 74 98 77 45 64 83</span>
<span class="eg">13 20 33 55 74 <b>98</b> <b>77</b> 45 64 83</span>
<span class="eg">13 20 33 55 74 <i>77</i> <i>98</i> 45 64 83</span>
<span class="eg">13 20 33 55 <b>74</b> <b>77</b> 98 45 64 83</span>
<span class="eg">13 20 33 55 74 77 <b>98</b> <b>45</b> 64 83</span>
<span class="eg">13 20 33 55 74 77 <i>45</i> <i>98</i> 64 83</span>
<span class="eg">13 20 33 55 74 <b>77</b> <b>45</b> 98 64 83</span>
<span class="eg">13 20 33 55 74 <i>45</i> <i>77</i> 98 64 83</span>
<span class="eg">13 20 33 55 <b>74</b> <b>45</b> 77 98 64 83</span>
<span class="eg">13 20 33 55 <i>45</i> <i>74</i> 77 98 64 83</span>
<span class="eg">13 20 33 <b>55</b> <b>45</b> 74 77 98 64 83</span>
<span class="eg">13 20 33 <i>45</i> <i>55</i> 74 77 98 64 83</span>
<span class="eg">13 20 <b>33</b> <b>45</b> 55 74 77 98 64 83</span>
<span class="eg">13 20 33 45 55 74 77 <b>98</b> <b>64</b> 83</span>
<span class="eg">13 20 33 45 55 74 77 <i>64</i> <i>98</i> 83</span>
<span class="eg">13 20 33 45 55 74 <b>77</b> <b>64</b> 98 83</span>
<span class="eg">13 20 33 45 55 74 <i>64</i> <i>77</i> 98 83</span>
<span class="eg">13 20 33 45 55 <b>74</b> <b>64</b> 77 98 83</span>
<span class="eg">13 20 33 45 55 <i>64</i> <i>74</i> 77 98 83</span>
<span class="eg">13 20 33 45 <b>55</b> <b>64</b> 74 77 98 83</span>
<span class="eg">13 20 33 45 55 64 74 77 <b>98</b> <b>83</b></span>
<span class="eg">13 20 33 45 55 64 74 77 <i>83</i> <i>98</i></span>
<span class="eg">13 20 33 45 55 64 74 <b>77</b> <b>83</b> 98</span>
<span class="eg"><u>13</u> <u>20</u> <u>33</u> <u>45</u> <u>55</u> <u>64</u> <u>74</u> <u>77</u> <u>83</u> <u>98</u></span>

Note that we do not know for sure that an item is in the correct location until the entire set has been sorted.

<h2>The Implementation</h2>
Here is a sample implementation written in <a href="http://en.wikipedia.org/wiki/C_Sharp" title="C Sharp">C#</a>. It is heavily commented in the hope that it will aid in understanding how the algorithm works:

```
/// <summary>
/// Example implementation of the Gnome Sort algorithm.
/// </summary>
/// <param name="dataSet">Array of items to be sorted.</param>
static void GnomeSort(int[] dataSet)
{
    // start at the second item
    int i = 1;

    // keep track of the 'next' item so we
    // can jump to it when the current iteration
    // is finished.
    int j = 2;

    // loop until we reach the end of the data set
    while (i < dataSet.Length)
    {
        // swap if required
        if (dataSet[i - 1] > dataSet[i])
        {
            Swap(dataSet, i - 1, i);

            // move backwards through the sorted items
            // until we find the rightful spot.
            --i;

            // stop at the head of the list
            if (i == 0)
            {
                // jump to the next item in the set
                i = j++;
            }
        }
        else
        {
            // jump to the next item in the set
            i = j++;
        }
    }
}
```

<h2>The Complexity</h2>
This is not a very efficient algorithm. It's time and space complexity are exactly that of the <a href="/posts/sorting-algorithms-the-bubble-sort/" title="Sorting Algorithms: The Bubble Sort">Bubble Sort</a>. That is, the time complexity is O(n<sup>2</sup>), and space complexity for in-place sorting is O(1).

<a name="GnomeSortBitBucket"></a>
<h2>Other Implementations</h2>
I'm slowly gathering a collection of implementations of all the sorting algorithms, including the ones listed above, that I'm covering in this series and posting them up on <a href="http://www.bitbucket.org/OJ/sorting/overview/" title="Sorting @ OJ's BitBucket">BitBucket</a>. The Gnome Sort implementations can be found <a href=http://www.bitbucket.org/OJ/sorting/src/3f2af8511799/04-GnomeSort/" title="GnomeSort @ OJ's BitBucket">here</a>.

<em>Note: You'll need <a href="http://www.selenic.com/mercurial/" title="Mercurial">Mercurial</a> if you want to pull directly from the repository, otherwise you'll have to copy and paste from the web.</em>

<h2>References and Other Information</h2>
<ol>
<li><a href="http://en.wikipedia.org/wiki/Gnome_sort" title="Gnome sort">Wikipedia - Gnome sort</a></li>
</ol>
Next up, we'll be looking at the first of the mind-bending algorithms: the <strong>Quick Sort</strong>.

<h2>Disclaimer</h2>
I'm no expert, nor an authority. The post above is based on my experience and a bit of research. If you find something wrong with anything I've said please let me know. Comments and feedback are greatly appreciated.

<em>Note: For those reading this article in an RSS reader, you may find the colours do not appear in the examples properly. For some reason the feed is stripping out some of the formatting. I will do my best to fix this up soon.</em>
