---
categories:
- Algorithms
- Software Development
- Sorting
comments: true
date: 2008-09-14T00:00:00Z
tags:
- Comb Sort
- Sorting
title: 'Sorting Algorithms: The Comb Sort'
---

<img src="/uploads/2008/09/combsort.jpg" alt="Comb Sort" title="Comb Sort" width="150" style="float: left; margin-right: 5px; margin-bottom: 5px;" />Welcome to this, the third post in the series on <a href="/category/sorting/" title="Sorting @ OJ's rants">sorting</a> algorithms. Next up, we're going to cover the <strong>Comb Sort</strong>.

Like the <a href="/posts/sorting-algorithms-the-cocktail-sort/" title="Sorting Algorithms: The Cocktail Sort">Cocktail Sort</a>, the Comb Sort isn't particularly well known. Most people manage to make their way through tertiary studies without ever hearing of it. This post is designed to change that!

The Comb Sort is another <em>comparison</em> and <em>exchange</em> sort which builds on the idea of the <a href="/posts/sorting-algorithms-the-bubble-sort/" title="Sorting Algorithms: The Bubble Sort">Bubble Sort</a> and adds a potential optimisation or two.

Make sure you read the articles on the <a href="/posts/sorting-algorithms-the-bubble-sort/" title="Sorting Algorithms: The Bubble Sort">Bubble Sort</a> and the <a href="/posts/sorting-algorithms-the-cocktail-sort/" title="Sorting Algorithms: The Cocktail Sort">Cocktail Sort</a> before you read this article. Doing so will make it much easier to understand.

<!--more-->

Right, enough with the introductions, let's break it down!

<h2>Common Terms</h2>
<ul>
<li><em>Data set</em> - the array or list of items that is to be sorted.</li>
<li><em>n</em> - the number of elements in the <em>data set</em>.</li>
<li><em>Turtle</em> - a name given to small numbers that appear towards the end of a <em>data set</em>. When used in a <strong>Bubble Sort</strong>, small numbers at the end of the set take a very long time to get to the front of the list, and hence are called <em>turtles</em> because of the lack of speed.</li>
<li><em>Hare</em> - a name given to large numbers that appear towards the start of a <em>data set</em>. When used in a <strong>Bubble Sort</strong>, large numbers move to the end of the set very quickly, and hence are called <em>hares</em> due to their high speed.</li>
<li><em>Killing the turtles</em> - a phrase that implies quickly moving <em>turtles</em> to the front of the <em>data set</em>.
<li><em>Gap</em> - the size of the gap between areas of the <em>data set</em> that are being sorted during a given iteration of the sort.
<li><em>Shrink Factor</em> - a factor which is applied to the <em>gap</em> prior to each iteration to reduce its size.
</ul>

<h2>A Bit of Background Information</h2>
The <strong>Comb Sort</strong>, along with the <strong>Cocktail Sort</strong>, was developed as an improvement to the <strong>Bubble Sort</strong> using the idea of <em>killing the turtles</em>. <em>Turtles</em> drastically reduce the efficiency of the <strong>Bubble Sort</strong> and hence are an obvious place to attempt optimisation.

The Bubble Sort algorithm always compares values that are adjacent to each other in the <em>data set</em>. The Comb Sort improves on this by adding a <em>gap</em> which allows non-adjacent numbers to be compared. After each iteration, the <em>gap</em> is reduced by a <em>shrink factor</em> until it reaches the value of <strong>1</strong>. Hence, at the very end, the Comb Sort behaves exactly like the Bubble Sort.

One interesting thing to note about this algorithm is that it's almost as fast as a Quick Sort!

<h2>The Algorithm</h2>
Each iteration of the algorithm consists of three stages:<ol><li>Calculation of the <em>gap</em> value.</li><li>Iterating over the <em>data set</em> comparing each item with the item that is <em>"gap"</em> elements further down the list and swapping them if required.</li><li>Checking to see if the gap value has reached one and no swaps have occurred. If so, then the set has been sorted.</li></ol>

<h3>1. Calculating the "gap"</h3>
The <em>gap</em>, which is essentially an offset used when comparing elements, is something that changes for each iteration. The value needs to change in a uniform manner in such a way as to provide optimum comparisons during the sorting phases.

Stephen Lacey and Richard Box, who popularised the algorithm, suggested that in each iteration the <em>gap</em> should be reduced by a factor of approximately 1.3 (see the <a href="http://en.wikipedia.org/wiki/Comb_sort" title="Comb sort @ Wikipedia">wikipedia article</a> for more information). Hence, calculation of the <em>gap</em> is as simple as starting with the size of the <em>data set</em> and dividing by a <em>shrink factor</em> of 1.3 each iteration.

<h3>2. Iterating and Swapping</h3>
This phase is the same as the Bubble Sort. The only difference in the Comb Sort is that the items which are compared are <em>i</em> and <em>i + gap</em> as opposed to <em>i</em> and <em>i + 1</em>.

For each iteration, we start at the beginning of the <em>data set</em> and loop until <em>i + gap</em> is greater-than or equal to the size of the <em>data </em>. For each sub-iteration in this loop, we compare <em>i</em> and <em>i + gap</em> and swap the values if required.

At this point the <em>gap is recalculated</em> and we go again.

<h3>3. Terminating the Loop</h3>
When the <em>gap</em> value reaches one, we know that we're now behaving the same as the Bubble Sort algorithm (since we're comparing <em>i</em> and <em>i + 1</em>), so we know that if we don't perform any swaps during the iteration then the set must be sorted.

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

We start by setting our <em>gap</em> value to 10, which is the size of the set. We then shrink the gap by the <em>shrink factor</em>, 1.3, which results in the value of 7 (when rounded down).

The first iteration starts at the beginning of the set, comparing the 1st item with the item that is "<em>gap</em>" elements further up the set (the 8th item). These two items are shown in red:

<span class="eg"><b>33</b> 98 74 13 55 20 77 <b>45</b> 64 83</span>

33 is less than 45, so no swap is required. We then move to the 2nd item, and compare that item with the 9th item:

<span class="eg">33 <b>98</b> 74 13 55 20 77 45 <b>64</b> 83</span>

We perform a swap at this point because 98 is bigger than 64. The swap is shown in blue:

<span class="eg">33 <i>64</i> 74 13 55 20 77 45 <i>98</i> 83</span>

We then move on to compare the 3rd and 10th elements:

<span class="eg">33 64 <b>74</b> 13 55 20 77 45 98 <b>83</b></span>

Again, no swap is required here as the numbers are in the right order.

At this point we have completed our first iteration as we've reached the end of the set. Given that the <em>gap</em> value is not 1, we know we still have more iterations to go. So we need to recalculate our <em>gap</em> value by dividing it again by the <em>shrink factor</em>, resulting in a value of 5 (after rounding down).

We then start another iteration from the beginning of the set, using the <em>gap</em> value of 5 as the offset. We start with the 1st and 6th items:

<span class="eg"><b>33</b> 64 74 13 55 <b>20</b> 77 45 98 83</span>

Given 33 is bigger than 20, we swap:

<span class="eg"><i>20</i> 64 74 13 55 <i>33</i> 77 45 98 83</span>

Then we compare the 2nd and 7th items:

<span class="eg">20 <b>64</b> 74 13 55 33 <b>77</b> 45 98 83</span>

No swap required, move to the 3rd and 8th items:

<span class="eg">20 64 <b>74</b> 13 55 33 77 <b>45</b> 98 83</span>

Perform the swap since 74 is bigger than 45:

<span class="eg">20 64 <i>45</i> 13 55 33 77 <i>74</i> 98 83</span>

We then compare the 4th and 9th items:

<span class="eg">20 64 45 <b>13</b> 55 33 77 74 <b>98</b> 83</span>

No swap required, compare the 5th and 10th items:

<span class="eg">20 64 45 13 <b>55</b> 33 77 74 98 <b>83</b></span>

Again, no swap required. We've hit the end of this next iteration, and since <em>gap</em> doesn't equal 1, we recalculate (resulting in the value of 3) and go again.

This time I'll just show a summary of the steps:

<span class="eg"><b>20</b> 64 45 <b>13</b> 55 33 77 74 98 83</span>
<span class="eg"><i>13</i> 64 45 <i>20</i> 55 33 77 74 98 83</span>
<span class="eg">13 <b>64</b> 45 20 <b>55</b> 33 77 74 98 83</span>
<span class="eg">13 <i>55</i> 45 20 <i>64</i> 33 77 74 98 83</span>
<span class="eg">13 55 <b>45</b> 20 64 <b>33</b> 77 74 98 83</span>
<span class="eg">13 55 <i>33</i> 20 64 <i>45</i> 77 74 98 83</span>
<span class="eg">13 55 33 <b>20</b> 64 45 <b>77</b> 74 98 83</span>
<span class="eg">13 55 33 20 <b>64</b> 45 77 <b>74</b> 98 83</span>
<span class="eg">13 55 33 20 64 <b>45</b> 77 74 <b>98</b> 83</span>
<span class="eg">13 55 33 20 64 45 <b>77</b> 74 98 <b>83</b></span>

Again, we iterate the value of <em>gap</em>, which becomes 2, and we go again:

<span class="eg"><b>13</b> 55 <b>33</b> 20 64 45 77 74 98 83</span>
<span class="eg">13 <b>55</b> 33 <b>20</b> 64 45 77 74 98 83</span>
<span class="eg">13 <i>20</i> 33 <i>55</i> 64 45 77 74 98 83</span>
<span class="eg">13 20 <b>33</b> 55 <b>64</b> 45 77 74 98 83</span>
<span class="eg">13 20 33 <b>55</b> 64 <b>45</b> 77 74 98 83</span>
<span class="eg">13 20 33 <i>45</i> 64 <i>55</i> 77 74 98 83</span>
<span class="eg">13 20 33 45 <b>64</b> 55 <b>77</b> 74 98 83</span>
<span class="eg">13 20 33 45 64 <b>55</b> 77 <b>74</b> 98 83</span>
<span class="eg">13 20 33 45 64 55 <b>77</b> 74 <b>98</b> 83</span>
<span class="eg">13 20 33 45 64 55 77 <b>74</b> 98 <b>83</b></span>

Finally, we reduce the value of <em>gap</em> one more time, resulting in the value of 1. We're finally at the <strong>Bubble Sort</strong> stage, and hence the last iteration looks like this:

<span class="eg"><b>13</b> <b>20</b> 33 45 64 55 77 74 98 83</span>
<span class="eg">13 <b>20</b> <b>33</b> 45 64 55 77 74 98 83</span>
<span class="eg">13 20 <b>33</b> <b>45</b> 64 55 77 74 98 83</span>
<span class="eg">13 20 33 <b>45</b> <b>64</b> 55 77 74 98 83</span>
<span class="eg">13 20 33 45 <b>64</b> <b>55</b> 77 74 98 83</span>
<span class="eg">13 20 33 45 <i>55</i> <i>64</i> 77 74 98 83</span>
<span class="eg">13 20 33 45 55 <b>64</b> <b>77</b> 74 98 83</span>
<span class="eg">13 20 33 45 55 64 <b>77</b> <b>74</b> 98 83</span>
<span class="eg">13 20 33 45 55 64 <i>74</i> <i>77</i> 98 83</span>
<span class="eg">13 20 33 45 55 64 74 77 <b>98</b> <b>83</b></span>
<span class="eg">13 20 33 45 55 64 74 77 <i>83</i> <i>98</i></span>
<span class="eg"><u>13</u> <u>20</u> <u>33</u> <u>45</u> <u>55</u> <u>64</u> <u>74</u> <u>77</u> <u>83</u> <u>98</u></span>

At this point we know, due to observation, that the list is sorted. Unfortunately, the algorithm is yet to know. It looks at the value of <em>gap</em> and sees that it is set to 1, but during the last iteration there were swaps -- so the code will iterate again.

At the end of this iteration, given that there were no swaps, it will know that the list is sorted and the code will terminate.

<h2>The Implementation</h2>
Here is a sample implementation written in <a href="http://en.wikipedia.org/wiki/C_Sharp" title="C Sharp">C#</a>. It is heavily commented in the hope that it will aid in understanding how the algorithm works:

```
/// <summary>
/// The implementation of the standard comb sort algorithm which sorts
/// an array of integers.
/// </summary>
/// <param name="dataSet">Reference to the dataset that is to be soroted.</param>
static void CombSort(int[] dataSet)
{
    // start by using the length/size of the set as the gap.
    int gap = dataSet.Length;

    // loop indefinately.
    while (true)
    {
        // update the gap value so that it shrinks
        // towards 1.
        if (gap > 1)
        {
            gap = (int)((double)gap / SHRINK_FACTOR);
        }

        // do the comb sort with the current gap
        bool swapped = false;
        for (int i = 0; i + gap < dataSet.Length; ++i)
        {
            if (dataSet[i] > dataSet[i + gap])
            {
                Swap(dataSet, i, i + gap);
                swapped = true;
            }
        }

        // if we're down to a gap of 1, and we haven't swapped
        // anything, then we're sorted.
        if (gap == 1 && !swapped)
        {
            break;
        }
    }
}
```


<h2>The Complexity</h2>
This is quite surprising. Despite being based on the idea of a <a href="/posts/sorting-algorithms-the-bubble-sort/" title="Sorting Algorithms: The Bubble Sort">Bubble Sort</a> the time complexity is just O(n log n), and space complexity for in-place sorting is O(1). Amazing for such a simple sorting algorithm!

<h2>A Note on Performance</h2>
What I'm about to say is almost a direct rip-off of Wikipedia ;) But at least I'm being honest!

When you use a <em>shrink factor</em> of 1.3, as recommended, it turns out that there are only 3 ways for the gap sizes to reduce to 1. They are:

<span class="eg">1. - 9, 6, 4, 3, 2, 1</span>
<span class="eg">2. - 10, 7, 5, 3, 2, 1</span>
<span class="eg">3. - 11, 8, 6, 4, 3, 2, 1</span>

According to Wikipedia, it turns out that the 3rd option is the only one which manages to <em>kill all turtles</em> before the <em>gap</em> becomes 1. I haven't been able to find anything that backs this up though.

Assuming that this is true, we can create a modified version of the <strong>Comb Sort</strong> which has this extra little check:
```
if(gap == 9 || gap == 10)
{
    gap = 11;
}
```

This is a little hack which makes sure that we get the third option before we reduce the <em>gap</em> to 1. Of course, this would only have an effect on sets of data which have more than 11 elements.

<a name="CombSortBitBucket"></a>
<h2>Other Implementations</h2>
I'm slowly gathering a collection of implementations of all the sorting algorithms, including the ones listed above, that I'm covering in this series and posting them up on <a href="http://www.bitbucket.org/OJ/sorting/overview/" title="Sorting @ OJ's BitBucket">BitBucket</a>. The Comb Sort implementations can be found <a href="http://www.bitbucket.org/OJ/sorting/src/b26f42ecdbe0/03-CombSort/" title="CombSort @ OJ's BitBucket">here</a>.

<em>Note: You'll need <a href="http://www.selenic.com/mercurial/" title="Mercurial">Mercurial</a> if you want to pull directly from the repository, otherwise you'll have to copy and paste from the web.</em>

<h2>References and Other Information</h2>
<ol>
<li><a href="http://en.wikipedia.org/wiki/Comb_sort" title="Comb sort">Wikipedia - Comb sort</a></li>
</ol>
Next up, we'll be looking at the <a href="/posts/sorting-algorithms-the-gnome-sort/" title="Sorting Algorithms: The Gnome Sort">Gnome Sort</a>.

<h2>Disclaimer</h2>
I'm no expert, nor an authority. The post above is based on my experience and a bit of research. If you find something wrong with anything I've said please let me know. Comments and feedback are greatly appreciated.

<em>Note: For those reading this article in an RSS reader, you may find the colours do not appear in the examples properly. For some reason the feed is stripping out some of the formatting. I will do my best to fix this up soon.</em>
