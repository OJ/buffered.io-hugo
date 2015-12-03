---
categories:
- Algorithms
- Software Development
- Sorting
comments: true
date: 2008-08-14T00:00:00Z
tags:
- Bubble Sort
- Sorting
title: 'Sorting Algorithms: The Bubble Sort'
---

<img src="/uploads/2008/08/bubbles.jpg" alt="Bubbles" title="Bubbles" width="225" height="154" style="float: left; margin-right: 5px; margin-bottom: 5px" />This is the first of many posts covering the fascinating topic of <a href="/category/sorting/" title="Sorting @ OJ's rants">sorting</a>.

I chose the Bubble Sort algorithm as the first to cover because of its simplicity. This algorithm tends to be the first sorting algorithm that is taught to students, and hence is a rather apt starting point.

Let's break it down.

<!--more-->

<h2>Common Terms</h2>
<ul>
<li><em>Data set</em> - the array or list of items that is to be sorted.</li>
<li><em>n</em> - the number of elements in the <em>data set</em></li>
</ul>

<h2>The Algorithm</h2>
The algorithm consists of a repeated iteration over the elements of the <em>data set</em>. In each iteration, adjacent elements are compared. If those two adjacent items are in the wrong order, they are swapped. The result of each iteration is that the next highest value is placed in the appropriate location in the <em>data set</em>. After repeating the iteration <em>n - 1</em> times, the entire <em>data set</em> will be sorted.

The Bubble Sort fits in the category of <strong>Exchange Sorts</strong> due to the manner in which elements are moved inside the <em>data set</em> during the sorting process.

<h2>The Example</h2>
<div>
<style type="text/css">
span.eg { font-family: Courier new; font-size: 12px; display: block; }
span.eg b { color: Red; }
span.eg u { color: Green; }
span.eg i { color: Blue; }
</style>
</div>
We start with an unsorted <em>data set</em> of 10 elements which we want to sort in <em>ascending</em> order:

<span class="eg">33 98 74 13 55 20 77 45 64 83</span>

The start of the first iteration looks at the first two elements (marked in red):

<span class="eg"><b>33 98</b> 74 13 55 20 77 45 64 83</span>

As per the algorithm we compare the two values, swapping them if the first item is bigger than the second. In this case, 98 is bigger than 33, so no swap is required.

Next we look at the two adjacent items, starting at the second item in the list:

<span class="eg">33 <b>98 74</b> 13 55 20 77 45 64 83</span>

In this case 98 is greater than 74, so the items must be swapped (marked in blue):

<span class="eg">33 <i>74 98</i> 13 55 20 77 45 64 83</span>

We do the same again, this time starting at the third item:

<span class="eg">33 74 <b>98 13</b> 55 20 77 45 64 83</span>

Again, we need to swap the items since they're not in order:

<span class="eg">33 74 <i>13 98</i> 55 20 77 45 64 83</span>

We repeat this process until we get to the end of the list (marked in green):

<span class="eg">33 74 13 55 20 77 45 64 83 <u>98</u></span>

As we can see, the result is that the largest number is placed at the end of the list. This is the end of the first iteration.

We then iterate again but only cover the section of numbers that haven't already been sorted.

<span class="eg"><b>33 74</b> 13 55 20 77 45 64 83 <u>98</u></span>

No swap required, move to the next pair.

<span class="eg">33 <b>74 13</b> 55 20 77 45 64 83 <u>98</u></span>

Swap required:

<span class="eg">33 <i>13 74</i> 55 20 77 45 64 83 <u>98</u></span>

Move to the next pair:

<span class="eg">33 13 <b>74 55</b> 20 77 45 64 83 <u>98</u></span>

Again, swap required:

<span class="eg">33 13 <i>55 74</i> 20 77 45 64 83 <u>98</u></span>

We continue this trend for another iteration before hitting the following case:

<span class="eg">33 13 55 20 <b>74 77</b> 45 64 83 <u>98</u></span>

Here, there is no swap required, so we leave behind the number we've been "carrying" (74) and pick up number 77.

<span class="eg">33 13 55 20 74 <b>77 45</b> 64 83 <u>98</u></span>

The result of the iteration is as follows:

<span class="eg">33 13 55 20 74 45 64 77 <u>83</u> <u>98</u></span>

As we can see, this "bubbles" each of the numbers to the top of the list in the order of highest to lowest. Here is how the rest of the numbers are sorted at the end of each following iteration:

<span class="eg">13 33 20 55 45 64 74 <u>77</u> <u>83</u> <u>98</u></span>
<span class="eg">13 20 33 45 55 64 <u>74</u> <u>77</u> <u>83</u> <u>98</u></span>
<span class="eg">13 20 33 45 55 <u>64</u> <u>74</u> <u>77</u> <u>83</u> <u>98</u></span>
<span class="eg">13 20 33 45 <u>55</u> <u>64</u> <u>74</u> <u>77</u> <u>83</u> <u>98</u></span>
<span class="eg">13 20 33 <u>45</u> <u>55</u> <u>64</u> <u>74</u> <u>77</u> <u>83</u> <u>98</u></span>
<span class="eg">13 20 <u>33</u> <u>45</u> <u>55</u> <u>64</u> <u>74</u> <u>77</u> <u>83</u> <u>98</u></span>
<span class="eg"><u>13</u> <u>20</u> <u>33</u> <u>45</u> <u>55</u> <u>64</u> <u>74</u> <u>77</u> <u>83</u> <u>98</u></span>

Note that the last iteration results in two numbers being locked in due to the fact we no longer have any numbers to sort.

<h2>The Implementation</h2>
Here's a commented sample in <a href="http://en.wikipedia.org/wiki/C_Sharp" title="C Sharp">C#</a> which is easily translatable to many other languages:
```
/// <summary>
/// This is the basic bubble sort algorithm, hard-coded to work
/// with integer values.
/// </summary>
/// <param name="dataSet">Array of integers to sort.</param>
static void BubbleSortBasic(int[] dataSet)
{
    // loop n-1 times.
    for (int i = dataSet.Length - 1; i > 0 ; --i)
    {
        // for each loop, iterate through the first i
        // items (ie. the unsorted ones)
        for (int j = 0; j < i; ++j)
        {
            // if adjacent items need to be swapped
            if (dataSet[j] > dataSet[j + 1])
            {
                // swap them
                Swap(dataSet, j, j + 1);
            }
        }
    }
}
```


A general form of this algorithm which applies to any comparable type can be found in the source repository listed <a href="#BubbleSortBitBucket">below</a>.

<h2>A Minor Optimsation</h2>
The Bubble Sort can be optimised very slightly, though it's not guaranteed to provide much benefit depending on the structure of the <em>data set</em> that is to be sorted.

If, for any iteration, there are no items swapped then all of the items in the <em>data set</em> must be in the correct order. As a result, subsequent iterations are unnecessary. The optimised version is listed below, again in C#:
```
/// <summary>
/// This is the basic bubble sort algorithm, hard-coded to work
/// with integer values but with a slight difference - a minor
/// optimisation.
/// </summary>
/// <param name="dataSet">Array of integers to sort.</param>
static void BubbleSortBasicOptimised(int[] dataSet)
{
    // loop n-1 times.
    for (int i = dataSet.Length - 1; i > 0 ; --i)
    {
        // keep track of whether items were swapped
        // for this iteration
        bool swapped = false;

        // for each loop, iterate through the first i
        // items (ie. the unsorted ones)
        for (int j = 0; j < i; ++j)
        {
            // if adjacent items need to be swapped
            if (dataSet[j] > dataSet[j + 1])
            {
                // swap them
                Swap(dataSet, j, j + 1);

                // indicate that we found a swap
                swapped = true;
            }
        }

        // if nothing was swapped, then we should
        // already have everything in order
        if (!swapped)
        {
            break;
        }
    }
}
```

<em>Caveat</em>: This "optimisation" may actually result in lower performance, particularly in the case where every iteration results in a swap.

<h2>The Complexity</h2>
<h3>Space Complexity</h3>
Bubble Sorts are extremely efficient in terms of memory usage, due to the fact that all sorting and swapping operations are done on the original <em>data set</em>. Regardless of the size of the original <em>data set</em>, the amount of memory overhead is constant as we don't allocate any memory for each of the items. Hence, the space complexity is <strong>O(1)</strong> (in <a href="http://en.wikipedia.org/wiki/Big_O_notation" title="Big-O notation">Big-O notation</a>). This, of course, doesn't include the <strong>O(n)</strong> space complexity taken up by the original <em>data set</em>.

<h3>Time Complexity</h3>
This is where the Bubble Sort fails to shine. For each iteration <em>i</em> starting at <em>n</em>, we must loop through the previous <em>i - 1</em> elements and swap if required.

The first iteration loops through <em>n - 1</em> elements.
The second iteration loops through <em>n - 2</em> elements.
The third iteration loops through <em>n - 3</em> elements.
And so on.. which means the number of compares/swaps that we do is equal to:
(<em>n</em> - 1) + (<em>n</em> - 2) + (<em>n</em> - 3) ... + 2 + 1
This indicates that there <em>n</em> lots of <em>n - i</em>, or <em>n<sup>2</sup> - ni</em> (where <em>ni</em> varies for each iteration).

Give that the highest power of <em>n</em> is <strong>2</strong> this indicates a time complexity of <strong>O(n<sup>2</sup>)</strong>.

Or in layman's terms: it's f**king slow :)

Bubble Sorts should only be used on <em>data sets</em> that are rather small. If you're dealing with medium-sized or larger sets of data, then the Bubble Sort is not the right algorithm to choose. So what would be a better option? You'll have to read the rest of this series and answer that yourself ;)

<a name="BubbleSortBitBucket"></a>
<h2>Other Implementations</h2>
I'm slowly gathering a collection of implementations of all the sorting algorithms, including the ones listed above, that I'm covering in this series and posting them up on <a href="http://www.bitbucket.org/OJ/sorting/overview/" title="Sorting @ OJ's BitBucket">BitBucket</a>. The Bubble Sort implementations can be found <a href="http://www.bitbucket.org/OJ/sorting/src/2ce17136dbac/01-BubbleSort/" title="BubbleSort @ OJ's BitBucket">here</a>.

<em>Note: You'll need <a href="http://www.selenic.com/mercurial/" title="Mercurial">Mercurial</a> if you want to pull directly from the repository, otherwise you'll have to copy and paste from the web.</em>

<h2>Closing Thoughts</h2>
I haven't included information on multithreading because the post would be insanely big. I will put up a follow-up post covering multithreading another day. The code stored in the BitBucket repository contains a sample of how you might use multithreading in conjunction with this sorting algorithm.

To wrap up, Bubble Sorts are easy to understand and are a great place to start when learning sorting algorithms. Unfortunately, this simplicity results in a fairly expensive and slow sorting implementation which really isn't an option when dealing with anything other than small <em>data sets</em>.

<h2>References and Other Information</h2>
<ol>
<li><a href="http://en.wikipedia.org/wiki/Bubble_sort" title="Bubble sort">Wikipedia - Bubble sort</a></li>
</ol>
Next up, we'll be looking at the <a href="/posts/sorting-algorithms-the-cocktail-sort/" title="Sorting Algorithms: The Cocktail Sort">Cocktail Sort</a>.

<h2>Disclaimer</h2>
I'm no expert, nor an authority. The post above is based on my experience and a bit of research. If you find something wrong with anything I've said please let me know. Comments and feedback are greatly appreciated.
