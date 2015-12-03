---
categories:
- Algorithms
- Series
- Software Development
- Sorting
comments: true
date: 2008-08-13T00:00:00Z
tags:
- Algorithms
- Series
- Sorting
title: Sorting Things Out
---

It's time to recap a topic that is, or should be, close to the heart of every developer. A topic that is often overlooked or glossed over, rarely fully understood and not often discussed. Yet this topic is hugely important.

That topic is <strong>Sorting</strong>.

<!--more-->

<img src="/uploads/2008/08/sorting-beans.jpg" alt="Sorting" title="Sorting" width="168" height="224" style="float: right; margin-left: 5px; margin-bottom: 5px;" />On the surface it appears to be so simple. Just arrange some stuff in a certain order. What could be easier?

Unfortunately the implementations of various methods of sorting can be anything but easy. Determining <em>which</em> algorithm to choose can be difficult enough given that some lend themselves to sorting certain data types better than others do.

As a result, I've decided to start writing a series on the different well-known (and perhaps not-so-well-known) sorting algorithms. For each algorithm, my goal will be to:
<ol>
<li>Paint a very clear picture of how it functions. I aim to do this via pictures and discussion. If I get time, I will aim to provide an animated visualiser of the algorithm (though at the moment this might be a tough ask given time constraints).</li>
<li>Explain the <a href="http://en.wikipedia.org/wiki/Big_O_notation" title="Big O">order of complexity/Big-O notation</a> so that it's clear just how expensive it is to use, along with details of best and worst cases.</li>
<li>Give examples of data sets where the algorithm fits well, and examples of where it doesn't.</li>
<li>Explain if and how the given algorithm can be used in a multhithreaded environment.</li>
<li>Demonstrate errors that are found frequently in various implementations (if there are any), and show how to resolve/avoid them.</li>
</ol>
By the end of the series, I hope that you (and I) will have a really solid understanding of the Sorting world.

To start off with, I'll be covering the age-old <a href="/posts/sorting-algorithms-the-bubble-sort/" title="Sorting Algorithms: The Bubble Sort">Bubble Sort</a>. I'm choosing this because it's very simple, and is an easy target for me to get going.

As always, comments and feedback will be greatly appreciated. I'm hoping that I'll learn more from this exercise than you guys will.
