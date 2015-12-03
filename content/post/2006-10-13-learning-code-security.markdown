---
categories:
- Being in the Industry
- Security
- Software Development
comments: true
date: 2006-10-13T00:00:00Z
title: Learning Code Security
---

As a regular read of Scott Gu's blog (see blogroll) I often find nuggets of information that are handy for the work that I do, but I also often end up with a few questions :)

The latest one that fired up a bit of thought was his post on <a href="http://weblogs.asp.net/scottgu/archive/posts/Tip_2F00_Trick_3A00_-Guard-Against-SQL-Injection-Attacks.aspx" title="Guard Against SQL Injection Attacks">guarding against SQL injection attacks</a>.  The information posted very handy, and is something that I would assume most web developers already know, but it made me wonder how many devs out there <em>are</em> actually aware of these kinds of issues while they're building their applications.

I starting hacking code together from a young age, and I've written my fair share of code that I hope to God never made it onto the web :) I'd like to think that over the time that I've spent reading, writing and working I've gained a pretty good coverage of the code security issues that are faced when building all kinds of applications - though I'm sure I have a stack more to learn! One thing struck me though, and that was that almost none of this stuff was covered during my course of formal study at University.

I transferred to different Unis during my time as a student, and out of the 3 that I went to, <strong>none</strong> of them had any form of code security as part of the core syllabus. Sure, there were special subjects that you could take which focussed on things such as this, including SQL injection, buffer overflows, etc, but you actually had to <em>choose the subject</em> out of a stack of others to get a good amount of exposure to the principles.

As time goes by, it becomes harder and harder for the developer to get themselves into trouble when writing code due to the nature of the languages and the support that you get via the accompanying frameworks - but we do manage to find new and startling ways of creating holes in our softy that the malicious and crafty can exploit.

So I do think that learning at least the basics of code security (particularly in web-based environments) is something that every developer should do.  Sure, if you're using C# you might not have to worry about buffer overflows. If you're not using an SQL back-end, you won't have to worry about SQL injection. Regardless of the application and language, there are always different ways in which you can slip up. Coverage should be <strong>mandatory</strong> in courses at any formal education centre so that budding developers are aware of those issues before they hit the streets. To me, this is as obvious as having English and Maths as mandatory subjects during school if you're going to work as a coder!
