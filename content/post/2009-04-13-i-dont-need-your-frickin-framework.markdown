---
categories:
- Being in the Industry
- Software Development
- WTF
comments: true
date: 2009-04-13T00:00:00Z
tags:
- .net
- bloat
- framework
- overengineering
- programming
title: I Don't Need Your Frickin' Framework!
---

<a href="/uploads/2009/04/scaffold_fail.jpg" title="Scaffold Fail"><img src="/uploads/2009/04/scaffold_fail.jpg" alt="Scaffold Fail" title="Scaffold Fail" width="200" style="float: right; margin-left: 5px; margin-bottom: 5px;" /></a>How many companies have you worked with/for that have their own framework? How many have been in the process of developing their own framework? How many have been in the process of <strong>re</strong>developing their own framework? How many have taken another framework and <del>hacked</del> improved it?

When I attempt to answer the questions listed above, I start to shiver. I feel the need to burn my clothes and take a very long shower.

<!--more-->

In the early days of my career, frameworks were few and far between. These days it is no longer the case. An alarming number of companies are carrying the burden of their own frameworks. A huge number of private and public sector organisations are <em>currently paying people to write new frameworks</em>! For some reason, many nave project managers and team leads feel the need to invest a great deal of time and <abbr title="Cash">moolah</abbr> into building the next-generation, latest-and-greatest, all-singing-all-dancing, cover-every-possible-web-and-rich-client-case framework before they start to do what they're being paid to do: <strong>solve a business problem</strong>.

The last job I worked on was one of those places. On my first day I was told that the project team (which at that point was just 2 of the 6 developers) were building a new framework. They were also building a code generator that used this framework. This dynamic duo of programs would be used in conjunction with each other to build any kind of application under the sun. It would solve all of the issues without the developer having to think, all while making poverty history.

What a joke. The code generator was never finished and the framework was never used. The money that was intended to be burned while <em>adding more features to an existing application</em> disappeared in a blur of <a href="http://en.wikipedia.org/wiki/Greenfield_project" title="">Greenfield</a> over-engineering of a framework that simply <strong>was not required</strong>.

This kind of problem happens so much these days, and it's just appalling. What I want to say is: <em>I am <a href="http://www.urbandictionary.com/define.php?term=frickin" title="">frickin'</a> tired of it!</em>

The example I mentioned above is probably the worst I've seen for a while. I'd love to go into a fair bit of depth to uncover some "interesting" features of the framework, but it's not worth it. I <em>would</em> like to highlight just a few points about it though:
<ul>
  <li>It was based on <a href="http://www.lhotka.net/cslanet/" title="CSLA">CSLA</a>, which is already a rather large framework that attempts to do way too much via an enormous amount of code. The author decided that some parts of CSLA weren't good enough and hence needed to be "improved". The "improvements" were atrocious.</li>
<li>It was supposed to be Open Source, but as far as I'm aware it never made it onto the Web, hence I can't point you at it.</li>
<li>In some areas of code, you were required to invoke functions through a generic <kbd>MessagePortal</kbd> helper object which invoked the required function via reflection after attempting to infer a stack of unnecessary information. This was interesting, as you knew what you needed to call. But you still had to pass in function names as string parameters, hence losing knowledge, and wait for the framework to regain that knowledge by inspecting the code at runtime. Even though the excessive use of reflection was &lt;sarcasm&gt;an obvious win&lt;/sarcasm&gt;, we also copped the added bonus of having another 35 (yes, that's right <strong>35</strong>) function calls appearing in our call-stack.</li>
<li>Those developers who attempted to use it were bitten constantly by esoteric and undocumented pitfalls in the design.</li>
<li>It took around 3 times longer during development to do the "basics" (such as create objects and have them load from a DB). This is compared to using no framework and loading objects manually via ADO.</li>
<li>It was unable to load any form of object graph.</li>
<li>It not only leaked database connections every time you made a database call, it leaked <em>two extra connections</em> during start-up while it attempts to read some database information, such as schema, into memory.</li>
<li>The code generator was written in such a way that the first half of the generator would be used to generate the other half (epic fail)!</li>
<li>The author wasn't able to use it.</li>
</ul>
I hence conclude that this framework was a large, unwieldy, bloated, over-engineered, slow and riddled with design flaws. This is all for something that was actually totally unnecessary. Why? ...

.. because this was <strong>"The Framework" v2.0!</strong> Yup, it was the second version. The first was also a dog's breakfast, but at least it was finished.

So what's my point? Am I trying to say that use of <em>any</em> framework is bad? Having helper libraries and well-designed platforms to build on are an epic fail?

Most definitely not. What I am saying is this: there are people out there with minds immeasurably superior to ours who have already done a better job of solving this problem. Stop writing your own frameworks. Stop writing code generators. There are already options out there.

So to all of you wannabe framework developers: stop what you're doing. You're wasting cash and not adding value. We've already got a framework. It's called <strong>Microsoft .NET</strong>. Now get out of the way and let me use it!
