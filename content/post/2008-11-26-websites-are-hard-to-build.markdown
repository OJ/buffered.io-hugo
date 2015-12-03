---
categories:
- Databases
- Security
- Software
- Software Development
comments: true
date: 2008-11-26T00:00:00Z
tags:
- misconception
- Software
- Software Development
- web development
- websites
title: Websites are Hard to Build
---

{% img right /uploads/2008/11/elope-harlequin-jester.jpg 'Uninformed client' %}
> "It's just a small site, how hard can it be?"
> 
> "I thought you could do it for me as a favour. It's not a complicated site."
> 
> "This would take me an afternoon, but I don't have time, can you do it for me?"

Have you ever heard these comments before? Have you heard ones that are similar? I've been hearing them a lot in the last few weeks and it's starting to get to me.

I have just had a chat with a mate who is also suffering this pain, and that conversation is what inspired me to write this, the first random rant in a while.

<!--more-->

Let me start by saying that _building software is **hard**_. Building _quality_ software is **harder**. I've been in the industry for 10 years now, and I can say with a certain level of confidence (and perhaps a little bit of authority) that it's harder than most people think -- particularly business users, clients and lots of family and friends.

Let me follow that on by saying that websites are software (no way!). That puts them in the "hard" basket. Every site, even if it's a static page with some simple content, comes with its own set of challenges -- and those challenges are rarely understood by anyone who hasn't experienced the pain of building software.

The point of this post it to give a little bit of exposure to some of these issues. I hope that this post gets read by those people who have plans to have some sort of software built but haven't really got a clue what's involved.

Requirement Issues
------------------
Let's be fair ... you don't know what you want. You _think_ you do, but you really don't. You have an idea for something that will either make your life easier or "be really cool", but as far as actually **thinking** about how it would function ... ? You haven't done that at all.

Here lies the first of the big stumbling blocks. You want a website, because "everyone has a website these days" for almost everything. Started a new business? You must have a website! Released an album? You must have a website! Had a child? You must have a website! Never had a website? You must have a website!

Before you dive headlong into the quest to find the person to build the site for you, have a good long think about what it is you want that site to be. What is its purpose? What content is required? Will it be dynamic? Will it be static? Will it be data-driven and require a <a href="http://en.wikipedia.org/wiki/Content_management_system" title="Content Management System">Content Management System</a>? Do you want other people to submit content? Do you want to have the facility available for readers to add their own comments? Will it be product driven and need the ability for people to buy stuff directly from the site? Do you need it to work on all browsers? Does it require access via a mobile phone or hand-held gadget?

These questions need to be answered well before any design is even contemplated, let alone structure and technical architecture. Do yourself and your site development team a favour. Think long and hard about what you want, and while you're doing that, ponder what possible issues you might face. The more issues you think of now, the cheaper it will be to build the site that fits your needs. Not just that, but you'll reduce the "scope on a rope" which results in the blowout of timelines and budgets.

Design Issues
-------------
We're coming to the end of 2008 and yet people still don't realise that development is **not the same as design**. Ask yourself the question: if design is part of development, then why do people make a living out of website design? The answer is simple: It's a big job, and it's very hard to do it right. I'm a developer, and I feel that there are very few (if any) areas of development that I couldn't work in, but I can tell you this for certain: I am **not a designer**. <a href="http://shiftperception.com/blog/" title="Shiftperception">Dan</a> will no doubt appreciate that.

It's true, I am not a designer. I can not design websites. I can do the technical desing, the architecture and the implementation. I can not do the design. Creating the right design for a website isn't an easy thing to do. I can tell you when people get it right, and I can tell you when people get it wrong, I just can't tell you why.

So, again, do yourself a favour. Get a proper web designer with a great portfolio of projects and go through the ideas you have with them. They will end up providing you an interface which makes sense for what you're trying to do. Do not fall into the trap of assuming that this part of the process isn't important. Yes, up front the cost most appear higher, but in the long run you will not regret it.

Technical Issues
----------------
Stumbling block number 3 is the lack of understanding of the technical issues. It's no surprise though, considering that most people wanting sites are not technical in nature. While it's fine for a client to not have (or want) an understanding of the technical issues, it's certainly _not_ fine for them to ignore the fact that they even exist. Sticking your head in the sand and saying things like "well you'll figure it out" or "isn't that what I'm paying you for" doesn't really cut the mustard, particularly when you haven't thought about what you want in the first place.

When building websites there are all kinds of technical hurdles to leap over. To name a few...

* **Platform** - I think most of the non-techie readers will have no clue of what I mean here. The platform is what the site runs on. That is, the operating system, the runtime, the software that supports it, etc.
* **Cross-browser compatibility** - The biggest issue in any Internet-facing website is cross-browser issues. Not all browsers were made equally. Each and every one has its own set of quirks, and the people responsible for building the interface to the site need to cater for all these quirks. If they don't, then they may alienate a subset of potential readers which would do damage to the site's potential traffic and income. **DO NOT** underestimate how painful this can be. Yes, there are tools out there which apparently take care of these issues for you, but let me tell you that most of the time they fail too. Be prepared for a great deal of work to go into making your site look and behave on the same on a variety of browsers. As a small side-note, some browsers actually behave differently on, say, Windows compared to how they would on, say, Mac OSX.
* **Secure payments** - Sounds easy right? "I just want the ability for people to enter credit card details and pay for stuff." Unfortunately it isn't that simple. You have all kinds of issues to worry about if you want that to happen directly in your site. You have to set up <a href="http://en.wikipedia.org/wiki/Transport_Layer_Security" title="Transport layer security">SSL</a>, you have to arrange for a proper payment portal to site behind your site and handle the requests. You need to make sure your site is set up in such a way that any malicious users can not buy stuff without paying, nor have honest users' information compromised and shared. There are other options, such as <a href="http://www.paypal.com/" title="PayPal">PayPal</a> and <a href="https://payments.amazon.com/sdui/sdui/index.htm" title="Amazon Payments">Amazon</a>, but these facilities operate externally and won't appear "incorporated" in the site.
* **Accessibility** - You've done well if you already know what this is. In short, it refers to your sites ability to be read by people who are visually impaired who use devices such as <a href="http://en.wikipedia.org/wiki/Screen_reader" title="Screen reader">screen readers</a>. If you want your site to be fully accessible, then expect a great deal more effort to be involved. Your site needs to be structured in a special way and it needs to be **fully** compliant with a markup standard (such as <a href="http://www.w3.org/TR/html4/sgml/loosedtd.html" title="HTML 4.0">HTML 4.0 Transitional</a>).
* **Hosting** - This issue comes with a stack of other issues that most people don't even think about (sometimes even developers!). Do you have your own host? Do you have a shared host? Dedicated host? Do you need it in your own country? Do you need Windows or Linux? Do you need email facilities? Do you need SSL? Do you need a <a href="http://en.wikipedia.org/wiki/Content_Delivery_Network" title="Content delivery network">CDN</a>? Choosing a location for your site is affected by so many things. For example, if you're looking for a fully secure site which keeps track of user details and sensitive information (such as credit cards) then a shared hosting solution is an _EPIC FAIL_. In case the reason isn't obvious I'll tell you. If you use a shared host, then the entire machine is only as secure as the weakest account. If another site, owned by someone else, is compromised then the attacker owns the box which contains YOUR sites as well.
* **Database** - This isn't just about which one to use. It's about how to use it. Databases are underestimated and often misunderstood. It's _very_ easy to do databases wrong. This is a real kicker for those applications that are very database heavy. In particular, applications which require complex relationships and/or deep searching capabilities. If your application relies on an insanely speedy, distributed database or is search-heavy, you had best get yourself a DB guru ... and be prepared to pay for them! A recent example with some interested details was recently posted on <a href="http://blog.stackoverflow.com/2008/11/sql-2008-full-text-search-problems/" title="SQL 2008 Full Text Search Problems">StackOverflow</a>, go have a read!
* **Traffic** - If you're aiming to build a site that isn't going to be used often or doesn't have a great deal of content, then this might not be so much of an issue. But if you have a site that is going to be bombarded with traffic, don't expect your first cut of your system to, er, cut it. I once heard a semi-famous man say "<a href="http://www.codinghorror.com/blog/archives/000957.html" title="Everything is fast for small n">Everything is fast for small _n_"</a>, and he's dead right. In development and testing, your app will probably fly. But under load, with thousands of people hitting it at the same time, it may just keel over and die. Be prepared to face this problem in the development stage.
* **Maintainability** - I left this one last because it's a little verbose and somewhat random. So many things can affect the maintainability of the site. The main problem here though is that in general, the idea of maintenance after a site has been developed is far from anyone's thoughts. Maintenance is where you'll spend most of your cash long term. Minimising the potential issues during development really is key to making a site which isn't going to cost the earth to maintain or enhance. The most common cause of highly unmaintainable websites is "Little Johnny" (LJ). LJ is the bloke that someone knows through a friend of a friend, or through the family. He's the guy who "knows stuff about computers". Of course, that means that he can &lt;sarcasm&gt;build commercial websites that are flexible, secure, fast, extendible, easy to maintain and that WORK&lt;/sarcasm&gt;. In all seriousness, you might know someone who knows something about computers, but if you're serious about your site, then get someone who knows what they're doing. You'll pay more in the long run for someone who doesn't. Either that, or take one of your employees, and put them through a course so that they can learn how it's done. To sum up, if you want to reduce the cost of maintenance there is a lot of work involved, and your developers need to know what they're doing. Be prepared for a lot of work in this area.

Testing Issues
--------------
Testing? What's that?

It's frightening that even in Enterprise software, testing is generally an afterthought. Recently I was fortunate enough to be part of a team where testing was a huge part of the development process. It was _really_ good and the result was a seriously higher quality application.

Developers do not know how to test their own code properly. Sure, they can write unit tests, and they do some form of manual functional testing on a daily basis while they're building the software. This is **not** the same as having a full test plan and suite of tests to verify the quality of the application.

Do yourself a favour, spend some of your budget and get some testers in. They will put your application/site through its paces instead of your users!

Deployment Issues
-----------------
Once you've worked with a development team, you'll become very familiar with the following phrase:

> It works on my machine.

It's something that you'll hear constantly. I'll admit to saying it on a regular basis.

The point here is that it is very common for things to work in the development environment, but end up breaking when deployed to a production environment. Deployment is another often overlooked area of the process. When it _is_ considered, it's underestimated.

Be careful. Here be dragons. Before you get stuck, speak to someone who <a href="http://johnmcfadyen.spaces.live.com/" title="John Mcfadyen's Windows Installer blog">knows what they're doing</a>.

Support Issues
--------------
So you've managed to ship your application. It's online, it's deployed and it's running. But something goes wrong. The site breaks every now and then. Users are having issues with the payment portal. People using Firefox on Mac OSX can't sign in. Reports are coming back with incorrect values.

Do you have a plan in place to handle these requests? Do you have a <a href="http://en.wikipedia.org/wiki/Bugtracker" title="Bug tracker">bug tracking system</a>? Do you have a support team ready to handle requests? Have you kept any of the development team on board to help you with the support issues?

Generally, the answer to all of these questions is _no_. My advice would be to make it a _yes_. Put a plan in place to help you with the teething issues once you've gone live. Trust me, there _will_ be issues.

Conclusion
----------
I hope that the target audience have a little more visibility of the issues that are faced when building websites (and software in general). I hope that they learn from it and take some of the advice and do proper preparation before undertaking a development project.

Finally, I really do hope that it stops some of you from coming out with the comments listed at the start of this post.

As always, feedback and comments are welcomed and appreciated. Cheers!
