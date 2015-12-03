---
categories:
- Security
comments: true
date: 2015-01-03T09:29:39Z
title: The Offensive Security Playground
---

In November last year, I was fortunate enough to participate in the beta testing of "[The Playground][]" -- a new product from the folks who gave us [OSCP][], [OSCE][] and others, [Offensive Security][]. The Playground, otherwise known as the "Virtual Penetration Test Labs", is an environment designed to aid in practising and honing your skills as a penetration tester.

Offensive Security have posted some detail of this lab [on their site][playground-blog]. It's worth reading to give context to this post.

This post is a mini-review of the lab, along with some thoughts as to why this could be good for you and/or your organisation.

<!--more-->

## Overview

When I finished [OSCP][] in 2013 I was actually saddened by the fact that I wasn't able to play in the labs any more. I knew that I could invest more money in more lab time if I wanted to, but there was a limit to the number of machines left and I didn't really see it as a valuable investment.

Fast forward to November 2014, and you can imagine my excitement when I was graciously asked to help beta-test a whole new lab environment! I leapt at the chance.

As noted on the Offensive Security site, The Playground is a virtual lab made up of 4 separate networks:

1. The "Starting Network" - referred to as "The DMZ" by those doing the beta testing, as this was the entry-point into the lab. This is akin to the "Student" network in the PWK lab.
1. The "Starfleet Network" - contains various systems that resemble a working production environment for a set of interesting services.
1. The "Middle Earth" network - made to appear similar to an office-style environment.
1. The "Grey" network - contains machines that are owned by administrators.

Each network has its own quirks and its own style. They are made up of an interesting set of machines that have various operating systems, patch levels and custom software installed on them. Again, this resembles the experience that you expect to see in the PWK lab.

I can't and won't go into too much detail about each of the networks because I don't want to give away anything. It's too much fun discovering it for yourself.

So if it's so similar to the PWK lab, why does it exist?

## Sharpening the Saw

This lab is all about practice. It's about honing your skills. It has new challenges, configurations, set ups and software. The difficulty level is higher. The workflow through the lab is different.

It's not about attaining a certification.

If you're in the game as a penetration tester, or at least want to be, then this kind of environment is a great place to spend time. One of the things that I really liked about this environment is that there were targets that I could try to break into that are rarely considered in-scope by clients. This kind of exposure was priceless for me, especially given that setting up such a lab is a job so big that I probably wouldn't undertake the task myself.

Some of the targets are even surprising. I'll leave it up to you to infer what that could mean.

## Difficulty

Those of you who have experienced Offensive Security certifications in the past know what it's like to tackle an Offsec challenge. The Playground is only different in that some of those challenges are harder.

During my time in the lab I wasn't able to own all of the machines. Some of them were brutal! Of the machines I did manage to compromise I can honestly say that there was a really good mixture of difficulty levels. Everyone who tackles this lab will learn something ...

... and suffer in the process.

## Competition

Each target in the lab has a point value associated with it. When a machine is successfully compromised, the attacker can retrieve "loot" which proves that access has been gained. This proof can be entered into the attacker's personal dashboard, which keeps a tally of the point score, as well as shows you which machines have been owned and which have not.

This kind of facility is great for a set of testers who all work for the same organisation, and that are interested in having some friendly competition. Gamification of such practice really does make this whole thing a lot of fun.

I had a lot of fun fighting the lab alongside a bunch of other fine folks. I must shout out to [Peleus][] and [rasta_mouse][] for leaving little messages scattered throughout the lab, to [barrebas][] and [superkojiman][] for constantly scaring me with the speed at which they owned stuff, and finally to [highjack][] for generally being an awesome dude. There are many others as well, too many to mention them all.

## Isolation

Organisations can request to have an instance of the lab set up so that they have their own isolated network that nobody else can share. This was one of the things that I didn't like about PWK, because I often stumbled on other people's exploit attempts - I hate spoilers!

I think this would appeal to many people and I wouldn't be surprised to see lots of companies take this up as an option when training their staff.

## Conclusion

I had a blast during my testing period. I would happily pay money to spend more time in this lab honing my skills. I think the experience is something that many other people will get a lot out of and I would highly recommend giving it a go, even as an individual.

I owe a great deal of gratitude to Offsec for giving me the chance to participate in their beta testing. In particular, thank you to [g0tmi1k][] and [muts][] for all their hard work and support.

![Thumbs up](http://i.imgur.com/sgq5HsO.gif)


  [Peleus]: https://twitter.com/0x42424242 "Peleus"
  [rasta_mouse]: https://twitter.com/_rastamouse "rasta_mouse"
  [barrebas]: https://twitter.com/barrebas "barrebas"
  [superkojiman]: https://twitter.com/superkojiman "superkojiman"
  [highjack]: https://twitter.com/highjack_ "highjack"
  [g0tmi1k]: https://twitter.com/g0tmi1k "g0tmi1k"
  [muts]: https://twitter.com/kalilinux "muts"
  [Offensive Security]: http://www.offensive-security.com/ "Offensive Security"
  [OSCP]: http://www.offensive-security.com/information-security-certifications/oscp-offensive-security-certified-professional/ "OSCP"
  [OSCE]: http://www.offensive-security.com/information-security-certifications/oscp-offensive-security-certified-expert/ "OSCE"
  [The Playground]: http://www.offensive-security.com/offensive-security-solutions/virtual-penetration-testing-labs/ "Virtual Penetration Testing Labs"
  [playground-blog]: http://www.offensive-security.com/offsec/professional-penetration-testing-labs/
