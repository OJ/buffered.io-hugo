---
categories:
- Security
- CTF
comments: true
date: 2018-11-26T16:41:10+10:00
title: Police Quest CTF Challenge
---

<img style="float: left; margin-right: 10px; margin-bottom: 10px" src="/uploads/2018/11/pq.png" width="300px"/>
Earlier this month, I donated a CTF challenge to the legendary bunch of folks that ran the [Kiwicon](https://www.kiwicon.org) CTF in Wellington. It's a bit of a tradition for me to pass on at least one challenge, and I felt it was worth keeping that tradition going this year.

The challenge was attempted by a few people during the conference, however I don't believe it was successfully owned. As a result, I wanted to release this to the general public for funzies.

In short, this thing is a Linux binary exploitation challenge. It's a 64-bit binary, and has a few interesting quirks that you need to get around. My aims for this challenge were:

1. Create something that was interesting!
1. Build an application that was "fun" to interact with that happened to have some security concerns, rather than a shell of an application with a bug in it.
1. Force people to work through a series of issues, rather than getting a flag due to a single bug.
1. Make good use of most of the relevant security protections available today.
1. Try to make it reasonably realistic.
1. Try to make it _hard_ without making it stupid. Many challenges I've seen in the past tend to be hard for the sake of it and result in not giving any ideas as to how things can be solved. I hope that the method required to solve this shows itself as you progress, while making you work for each and every step.
1. Create something that had both dynamic and static analysis elements.
1. Create a learning experience for people.

Based on the feedback so far from my playtesters I think I nailed a good chunk of the brief. I look forward to hearing what you have to say about it.

So the result is something that I called **Police Quest**, in the spirit of the old school Seirra games. Why? Well we had a theme for the CTF that we were building last year, and this fit within the theme. Plus I loved those games, so why not. It's a mini-mud that may keep you entertained for a few hours.

I also hope that this nerd-snipes lots of people who end up hating me.

## Where to get it

Full details of the challenge can be found in the [Github Repo](https://github.com/OJ/police-quest). The easiest thing to do is clone the repo, as this gives you the binary to work on, and a Docker container that has the challenge running in it. You can use the `Makefile` to spin the container up.

## With thanks to ...

As always there are a bunch of people behind the scenes who helped get this thing on the road. In no particular order:

* Thank you [Justin](https://twitter.com/justinsteven) and [Smash](https://twitter.com/_smashery_) for being my play-testers and making sure that the thing was understandable, and doable.
* Thank you [Shaun](https://twitter.com/ausjock), [Rick](https://twitter.com/rickoates), [Silas](https://twitter.com/_switch_) and [Ryan](https://twitter.com/0x42424242) for offering opinions and feedback along the way and helping me polish the story.
* Thank you to the Kiwicon crew for letting me include this challenge in their CTF.

## Conclusion

If you decide to give this a go, please feel free to create a video, blog post, or whatever else takes your fancy and publish it somewhere that people can get to. If you do, then let me know, as I'd love to see how you solve it. I'll release the source code to the binary to each person that solves it themselves, if they're interested.

Feedback as always is welcome. Thanks again, and enjoy!

OJ
