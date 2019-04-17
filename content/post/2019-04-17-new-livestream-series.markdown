---
categories:
- Security
- Livestream
- Development
- C#
- Metasploit
comments: true
date: 2019-04-17T16:55:43+10:00
title: New Livestream Series
---

## TL;DR

We're going to build a .NET implementation of Meterpreter live on stream. Together. From scratch. Read on for all the details!

## The Backstory

I remember kicking off my first ever live stream on [Twitch](https://twitch.tv/ojreeves) back in September 2016. It feels like a lifetime ago already. That stream, broken up into two parts ([1](https://www.youtube.com/watch?v=pJZjWXxUEl4), [2](https://www.youtube.com/watch?v=UGWqq5kTiso)), was me breaking open the Capcom.sys driver that had a blatant backdoor in it and abusing this backdoor to get `NT AUTHORITY\SYSTEM` privileges. After that, I went on to write a [Metasploit post module](https://www.rapid7.com/db/modules/exploit/windows/local/capcom_sys_exec) to abuse this from a Meterpreter session.

It was fun! It was also bloody scary. I've spoken at conferences to rooms full of people without too much concern, but I found this whole "live streaming" lark to be a completely different kettle of fish. There was something intimidating about opening myself up to an audience that I couldn't see. I was able to interact, but just via a chat function. The inability to respond to body language was really unusual and took a bit of time to get used to.

I'm glad I pushed through it. I've carried on the live streaming whenever I have the time, and have had a great deal of fun while learning alongside those who have joined me in the venture. Thanks to everyone who has supported me and joined in so far.

## The Importance of Fresh Content

Live streaming the same kind of things over and over again is pretty easy. While the content is technically different, and most hacker nerds enjoy a good binary exploitation challenge, I can honestly say that the content isn't fresh.

Sure there are cases where the exploits are quirky, or there is some new exploitation technique that has to be used. Those cases are indeed fun, and we all learn something. But at the end of the day we're just continuing to pop similar things.

Don't get me wrong, I love it! It's fun, I generally learn something new, but I think it's important to venture into other things. This is part of the reason why I do various streams that focus on development as well.

## Forcing Development on Security Folks

I'm sure it's common knowledge that I've spent a fair bit of my life writing (bad) code. I've been really fortunate to work on some pretty crazy stuff in my time, and along the way have got to work with some seriously amazing people. I don't look back on my time as a dev and think "holy shit that was a waste of time". I am very grateful for the experience, exposure and education that I gained in those years.

I have a fair bit of evidence that this history has helped me be better at security-related things that I might not be otherwise (particularly in the software security space).

After moving into security full-time, I found that there was a bit of a misconception that developers were clueless and careless. That they couldn't give a hoot about security, and that they're too busy with building features. That they don't want to handle security bug reports and don't care if their users are at risk.

Based on my own experience, given I can't speak for others, this is completely false. The developers I know now, along with those I've worked with in the past, _definitely_ care. They care way more than their employers and users will ever know.

The reality is that being a developer is much harder than it looks. I mean _much_ harder.

Developers work under tight timelines, strict constraints emposed by management or users, and have to constantly prove that they're adding value. Value is subjective, and hence making sure that value is visible to all of those in the management chain is a tough exercise.

So please, give developers a fucking break. They're working as hard as they can and facing obstacles that you don't even know exist. And don't pretend you know what it's like when all you've done is bash a few tools together in Python/Go/C#/whatever. Development and support of complex software is way harder than pushing a butchered script to GitHub.

Herein lies another part of the reason I like to live stream development: **I want security people to see how hard it is!**

And now I'm going to take it up a notch.

## What's Coming?

I've decided to do a new livestream series, and yes it's going to be development related with a heavy security focus.

We're building it from scratch. We're integrating it with something that already exists and deal with the baggage and constraints that come with doing it. We're going to make it work on various platforms/versions, and we're going to suffer the pain of handling the gaps across those versions.

I want us to build a version of **Meterpreter on the .NET platform**. Together. With all the pain that comes with it. I want us to learn together, to suffer together, and hopefully collectively gain an appreciation for what it's like to build something that is large and complex.

This is no small task. As someone who is fairly intimate with how Meterpreter works now, I can honestly say that we've got a pretty long road ahead of us. But I really think it's worth it. We all know that PowerShell is becoming harder to abuse in target environments. We know that native code is getting caught all over the shop. We now that variety and options are important when we're conducting our assessments. To me it makes sense to look at a .NET Meterpreter, as I think it'll extend it's shelf life and open up other options with regards to post-exploitation.

I plan on going over my rationale on stream, so please join me for the first one! I plan on kicking this off sometime next week when I'm back from school holidays with the family. I will be keeping these streams below 2 hours for each session to avoid burning out. The first few might not even have any code-writing in it. I plan on doing 2 or 3 per week for the first month, as this will hopefully ramp us all up on what we're going to be doing. It'll also help me get to [Twitch Affiliate](https://affiliate.twitch.tv/) status, which will allow me to better interact with all of you along the way.

## Stay With Me!

I will also stream the other things that I usually stream. I'll be doing exploitation, arbitrary tool hacking, and various other things that take my fancy (or you suggest). But I hope you enjoy the idea of building this with me. At the end I think you'll have learned many/all of the following:

* Complex software is hard.
* Integration comes with pain and suffering.
* Software contains baggage.
* Operating under constraints is hard, and painful.
* Hacking stuff together is easy, building proper software is hard.
* The first step of building something should not be "writing code", but instead getting a good understanding of the problem.
* C# is a great language, but has a few issues that make it painful.
* It's very easy to write shit code, and while it's harder to write good code it's very much worth it.
* .NET is amazing, and the framework is huge.
* Managing different versions of software is surprisingly hard.
* Ruby is terrible.
* It takes a long time to build something large/good, expecially from scratch.
* There are many ways to solve a problem, and sometimes it's hard to find the best one.
* I am actually a terrible developer. I'm just striving to get better every day.
* Finally ... building a new Meterpreter implementation and wiring it into Metasploit requires a lot of extra stuff that you didn't realise.

As always, the sessions are _interactive_. I want you to get involved. Share ideas. Point out my issues and stupidity. Let's see how good we can do together.

Right, time to stop rambling. Expect the first stream to appear on my [Twitch](https://twitch.tv/ojreeves) channel in the very near future. If you're keen to learn with me, please join in!o

Peace.
