---
categories:
- Challenges
- Software Development
comments: true
date: 2006-09-15T00:00:00Z
title: Who's up for a challenge?
---

Before starting this blog up, I spent a bit of time thinking about the kind of things I would like to talk about which other people might find interesting. I also wanted to find some areas of discussion where people would be interested in giving me feedback and comments containing their thoughts and opinions so that I can learn something and possibly benefit from other people's experience. While this is all very enlightening, it can be fun and it can be, shall we say, political.

So as well as having lots of industry-specific rants, posts on what's considered good and bad, and large chunks of text relating to experiences while out on site working for clients, I thought it'd be a good thing to post some other bits and pieces that are a bit more light hearted, and dare I say it... FUN!

Way back before I went to the U.K. I used to have a site up and running that had a small community of people interested in programming - specifically game development. While this site is long gone, never to be remembered, there were a few things that we used to do there which I thought were really good. One of which was listing programming challenges. Every now and then I'd come up with (or rip off) a programming challenge that I thought would be a good exercise for members of the community to undertake. It was surprising how many people were interested, and we ended up with many responses and submissions containing solutions to the problems.

The beauty of it was that people learned a lot from the exercise - especially from each other's varied approaches. They would come up with a solution and be eager to post it. After noticing that someone else had created a "better" solution than them, members would go away and try and improve what they had. It was great, as it bred a mindset of iteration and improvement, and over time most of the community members ended up being comfortable with the idea that their first solution generally isn't perfect and they immediately look to make it better.

I'm of the view that the first solution that you write to solve a given problem is designed to give you a better understanding of the problem space. Sure, it's not always the case that you can throw away the code you've written (depending on what you're working on, and what timelines you have to adhere to), but if you can it's a good idea to do it and start again fresh with the knowledge you've gained from your first attempt.

I would like to get this type of thing going again. I'm going to get some ideas together for problems and start posting them here for other people to have a stab at. I'll be giving some of them a go as well, and if people are interested in sharing their solutions and discussing them, they can post their solutions as comments. If the idea becomes popular and the number of submissions goes up, then I may think about setting up a basic <em>Coding Challenges</em> forum for the discussion and submission of the problems and solutions. If people don't like it, I'll probably continue to post them anyway just in case someone at some stage finds them as interesting as I do :) If anyone's interested in seeing my solution, then I'd be happy to post it.

The problems themselves will most likely be small. I won't be suggesting that people should go away and write a 3D rendering engine just to see the frames-per-second they can achieve on a 386! The problems will have a very small scope - meaning that solutions can be built quickly, and people won't feel that they need to put a massive amount of time in to get the results required. Hopefully they'll be quite challenging despite being small in stature.

One thing I will attempt to do when writing these problems up is make sure that the specification forces the programmer to do as much of the work as possible - without relying on the supporting framework or library set that comes with the language. In general the problems will not have a target language (or language set), but there may be cases where I state that the problem is intended to be solved using C++ or Miranda!

I hope that you'll find the challenges interesting, and that you'll perhaps find the time to give a few of them a go to exercise the mind. I think some of them will be more useful to beginners learning the ropes of development, but I can't see why experienced programmers shouldn't give them a shot. There's always the possibility that we'll learn <em>something</em>.

So let's begin with the first one that's popped into my head. It's a bit of a classic problem, but there's still a chance that people out there might not have heard it before - or more importantly attempted to solve it themselves in code. I first heard about this problem through a friend of a friend of a friend of a ... you get the idea ;)

<strong><u>Challenge #1 - Linked List Loops</u></strong>

<blockquote>"What do you think is the most efficient and resource-friendly way to determine if a singly-linked-list contains a loop?"</blockquote>

When you have an instance of a linked list, it is usually <strong>NULL-terminated</strong>, that is, the last reference is a NULL reference which tells you that the end of the list has been found. The question asks how you determine if the list is <em>not</em> NULL-terminated, instead the "last" item in the list points back to another item in the list (thus creating a loop in the list).

To solve this problem you will need to have an understanding of <a href="http://en.wikipedia.org/wiki/Linked_list#Singly-linked_list" title="Singly-linked list">Singly-linked lists</a>. You don't need to write code, I'm just interested in the algorithm you'd use.

Sounds easy, and there are quite a few ways of doing it. So give it a shot! I'm interested to see what you guys come up with.

Happy problem solving!
