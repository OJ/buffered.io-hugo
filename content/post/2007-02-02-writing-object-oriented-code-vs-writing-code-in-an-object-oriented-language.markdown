---
categories:
- Software Development
comments: true
date: 2007-02-02T00:00:00Z
title: Writing Object-Oriented Code vs Writing Code in an Object-Oriented Language
---

I'm going to try and keep this post short, otherwise I'll be here all day ranting away ;) This particular topic is one that's fairly close to my heart because it's a bit of a pet hate for me.

As we (the geeks) know, <a href="http://en.wikipedia.org/wiki/Object-oriented_programming" title="Object-Oriented Programming">object-oriented (OO)</a> programming is a different concept to <a href="http://en.wikipedia.org/wiki/Functional_programming" title="Functional Programming">functional</a> or <a href="http://en.wikipedia.org/wiki/Procedural_programming" title="Procedural Programming">procedural</a> programming. OO was a bit of a move forward in the direction of easy to understand, more managable and reusable code, and has been adopted all over the shop.

One of the first widely adopted OO languages was <a href="http://en.wikipedia.org/wiki/C%2B%2B" title="C++">C++</a> probably because of it's close relationship and similar syntax to <a href="http://en.wikipedia.org/wiki/C_%28programming_language%29" title="C">C</a>. Unfortunately, this resulted in something that continues to be a huge bane for true OO programmers all over the planet: <strong>the OOPP (object-oriented procedural programmer)</strong>.

OK, so that's a term that I like to use when talking to this kind of programmer :) But the idea behind them is this: when they made the switch to C++ from C, they didn't adopt a true OO approach to software development - all they really did was switch compilers.  There are so many people still to this day writing software using OO languages without actually writing good/proper OO code. The benefits of OO are lost on these kinds of people, and the resulting code is just a nightmare.

Before my current job, I worked in a couple of places where there were a few stagnant coders writing C++ - but in fact it was just C being compiled with a C++ compiler. Coupling was through the roof. Encapsulation was non-existant. Even funky features such as polymorphism were avoided to be replaced instead by huge 'switch' statements which did similar things based on the 'type' of a structure. Absolutely awful.

At the end of last year I worked with a lady who was by far the worst culprit for this. She was an advocate for "copy and paste coding", structureless code and job security. She was appalling. How she's managed to gain contracts for the last 13 years as a developer I will never know. It's people like her that end up producing systems that are so unmaintainable, badly written and poorly executed that lots of people lose faith in software. She even managed to this in <a href="http://en.wikipedia.org/wiki/C_Sharp" title="C-Sharp">C#</a>!

When are people going to learn that writing procedural code doesn't make them an OO programmer? Using an OO language isn't enough, you need to write code in an OO manner.

Before I make my final point I want to make it clear that I have nothing against C, or procedural programming. They both have their place. What I feel doesn't have a place is a programmer who refuses to write OO code when that's what they're hired for.

So, to finish up, what I want to say is this: just because you've written procedural code in the past and it's always worked for you, it doesn't mean that you can continue to do it in an OO language when you're paid to write proper OO software. If you can't keep up with technology, proper methodology, and best practice, then perhaps it's time to retire?
