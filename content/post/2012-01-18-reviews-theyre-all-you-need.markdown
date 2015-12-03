---
categories:
- Testing
- Agile
- Software Development
comments: true
date: 2012-01-18T00:00:00Z
title: Reviews - They're all You need
---

<a href="/uploads/2012/01/pair-programming.jpg" rel="lightbox"><img src="/uploads/2012/01/pair-programming.jpg" style="float:left;margin-right:5px;margin-bottom:5px;" width="200" /></a>I can clearly remember the first time I got involved in an Agile project -- it was back in 2004, it was in London and in the finance industry (insurance to be exact). When I joined the project the team was small though over time it become much bigger.

While on that gig I met and worked with a few of the nicest and best devs that I've worked with ([RobG][], [Yoann][] and [The Chief][]).

It was an exciting project. We were using [XP][] in very much its purest sense, right from the beginning. We always paired up to do our programming, we created stories, we estimated our points using a point system that wasn't tied to hours but instead was relative. We had mini-retrospectives, continuous integration, unit tests and more. For 2004, that was pretty good.

Since then I've worked on quite a few other Agile projects and over time, I have come to believe a few of the absolute purest views on Agile aren't necessarily the best.

<!--more-->

Any Agile practitioner will say "You should use the process that works well for your team" and while that's true they tend not to mean things like pair programming and unit testing which are considered an absolute must. The topic I'm interested in for the purpose of this post is **pair programming**.

Does Pair Programming Work?
---------------------------

Well does it? It's a hard question to answer given its generalised nature. It sounds clichéd, and I guess to a point it is, but the answer is both yes and no. I don't mean that in the same way that most people will mean it. I don't mean that it might work in some teams but not in others. What I mean is that it works when approaching a particular kind of problem but not for others.

To add another generalisation, it works well in cases where the problem is quite difficult, or requires one or more difficult and hand-coded algorithms to solve. The type of problem where the possibility of getting it _drastically_ wrong is a perfect candidate.

Drastically wrong is subjective, but I think we all know what I mean by it.

Some might argue that pair programming isn't required even in this case, and perhaps they are right, but I don't think so. Yes, you can have a design session where you get people together and talk about the options for implementation, nut out of a few of the details, etc. You can take the value from the discussion and, as the developer, dive into your dark little world of editing and bash out what you think was the design that everyone else had discussed. The result is usually vastly different to what the rest of the collective thought was going to be implemented.

Pair programming here helps. It helps make sure that one person isn't skewed or biased, keeps discussion going around the problem and forces the developers to think more while they're writing the code. Each key design decision is shared across two brains and (in theory) a better design will result.

That's where pair programming fits. Anything _less_ than that and I can't help but feel that it's overhead. It's a burden. It doesn't add enough value to justify its cost.

To be direct, yes I am saying that _pair programming is a waste of time and resources for anything less than the mind-boggling problems_. This assertion does come with a few assumptions:

1. There are no junior members who may need mentoring.
1. All members of the development team are competent.
1. There is enough trust between the team members that each developer will not only _not_ produce a pile of rubbish, but will also put their hands up if they feel they're getting into something which requires input from the other minds in the room.

If any of those assumptions is false then pair programming may have to come into play, or not! Point #2 interests me a little more than the others. If someone isn't competent then it doesn't matter if you do pair programming with them. What tends to happen is the competent person ends up doing everything while the numpty sits there playing a rather passive role, checking Facebook on their phone.

If PP doesn't work, what should we do?
--------------------------------------

I found the answer to this question in 2008 when I joined a team of people working on a system here in Brisbane. This project, like the one I mentioned before, had people who really knew what they were doing ... I think! (in no particular order: [JoCo][], [Mr B][], [Joel][], [Rhys][]). I worked with these guys for nearly a year without really doing any pair programming at all.

Fast forward to now. I am again working with Leon, Rhys and Joel. I am again using the process that was used back then in 2008. I am again amazed at the quality of the output compared to so many other projects where pair programming was used to a very large extent. Not only that, the speed at which things are getting done is also pretty amazing. I am sure this is partly down to the developers themselves, but I think the lack of pair programming allows the team to be really productive.

So what are we doing instead?

Simply: Test Reviews and Code Reviews.

A developer works on a story by himself. During the course of development, the developer often shouts out to the rest of the team for advice on a given issue. They talk to the business owner to get clarification on the issues inside the requirements, the conversation is positive and constant. If the problem is really difficult, two of the guys will pair up and bash it out, but this proving to be a rarity.

At the end of development, prior to pushing any changes to the server, the developer must have one of the testers perform a test review of the work that has been done. When that has passed, a developer must do a full code review of what has been produced for that story.

It doesn't sound any different!
-------------------------------

It might not appear to be different, but it is _very_ different.

[Rhys][] is our resident technical tester. In fact, he's a developer dressed up in tester's clothing. He has an incredibly sharp mind and an uncanny ability to find issues and break stuff. The number of times I have had my ass saved by Rhys prior to pushing my <del>shit</del> code to the central source repository is astounding. In all my time doing development, this one thing by itself has proved to catch more issues than anything else (including unit testing, automated testing, smoke testing and regression testing).

Testers have intimate knowledge of the requirements. They have experience with the system. They are aware of dependencies in business process that you are not. These factors, along with many others, are what make testers a great point of call prior to your commits.

It might be unfair to assume that every team has a technical testing resource that is the calibre of Rhys, but to put it bluntly **your team should have one**. Before passing your code on to the rest of the team it's your duty to make sure it is as unbroken as possible. Test reviews have proved to be the most effective thing in accomplishing this.

Back it up with a proper code review
------------------------------------

That's right, not just any code review, but a _proper_ one. One that requires you to go through your own code in quite a bit of detail. One that forces you to justify your design decisions. One that makes you critically analyse the legacy that you've just created.

Anyone who has gone through this process with a colleague of any reasonable quality will know that:

* ... any time you cover off a bit of code that isn't that nice you get nervous before you even talk about it.
* ... the code you wrote that you knew was sub-par is going to be picked up.
* ... any stupidity that you failed to see will be thrown back in your face (in a positive way of course).
* ... even the most subtle things can prove to be important and a good code review will often demonstrate why.

In short, you know that you'll get owned if you try to get away with something that you knew, deep down, wasn't good enough in the first place.

As someone who conducts code reviews for other people in the team, you'll not only learn about other people's styles, flaws and strong points, you'll also cover off areas of the system that you would otherwise _have absolutley no idea about_. This is the kind of knowledge sharing that you would get doing pair programming, but I think it's more effective because you're not dragged through the whole design and development process from the start, and you tend to be more interested when the discussion of the detail is condensed.

And let's face it, being critical of someone else's code is something we all enjoy, and we're much more likely to pay attention during a code review than we are being a [navigator][] during a pair programming session.

Bring it on
-----------

Disagree? Feel free to let me know in the comments.
Think I'm full of shit? Feel free to let me know in the comments.
Anything else? Feel free to let me know in the comments.

I'm keen to hear what you all do, how you do it, and why you think it's better or worse. In particular, do those of you who do pair programming at least _most_ of the time believe that it is adding enough value to justify the cost? Do you feel better for it?

Until next time!


  [RobG]: https://twitter.com/robertthegrey "Robert @ Twitter"
  [Yoann]: https://twitter.com/thenapoleon "Yoann @ Twitter"
  [The Chief]: https://twitter.com/RockThunderUK "Paul @ Twitter"
  [navigator]: http://effectif.com/agile/pair-programming/be-a-better-navigator "Pair Programming Navigator"
  [XP]: http://www.extremeprogramming.org/ "Extreme Programming"
  [Mr B]: https://twitter.com/secretGeek "Leon @ Twitter"
  [Joel]: https://twitter.com/joelpob "Joel @ Twitter"
  [Rhys]: https://twitter.com/rhysparry "Rhys @ Twitter"
  [JoCo]: https://twitter.com/josephcooney "Joseph @ Twitter"
