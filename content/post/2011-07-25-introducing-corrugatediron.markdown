---
categories:
- C#
- Riak
- Mono
- NoSQL
- Open Source
- CLR
comments: true
date: 2011-07-25T00:00:00Z
tags:
- corrugatediron
- dotnet
- csharp
- clr
- mono
- riak
title: Introducing CorrugatedIron
---

## It's Alive! ##

It is with great pride that I introduce my first ever Open Source product release: [CorrugatedIron][]! A feature-rich .NET client for the [Riak][] Key-Value store. Together with my partner-in-crime [Jeremiah][JP], we've put together a driver which exposes a great deal of Riak's functionality. CorrugatedIron is at [v0.1.0][], and while it doesn't support every feature the Riak has to offer, it covers most, if not all, of the most common features that are required to effectively communicate with the system.

<!--more-->

## Features ##

I would love to cover off all of the features here, but we've already done it on the [official site][CorrugatedIron], so head over there to take a look at the feature list, documentation and sample projects.

## The What, Where, Why and How ##

### How it all began ###

I don't remember the exact date that I was first exposed to [Riak][], [Basho][]'s bomb-proof database, but I do remember being very impressed. It was around the time I really started to get an interest in [Erlang][] programming, so it probably isn't a surprise, given that Riak is written in Erlang, that it piqued my interest. I have a bit of a fascination with systems that don't stop, and Riak fits firmly in that category.

So after playing with it for a while and marvelling at the sturdiness, the ease of set-up (even with clustering), the clever architecture and the make-up of the system, I realised that Riak was actually pretty special. Almost in a class of it's own. Its properties really appealed to me, and I felt the need to do something with.

Back then, I wasn't even working with Erlang professionally. The clients that I had at the time were pure .NET shops and didn't feel the need to consider anything other than the "tried and true" [RDBMS][] (which in Brisbane seems to be mainly MS SQL, particularly in the .NET circles). Given that the likelihood of my working with Riak in a professional sense in the short term was fairly slim, I wanted to look to other areas where I could work with it and contribute to it at the same time. Needless to say, my Erlang-fu wasn't (and still isn't) up to scratch, so contributing to Riak itself wasn't yet something I thought I could take on. I needed something else.

Late last year, I was starting to look for projects that I could build and release as [Open Source][]. I have, on my occasions, contributed to other Open Source projects but I hadn't worked on one of my own and released it into the wild. This is something that I really wanted to do and so was looking for something to build.

During my travels in the Riak circles I had noticed that there were quite a few clients available which allowed people to talk to Riak from various languages. Basho themselves [support][BashoClients] ones for Erlang, Java, PHP, Python and Ruby, and there are many more listed on the [Riak community clients page][CommunityClients] which cover languages like C, Clojure, Go, Node.js, Perl, Scala and more.

Amongst this libraries there were two listed for .NET. Both of them seemed to have a small set of functionality, they both weren't finished and at the time they both had not been touched for quite a while. In short, for .NET people, there really wasn't a viable option for Riak connectivity. What a travesty!

I remember sending an email to [Mark][] telling him that I was pondering the thought of building this library to make sure that there wasn't already someone else out there making a go of it. He was aware of the two existing solutions but didn't know what the plans were with them, and he wasn't aware of any others at the time. This was all the validation that I needed.

So, in late 2010, I decided that the first project I wanted to build and release to the world as an Open Source application was a .NET client for Riak, one that worked on both the [CLR][] and on [Mono][]. This is where CorrugatedIron was conceived.

### So why the long wait? ###

If you look at the [history][HistoryGraph] of the code-base you'll see that I had an initial flurry of activity in early 2011, but didn't really do anything else for quite some time. There's a reason for that!

When I first decided on the project, I spent a bit of time thinking about the design. I wanted the interface to be more "functional" in many ways. I wanted to remove the idea of resource management away from the caller. I didn't want to give them rope (such as `IDisposable` instances) with which to hang themselves (such as forgetting to `Dispose()`). I wanted the interface to be clean, simple, intuitive and safe.

This little in-memory design session went on for quite some time, but I didn't really put anything down on paper. Nor did I write any code. Instead, I though that I would put something together which wasn't really related or as important as the API. Something that was lower level which the user of the library would not (and should not) see.

A Riak node has two interfaces which clients can connect to. One of them is a [REST][] API, and the other is a binary API which utilises [Protocol Buffers][Protobuf]. I thought that it would be fun to start working on the Protocol Buffer handling while the idea of the API formed slowly in the back of my mind.

### Then along came JP ###

Then, just before Christmas (23rd December to be exact) I received an email out of the blue from a chap in America. Here's how it started:

> Greetings from America!
> 
> Hope your summer is going well. Mark Phillips told me that you were interested in working on a good .net driver for Riak. Have you made any progress or is it still a general idea in your head?

There was much more to the email than that, but it certainly started off well! The email was from [Jeremiah Peschka][JP], a chap who's name I had seen floating around the Riak [mailing list][]. In fact, I remember his name catching my eye on more than one occasion because his email signature contained the following:

> Microsoft SQL Server MVP<br/>
> MCITP: Database Developer, DBA

This resonated with me because he was obviously into RDBMSs, but hadn't been a complete asshat on a list full of people working with [NoSQL][]. This was a rare and surprising thing.

Moving on. After a few email exchanges, Jeremiah indicated that he was interested in helping to build CorrugatedIron (despite the whacky name) and we decided to team up. I knew that he'd definitely add value to the whole process and would also keep me motivated. Plus, his obvious skills in the SQL realm would no doubt be useful too!

We continued to talk into January and I thought that it was past time that I shared the code that I had hacked together so that we had a starting/talking point. On the 7th January, I committed my first [batch of code][FirstCommit] to the repository which contained a stack of very untested code. I don't even know if it worked! The result: we had a lot to talk about.

### Another intermission ###

Though JP and I continued to talk a great deal via email, we were both quite under the pump with our respective places of employment. We shared ideas along the way, but neither of us were really into the project as a result of the intense work we had on elsewhere. Though I'm fairly certain that the thought of the project wasn't far from our minds the whole time.

Then on the 12th April, out of the blue again, another email came. I'm not going to divulge all the detail, but the crux of it indicated that there was a growing interest in seeing a .NET client for Riak from people in other areas of the world. It also asked what the story was with the client that we were building, and wondered if we had a timeline down with a potential release date.

This was scary, exciting and a slap in the face at the same time. Scary and exciting because there was a possibility that someone out there might want to use what we were building. A slap in the face because we hadn't really done much at all other than the initial commit and a great deal of talking. It was just the wake up call that we needed.

I spoke to JP about the email and we both decided that it was well past time to get our heads down and start working on this thing for real. We needed to lock in a set of features, a time-frame for development and, most importantly, a release date.

So, we did!

### All ahead flank ###

In early May, JP and I managed to start freeing up time that we could then contribute to our project. Development ramped up and kicked off in mid-May, and on the 18th, we committed our first changeset to the repository since the very first commit in January. From there, we went nuts!

We consistently worked on things and pushed our code back and forth for the latter half of May and well in to June. At this point, things really started to get exciting.

We had managed to get quite a few features out in a small period of time, and were generally very happy with our progress. JP was making the most of his superior Riak knowledge and was banging out API features like there was no tomorrow. Meanwhile, I had my head down in the guts of the underlying bits, trying to keep things sane.

It was at this point we were told of _more_ people who were keen to get their hands on a quality .NET client, and that if we could get the client ready in time, various individuals would be happy to talk about it during [OSCON][], the biggest Open Source convention I know of. Awesome! This was an opportunity too good to miss.

We finalised our feature-set for v0.1.0, wrote down our final time-line and informed various parties of what we were planning to do. It was locked in. We were heads down, bums-up trying to get things into shape. It was all very exciting.

### Even more interest ###

By early July, we had somehow managed to attract the attention of two more individuals who were looking for this functionality. Both of which put their hands up to the opportunity to look at our Alpha software, take it for a spin and give us some feedback. This was awesome. Having other people look over the code and critique it while, in some ways, evaluating it for their own needs is a great thing.

After a short period of time, we received very constructive (and, just quietly, rather gratifying) comments from both guys. It made us feel like we couldn't be doing too badly!

We opened up the repository to them so that they could get the latest code whenever they wanted, and also opened it up to some of the Basho guys so that they could also cast their eyes over it. The cat was slowly coming out of the bad.

On the 20th July, we locked in the feature-set for v0.1.0 and froze the codebase for all but minor changes, bug fixes and tweaks. It was time to do what everyone _loves_ to do: **documentation**.

Given that I tend to hate documentation, straight away I was looking for something else to fill the time with. Thankfully, alongside documentation, we also needed a few sample applications (let's face it, as devs we learn much faster from working code compared to reams of documentation). So I leapt on the opportunity to crank out [YakRiak.NET][], a .NET client for [Sean Cribbs][]' [YakRiak][] chat application. It was incredibly simple to do and didn't take very long at all. When building the app, and finally _using_ my own software, I have to admit I felt pretty good. It was nice to use my own software for something fun!

After that, JP put together a new [Session State Provider][] which used Riak as the back-end store. How good is that! Riak-backed session state in .NET. Awesome sauce.

I also finalised a small sample application which utilised some of the most common IoC frameworks to wire in the configuration, and began working on the 'real' documentation again.

### "Going live" ###

Finally, on the 25th July, after a couple of months of intense development, sample app creation, documentation and blog posts, CorrugatedIron was released to the world -- just in time for OSCON (phew!).

While the documentation isn't as thorough as we would like, and our unit test coverage isn't as high as we'd like, we're very happy with what we've managed to achieve. This first release is by no means the last, and JP and I are both excited about what we're going to add to it in the future.

### A small side note ###

When people first start working on projects like this there is always a risk that personalities will clash and the software will suffer. JP and I knew nothing of each other when we started this thing, yet over time have got to know each other and had a great deal of fun learning from each other. I think I've been really fortunate in having JP involve himself in this project. He has been open to different ideas and opinions, has never come across as an ass and has been a real pleasure to work with the whole time.

So, JP, thanks mate! I'm really glad you got involved. CorrugatedIron wouldn't be what it is now if you hadn't.

### The End ###

Thanks for reading this far! If you're a .NET mofo and you're keen to get your Riak on, [grab the source][source], [download the binaries][binaries] or [install the Nuget package][nuget] and get cracking! Feedback is always welcome, as are patches. So if you've got something to add, take away or refine, fork our [repository][source] and get those pulls requests happening!


[BashoClients]: http://wiki.basho.com/Client-Libraries.html "Client libraries"
[Basho]: http://basho.com/ "Basho"
[CLR]: http://en.wikipedia.org/wiki/Common_Language_Runtime "Microsoft CLR"
[CommunityClients]: http://wiki.basho.com/Community-Developed-Libraries-and-Projects.html#Client-Libraries-and-Frameworks "Client libraries"
[Corrugatediron]: http://corrugatediron.org/ "CorrugatedIron"
[Erlang]: http://www.erlang.org/ "Erlang"
[FirstCommit]: https://github.com/DistributedNonsense/CorrugatedIron/--SOMETHING-GOES-HERE--
[JP]: http://facility9.com/ "Jeremiah Peschka"
[Mark]: http://twitter.com/pharkmillups "Mark Phillips"
[Mono]: http://www.mono-project.com/ "Mono"
[NoSQL]: http://en.wikipedia.org/wiki/NoSQL "NoSQL"
[OSCON]: http://oscon.com/ "OSCON"
[Open Source]: http://www.opensource.org/ "Open Source"
[Protobuf]: http://code.google.com/p/protobuf/ "Protocol Buffers"
[RDBMS]: http://en.wikipedia.org/wiki/Relational_database_management_system "Relational Databases"
[REST]: http://en.wikipedia.org/wiki/Representational_State_Transfer "REST"
[Riak]: http://riak.basho.com/ "Riak"
[binaries]: https://github.com/DistributedNonsense/CorrugatedIron/downloads "Binary downloads"
[mailing list]: http://lists.basho.com/mailman/listinfo/riak-users_lists.basho.com "Riak mailing list"
[nuget]: http://www.nuget.org/List/Packages/CorrugatedIron "Nuget package"
[Session State Provider]: https://github.com/peschkaj/CorrugatedIron.Samples/tree/master/VisualStudio2010/Sample.SessionStateProvider
[source]: https://github.com/DistributedNonsense/CorrugatedIron "Source code"
[v0.1.0]: https://github.com/DistributedNonsense/CorrugatedIron/tree/v0.1.0
[HistoryGraph]: https://github.com/DistributedNonsense/CorrugatedIron/graph
[YakRiak]: https://github.com/seancribbs/yakriak "The original YakRiak"
[YakRiak.NET]: https://github.com/DistributedNonsense/CorrugatedIron.Samples/tree/master/VisualStudio2010/Sample.YakRiak "YakRiak.NET"
[Sean Cribbs]: https://github.com/seancribbs "Sean Cribbs"
