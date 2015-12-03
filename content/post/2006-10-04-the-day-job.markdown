---
categories:
- Being in the Industry
- Software Development
comments: true
date: 2006-10-04T00:00:00Z
title: The Day Job
---

I've been intending to write about this (and a few other things) for a while now, but due to wedding planning and work I've found it hard to find the time. So I'll take the opportunity while my disk is being defragged to crank out some thoughts.

Individual developers vary in many different ways, but I've often wondered what motivates other people and what they consider their job to be. To be more specific, what is it that a developer feels they should be doing in their day job? What does it encompass? Which bits do they worry about, which bits do they not worry about? What is it that they do which makes them what they are?

I've met way too many developers who have a very narrow description of their job, and follow that description regardless of the consequences - often to the detriment of the project they're working on and the team they're working with. Their views on their role often surprise me. So, I'm going to cover the things that I feel should be part of the job of a developer - and this applies to both contractors and permanent employees.

<strong><u>Understanding the Problem</u></strong>

How on earth are you supposed to come up with a valid and effecient solution to a problem if you have no idea what the problem is? This is surprisingly common out in the field. Developers can focus so much on the code and the technical aspects of the solution that they don't really know (or care) about the problem they're trying to solve. How many times have you guys worked with someone who just cranks code out thinking that it's adding value to the software when in fact it's totally irrelevant? How often have you seen modules implemented that only cover half of the problem? Can you think of a time where a colleague has written some code to solve a problem (which works really well) only to find that the problem they've solved has either already been solved or doesn't need to be solved?

Getting an understanding of your problem is the first thing any dev should do. If at all possible, the first implementation that the dev attempts should be thrown away as it's generally not the best solution and really should work as a way of gaining a better understanding of the problem.

Developers in general tend not to have a focus on the business side of the software. This is a general statement (because not all people work on software that's being built for a particular client or set of clients), but I think it's generally true. I've worked with some people who think that the job of understanding the business is purely the responsbility of the B.A., and hence they shouldn't have to worry about it. After all, they're there to write code! <strong>Rubbish!</strong> Your software is guaranteed to be off target if you don't get in the mind of the business users who will end up using the application you're working.

<strong><u>Know your Tech</u></strong>

This is a relatively broad title, so let me explain a little more. What I mean is that developers should have a very good understanding of the target platform, the language(s) that are being used to write the software, and the issues that might be faced when trying to bring these two things together. I truly feel it's the responsibility of the developers to <em>stay up to date</em> with advances in technology in their own domain <em>at the very least</em>. Preferrably developers should also have a general knowledge of what's going on in <em>all</em> areas of I.T., but since there's so much to take in this can be a very hard thing to do.

Example: If you're working on a C# application using .NET v1.1 in a web-based environment (so you're using ASP.NET) on a MS SQL server database then you have a few things to be on top of:
<ul><li><strong>C#</strong> - The syntax of the language is obviously a pretty important bit ;) but there's much more to it than that. Concepts such as <a href="http://msdn.microsoft.com/msdnmag/issues/01/04/net/" title=".NET: An Introduction to Delegates">delegates</a>, <a href="http://blogs.msdn.com/cbrumme/archive/posts/51371.aspx" title="Interning Strings &amp; Immutability">immutability</a> and <a href="http://blogs.msdn.com/cbrumme/archive/posts/51371.aspx" title="ref (C#)">ref</a> should be second-nature. Understanding them isn't enough, but knowing when and when not to use these features is key as well. Of course, this list isn't definitive ;) There's lots more to C# than this!</li><li><strong>.NET 1.1</strong> - This covers two areas: .NET (the framework itself) and v1.1 of the framework. Having in-depth knowledge of the whole framework is a mammoth task, and I'm not suggesting that everyone should be able to recite off the top of their heads the details of all the classes in the framework. The framework is pretty extensive, but despite that developers still seem to hand-code a lot of stuff that's already part of the framework - which isn't a particularly good use of time or resources. On top of understanding the framework, keeping abreast of the known issues of the framework version along with the improvements that were made over previous versions is important as well as it will allow you to make the most of the supporting libraries you have available.</li><li><strong>ASP.NET</strong> - This is a whole can of worms all by itself. It can be a hairy beast to work with if your knowledge of it is lacking and hence getting up to speed with it is very important if you're going to be working on a project that utilises it. Some examples of bits that should be understood are issues surrounding state management, session management, <a href="http://msdn.microsoft.com/library/default.asp?url=/library/en-us/dnaspp/html/viewstate.asp" title="Understanding the ASP.NET View State">View State</a>, <a href="http://msdn2.microsoft.com/en-us/library/ms178472.aspx" title="ASP.NET Page Life-Cycle Overview">page life cycle</a>, and <a href="http://msdn.microsoft.com/library/default.asp?url=/library/en-us/cpguide/html/cpconASPNETAuthentication.asp" title="ASP.NET Authentication">authentication</a>. There's a lot to get through, and it's hard to make a decent web-based .NET application without having a thorough grasp of how it all works. I'm still stunned at how often I come across development teams who are using ASP.NET and still don't really know some of the basics which would allow them to write better applications with fewer bugs - all they need to do is a bit of research/reading, but it just doesn't happen.</li><li><strong>MS SQL</strong> - This is another point that covers two areas: MS SQL as a technology, and connecting <em>to</em> MS SQL from your application code. In this example, knowing <a href="http://msdn.microsoft.com/data/learning/adonet/" title="Learning ADO.NET">ADO.NET</a> is a given, but having a better understanding of some <a href="http://msdn.microsoft.com/msdnmag/issues/05/05/DataPoints/" title="Data Access Strategies Using ADO.NET and SQL">strategies</a> behind using it more effectively would improve the quality of the application. Lastly, getting to grips with <a href="http://www.devguru.com/technologies/sqlsyntax/quickref/sql_syntax_intro.html" title="SQL Syntax Introduction">Transact-SQL syntax</a> is a good starting point for making the most of MS SQL. However, rather than just knowing how to write queries to pull data out, developers should get familiar with some of the best practices and common pitfalls when using the database in a production environment.</li></ul>

In a nutshell, what I'm saying is: developers get their hands dirty with new tech, and then when they feel they know enough they simply throw themselves into the development work without making sure they're doing what <em>should</em> be done with the technologies they're utilising. Knowing the syntax of C# isn't enough. Knowing that ASP.NET is a web-based "thingy" isn't enough. Having MS SQL as a DB and writing queries isn't enough. In order to write good and robust applications which aren't a nightmare to maintain, developers should do a heck of a lot more than just scratch the surface of the learning iceberg before they start development.

<strong><u>Stay up to Date</u></strong>

Technology, software development and practices are constantly changing. Sure it's not always positive, but most of the time the changes are a move forward. Developers should be making an effort to stay in touch with these changes, and take what they can from them. Utilise new ideas and technologies where it makes sense. Keep up to speed with what's going on in the industry to make sure that you don't slip behind your competition. Clients aren't just looking for a solution, they're looking for a solution that's going to last. Using the most up to date practices and technologies <em>should</em> help you achieve that.

As a final point on this issue: being up to date is going to make you more marketable as a software professional should you wish to move on! :)

<strong><u>Share your Knowledge</u></strong>

Software Development isn't a <strong>black art</strong> that should be kept secret so that your value is increased and your job is made secure. Knowledge of best practices, technology nuances, and business process is something that would benefit not only every developer on the team, but the team as a whole (including managers, analysts and testers). Teams should strive to share as much knowledge as they can, as it's only going to benefit the software in the long term - and after all, isn't writing <em>good and valuable</em> software the goal here?

Sharing knowledge in itself can be an issue, and some methods work in one team which might not work in another. However, I do think there's a stack of options to aid in achieving this. Here are a few:
<ul><li><strong>Use a <a href="http://en.wikipedia.org/wiki/Wiki" title="Wiki">Wiki</a></strong> - Use it and use it <em>often</em>. Dump your thoughts, keep a developer diary, write down every annoying little piece of information you may find that you think could help someone else in the future. Wikis often become a hub of information and an amazingly valuable resource. Their value can be proven time and time again when new members join the team and need to get up to speed with the project. More importantly, when team members leave they don't end up taking with them the only knowledge of a certain part of a system because the details are all on the wiki. I'm a big wiki advocate and I can't think of any case where a wiki isn't of value (feel free to prove me wrong :)).</li><li><strong>Code Documentation</strong> - Yes, this isn't always going to happen ;) But I can dream can't I? Utilising applications such as <a href="http://ndoc.sourceforge.net/" title="NDoc">NDoc</a> and <a href="http://www.stack.nl/~dimitri/doxygen/" title="Doxygen">Doxygen</a> will allow for automatic document generation from the source code. Of course this will rely on the developers maintaining the code appropriately so that meaningful documentation can be created, but this <em>should</em> be part of a team's coding standard anyway.</li><li><strong>Stand-up Meetings</strong> - This is something that is utilised a lot in Agile methodologies (such as <a href="http://www.extremeprogramming.org/" title="Extreme Programming">XP</a>) and can have great value so long as the team members all contribute appropriately. Stand-ups encourage interaction on a daily basis, and can aid in communication and knowledge sharing. Each team member can talk about what they did the day before, and what they hope to do on the current day. Discussion of issues faced can arise, and other developers can comment on possible solutions or known issues with the code-base which can help other team members do their job more efficiently.</li><li><strong>Others</strong> - There are others, but I'm going to stop here or I'll never get this rant finished :)</li></ul>

<strong><u>Be Up-Beat</u></strong>

I've lost count of the amount of "disgruntled" employees I've had to work with. Most of them seem grumpy for no apparent reason (ie. they just have issues with the world), and it doesn't help promote open communication in the team. I think developers should learn to leave their personal issues at home and come to work fresh and ready to take on the challenges of the day. If yesterday was frustrating and counter-productive, don't come to work grizzling about how bad it was - come in with renewed determination to nail the issue and move on.

<strong><u>Don't Pass the Buck</u></strong>

This is going to be another generalisation, but humour me for the point of this discussion :) As time goes by and developers (particularly permanent employees) get comfortable at work, they can start to form their own little working worlds that they don't want to venture out of. This is where you find most of the nominees of the renowned "Not My Job" awards. As a developer, your job is to be a problem solver! That means solving the problem regardless of the business unit, language, technology, platform or application. If there's a bug in a stored procedure that you didn't write, don't assume that it's the author's responsibility to fix it. If one of the other teams in your organisation has an issue, don't stick your head in the sand and let them deal with it themselves when you, as a fresh pair of eyes, could possibly help them sort it out quicker.

As a team member it's important to understand that the problem doesn't belong to an individual member of the team, it belongs to the team as a whole - even if the problem arises due to someone making a mistake. Get in there and fix it! After all, isn't fixing stuff like that part of the fun of being a developer?

<strong><u>Be Open to Ideas</u></strong>

It doesn't matter if you're the application's "star developer"; it doesn't matter if you're the known "guru" of a particular language; it doesn't matter if you've worked with technology X for 15x10<sup>32</sup> years; you can always learn something from other people. Listen to what they have to say, and see how you utilise their ideas in your own work. Two heads are (generally) better than one, and mixing ideas and listening to suggestions from other team members (yes, even if they're junior, fresh out of college or consider to be crap!) can really have a positive effect on what you're working on.

Rank is of no importance here. Lead or junior, permy or contractor. An awesome idea can strike at any time from anywhere! So be open to it.

<strong><u>Don't Settle for Hacks or Quick Fixes</u></strong>

Unfortunatley this rule can't be applied all the time (which is a shame), but it can be applied <em>most</em> of the time. Too often have we all seen cases where people quickly fix something because it needs to be "done" for a given iteration or release only to find that it lives on well after its intended death date. It rears its ugly head to bite the team in the arse over and over again.

Your job as a professional developer means that you don't just have the job of building a bit of functionality. You should be making sure you do it <em>well</em>. You should make it as robust as you can while using good programming practices and structures so that future maintainers of the software don't have a hernia at the though of working on the code every time a user logs a new bug.

This kind of thing will always happen, particularly in the later phases of development (such is the nature of the development beast), but I personally feel that there is no excuse for it when building an application prior to its first release. Fix it now, and fix it <em>properly</em>. Stop hacking and slashing. You know it makes sense!

<strong><u>Fix the Problem, not the Symptoms</u></strong>

I'm going to use an analogy here that a friend of mine uses all the time which I really like :)

A bunch of developers are sitting around a table that has a dead cat on it. The cat's been dead for at least 3 days and it's really starting to pong. It's in pieces, it's messy, and nobody wants to touch it. The developers are all sitting there fanning themselves trying to get fresh air and wondering how they can go about getting rid of the stench.

<em>"Let's use some air freshener!"</em> says one developer.

<em>"No, we need to get a new air-conditioner that can circulate the air faster."</em> says another.

<em>"Let's just move the desk into the corner and sit in the opposite corner."</em> says a third.

Of course, what they should be doing is putting the gloves on and undertaking the messy task of getting rid of the dead cat - entrails and all!

Some jobs in development can be like this. You know that fixing the symptom(s) of the problem would be easy, but the underlying cause of the problem is still there. My opinion: <strong>do your job!</strong> Fix the problem instead of alleviating the symptoms. Chances are that if you don't, those symptoms (and others) will arise in other areas of the application which you'll end up having to "fix" anyway. The overall time that it takes to cover the symptoms, along with the negative affect on the code, really isn't worth it in the long run. It's your job to do it properly.

<strong><u>Stick to your Guns</u></strong>

Sometimes we all come across someone who refuses to listen, or doesn't like your idea because <em>you</em> came up with it and not them. Remember, your job is to add value, solve problems and do your tasks <strong>properly</strong> regardless of other team members' issues. If you know you're idea is going to add value to the current context then don't back down on it because other people are more senior, more known, or more intimidating than you.

<strong><u>Take Ownership and Responsibility</u></strong>

Your time at work is paid for by someone other than you! So while you're there you should be adding value at the very least. Don't approach your tasks with a "won't be my problem" attitude. The problem is yours, the software is yours and you should take full responsibility and ownership of what you are working on as if it was going to be with you for the rest of your days. Too many devs (especially contractors) write code knowing that they're not going to be "stuck" with it when they're done. This is a bad attitude to have, and it'll result in your reputation slowly turning bad. It's your development duty to aim longer term, and to treat other people's problems as your own.

<strong><u>That's all folks!</u></strong>

So, there's a <em>small</em> summary of what I think it is that makes up the job of the developer :) I think it'll vary compared to lots of other people's, but I think that in general we have a lot more responsibility than most people are prepared to take on.

What do you guys think?
