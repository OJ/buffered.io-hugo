---
categories:
- Databases
- Software Development
- Technology
comments: true
date: 2006-09-21T00:00:00Z
title: Dealing with the CRUD
---

One of the things that I think every web developer on the planet is sick of doing is building <a href="http://en.wikipedia.org/wiki/CRUD_%28acronym%29" title="CRUD">CRUD</a> methods for their applications. It's the kind of thing that we seem to do as developers over and over again despite the fact that this task is essentially monkey work.

There have been quite a few attempts to nail this issue on the head in various programming camps, and it's these attempts that I'd like to go over - partly because I'm also sick of CRUD, and partly because I don't know enough about <em>all</em> of these solutions to know which one's work best in what scenario (hopefully this is where you guys come to the rescue :)).

I have to make sure that I'm clear - I am not talking about creation of supporting object-graph loading or anything funky like recursive deletion in code (rather than as a DB setting). I'm simply talking CRUD and CRUD alone :)

<strong><u>1. Code Generation</u></strong>

This is quite a common solution to the problem. One of the developers either builds a tool, suggests a tool, or downloads a tool that can be used to generate a stack of CRUD methods off some form of data definition. Some of these utilities are able to read the database schema directly, some of them need XML definitions, others do neither and require some other form of manual intervention. The good utilities simple provide a basic interface to the database's definition and allow you to wrap your own templates around them.

The thing I don't get with this solution is that there's still a hefty chunk of work to be done to get the templates for the code <em>and</em> the stored procedures ready and bug-free.

<strong><u>2. Hibernation</u></strong>

This is something I know very little about at the moment. From what I can see on the surface, Hibernation refers to the handling of the persistence of objects (eg. in <a href="http://www.hibernate.org/343.html" title="NHibernate">.NET</a> or <a href="http://www.hibernate.org/" title="Hibernate">Java</a>) to and from relational databases such as MS SQL Server or MySQL. It attempts to decouple the database implementation and schema from the system's <a href="http://en.wikipedia.org/wiki/Domain_model" title="Domain Model">domain model</a> - which is a good practice that aids us in preventing the domain model from being polluted with details of the underlying data store. Apart from that, the framework handles the mapping of objects to the data model and vice versa - I liiiiike, it is niiiiiice.

This sounds very nice, but I have to reserve judgement as I'm yet to give it a spin myself. If it does what it says on the tin, then we're all in happy joy joy land. I'll try and get round to having a play with it in the next couple of weeks and see how easy it is to get going. If I can, I'll try and benchmark it a bit too so we can get an idea of the performance. I'm interested to know how well this solution scales.

Apparently Microsoft are looking to create something similar to this (or are in the process of creating it). I believe they're calling it "ObjectSpaces", but don't quote me on that :)

<strong><u>3. Object Databases</u></strong>

I haven't really kept up to speed with the advances in the field of object databases, but I've recently been handed a link to something that looks quite interesting called <a href="http://www.db4o.com/" title="db4objects">db4o</a> which looks to be a Java and .NET implementation of an object database. The idea of an object database isn't new, but this framework looks like it's making good ground.

Unfortunately, in this case, it looks like the performance isn't too crash hot and it's not really scaling that well. Not just that, but I'm not sure if the issues of concurrency and data reporting have been nailed either. Again, this is something I'm yet to really play with, and I'll give it a fair go before dismissing it. At the moment my gut feeling is that it's not yet a viable solution for anything more than a hobby project, despite the apparent ease of use. I love the way it's so easy to persist things and retrieve them again, but until the above issues start being nailed there's no way it can be considered for anything of a larger scale.

<strong><u>What are the others</u></strong>

This is a question for you guys? I'm sure there are stacks out there which are worthy of mention, and I just have no idea what they are. Feel free to send over some links to other viable solutions and let me know what you think of them.

<hr/>
<strong>EDIT --</strong>

I'd just like to add a quick follow-up to this. Christof over at <a href="http:/www.db4o.com/" title="db4objects">db4o</a> did me a favour and brought to my attention the current purpose of db4o, which is something I didn't really know before (his comment can be read in the comments section). My focus while writing this blog post was to explore the options for the replacment of the issues faced in an "enterprise" system where there are potentially hundreds of thousands (if not millions) of records and a large amount of simultaneous transactions taking place. It looks like, at least for now, db4o isn't intended to be used in such a scenario. Perhaps in the future this is something that will be targetted? Also, when I stated that it looked like it was handy for a hobby project, I wasn't implying that hobbyists are the only people who would find it useful. Clearly it already has a use in embedded environments - I simply meant as a back-end DB for people building systems they may be using at home as part of a hobby project.

I'm sure there's stacks of information on this on their site and users forums, so I should get my butt over there and do a bit more reading :)
