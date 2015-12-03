---
categories:
- Being in the Industry
- Software Development
- Technology
- WTF
comments: true
date: 2010-01-28T00:00:00Z
tags:
- WTF
- bad decisions
- software developer
title: The Wrong Decision by the Wrong Person
---

<a href="/uploads/2010/01/pizza.jpg" title="The wrong tool." rel="lightbox"><img src="/uploads/2010/01/pizza.jpg" alt="The wrong tool." title="The wrong tool." style="float:right; margin-left:5px; margin-bottom:5px;"  width="150"/></a>There is one thing about my industry that I still find truly amazing (and not in a good way). This is despite the fact that it has happened to me so many times that you think I'd be used to it! I'm talking about non-technical people making technical decisions.

For some reason, it's a very common practice for those people who don't have expertise in a certain field to make decisions for people working in that field. The best example I can think of off the top of my head is the answer to the question: <em>"Which technology should we use?"</em>

Let me take a step back in time for a moment.

<!--more-->

I applied for a job with a certain company (which shall remain nameless) that was looking for a number of C#/.NET developers of a certain level of experience. After confirming an interview date and time, I turned up to find myself facing a panel of 3 interviewers. One of those people held a technical position, though I found out later that the position he held was self-appointed and certainly not an indication of the person's level of technical knowledge or expertise. The other two were in non-technical positions.

During the interview we had the usual discussions and questions around technial knowledge, depth of understanding of certain areas of the .NET framework, views and approaches to dealing with people in teams, agile experience, etc. It all went very well. At the end of the interview I was given my chance to ask a few questions. The main one I wanted to ask was <em>"Why did you decide to use .NET for this project?"</em>

For the record, the <em>previous</em> version of the system was written using an older Microsoft technology stack. The decision had been made to go with .NET for this new version. Can you see the correlation?

The answer was quite stunning. The only reason the project was using .NET was because the previous version was a Microsoft-based solution.

The first thing I thought was "surely that can't be it?". As a matter of fact, that wasn't the <em>only</em> reason for it, but it was the <em>main</em> reason. The decision wasn't made by a technical person, it was made by someone in upper management.

Scary huh!?

In my personal opinion, there is one particular technology that I think would have done a much better job of solving this particular problem, and it's not a Microsoft solution. It's not a Sun solution, or any other mainstream option for that matter. In my view, <strong>Erlang</strong> would have been perfect.

I don't think that a company like that would have ever even considered Erlang as a viable option. Mainly for two reasons. The first is that the amount of skilled Erlang developers available on the East Coast of Australia is extremely small. The second is fear of the unknown/non-mainstream tech. That is, companies like to go with what other companies are going with.

The crazy thing about this scenario is that management go to market to hire people who are domain experts, and then proceed to tell them which technologies to use. Surely you would be better off hiring people to know how to solve problem X really well, and then listen to what they have to say about the technology stack that should be put in place? No, not in this industry. Probably not in my lifetime either!

So if you're a non-tech person leading a team of techies, please PLEASE listen to what they have to say. Ask them what they think is the right approach and the right toolset. Don't take control of the technical decisions. Let them do what you hired them to do: Solve the problem, and use the right tool for the job. If they don't know the answer, or the answer is always the same, then you've got the wrong developers.

Thanks for listening :)
