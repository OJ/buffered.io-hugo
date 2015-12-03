---
categories:
- HOWTO
- Software Development
comments: true
date: 2007-07-20T00:00:00Z
title: Avoid Writing Unintuitive Code
---

This blog post was inspired by a brief chat I had recently with Kirupa of <a href="http://www.kirupa.com/" title="Kirupa">kirupa.com</a>. I subscribe to his <a href="http://blog.kirupa.com/" title"Kirupa Blog">blog's</a> <a href="http://blog.kirupa.com/?feed=rss2" title="Kirupa Blog RSS">RSS feed</a> as he comes out with some really good stuff. His <a href="http://blog.kirupa.com/?p=111" title="Randomizing Elements in a List (C#)">recent post</a> which showed a way of shuffling a List of strings (using C#) inspired a bit of thought on the topic of code readability, how and when it's learned (if at all) and why there is so little of it around.

Code quality and readability is something that isn't necessarily learned at University, nor is it something that can be mastered in a short period of time. It <em>is</em> something that <strong>anyone</strong> can learn. The main ingredients that are required are a bit of self-scrutiny, and the removal of the assumption that working code is the same as finished code.

<!--more-->

Let me start by applauding Kirupa for the effort he puts into his posts, and the level of quality he achieves. This is by no means a stab at his work. I'm grateful that he's happy to discuss how his code can be improved. It's rare to find someone who's open to constructive criticism! Like me, he appears to be keen to learn from other people - if only there were more of those people around!

To avoid confusion, I think I should clarify exactly what I mean by "Unintuitive Code". In a nutshell, what I mean is: <em>Code which fails to imply its purpose/functionality through poor use of language features, semantics, structure, naming conventions and natural language</em>. Phew!

I'd like to share my thoughts on the steps that you can take to aid in writing more readable and maintainable code, developing interfaces to objects that are intuitive, and imply the right kinds of things with regards to the way the functions operate.

<h3>Solve the problem first</h3>
This doesn't mean write code! This means get a pen and paper, or a white board and a marker! Write down what you think the problem really is and break it into smaller chunks which can be broken down into a set of logical steps. When you give yourself a clearer understanding of what the problem is you'll increase the chance of writing more meaningful code with a much clearer interface.

When you've broken the problem down into smaller chunks, it's highly likely you'll then find issues that you wouldn't have seen or considered if you'd just fired up your code editor first. This is good because you've now got a greater understanding of the problem space, and can now create a more meaningful and appropriate code architecture while keeping in mind the issues that you've already stumbled across. The amount of "coding in the dark" you have to do has already been minimised.

At this point you've already prevented the need to refactor your code before you've even finished the first iteration. Your interfaces will be cleaner, and the number of 'hacks' that you'll have to put in to fix things you hadn't initially thought of will be substantially reduced. Don't underestimate how valuable a bit of up-front analysis and design can help with keeping your code clean.

Once you're done with improving your understanding of the problem space, you're ready to open your editor and start writing code. Writing defensively from the start is a good idea, particularly if the code is going to end up being maintained by someone else.

<h3>Write your first iteration</h3>
Put your ideas down in code. Try your best to make an intuitive model, using <a href="/posts/creating-concrete-objects/trackback/" title="Creating Concrete Objects">concrete objects</a> where applicable, trying your best to reduce coupling, and separating implementation from interface. While this is a key phase, it's not as key as the next one! Make sure that when you're done writing your first version, you prepare yourself to be happy to rip out a large portion of what you write - because you're going to be replacing it with something better.

<h3>Call a spade a spade</h3>
If you want the marks, state the bloody obvious! That is, your funtions should state exactly what they do. I'm not suggesting that you have function names like <em>GetAReferenceToTheNormalVectorAndUseItToCalculateTheLightingForThePolygon()</em> as that's a bit over the top. Try to avoid names like <em>DoStuff()</em>, <em>ProcessData()</em>, <em>DoEvents()</em> (yes, that's <a href="http://msdn2.microsoft.com/en-us/library/system.windows.forms.application.doevents.aspx" title="DoEvents">a stab at you, Microsoft</a>) and <em>GetDoohicky()</em>. Names like <em>CalculateLighting()</em> and <em>GetNormal()</em> are descriptive enough and do a good job of implying their function. Don't mislead the user of your class by using names that quite clearly suck.

<h3>Review your interfaces and functions</h3>
There's a strong possibility that even though you've put a great deal of thought into your solution, you've still managed to imply certain things that you didn't necessarily mean.

The interface to an object and the signature of your functions can (and should) say more about your code than you think. So ask yourself the following:<ul><li>Does your object name reflect the functionality that it encapsulates?</li><li>Does the function imply operations that don't happen?</li><li>Does the function not imply functions that <em>do</em> happen?</li><li>Is the function's name descriptive of the type of function that it actually is?</li><li>Does your function return a meaningful value based on the context?</li><li>Does your function require intuitive parameters for it to function?</li><li>Is the parameter list as minimal as possible without affecting the functionality?</li></ul>I'm sure there are more, but I can't think of them off the top of my head.

When you've finished with your review, ask someone else to have a read of it. Ask them to give you a brief overview of your classes/functions as if you didn't know the code and they did. If they give a pretty good picture of what the object does, then you've done well. If they can't tell you what goes on at a high-level then you'll have to go through another review iteration.

<h3>Add comments that clarify, and remove comments that don't</h3>
I'm almost certain that every maintenance coder on the planet has been faced with the nasty combination of confusing code and invalid comments. It not only doesn't help, it makes things <em>worse</em>. Here is an example of a "WTF comment":
```
/* this class handles date formats */
class Profile
{
  // code goes here
};
```

If you didn't say "WTF!?" to the above code, then please go to the start of this post, and reread!

<h3>Iterate</h3>
Make sure that you don't stop working when the code starts working. Working code is <em>not</em> the same as finished code. Keep reviewing your work. Look for possible improvements. Be critical of your own work. Treat your own code as if it was someone else's and treat the task as though the result is going to be shown to a stack of other developers.

<h3>Be receptive to post-release feedback</h3>
When your code gets used by other people, you'll end up with more suggestions and possible tweaks to make it less unintuitive. Don't be ignorant, even if the suggestions are petty. Take them on board and see if you can utilise those bits of feedback to aid in production of even better quality code!

Good luck :)
