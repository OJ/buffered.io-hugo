---
categories:
- HOWTO
- Miscellaneous
- Tips/Tricks
comments: true
date: 2007-07-14T00:00:00Z
title: Help Me Help You!
---

<blockquote><p>Help!!<br />I need somebody...<br />Help!<br />Not just any body...<br />Help!<br />You know I need someone...<br />HEEEELP!</p></blockquote>
I have lost count of the times I've heard this (and not just when playing The Beatles). In fact, to be honest, I never kept count, but I know it's a lot.

On a daily basis I lose a fair bit of time helping my workmates. I get asked for help while I'm on IM from various "students" and friends I have around the web. I also get to help out family and friends on a regular basis face to face when I vist them or they visit me. I'm basically a constant source of solutions for I.T. and development related problems.

This is something I <em>like</em>! I enjoy helping people solve their problems. I enjoy helping them learn about something they don't know enough about. I like to see them take their new knowledge and apply it to other problems. It's a very satisfying experience.

<!--more-->

But there is one problem. The <strong>questions</strong>. For some reason, most people don't seem to think too hard before they speak, and the questions they are ask so esoteric, vague and/or broad that it's close to impossible to give a meaningful answer. This post is about those questions! The target of this post is anyone who needs to ask for help - geeks, non-geeks, students, etc. This will also help those who, on more than one occasion, have asked for help and received <u>none</u>.

<h3>An example of a bad question</h3>
<blockquote><p>Oliver, I have an issue with my code. It isn't adding the item to the list when a button is clicked, what's wrong?</p></blockquote>
Some of you might argue that this a fair question, and I'll agree, it <em>is</em> a fair question. But what it lacks is <strong>context</strong>.

My response to the above question was:<blockquote><p>OK, there are quite a few answers to the question, so can you please clarify a few things for me so I can give you a meaningful answer:<ul><li>What is "it"?</li><li>What is the "item"? Is it a primitive type, or a class you've created?</li><li>Is the application a fat-client app, or a web application?</li><li>Which button are you clicking?</li><li>...etc</li></ul></p></blockquote>
You get the idea. I could have answered straight away with a massive rant about the possible reasons why a given scenario results when a button is pressed, but no doubt it would have been a huge waste of time. Instead, I went for the other option, which was to get a better idea of the problem. After a bit of a digging around, it turns out that the above question <em>should</em> have been:<blockquote><p>Oliver, I have an issue with my web form application. When I press the 'add' button, it is supposed to create a new instance of type <em>Mytype</em>, add that new instance to a collection that is stored in the session, and then post back with the data table populated with the new list of items. When I press the button, the application posts back, but the contents of the list doesn't change. Do you have any idea what's wrong?</p></blockquote>
Man what a difference that made!

The essence of getting a good answer to a question is to make sure that the question is a <strong>good question</strong> in the first place.

<h3>An example of a not-so-bad-but-could-be-better question</h3>
<blockquote><p>hey, any idea where i could get an example html for world timezones? pref with country/region and offset?</p></blockquote>
Now this certainly isn't bad, and it is possible to deduce the intended meaning of the question from the question! But it turns out that the question should have been:<blockquote><p>hey, do you have any idea where i could get a list of timezones mapped to cities/countries which I can use inside a <em>&lt;select&gt;</em> tag in HTML?</p></blockquote>

<h3>The art of meaningful question construction</h3>
You wouldn't think that this could be considered an 'art', but it is. I'm sure there are lots of people out there who have written up a question on a programming forum only to find that it is totally ignored. Nobody responds, and your post simply makes it way to the bottom of the forum pile regardless of the number of times you bounce it to the top again. If this happens, you've asked a shit question. It's that simple.

There are only a few subtle yet important differences that make a question bad instead of good:
<ol>
<li><strong>Assumed knowledge.</strong> 9 out of every 10 questions that are asked are ruined by the fact that the asker assumes that the answerer knows enough about the problem space to just belt it out without too much difficulty. Well, here's a newsflash: <em>they don't!</em>. People who are spending time on forums, or at work, helping people solve their problems require more than just a generic set of information to be able to answer the question properly. In my view, you should always do what my old high-school English teacher told me before going into my final exams: "<em>To get the marks, state the bloody obvious!</em>". Thank you Mrs Price, you're dead right. Too much information on the topic is better than not enough. So be forthcoming, don't assume that the person you're asking is already in the loop.</li>
<li><strong>Vague terminology.</strong> In short, don't use words that don't make sense, that are vague, or that don't add to the meaning. Some examples are: <em>it, item, object, doohicky, thingy, whatsit, dooverlacky, thingamajig, woozit, etc</em>. Be specific. Be direct. Be helpful!</li>
<li><strong>Specific terminology.</strong> This is the total opposite to the previous point. Don't bombard someone with jargon, large words, and acronyms. Eg. instead of saying that you've broken your <em>pre-regulation destroyer-class solid-fuel recoil booster</em> you should instead use the word <em>spring</em>. Large words won't make you look smart, they'll just piss people off and prevent you getting the help you need.</li>
<li><strong>Give a good example.</strong> A lot of the time, the problem that you have is one that's come about from being in the depths of your code. If you can, break it out into a separate application, or "spike" as they like to call it in an <a href="http://en.wikipedia.org/wiki/Agile_software_development" title"Agile Software Development">Agile</a> world, and demonstrate the issue in a much cleaner environment/situation. You're more likely to receive help if you post a snippet of code than if you dump your entire application in source form on a forum.</li>
<li><strong>Think before you speak.</strong> You're in a rush. We know that. Everyone's always in a rush. People tend to jump headfirst into a question without really thinking about what they're asking, and it doesn't benefit them or the person they're speaking to. Take that extra 10 seconds to think about what it is you actually need to ask. When you're able to word in such a way that almost anyone would be able to understand you without prior knowledge, your question is ready to be asked.</li>
<li><strong>Keep it as short as possible without losing information.</strong> Yes, this point slightly contradicts the point I made earlier about too much information being better than not enough. But the point is that you needn't be verbose. Keep it as short as you can, but make sure you've got enough information out to make things clear. Pages and pages of text, and/or a 10 minute ear-bashing is enough to put anyone off helping you.</li>
<li><strong>Do a bit of research first.</strong> This is more for those technical problems than anything, but the rule does generally apply to anything. Chances are that someone else out there has had the same problem as you. It's highly likely that the problem has been solved, and there is information out there which is freely available should you make an effort to seek it out. If you can't find it after a few minutes of searching, then get your thoughts together and write up a decent question. One guaranteed way of pissing someone off is to ask them a question which can be answered by the first result of a basic a Google search. Make the effort to help yourself before you take up someone else's time.</li>
<li><strong>Try to solve the problem yourself first.</strong> This will not only give you a better understanding of the problem, but it will show that you've actually made an effort to solve the problem rather than just getting someone else to do your homework for you.</li>
<li><strong>Be polite.</strong> Be rude, be blunt, be expecting, and you'll be ignored (and rightly so). Don't expect help from people if you're not going to be courteous and/or use manners. If you get help, express appreciation. If someone takes the time to respond to get clarification on your problem, again you should make it obvious that you are grateful. They're on your side, so try and keep them there.</li>
<li><strong>Look at the question from the perspective of the listener.</strong> This is probably the most important point. While writing your question ask yourself:<ul><li>Do they know what *BLAH* actually is?</li><li>Do they need to know what *BLAH* actually is?</li><li>Are they aware of the environment I'm working in?</li><li>Would they need to know your hardware specs?</li><li>Should they be made aware of the language you're using?</li><li>Is there enough context in my question to remove confusion?</li><li>Is there evidence of the fact that I've tried to solve the problem myself?</li></ul>..the list goes on.</li>
<li><strong>Post a meaningful title.</strong> Again this applies in the online world when posting to forums. If you're going to write a post to ask for help, give some context to your problem in your post heading. Here are some things that you should avoid:<ul><li>HELP!! URGENT HELP REQUIRED!!</li><li>Stupid homework problem</li><li>Why won't this work?</li><li>I have 15 mins to finish this.. plz help!</li><li>GIVE ME HELP NOW!</li></ul>You don't have to be a rocket scientist to know why those titles are bad. Most regular forum users will just skip your post, and the moderators will probably tear you a new a**eshole for being stupid. Instead, try something like the following:<ul><li>Memory allocation problem using malloc() in C++</li><li>CSS problem with IE, works in Firefox</li></ul> These kinds of topic headings are going to not only help give context, but will most likely attract people who are experts in those respective areas.</li>
</ol>

<h3>Questions that should never be asked</h3>
There are some questions that should never, <em>ever</em> be asked.
<ul><li><strong>How do I write Quake XVI?</strong> You don't. You're not smart enough.</li><li><strong>How do I stop my console program from disappearing when it finishes?</strong> Ask <a href="http://www.google.com.au/search?q=C%2B%2B+pause" title="C++ pause">Google</a>.</li><li><strong>Where can I download *insert application name*?</strong> You don't. Go and buy it.</li><li><strong>How do I hack into Hotmail?</strong> The authors of Hotmail are much smarter than you. Instead, go outside and look at something shiny.</li><li><strong>Can you do my homework for me?</strong> Errr.. no.</li></ul>...etc.

<h3>A final thought...</h3>
Most of the time, people who give a great deal of thought to a problem before asking for someone's help end up solving the problem themselves. You may find that before you even get to voice your question, the answer becomes clear because you've put effort into defining your question!
