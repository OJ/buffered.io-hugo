---
categories:
- Security
- OSCP
comments: true
date: 2013-08-16T00:00:00Z
title: OSCP and Me
---

The [PWB][] course by [Offensive Security][] is absolutely awesome, as is the exam which earns you the prized [OSCP][] certification. I took this course and exam recently; I loved it and I nailed it! I am now equipped with a much better understanding of the security world and am in a better position to help businesses improve the security of their application architecture and infrastructure.

[Hit me up][contact] and let's talk about how I can help you make your applications more secure.

What follows is the full story of my path through PWB and OSCP. Enjoy.

<!--more-->

The Long Version
----------------

I started developing software professionally back in 1999, just shy of 15 years ago. Since then I've been fortunate enough to work in some pretty amazing domains, used a massive variety of technologies and have played a part in some [pretty][burnout360] [awesome][internetbanking] [software][mdpe]. I'm proud of what I've achieved so far. However I've recently found myself looking for a new challenge. Something that will make me think, push me to learn new things, and hopefully keep my motivation levels up to a high level. Ultimately, I wanted to have a bit of a career change without really changing my career.

Towards the end of last year, after some careful deliberation, I decided to follow a long-term passion of mine with a goal of incorporating it into my work. That passion is [Information Security][]. Infosec as a domain is made up of a _lot_ of different areas, and hence making it a point of focus means that anyone looking to get involved needs to first learn the basics of a broad set of topics and then, perhaps, _specialise_ in one of them.

For many years I've [dabbled with reverse engineering][blog_rce], kept up-to-date with various security topics, and have been quite a [security-minded developer][blog_xss_mvc]. I didn't want to just go and read up on the things that I already knew, but instead I wanted to throw myself in the deep end into an area that I wasn't too familiar with but had a keen interest in.

This led me to [penetration testing][] and ultimately to [OSCP][].

Exploring OSCP
--------------

I spent quite a bit of time searching for courses and material that would get me going with the basics of penetration testing servers and web applications. There's quite a lot out there, though much of it is rudimentary, is disjoint or lacks cohesion, or talks about "point and shoot" exploitation. While this might be helpful in becoming a [script kiddie][] it wasn't what I was looking for. I wanted to be challenged.

When I stumbled on [OSCP][] I was initially dubious as I am when it comes to any certification. Most developers out there are probably experienced with the certifications that exist in the world of software engineering and how they do not provide an indication of a person's ability. Before knowing better I assumed this would be the same.

However, after [reading](http://proactivedefender.blogspot.com/2012/01/oscp-my-review.html) [some](http://www.hackyeah.com/2010/12/brief-review-of-the-pwb-class-and-the-oscp-certification/) [reviews](http://blog.nullmode.com/2013/05/penetration-testing-with-backtrack-oscp.html) from people who have achieved the certification, I started to realise that this is exactly the kind of thing I was looking for.

Quotes from these reviews include lines such as:

> The truism "anything worth having doesn't come easy" is one I have often remembered when on a particularly difficult path to a goal. Never have the words rung quite so true when applied to my quest for \[the\] OSCP certification.

Others state:

> The OSCP certification, in my opinion, proves that it's holder is able to identify vulnerabilities, create and modify exploit code, exploit hosts, and successfully preform tasks on the compromised systems over various operating systems.

I was becoming convinced that OSCP was something that I had to do, despite not ever doing any form of penetration testing in the past (other than fumbling around my own web applications). At this point I reached out to a local security professional, [Ash D][ash_d] who is a seasoned Infosec guru, SANS mentor/teacher and who has passed the exam himself, and bribed him with a free lunch to come and talk to me about his experience. He turned out to be friendly, fun, informative and gave me just the confirmation I was looking for. Even if I failed miserably I'd already made a good friend out of the experience (a friend who later would keep encouraging me right through to the end [and beyond][ash_blog], thanks Ash).

OSCP requires you to spend a lot of time in a virtual lab practising the various techniques that you'll need to master to do well in the exam. While the learning material from Offensive Security is good (more on this later), the lab is what makes the whole thing **great**. This was the last key point for me. I wasn't just going to learn theory, I would actually learn to _do_ things and have to _demonstrate_ that in the exam to gain the certification.

Getting Started
---------------

It was early 2013 and I was working some longer hours for various clients which made it hard for me to find the time to put into some basic preparation. As a result it took me quite a while to sign up for the course as I wanted to make sure that I would be able to give it the time it deserves.

As April 2013 approached I came to the realisation that it was _never_ a good time to sign up for something like this and hence I should just go and make it happen. I spoke to my wife and kids about it and made sure they were OK with the idea of me being locked away to learn, and they gave me their full support (short interlude: my family is awesome).

I jumped on the web and went to the [sign-up][] page and was presented with quite a few options. I needed to specify how much lab time I wanted prior to my exam. This was a bit like telling a mobile phone carrier how many calls you're going to make; I really wasn't sure! Instead of deliberating for too long, I decided to go with the 30-day option and extend my time if I felt that I needed more. No big deal.

I signed up for the course, and locked in the date of June 16th to kick off my time in the labs.I was really excited, and couldn't wait for it to start. In the following days I was contacted by Offsec and asked for proof of identity. Offsec require that you don't use a "free" email address such as Gmail when signing up, however I don't have any email addresses that aren't Google Apps hosted (this will change soon) and hence I needed to verify my identity with them. This goes to show that they don't just let any unknown person take up the course to learn things which can easily be misused.

After what felt like an eternity, June 16th came around and my connection pack arrived in my inbox. Unfortunately for me, work and a few other things took over and I lost the first week of time. I was able to connect to the labs on the odd occasion but for very short periods of time. Given that I wasn't yet across the material that time I spent in the labs wasn't really fruitful.

At day 8 things finally settled down to a level where I was able to dive into the material and begin fumbling my way through the lab. This is where things really started to become entertaining.

Initial Lab Time
----------------

My first few days in the lab were interesting. I popped the SYSTEM account on one of the Windows boxes in the first 4 hours of my lab time. This was not only surprising but it gave me a confidence boost which didn't do me any favours. From there I failed repeatedly to compromise another machine. The second day yielded no results. The third day was also fruitless. By the end of the fourth day, when I still had just _one_ machine on my tally, I was beginning to ask myself questions. Am I cut out for this? Have I bitten off more than I can chew?

I stepped back for a while and pondered my approach. I realised that I wasn't thinking and looking to learn. I wasn't approaching the problems like a hacker would. I wasn't doing enumeration properly. I was investing too much time looking for out-of-the-box exploits rather than trying to connect the dots myself. I gave myself a slap, and started again.

This is when things started to change. Machines started to fall. I started to learn more. I improved in all the areas I was failing at before. It was wonderful.

On the fifth day I managed to pop 7 machines. What a difference!

Proper Lab Time
---------------

_Note: my coverage of the lab from here is "point in time". Offsec change and upgrade the lab all the time, and hence details of the lab and what you'll experience will change over time too._

The lab was a wonderful place to play, practice and learn. I was constantly blown away by the mixture of operating systems, patch levels, kernel versions, system application and feature versions, third-party applications, and even custom applications that had been built which emulated the kind of things you'd expect a developer to throw together to help them do something a little easier and quicker as part of their day job. The effort that has gone into the design and set up of the lab environment is commendable. It really felt like I was in a real network with real machines and real people using those machines.

It was made up of approximately 60 machines partitioned into a number of networks. Each machine has it's own identity and story, some are interesting and some not so much. I'm not going to elaborate on the detail too much because discovering that is all part of the fun. But I will say that I experienced `Pain` and `Sufference` \[sic\] in ways I didn't expect.

The networks are connected together in interesting ways, and you as the penetration tester will need to work your way through the machines and networks, pivoting your attacks off compromised hosts as you go with the end goal of compromising all of the hosts and making it to the **Admin** network.

As you would expect, some machines are very easy to break into and other machines are really quite hard. The beauty of the lab is that, depending on your background, exposure and interests, the machines that you find hard might not be hard for others and vice-versa. Some machines run applications you wouldn't expect them to run. Some machines have very new or very old configurations of software. The mix is truly great; it keeps you thinking rather than giving you the luxury of slipping into a "routine".

The lab exposes you to a very large range of exploitations; too many to mention here. Chances are that if a type of exploit exists that you need to abuse, you'll get the chance to use it in the lab.

At this point I think it's important to point out that in OSCP the focus is on knowing how to apply existing public exploits and known approaches to manual exploitation of vulnerabilities. While there is room for you to construct your own exploits if you choose to, there is another course offered by Offsec which covers that in more detail, and that's [OSCE][].

It took me a little bit of time before I realised that exploitation wasn't the only thing that I needed to do while in the lab. _Post-exploitation_ was very important. Looting the machines that I'd compromised was something that I didn't consider doing in the early days, and I suffered as a result. When I came to realise that the keys to some of the machines were located on others, I had to go back through my list of popped hosts and loot them properly. If you're going to do this course, make sure you do a good job of post-exploitation!

While doing the lab, you are supposed to keep track of all the work you've done as you need to provide a deliverable at the end of it: a full penetration test document. This document has to contain the detail of what you did and how you did it. My advice is to not put this off until the end, but instead work on it as you go. This includes screenshots, dumps of console output, source code to exploits you've written and scripts you've used to automate tasks.

Even though I was pretty good at taking notes, I had kept them all in markdown in a private [git][] repository instead of putting them in a well-structured document. As a result, I had to do this after my exam, which made the experience more painful than it needed to be.

Post-Lab
--------

By then time your lab time has ended, you _should_ have managed to compromise/pop/pwn a large percentage of the networks, if not 100%. In my case, I ran out of time and I missed about 8 machines in total. While I was disappointed with the result, a discussion with various OSCP alumni led me to realise that I had managed to defeat the harder machines in the labs and hence the rest of them would be quite simple. I decided not to extend my lab time as spending extra money for the sake of a few more machines didn't make sense. Instead, I booked in my exam for August 6th and used the lead-up time for practice, refining my documentation, and preparing my scripts and cheat sheets for the big day.

There are some great resources out there for practising this kind of thing, but the main one that I want to point out is [Vulnhub][] (a pet project of [g0tmi1k][]). That site contains downloadable _boot2root_ images that you can use to practice on with the added benefit of it all being legal.

I really focused on the areas that I felt I was weakest, with the main one being Linux privilege escalation. There's a bit of material out there on it, and there's also g0tmi1lk's fantastic [cheat sheet][gotmilk_privesc], but I still failed to connect some of the dots during my travels. It's about now I must give a bit hat-tip to [Pipes][] and [Metlstorm][], both of [Insomnia Security][], for being two awesome mentors and providing me with fantastic insights on ways to get root. Those guys are awesome.

The Exam
--------

The OSCP exam is a 24-hour "loser takes all" style exam. You are given access to a custom network, just like you are in the labs, and you have a number of machines assigned to you. The exam pack contains information on the machines, along with various rules that you must adhere to when attacking them. Each machine is worth a number of points and you earn those points if you:

* Compromise the host.
* Document your findings well enough with clear instructions on how it was done. This document should also contain the  "flags"; text files with what appears to be random characters in them which prove that you did what you did.
* Don't break the rules.

Points are allocated to you even if you don't managed to get SYSTEM/root on the machine. All is not lost if you can't do privilege escalation! However you _can not_ break the rules. In my exam, for example, there was at least one machine which wasn't allowed to be attacked at all using [MSF][]. Breaking this rule would have meant 0 points for that machine.

I started my exam at 8am just as the family were leaving the house (taking son #1 to school). I was buzzing. I couldn't sit still I was that excited. I realised how much I had missed my time in the labs and the thought of having another crack was making me twitchy.

By 8:45am I had popped root on my first machine.

By 10:00am the second had fallen. I was on a roll! I was riding high and felt really good.

The third machine proved to be a little more difficult, but it fell just before 1:00pm.

The last two machines, making a total of five, were quite a bit trickier. I'd say that the fourth was by far the hardest, but I loved it as it was yet another example of where I learned something new while doing something in an Offsec lab. I w00ted like a teenager when it fell and did a victory lap around the house.

By 10pm I was done with all 5 machines. My notes were in the typical markdown/git repository structure but were quite thorough and had captured everything I had done in quite a lot of detail. With the exam out of the way, and 100 points in the bag, I went to bed.

The following day I had the arguably arduous task of writing my exam document, which was supposed to be included with your lab report. All in all my document totalled 220 pages by the time it was done and I was glad to see the back of it! It was submitted late on Wednesday evening but with plenty of time to spare before the 8am Thursday cut-off.

I was done. I was relieved, excited and sad that it was all over.

Confirmation
------------

While I was pretty sure that I'd done a good enough job to get my certification, there is always some doubt that you might have missed something or done something silly. So until I received confirmation from Offsec I wasn't sure if I had passed.

It took less than 48 hours for me to receive the email:

> Dear Oliver,
> 
> We are happy to inform you that you have successfully completed the Penetration Testing with BackTrack certification challenge and have obtained your Offensive Security Certified Professional (OSCP) certification.
> 
> You will receive the certification by mail within 80 days. 

I was elated. What a journey! I really felt like I had achieved something. I truly felt, and still feel, that I'd be able to do a great job performing a penetration test for a client.

Summary & Conclusion
--------------------

There's no denying it, OSCP was just fantastic. But same parts of it were _hard_. Not impossibly hard, but hard enough to make you question your own abilities. It broke my ego and then built it up again. I can't recommend it strongly enough!

However if you're new to the security game, this probably isn't the first thing you should attempt to tackle. I think I managed to get through thanks to my polyglot development background, my history with a mixture of operating systems, and my time reverse engineering various binaries. Without those things I would have had a very, _very_ hard time.

I'm very happy that I took this challenge on. I feel like I really achieved something and that I have a certification that means a whole more than the paper it's written on. I can't wait to see the official paperwork come through the mail.

So what's next? Certification-wise it has to be [OSCE][], but I will give my family a break before I take that on. Work-wise, I will be looking to engage with new and existing clients to determine how best I can help them with the security of their infrastructure and applications in the hope that this becomes a major part of my work longer term.

If you're out there reading this and you are looking for some help in this area, please [drop me a line][contact].

Thank you all for reading. Feel free to hit me with any questions and I'll do my best to answer them.

[OJ][]

  [Offensive Security]: http://www.offensive-security.com/ "Offensive Security"
  [PWB]: http://www.offensive-security.com/information-security-training/penetration-testing-with-backtrack/ "Penetration Testing with BackTrack"
  [OSCE]: http://www.offensive-security.com/information-security-certifications/osce-offensive-security-certified-expert/ "Offensive Security Certified Expert"
  [OSCP]: http://www.offensive-security.com/information-security-certifications/oscp-offensive-security-certified-professional/ "Offensive Security Certified Professional"
  [penetration testing]: http://en.wikipedia.org/wiki/Penetration_test "Penetration Test"
  [OJ]: https://twitter.com/TheColonial "OJ on Twitter"
  [sign-up]: http://www.offensive-security.com/information-security-training/penetration-testing-with-backtrack/ "PWB sign up"
  [ash_d]: http://security.crudtastic.com/ "Security with added Cheese"
  [ash_blog]: http://security.crudtastic.com/?p=699
  [blog_xss_mvc]: http://buffered.io/posts/xss-flaws-via-mvc-model-binding-and-request.querystring-inconsistencies/ "XSS Flaws via MVC Model Binding and Request.QueryString Inconsistencies"
  [blog_rce]: http://buffered.io/categories/rce/ "Category: RCE"
  [burnout360]: http://burnoutrevenge.ea.com/360/player.asp?language=en "Burnout Revenge for Xbox 360"
  [internetbanking]: https://internetbanking.suncorpbank.com.au/ "Suncorp Internet Banking"
  [mdpe]: http://www.innovation.gov.au/Industry/Defence/CapabilityDirectories/Documents/JSFCapabilityDirectory/company%20profiles/ball_solutions/profile_3.html "Mission Data Planning Environment"
  [MSF]: http://metasploit.org/ "Metasploit"
  [gotmilk_privesc]: http://blog.g0tmi1k.com/2011/08/basic-linux-privilege-escalation.html "Basic Linux Privilege Escalation"
  [Pipes]: https://twitter.com/pipesec "Pipes on Twitter"
  [Metlstorm]: https://twitter.com/metlstorm "Metlstorm on Twitter"
  [Insomnia Security]: http://www.insomniasec.com/ "Insomnia Security"
  [Vulnhub]: http://vulnhub.com/ "Vulnhub"
  [g0tmi1k]: http://blog.g0tmi1k.com/ "g0tmi1k's blog"
  [git]: http://git-scm.org/ "Git"
  [Information Security]: http://en.wikipedia.org/wiki/Information_security "Information Security"
  [script kiddie]: http://en.wikipedia.org/wiki/Script_kiddie "Script Kiddie"
  [contact]: /contact "Contact OJ"

