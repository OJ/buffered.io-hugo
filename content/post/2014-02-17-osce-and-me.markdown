---
categories:
- Security
- OSCE
comments: true
date: 2014-02-17T21:08:15Z
title: OSCE and Me
---

I have always found it hard to separate myself from something that I have a keen interest, and this has certainly proven to be the case when it comes to Information Security. My recent foray into the field, both as a developer and as a wannabe pentester/researcher, has had a big impact on me and my desire to learn more has not lessened as time has passed.

When I passed my [OSCP][] exam last year I really felt like I had achieved something, but more importantly I had learned a _lot_ of stuff that I had never had exposure to before. Despite the feeling of achievement and the exhaustion that came from trying to do it all in 30 days, I was keen to take the next step; riding the wave while I felt like I was getting into the swing of it. After a few days of post-OSCP rest, I started investigating the next thing to tackle.

Given the fun I had while doing the [PWB][] labs, I felt that going back to [Offensive Security][] would be a good move. Offsec's reputation is understandably very good, and I felt that I would be doing myself another favour by taking on their next challenge. It was clear to me that the next logical step would be to do the [CTP][] lab with a goal of attaining the [OSCE][] certification.

Just as I did prior to deciding to do OSCP, I read reviews and did some other simple research to find out more about the course. It wasn't surprising to hear horror stories from some people who had taken on the exam; some who passed, others who did not. I knew I was going to be in for a treat, and I couldn't wait to do it.

I would have signed up within a few weeks of finishing OSCP, but I felt it was important to give my family a break from the stresses involved in me dedicating so much time to study. Not just that, the OSCE exam is a gruelling 48-hour marathon, and so I would require support from my legendary little clan if I wanted to succeed.

After chatting to my very understanding wife, we decided that it would be a good idea for me to do the course in the latter half of the school holidays when the pressure of daily life is off. I signed up for the course, ready to start on the 22nd December.

What follows is my OSCE story, from start to finish, written in a very similar to my [OSCP][oscp_and_me] post. Enjoy.

<!--more-->

Signing Up
----------

The fun of OSCE starts before you even get to sign up. Registration is not possible without first completing a small challenge and submitting the result of that challenge (a registration code) along with your registration request. Without a "Registration Code" and "Secret Key" you cannot register for the course.

The challenge is located at [http://www.fc4.me/](http://www.fc4.me/) and is worth taking a look at.

When I first saw this challenge it was quite a while before I was due to sign up. However, I wanted to complete the challenge early so that I was prepared for registration day. I decided to not only complete the challenge to obtain a registration code, but I would automate the process!

After `X` minutes (where `X` is greater than `10` and less than `60`) I had figured out the challenge and had written a script which would complete the challenge for me at the click of a button.

This really set the tone of the OSCE experience for me, and I was really looking forward to diving into the lab material.

Getting Started
---------------

Many of the reviews I had read of OSCE had placed a great deal of emphasis on the difficulty of the course. While this was slightly intimidating, it was also very appealing. I have to admit that I also felt a little more comfortable with the subject matter than I was with OSCP.

Why? Because during my misspent university days I lived inside tools like [SoftICE][] and [W32Dasm][] reversing software copy protection mechanisms and learning how things work. During my time as a paid coder I have been fortunate to have a fair few jobs that require me to be quite low level, requiring me to debug assembly or some pretty rough C. I even had the fun task of optimising pixel-shader assembly for [EA][].

The thought of diving into this stuff again didn't scare me as much as the thought of hitting a totally new field (like it did with OSCP), and so when the materials arrived on the morning of the 22nd I was pumped and ready to go.

As with OSCP, this course contained a large PDF document packed with information. This document was accompanied by a number of tutorial videos which had some overlap with the content in the PDF, but wasn't exactly the same.

I noticed that the course material was broken up into modules, which was not the case with OSCP. Each module talked about a particular type of attack or exploit, and dived deeply into the topic while exploiting a well-known application to demonstrate the technique in question.

I decided to start by skimming the document, making simple high-level notes and then going through the videos one after the other.

The process I wanted to follow for each module in the lab was:

1. Watch the videos for the entire module.
1. Make notes of the stuff that I had learned while watching.
1. After watching, I would compare those notes to the high-level notes I took while skimming the document.
1. If there was a noticable difference between the video and the document notes, I would review the document for that module to fill in any gaps that I might have had.
1. I would study my own notes in preparation for practice.
1. I would then attempt to recreate/replay the technique without depending on the original course materials (this wasn't always possible, despite my efforts).

It was with this approach that I charged into the material.

The CTP Lab
-----------

The lab set up is nothing like that which is made available during OSCP. You are allocated a number of machines that you can use and nobody else is able to use them. The number of machines is substantially smaller, but this makes perfect sense given the content of the course. The machines are not there for mass pwnage, but are instead there for you to learn from, practice on (repeatedly) and hone your skills on. This wouldn't be possible if the machines were shared by students all vying for use of the same tools at the same time.

I won't go into detail regarding the number of machines, what operating systems they run or what software is installed, as I think that information should be privvy to those taking the course. However, I will say that there's more than enough fun to be had on those boxes while you make your way through the lab material.

Areas of Focus
--------------

There are a few main areas of interest in the CTP labs, and they are:

1. **Backdooring portable executables** - This is the process of taking a binary, such as `Notepad.exe`, and making it do other things on top of what it was intended to do. This might mean making it invoke a reverse shell before continuing on its merry way. Some people might find this rather simple if they have been mucking with PE files for a while, but there are still some interesting things to be gained from going through the material.
1. **Antivirus evasion** - Every pentester loves a bit of AV bypass in their lives. This area of the course material dives into some of the techniques which can be employed to avoid having "tools" (or "malware" if you prefer) being caught in the antivirus net. There's a common myth out there which says that the CTP labs teaches old or outdated techniques, however I can say that based on the stuff that I learned from the course material (along with some extra self-study) I can now disguise known malware and receive 0 hits on Virus Total. In short: the techniques are definitely relevant today.
1. **Advanced web application testing** - This was the area that scared me the most, because as I've mentioned my background is not in penetration testing. I still have a lot to learn in the web pentesting space and I was grateful for the content of this section. It covered some really interesting and crazy approaches which allow for exploitation of certain types of vulnerabilities. The focus is placed on not just getting data, but getting shells. And everybody loves shells! I got a lot out of this material, but I knew that I'd need to do more prior to taking the exam.
1. **Exploit development** - There's no denying that the bulk of the course is related to exploit development. There's a lot of material, and a lot of emphasis here. It's enlightening, enjoyable and challenging at the same time. Muts takes you through some very interesting bugs in well-known (and not-so-well-known) software and demonstrates some rather ingenious ways to exploit those holes. This was the section I had the most familiarity with as I've spent a great portion of my life staring at a debugger.
1. **Network-level attacks** - This is the leanest of all the modules, but again there's some really great information in here which taught me a great deal about things I didn't know much about. In fact, I'd never really done any form of network-level attack prior to doing the course, and so this section was very welcome.

As always Offsec throw in some curly things that require you to go away and research by yourself before you can finalise the modules. Just like with OSCP, extra self-paced study is required to get a really good grasp of what's going on so that you are prepared for the exam.

What I did
----------

I followed my plan quite strictly and spent a lot of extra time reading supplimentary material that I found online. When I completed sections of the material I wrote lots of notes (in Markdown) and stored them in private Git repositories. I took what I learned and applied that to other things to get more practice.

I have to say that practising on other targets is an absolute must. Don't just rely on what the course shows you. It's a great idea to download software that has known holes and go ahead and find them yourself, investigate them yourself, exploit them yourself. There are a lot of applications out there that are great candidates for practice, so if you're going to do CTP and aim to get through the OSCE exam, then be sure to allocate time to this kind of practice before you take the test. It really was a good move for me, and I learned a lot of stuff which I ended up making good use of during the exam.

By trade I automate mundane, boring and not-so-boring tasks. I make computers do things that humans tend to suck at. I just happen to be human so I too suck at those things. The fact that I knew this while working through the course pushed me to automate almost everything that I learned. I wrote tools, scripts, and full applications which automated much of the pwnage explained in the lab guide.

It's one thing to know how to do something manually, but for me it's a whole other thing to get a computer to do it for me. I find it really helps me understand the process, and results in a tool which is way better and more consistent at doing these things than I will ever be. My recommendation to OSCE wannabes is to not be scared to attempt to automate what you can. You learn a _lot_ from it, and the tools you get at the end are _really_ helpful.

I went through the course material twice, and then picked about half the material to go through a third time. I managed to do this within a 30-day period, so I was happy that I didn't buy more time. Before my lab time had expired I booked my exam for a time which wasn't to far away from the end of the lab time yet far enough away to give me time to practice. This worked very well for me. By the time the exam came around I was feeling quite confident in my abilities and I was ready to take the challenge.

The Exam
--------

The OSCE exam has a bit of a reputation, and rightly so. You are given a total of 48 hours to complete 4 challenges, and then another 24 hours to finalise and submit your exam report with your findings.

At 6am on January the 31st my exam lab connectivity pack landed and I was pumped, ready to go! I was also scared. Just like last time!

I had a couple of early wins on the first day and somehow managed to nail the first two challenges within three hours. I was feeling good, confident and it was early in the day so I was riding high. The third challenge was quite a bit trickier for me and took much longer. Prior to taking on this challenge I was scared that this would be the one I was struggle the most to nail, but thankfully it didn't take me too long. Six hours later, I had managed locate the flaws/bugs/issues/etc which allowed me to nail the challenge and move on. So after 10 hours I felt I was really ahead of the game.

Then came the last challenge. The one that I thought I'd be prepared for the most.

Ouch. For a good eight hours this thing served me my ass. I felt like I wasn't getting anywhere and I was starting to get frustrated. I knew I'd had a solid day standing in front of the machine with the other challenges and so I decided to get to bed to try to get a good sleep in before getting up early and going again. This was a good idea, as I wasn't really getting anywhere and my frustration was building to a point where I wasn't thinking straight.

Fast forward to the next day, 6am (post-breakfast and post-shower, but pre-tea). I had a mixed sleep as my mind was racing; trying to find other pathways or approaches that I could use to tackle the problem. I'm pretty sure this was pointless, because when I got back to the machine, I didn't really have much of a clue of what to do.

I pushed on reworking scripts, automating things, and generally "Trying Harder". It wasn't until about midday (another 5 to 6 hours later) that I managed to locate my first foothold.

At this point I thought I had it made. I saw the entry point, I saw the potential ways to abuse what I had learned and I thought that the process would be a simple 1..2..3.._pop_! But again I wrong and Offsec showed me again how devious they could be.

During the rest of the day I built no less than 10 different "attacks" (I'm deliberately using vague words here so that I don't give anything away), all of which would work nicely in my test environments but would not work at all on the client. 2pm came. 4pm. 6pm. **9pm**. Idea after idea came and went, many working locally, none working remotely. It was driving me mad. It was making me feel _really_ stupid. I was hating it and loving it at the same time.

At 9pm something came to me, almost out of nowhere. It might have had something to do with the fact that I was sweaty, running on adrenaline, and my brain was going a bit bonkers. I had a moment of clarity and came up with something that was a little out of left field, which I admit was influenced by my development background a little.

When this happened I was both excited and scared; excited because I had another idea, but scared because I felt that if this didn't work I'd be out of ideas and running out of time.  Implementing this idea wasn't easy, for reasons that I won't allude to because I really don't want to spoil anything. Suffice is to say that I had to do a lot of rejigging to make it work, and I had to abuse certain properties of the target as well. As I was finalising my attack I was thinking to myself "I've got 60 points, 75 are needed to pass, and this target is worth 30. If I don't get this, I'm screwed." It really didn't help adding the pressure to myself.

A 10pm that night, the 13th attempt at popping the target succeeded. I was ecstatic and elated. I ran around the house cheering and wooting, and I almost woke the kids. My hands were shaking as I pulled the loot from the target but the relief was immense. 90 points down out of 90. I felt amazing.

Post-exam
---------

Thankfully I was able to go to bed by 10:30pm, but more importantly I was able to sleep. Had I not nailed that last challenge I would not have had a restful sleep. I woke up the following morning with a grin from earhole to arsehole; I had a huge sense of achievement.

I spent about 3 hours on my report and submitted it the following evening. It took just over 24 hours for Offsec to get back to me with the following:

> We are happy to inform you that you have successfully completed the Cracking the Perimeter certification challenge and have obtained your Offensive Security Certified Expert (OSCE) certification.
>
> You will receive the certification by mail within 80 days. 

Achievement unlocked. They event sent through the following images to include in my online profiles:

![OSCE](/images/offsec-student-certified-emblem-rgb-osce.png)

Was it hard?
------------

Yes. It was. It was hard for a few reasons:

* Covering the course material and doing self-paced study and research at the same time is intense.
* Lots of hours of the day need to be spent, unbroken, on many areas of the content so that you can properly digest it.
* Some of the content can be tricky, and mind bending.
* The stuff you need to practice is finnicky and can be really hard to get working. Some people have it easy, and things "just work", other people have very different experiences.
* The exam was tougher than I thought it would be, and it pushed me really hard. I am very happy that I was able to pass first time.

Should you do it?

Absolutely you should.

Thanks to my little clan
------------------------

My wife and kids rock. Seriously. Doing the OSCE certification took quite a bit of time and effort, and I had to go dark quite often to get through it. The support that my wife and kids gave me was amazing. My wife brought me breakfasts, dinners and organic coffees without me asking. She's an absolute treasure. My kids knew I was doing an exam and that it was "important", and so they kept the noise down and did their best to have fun without being distracting. This is a _very tall order_ for kids that are 6 and 4 years old.

I'm eternally grateful to all of them. They're awesome.

What's next?
------------

With OSCP and OSCE out of the way, the question I have is what to do next? I would really like to start working what I've learned and do some penetration testing, automation of attacks, exploit research and development, etc. I am also going to get my head down and work on a product idea with a friend (more news on this later). I promised the family that I would take a break from certifications but I am keen on learning more and expanding my skillset.

I am hoping to convince [CorelanC0d3r][] to come to Australia and do his training, and I'm also keen to get some Offsec people here to do on-site training for [OSWE][] and/or [OSEE][]. In any of those cases I need to find Aussies, New Zealanders and other folks within a reasonable distance to put their names down as interested parties. Without that, there's no chance of it happening any time soon. Please contact me if you're interested in any of the above!

Also, contact me if there's anything else security-related you'd like to talk about.

Thanks again for reading. Until next time!

  [oscp_and_me]: /posts/oscp-and-me/ "OSCP and Me"
  [PWB]: http://www.offensive-security.com/information-security-training/penetration-testing-with-kali-linux/ "PWK (was PWB)"
  [CTP]: http://www.offensive-security.com/information-security-training/cracking-the-perimeter/ "Cracking the Perimeter"
  [OSCP]: http://www.offensive-security.com/information-security-certifications/oscp-offensive-security-certified-professional/ "OSCP"
  [OSCE]: http://www.offensive-security.com/information-security-certifications/osce-offensive-security-certified-expert/ "OSCE"
  [SoftICE]: http://en.wikipedia.org/wiki/SoftICE "Numega Softice"
  [W32Dasm]: http://www.softpedia.com/get/Programming/Debuggers-Decompilers-Dissasemblers/WDASM.shtml "W32Dasm"
  [EA]: http://www.ea.com/ "Electronic Arts"
  [CorelanC0d3r]: https://twitter.com/corelanc0d3r "CorelanC0d3r"
  [OSEE]: http://www.offensive-security.com/information-security-certifications/osee-offensive-security-exploitation-expert/ "OSEE"
  [OSWE]: http://www.offensive-security.com/information-security-certifications/oswe-offensive-security-web-expert/ "OSWE"
  [Offensive Security]: http://www.offensive-security.com/ "Offensive Security"
