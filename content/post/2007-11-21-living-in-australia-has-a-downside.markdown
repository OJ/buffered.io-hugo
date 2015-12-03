---
categories:
- Microsoft
- Networks
- Technology
comments: true
date: 2007-11-21T00:00:00Z
tags:
- internet
- Microsoft
- Networks
title: Living in Australia has a Downside
---

This may come as a surprise, but living in Australia <em>can</em> have it downsides :) Right now I can only think of one, and that's (part of) the reason for this post.

We may have constant sunshine, lovely beaches and a lifestyle to die for, but our broadband just <strong>sucks</strong>. That's a fairly sweeping statement, so let me clarify a little more.

Broadband plans in this country are <em>extremely</em> poor value. The service that you receive for the amount you pay is substandard. It's that simple. Those of you in places like the UK, USA and Hong Kong may find it obscure to consider that a country exists where you can't get an unlimited data connection for DSL. You may also find it difficult to believe that some connection speeds are cut to that of a dial-up connection if you download too much. But that's exactly what happens here.

<!--more-->

These days in Australia most ISPs provide a limited download quota per month for a set fee. What happens when you download too much will vary from provider to provider. Many of the providers simply cut your download speed so that it feels like you're back on a dialup modem. Others simply charge per MB or GB that you go over (and they're not cheap either!). What's "nice" about the latter option is that many of these ISPs don't actually let you know that you've gone over quota. So instead of being warned, you just cop a huge bill at the end of the month. Yes, I believe it's your job to monitor your bandwidth usage, but that's still pretty shoddy.

Every 6 months or so I have another look around to see if I can find another provider that will offer me more than what I currently have. Right now I have a connection through <a href="http://www.internode.on.net/" title="Internode">Internode</a>. They provide me with an <a href="http://en.wikipedia.org/wiki/ADSL" title="ADSL">ADSL</a> connection at 1.5mbps/256kbps with 10GB download per month. If I go over that my speed is "shaped" to 64kbps/64kbps. All of this is delivered down my copper wire for the princly sum of AUD $54.95 per month. No, that doesn't include the cost of having a phone line in the first place.

In day to day life for me this is actually adequate. I don't download too much stuff, and most of my browsing (and the wife's) is fairly light on the bandwidth. But every now and then I feel the pinch and the frustration that comes with my plan...  <ul><li><strong>Windows Updates.</strong> I'm sure that every user of Windows is aware of the joys of constant updates from Microsoft. Some of them are small, some of them are absolutley <em>massive</em>. Multiply these updates by the number of computers in your home, and you've got quite a hit on the quota.</li><li><strong>Linux Updates.</strong> I just happend to dual-boot Windows XP x64 with <a href="http://www.linuxmint.com/" title="Linux Mint">Linux Mint</a>, and so I have the added benefit of constant updates through that as well. Thankfully I can pick and choose what I want updated, and most of the time the updates are reasonably small (<em>and</em> they don't force me to reboot my machine).</li><li><strong>Steam.</strong> I have a <a href="http://www.steampowered.com/" title="Steam">Steam</a> account which I use to buy pretty much all the games that I play (which aren't many). Most games these days are also bloody massive. Get yourself a copy of Bioshock and Half-Life 2, and you've nearly blown your monthly quota away. Add to that the updates to the Steam platform, and some of the media/demos you can get for free, and your quota begins to look pitiful.</li><li><strong>Development.</strong> Yes, I'm a Geek. I write code. I need tools. I need to keep up to speed. I have a subscription to things like <a href="http://www.microsoft.com/msdn/" title="MSDN">MSDN</a> so I can get access to this stuff. Everything that I need has to be <em>downloaded</em> and hence is yet another huge hit on the download quota.</li></ul>There's obviously some motivation for this post, and strangley enough it's due to the bandwidth issue <em>and</em> MSDN.

Those of you who are reading this are probably geeks too. So you'll know that recently Microsoft decided to release <a href="http://msdn2.microsoft.com/en-us/vstudio/products/aa700831.aspx" title="Visual Studio 2008">Visual Studio 2008</a> which comes with all the creamy goodness of .NET 3.5 and C# 3.0. Not only was I interested in getting this because I wanted to play with it, but I <em>needed</em> it for work. So a download was required (all 3.31GB of it).

Initially I thought I'd download the disc image while I was at work. I started the download off yesterday morning when I got into the office only to find ... work suffers from download quotas issues just like the rest of us! It appeared that our bandwidth quota had been blown, and the download speed was being shaped (ie. I was getting a massive 1.2Kb/s off microsoft.com, not good for a 3.31GB download).

Fair enough, I know what that's all about. So I decided to wait til I get home and fire the download off on my home machine over night. When I got home I jumped on MSDN, fired up the download and away it went at 160Kb/s! SWEET! Shame we're stuck using the stupid MSDN downloader application, but hey, it appears to support resuming of downloads so if anything goes wrong I can simply kick it off again and it'll resume from where it left off.

I think you can see where this is going. Before I say any more I'd like to ask a key question: Why is it that all downloads die at 99%?

I got up this morning and quickly checked to see if the download had finished. To my dismay, it had reached 99% and then thrown up an error message. I don't have the exact wording handy, but it essentially said:<blockquote><p>The download died for some unknown reason. Give it another go.</p></blockquote>So I hit the resume button with a huge grin, thinking that it was just going to carry on nicely, only to find that MS had slapped me in the face...<blockquote><p>The file no longer exists on the server. I can't resume. Press X to delete everything you've downloaded so far, Press Y otherwise.</p></blockquote>I pounced on Y and was grateful to find that the majority of the download was still on disk. I thought I'd go back to MSDN downloads to kick the download off again from the start, and try some geek trickery to get it to think that the imgae I'd already downloaded was the one I had just started. When I return to MSDN, I find that the link for Visual Studio 2008 Professional constantly redirects back to another page. I wasn't able to get to the same download page to start the download off again and grab that last 1%. Just when I thought it couldn't get any worse...

... the downloader closed. Without me asking it to. And with it, it took all 99% of that 3.31GB that I had downloaded.

I won't tell you exactly what I said, but I can tell you it wasn't pretty. Various expletives flew around the room, generally in the direction of MS and their stupid downloader. Then I realised that I'd blown well over 3GB of my 10GB download per month and achieved absolutely nothing. It was at this poin I hurled abuse at Internode for having such a shitty selection of plans.

After calming down I realised that it wasn't so bad. It could be worse.

... I could be living in the UK! :D
