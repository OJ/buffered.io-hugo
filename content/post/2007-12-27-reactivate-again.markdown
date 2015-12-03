---
categories:
- Digital Rights
- Funny
- Hardware
- Linux
- Windows
- WTF
comments: true
date: 2007-12-27T00:00:00Z
title: Reactivate? AGAIN!?
---

This is just a quick post to say how much Microsoft and their draconian OS licensing mechanism are pissing me off right now.

Last week my computer stopped booting. I didn't get around to looking at it until a few days ago because I had visitors over from the UK. Not just that, it is Christmas, and the last thing you want to be doing on your Christmas break is to be fixing computers.

<!--more-->

I spent an hour or two playing with bits and pieces to try and determine the error. The problem was that the third <a href="http://en.wikipedia.org/wiki/DIMM" title="DIMM">DIMM</a> slot on <a href="http://us.dfi.com.tw/Product/xx_product_spec_details_r_us.jsp?PRODUCT_ID=4556&CATEGORY_TYPE=LP%20UT&SITE=NA" title="DFI Lanparty CFX3200-DR">my motherboard</a> (which, incidentally, has been a bit of a nightmare since I bought it) is dodgey. I removed a 512MB RAM stick from the offending slot over to slot two and the machine started to come alive again. I was pretty happy about this, despite the reduction in RAM speed due to the configuration, since I didn't need to buy anything to make it work again!

So I left the machine for a couple of days again as I was happy with what I achieved. But today, because the study was in a bit of a state due to the bits of computers, manuals, and various other things I'd strewn around the room while investigating, I thought I'd finalise everything and tidy up.

Since my computer case was open, I thought I'd do a bit of housekeeping. I wasn't really happy with the way it was laid out inside so I thought I'd tidy that up a bit first. I removed <a href="http://www.linksys.com/servlet/Satellite?c=L_Product_C2&childpagename=US%2FLayout&cid=1150490054358&pagename=Linksys%2FCommon%2FVisitorWrapper" title="Linksys WMP54G">my WLAN card</a> because it was flaky at best in 64-bit, and I'd had enough of the lag spikes (time to go back to cabled LAN). I moved <a href="http://www.hisdigital.com/html/product_ov.php?id=217" title="HIS Radeon x1900xtx">my video card</a> to a different slot to create a bit more space between it and <a href="http://www.scythe-usa.com/product/cpu/006/scnj1000p.html" title="Ninja Scythe">my HSF</a> (which is awesome, and does a great job even if it is a behemoth!). I also adjusted the fans a bit, cleaned up a lot of the dust, and moved a few of the cables around to improve air-flow.

So with all that done, I fired up the machine.

.. and got nothing. <strong>Shit</strong>.

The investigation started again. I thought that I'd go right for the previous culprit and rip out the RAM stick that I'd moved to see if that would resolve the problem. Thankfully, it did. So right now I'm running on 1/2GB RAM, not ideal to say the least but the machine is booting which is a lot better than it not.

The machine booted, and up came windows. It was at this point that I remember it telling me that a few days back it wanted me to reactivate Windows because of a <em>substantial change in hardware</em>. Of course, I ignored this stupid box because at the time I hadn't changed <strong>anything</strong>. I'd <em>moved a RAM stick</em>!

But today, it greeted me again. This time the message was different. In essense, my 3-day "window" of reactivation had expired, and I was being forced to reactivate it now. And I do mean <strong>now</strong>. Now as in right now before I was allowed to log on. Oh, but there's a problem here you see, because prior to logging on, there's no network connection. So when you try and activate Windows before logging on, it can't activate over the Interweb because it can't get to it. Of course, the activation fails. But does it let me log in? No. What does it do? I ties itself into an infinite loop of ...<ol><li>Try to log in.</li><li>Fail because it's not activated.</li><li>Try to activate.</li><li>Fail to activate because it's not logged in and hence can't connect.</li><li>Goto step 1.</li></ol>Not happy. Not happy at all. Not only had I <strong>not</strong> changed any hardware in the system (all I'd done is move it around, and remove a WLAN card) to force the reactivation, I am now stuck with a machine that won't let me log in. Nice eh?

Here's a nice little side-note for the Linux fanboys :) (yes, you know who you are). For yonks, a slight modification in anything to do with the video hardware in my machine would result in my linux graphics setup shitting itself. I'd then have to spend a chunk of the day trying to get it to work again with my dual-screen setup. But not this time. I'd mucked around with everything inside the machine, and what happened when I booted Linux??

Wait for it... WAIT FOR IT!

Nothing. It booted as if nothing had changed. Video settings were the same. Dual-screen worked. I sat here a little stunned for a while and I was brought back from my daze by the little blinking icon in the system tray telling me that I had updates to download. The network had connected, and everything was sweet.

See that Microsoft? Right there. <strong>That</strong> is what <em>should</em> happen. Nothing more, nothing less. When I reboot after a hardware change, I expect you to continue as if nothing had happened. I expect the network to connect without me dicking with it. I expect a nice little "Hello OJ, you legend, how else can I make your life easy today?"

So hats off to you <a href="http://www.linuxmint.com/" title="LinuxMint">LinuxMint</a>. You made my day! Now, if only you hadn't borked my Firefox install :) Time to update you to the last version me thinks, and give you a bit of the love and attention that you deserve.

As a final note, it looks like I'm going to have to buy some new RAM, and while I'm there I'm going to ditch this UK keyboard as it has been driving me nuts for the last year and half. Time to get back to the Aussie (some might say US) layout.
