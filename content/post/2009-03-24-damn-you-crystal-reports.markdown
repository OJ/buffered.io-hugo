---
categories:
- Microsoft
- Software
- Software Development
comments: true
date: 2009-03-24T00:00:00Z
tags:
- crystalreports
- installation
- visualstudio
- WTF
title: Damn you, Crystal Reports!
---

I've had <a href="http://msdn.microsoft.com/en-us/vstudio/default.aspx" title="Visual Studio on MSDN">Visual Studio 2008</a> installed for quite a while. When I first installed it I decided not to install the <a href="http://www.sap.com/solutions/sapbusinessobjects/sme/reporting/crystalreports/index.epx" title="Crystal Reports">Crystal Reports</a> components because I was fairly certain that I'd never need them at home.

Well, the worm has turned! On my new gig, I have the joy of working from home. It just so happens that I <em>also</em> need to use Crystal Reports. How silly of me to not bother installing a few extra components the first time so I didn't have to go through the pain of going through another VS Setup.

On the surface this doesn't sound like a painful experience right? <strong>WRONG</strong>. It's a lot more painful than you think.

<!--more-->

I fired up the <em>Programs and Features</em> section of <a href="http://www.microsoft.com/windows/windows-vista/default.aspx" title="Windows Vista home page">Vista's</a> control panel to fire up the add/remove components section of the install.

Despite choosing just one extra component in the options list, it decided to attempt install of other components too. Observe

<a href="/uploads/2009/03/vs2008_update.png" rel="lightbox[vs]"><img src="/uploads/2009/03/vs2008_update.png" alt="Screw you VS Install! Sod off Mr Tiny Face!" title="Screw you VS Install! Sod off Mr Tiny Face!" style="float: left; margin-right: 5px; margin-bottom: 5px;" width="150" /></a>Visual Studio 2008 again? SQL Server Compact 3.5 SP1 Design Tools!? SQL Server Compact 3.5 for Devices?! Shared Management Objects?! WHAT THE F**K?! I didn't ask for any of that crap. It's not as if looking at <a href="http://secretgeek.net/vs2008_bugeye.asp" title="Bug Eyed VS2008 Guy Frakes Me Out">Mr Tiny Face</a> wasn't bad enough. Now I get to put up with a bunch of other shit that I don't want or need?

I was hoping that it was going to have enough "stuff" on the HDD to update without me needing to get the VS ISO I was wrong. I had to search for the ISO that I downloaded from MSDN. This wasn't too much of an issue, but was still painful as I had to copy it onto my local machine from my <a href="http://www.buffalotech.com/products/network-storage/terastation/terastation-pro-ii/" title="Buffalo Technology - TeraStation Pro II">Terastation</a> -- all 3.4GB of it.

It then proceeds past the unnecessary installs until it gets to Shared Management Objects at which point I get another epic fail.

<a href="/uploads/2009/03/sharedmanagementobjectfail.png" rel="lightbox[vs]"><img src="/uploads/2009/03/sharedmanagementobjectfail.png" alt="Shared Management Objects fail" title="Shared Management Objects fail" style="float: right; margin-left: 5px; margin-bottom: 5px;" width="150" /></a>The bloody thing wasn't on the disc! So I grab it off the web, point the installer at the download location and it tells me that the path is invalid.

I was starting to get rather narked at this point. I hit cancel at which point I was greeted with a message telling me that: <em>The operation in progress cannot be cancelled.</em>

Then the cancel succeeded! <strong>Make up your mind!</strong> The final screen implied install fails of epic proportions, but only turned out to be that the Shared Management Objects didn't install.
<a href="/uploads/2009/03/sharedmanagementobjectepicfail.png" rel="lightbox[vs]"><img src="/uploads/2009/03/sharedmanagementobjectepicfail.png" alt="Epic Install Fail" title="Epic Install Fail" style="float: left; margin-right: 5px; margin-bottom: 5px;" width="150" /></a>I won't be crying about that given that I didn't want it in the first place! If I find that I do need it down the track I can install it manually. After all, I did download it myself anyway.

The next question I have is: What level of pain will I feel if I <em>don't reinstall <a href="http://www.microsoft.com/downloads/details.aspx?FamilyId=FBEE1648-7106-44A7-9649-6D9F6D58056E" title="Visual Studio 2008 Sevice Pack 1">VS 2008 SP1</a></em> before I attempt to do any development? I'm too scared to find out the answer. Now I get to wait for another decade for the service pack install to finish.

Yet another win for the Microsoft Installation process.
