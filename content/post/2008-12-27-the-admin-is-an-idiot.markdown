---
categories:
- Linux
- Software
- WTF
comments: true
date: 2008-12-27T00:00:00Z
tags:
- idiot
- Linux
- sysadmin
title: The Admin is an Idiot
---

Yes, and that admin is me!

I usually do a fairly decent job of keeping the server patched, up to date, and clean. Over the Xmas period where I wasn't really looking at anything online, something went wrong.

I logged on this morning only to find that the server wasn't responding. I could connect via SSH, but nothing was being served via HTTP. So I tried to reboot <a href="http://litespeedtech.com/" title="LiteSpeed technologies">LiteSpeed</a> to see if that would resolve it (as you do ;) ). The service stopped, but failed to restart!

<!--more-->

I was a bit concerned when this happened. So I thought I'd check to see if there were any server updates (in case there was a known issue preventing the server from working). I found that there had been an update, so I jumped to my first conclusion and assumed that this was broken. I attempted to download the new version onto my <a href="http://en.wikipedia.org/wiki/Virtual_private_server" title="Virtual Private Server">VPS</a>, but it failed. The error message said that it couldn't download the file because I was out of disk space!

A quick run of <a href="http://www.computerhope.com/unix/udf.htm" title="Linux 'df' command">df</a> told me that the error message was indeed correct. 19GB out of 19GB was used. Crap.

I then jumped to my second conclusion and immediately blamed <a href="http://en.wikipedia.org/wiki/Portage_(software)" title="Portage">portage</a> for downloading too much stuff that it didn't need. After a quick <a href="http://linuxlore.blogspot.com/2007/04/howto-cleanup-gentoo-portage-distfiles.html" title="Clean up Gentoo Portage Distfiles">eclean</a> I was presented with a huge saving of 350MB. Crap.

I then had to fall back on <a href="http://www.computerhope.com/unix/udu.htm" title="Linux 'du' command">du</a> (<em>du -h --max-depth=1</em> to be exact) to try and narrow down the folder(s) which were chewing up disk space.

To cut a long story short, there were two culprits. The first was the linux source folder. I had 15 copies of the Gentoo source in there, and freed up 4.7GB by deleting 13 of them. The second was, much to my shame and dismay, the web server log files folder. Oh woe is me.

Schoolboy error. I scrapped all the unnecessary logs and freed up yet another 5.6GB. Amazingly, everything has started behaving itself again! Who'd have thought.

So the server is now back online. Normality has been restored. I also have more proof that I'm an idiot.

That is all :)
