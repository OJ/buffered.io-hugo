---
categories:
- HOWTO
- Linux
comments: true
date: 2006-10-25T00:00:00Z
title: 'HOWTO: Getting ATi Drivers Working in Ubuntu'
---

As I've already mentioned, I've started dabbling in the joys of Linux by installing Ubuntu (<a href="http://www.ubuntu.com/support/faq#head-3df1e0e29bc1a807664ce156fb9fe6f09c35bfdd" title="Ubuntu FAQ">Dapper Drake</a>). This was partly joyous and partly painful - but mostly joyous :) While going through some of the less enjoyable moments I spent a fair bit of time trawling forums and hanging in IRC channels (#ubuntu-au on irc.freenode.net to be exact, check out <a href="http://ubuntu.com.au/welcome/" title="Ubuntu Australian Team">here</a> and <a href="http://www.matthewv.id.au/ubuntu-au/" title="Ubuntu Australian Team">here</a>) trying to find the information I needed to get things going.

The most recent issue that I have since solved is that of getting drivers working with my ATi Radeon X1900-XTX, and it was the pain of this set up that inspired me to write this post - and in fact, what will look to be a long stream of posts. After the quality of feedback and help, and the time investment by some of the guys in #ubuntu-au (thanks lads! Especially <em>DarkMageZ</em> and <em>siccness</em>), I thought that I want to help share this information to aid in getting other people up to speed to. In fact, I've decided to start a whole new category of blog posts - HOWTOs! You can consider this the first :) I won't just be doing HOWTOs on things that I experience with Linux, but I'll aim to cover code examples, bits and pieces to do with Windows, and whatever else takes my fancy. Information should be shared, so feel free to let me know if anything that you would like me to share, and I'll do my best to cover it.

So, on with the juicy bits. The default installation of Ubunutu worked fine with the vesa driver which allowed Xorg to fire up without any issues, but of course, I wasn't happy to suffer for second best while having a fairly beefy video card. The goal was to get the ATi drivers working in both 2D and 3D.

The first mistake that I made was to approach the issue in the same way that I would approach it as a Windows user - that is, go to the ATi driver website and download the latest driver. This turned out to be a bit of a mistake, but I'll go through the process I took anyway just FYI :) I downloaded the driver from the <a href="http://www.ati.com/online/customercareportal/linux.html" title="Linux ATi Drivers">ATi Linux driver page</a> and read the instructions that came with it. Seemed fairly simple, so I did as I was told and waited for the magic to happen.

The first thing that I failed to notice was that the install notes stated that the driver had only been tested with Red Hat Linux. Now I don't know if there is that much of a difference between that and Ubuntu, but given that certain things refused to work I'm <em>guessing</em> they are. After performing the installation, I restarted Xorg (which, FYI, can be done using the shortcut <strong>CTRL+ALT+BACKSPACE</strong>) and I was knocked off my chair by the clarity of what I was seeing. It was magic :) But to my dismay, things were not all good under the hood. It turned out that even though 2D was clearly working, the 3D part (and with it OpenGL functionality) was nowhere to be seen. This is what got me started with the forum trawling and the IRC rants. I learned very quickly that downloading the driver from the ATi site was a bad move, and that I should have instead used the <strong>apt-get</strong> functionality to get the drivers set up.

At this point I went through the process of uninstalling the drivers that I had downloaded, and got my machine back to a point where I could try and install the new ones. The <em>vesa</em> driver fuzzyness returned and I was ready to rock with the instructions I was given. I downloaded and installed the correct drivers using the correct utility and did exactly what I was told to do, only to find that this refused to work as well. The driver install was apparently successful, but I still had the VESA fuzzyness indicating that the driver was not loading properly. I read around some more, and chatted some more, and determined that I needed to modify my <strong>/etc/X11/xorg.conf</strong> file to set the default video driver to <strong>fglrx</strong> (not VESA), so that's what I did!

Restarting Xorg resulted in my world crumbling (well, not quite ;)). My screen was blank. I had no UI, I had no idea what was on screen, so I ended up having to boot back in using recovery mode and modifying the file so that it was set back to VESA. It was about this time I was getting pretty narked as I'd been messing with quite a few things for a few hours and my patience was wearing thin.  It was 11:45pm when I thanked my Aussie Ubuntu colleagues for their help and made the move to shut down, as I was going to continue tackling the problem the next day. While shutting down, I was just having a last flick through the tabs I had open in FireFox when I noticed a comment at the bottom of a forum thread. This turned out to be the magic missile!

There are a stack of modules that you can specify to load in Xorg, and apparently there's issues with the ATi driver when two of the many are loaded at the same time. The suggestion was to comment out the culprit (which was called <strong>extmod</strong> I believe) which was clashing with the one you needed (which was called <strong>dri</strong>, I think :)). After making this change, I restarted Xorg and all was clear and beautiful! And for the first time ever on this new installation both <strong>fglrxinfo</strong> and <strong>glxgears</strong> behaved the way they should. 2D and 3D was up and running! Mission accomplished.  So, now that my story is told, please find listed below the steps that I had to take to get the ATi drivers installed on Ubuntu AMD64 Dapper Drake. I hope you find them useful.

    sudo apt-get install xorg-driver-fglrx fglrx-control
    sudo aticonfig --initial

    take a backup copy of /etc/X11/xorg.conf
    edit /etc/X11/xorg.conf using your favourite text editor:
    - change the Driver option from "vesa" to "fglrx"
    - comment out the line that says : Load "extmod"

    restart xorg (CTRL+ALT+BACKSPACE)


Good luck all! Hopefully next up will be my success story with my wireless network card.
