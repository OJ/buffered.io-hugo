---
categories:
- LiveStream
- Security
comments: true
date: 2016-10-12T08:14:09+10:00
title: Linux SRP Overwrite and ROP
---

Recently I started live-streaming some security-related stuff on [Twitch][] because I enjoy teaching other people and showing them the processes, tools and techniques that I use while attempting to not suck at breaking stuff. Last night I did my second stream, which aimed to cover the following:

* A quick analysis of a vulnerable 32-bit Linux binary.
* An explanation of how stack buffer overflows can result in the Saved Return Pointer (SRP) being overwritten.
* A description of how SRP overwrites lead to control of the EIP register.
* A demonstration of how this control can lead to execution of shellcode on the stack thanks to the lack of NX.
* Development of an exploit that abuses the flaw resulting in attacker-controlled code execution.

With this first binary out of the way, a second one was also abused. The second binary was exactly the same as the first, except that it was compiled with NX _enabled_, and so the previous exploit would not work. This section attempted to cover:

* The reason NX causes the previous exploit to break.
* How control of EIP can still be abused to execute chunks of code.
* A "reasonable" description of ROP, and how it works.
* A demonstration of ROP in action (this was deliberately tedious to help those that haven't seen it before).
* Construction of an exploit that results in code execution even with NX enabled.

The latter part of this stream didn't quite go to plan, and I ended up taking a lot more time than I had hoped. The resulting exploit specifically targets the machine I was running it on (Fedora Core 24), and so wouldn't work on a remote system. However, my original intent was to demonstrate how it is possible to read entire areas of memory searching for instructions of interest (which in this case was `int 0x80 ; ret`). Due to time, I decided to skip on this and do it on easy-mode instead.

The video has been posted to YouTube, and I'm embedding here as well.

<iframe width="560" height="315" src="https://www.youtube.com/embed/XOmawM1EXnM" frameborder="0" allowfullscreen></iframe>

Apologies for the stupid DoubleClick Javascript crap that gets included by default when you embed YouTube clips. Be sure to run uBlock or something similar so that you're not tracked.

Finally, I promised to make the two binaries available for others to play with, and so you can download them [right here][download-bins].

Apologies for any frustration caused by font sizes, and me failing generally at "knowing what I'm doing". Hit me up if you have any questions or comments. Thanks!


  [Twitch]: https://www.twitch.tv/th3colon1al
  [download-bins]: /uploads/2016/10/guessing-bins.tar.gz
