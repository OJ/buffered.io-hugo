---
categories:
- Mac OS
- Security
- Tips/Tricks
comments: true
date: 2013-02-03T00:00:00Z
title: Irssi and SASL on OSX
---

Given my renewed focus on security I've been looking to lock down much of my communications so that I _feel_ more secure online. One of the things that I use quite a lot to connect with people who know WTF they're talking about is IRC. [#freenode][Freenode] is my network of choice as it has a bunch of interesting places with fairly active communities. [irssi][] is my current client of choice for a few reasons.

[SASL][], among other things, is one thing that can be enabled to secure your communication on IRC. There's quite a bit of information out there already about how to [enable SASL for irssi on Freenode][enablesasl]. These instructions worked out of the box for me on Linux, but not on Mac OSX. After downloading `cap_sasl.pl` and putting it in `~/.irssi/scripts/autorun` and installing the required Perl modules I went through the steps to get SASL configured. When I attempted to enable it as per the instructions using the command `/sasl set network TheColonial <password> DH-BLOWFISH` I had the following error:

> Can't locate object method "send_raw_now" via package "Irssi::Irc::Server"

This took a bit of Googling to nail. It turns out that, for some reason, on Mac OSX `irssi` actually requires an extra package to be included in the script. So open `cap_sasl.pl` and look for this (it's right at the top):

    use strict;
    use Irssi;

Immediately below this, add the following:

    use Irssi::Irc;

The script should now run fine and `irssi` should stop complaining.

This might seem obvious to some, but it wasn't to me! From what I can tell other people around the traps have had the same problem so I thought I'd post this in the hope that it helps others.

  [Freenode]: https://freenode.net/ "Freenode IRC network"
  [SASL]: https://en.wikipedia.org/wiki/Simple_Authentication_and_Security_Layer "SASL"
  [enablesasl]: https://freenode.net/sasl/sasl-irssi.shtml "Enabling SASL in irssi on Freenode"
  [irssi]: http://www.irssi.org/ "irssi IRC client"

