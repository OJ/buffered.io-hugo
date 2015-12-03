---
categories:
- Security
- Metasploit
- Meterpreter
comments: true
date: 2014-02-18T07:01:37Z
title: Building Meterpreter is Easy
---

I might not have mentioned this before, but I have to tell you that _building Meterpreter is easy_. In the old days, downloading the source was the easy bit and compiling it was the hard bit. The steps involved in getting a Meterpreter build environment together were extensive and prone to error. In fact if you got one wrong, or you did things in the wrong order, then you could end up having to remove everything from your machine and starting again.

It shouldn't be surprising that the contributions to Meterpreter from the community have been a bit lean as a result. Nobody wants to deal with a painful set up or horrible build experience when it comes to any software, let alone open source.

One of my goals when working on Meterpreter was to make this problem go away. While it was partly because I didn't want to suffer myself, it was also because the Metasploit team wanted it to be easy for anyone to contribute.

Thankfully the pain has been removed! In fact, building this puppy it is just as easy as getting the source itself! My mum could do it. My mum's mum could do it. _Anyone_ can do it, **even if they don't have a computer!**

OK, that last bit might not be true, but it is super easy to build and doesn't require expensive build tools either. All you need is a recent-enough copy of Windows and you're good to go. Now that we use Visual Studio 2013 we have to match the requirements for this baby to run. This means that you need a copy of **Windows 7 Service Pack 1** or later. It can be x86 or x64, but the version is important. If you're on this version, or using Windows 8, 8.1, 2008 or 2012 then you should be fine. VS requires .NET 4.5, so if you can get that running happily you should be fine.

To prove just how easy it is, I have put together a short video which demonstrates it. The video takes you from a bare Windows 7 x86 SP1 installation (with a few MS updates applied thanks to Windows Update automatically installing stuff) through to compiled Meterpreter binaries.

The steps are:

1. Install Windows 7 and update to SP1 (not in the video because that's _boring_).
1. Download and install Visual Studio 2013 Express for Windows Desktop. It's important to emphasise that the correct edition is the **Windows Desktop** edition, and if you don't install this edition then building the source will not work.
1. Download and install git.
1. Clone the repository from Github and initialise submodules. If you're interested in contributing to Meterpreter then you should fork the repository yourself before. Github had a great [how-to up on their site][fork] so have a read of that if you're not sure what to do.
1. Open a "Developer command prompt".
1. Type `make` and hit enter.

And here it is in action. See it in all its Youtubey glory!

<iframe width="640" height="480" src="//www.youtube.com/embed/5WgLlGCMez0" frameborder="0" allowfullscreen></iframe>

Feedback, comments and questions are welcome as always.

  [fork]: https://help.github.com/articles/fork-a-repo "Fork a Repo"
