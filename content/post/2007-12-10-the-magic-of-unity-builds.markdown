---
categories:
- HOWTO
- Software Development
- C#
comments: true
date: 2007-12-10T00:00:00Z
tags:
- unity builds
title: The Magic of Unity Builds
---

I realise that as time goes by, people are using my beloved <a href="http://en.wikipedia.org/wiki/C++" title="C++">C++</a> less and less. <a href="http://en.wikipedia.org/wiki/.NET_Framework" title=".NET Framework">.NET</a> (<a href="http://en.wikipedia.org/wiki/C_Sharp" title="C#">C#</a> and <a href="http://en.wikipedia.org/wiki/Visual_Basic_.NET" title="VB.NET">VB.NET</a>) and <a href="http://en.wikipedia.org/wiki/Java_%28programming_language%29" title="Java">Java</a> seem to be taking over the mainstream coding world. Languages such as <a href="http://en.wikipedia.org/wiki/Ruby_%28programming_language%29" title="Ruby">Ruby</a> and <a href="http://en.wikipedia.org/wiki/Python_%28programming_language%29" title="Python">Python</a> seem to be taking over the scripting world. For the most part, C and C++ seem to exist only in the gaming/entertainment, real-time and driver worlds.

In many colleges and univerties C++ is no longer taught as a core subject (along with <a href="http://en.wikipedia.org/wiki/Assembly_language" title="Assembly Language">Assembly language</a>) which I find quite galling. It's a great language to learn, even if you never use it again. But the purpose of this post is not to preach the virtues and failures of the C++ language, but instead to talk about something that might aid those people who <em>are</em> using C++.<!--more-->

Anyone who's worked on a large C++ (or C) application has felt the pain of long build times. The first "big" C++ system that I worked on took 10 minutes to build. This was much bigger than anything I had experienced up until that point. Later in my career when I joined the games industry I really felt the pain of rebuilding a <em>massive</em> code base. The game took just under an hour to build from start to finish without any distributed compiling mechanism in place. When I finally got a copy of <a href="http://www.xoreax.com/" title="Xoreax Incredibuild">Incredibuild</a> the build time dropped noticably, but not to a point where recompilation was an option every time I wanted it.

The solution that we implement: <strong>Unity builds</strong> (which I'll refer to as UB from now on because I'm lazy).

Now, a lot of C and C++ that have used these before might consider the idea of a UB to be a bit of a hack. I'm not sure if I agree or disagree. There is an air of "hackyness" about it, but it solves the problem of huge compile times on most platforms with most compilers.

Before I go into the definition, and give instructions on how to set up a UB, let me just state for the record that this isn't designed to replace normal builds for releasing code. The idea behind these is to dramatically reduce the build times for developers who are spending 8 hours a day modifying the code. Fixing bugs and adding enhancements generally means that a developer is constantly recompiling. Every time you compile, you have to wait (or perhaps <a href="http://xkcd.com/303/" title="Compiling">entertain yourself in some way</a>), and in waiting you are wasting precious time. Wasting time means a reduction in productivity. This can only be a bad thing. It's bad enough with short build times, but with long build times it becomes a huge problem. So please bear in mind that the normal release and product builds, builds that happen on automated build servers, etc, do <em>not</em> need to have a UB in place, they can continue to use normal builds.

The principle behind a UB is quite simple. It's all about reducing...<ol><li>... the number of times a file is opened.</li><li>... the number of files that are opened.</li></ol>In esssence, we're going to abuse the power of the <a href="http://en.wikipedia.org/wiki/Preprocessor" title="Preprocessor">preprocessor</a> to do the above.

Every source file (.c, .cxx, .cc, .cpp, etc) that is compiled results in the creation of an <a href="http://en.wikipedia.org/wiki/Object_files" title="Object File">object file</a>. When all of the source files are compiled into object files, the <a href="http://en.wikipedia.org/wiki/Linker" title="Linker">linker</a> then collects up all these object files and links them together into an executable application. For those of you who don't know: reading/writing to/from disk is the slowest thing you can do on a computer. Actually, it isn't so much the reading and writing, it's more the <a href="http://en.wikipedia.org/wiki/Seeking" title="Seeking">seeking</a>. But the point remains the same. Disks are the only core parts of your system that have moving parts, so they're going to be slow! So if you want to speed things up, a good place to start is reducing I/O.

I don't want to turn this into a lesson on how C and C++ programs are compiled, so I'm going to cut to the chase. We know that the preprocessor parses each file before it's compiled which means that each file is going to be opened. When those parsed source files are then compiled, they're essentially opened again. Each of those files requires another seek. When the object file is generated, that's another seek. When the file is linked, that's another seek. There's lots of disk activity going on here!

If you haven't caught on yet, let me give you another hint. Each source file results in a couple of new files and a stack of seeks. We also have a neat preprocessor statement at our disposal (<em>*cough*</em> <a href="http://en.wikipedia.org/wiki/Header_file" title="Header File">include</a> <em>*cough*</em>).

That's right guys, we don't actually compile every file individually. We instead create a master file (or set of master files, depending on how big your source base is), which <em>includes</em> all of the other source files. This file (or set of files) is compiled instead.

<a href="/uploads/2007/12/unitybuild-01.png" rel="lightbox[unitybuild]" title="Sample Project"><img src="/uploads/2007/12/unitybuild-01.thumbnail.png" class="InlineImageLeft" alt="Sample Project" /></a>To make it clear I'm going to show you the principle in action on a fake project in Visual Studio 2008. However, the principle can be applied regardless of the compiler and platform. Check out the screenshot on the left. This shows a standard C++ with a few files in it. Ordinarily each of those files will be compiled individually, and hence slowly.

The first step in creating a UB is to make a new project configuration which you can play with. Doing so will allow you to set up an area you can modify without breaking the "normal" project which will continue to be built as usual. So let's do that now. Create a new project configuration based on the debug build. See the following screenies:
<a href="/uploads/2007/12/unitybuild-02.png" rel="lightbox[unitybuild]" title="Configuration Manager - New Config"><img src="/uploads/2007/12/unitybuild-02.thumbnail.png" class="InlineImageLeft" alt="Configuration Manager - New Config" /></a>
<a href="/uploads/2007/12/unitybuild-03.png" rel="lightbox[unitybuild]" title="New Solution Configuration"><img src="/uploads/2007/12/unitybuild-03.thumbnail.png" class="InlineImageLeft" alt="New Solution Configuration" /></a>
<a href="/uploads/2007/12/unitybuild-04.png" rel="lightbox[unitybuild]" title="Configuration Manager - Config Added and Selected"><img src="/uploads/2007/12/unitybuild-04.thumbnail.png" class="InlineImageLeft" alt="Configuration Manager - Config Added and Selected" /></a>
<div style="float: none; clear: both;"></div>
<a href="/uploads/2007/12/unitybuild-05.png" rel="lightbox[unitybuild]" title="Build Toolbar - Config Selected"><img src="/uploads/2007/12/unitybuild-05.thumbnail.png" class="InlineImageRight" alt="Build Toolbar - Config Selected" /></a>When you've created the new configuration, make sure you have it set as the active configuration as shown on the right.

<a href="/uploads/2007/12/unitybuild-06.png" rel="lightbox[unitybuild]" title="Solution with Unity Build File"><img src="/uploads/2007/12/unitybuild-06.thumbnail.png" class="InlineImageLeft" alt="Solution with Unity Build File" /></a>Then, you need to add a new file which will be the master UB file. I usually create a sub-folder in the project and add the file there. Both folder and file I tend to call UnityBuild.

<a href="/uploads/2007/12/unitybuild-07.png" rel="lightbox[unitybuild]" title="Unity Build File Contents"><img src="/uploads/2007/12/unitybuild-07.thumbnail.png" class="InlineImageRight" alt="Unity Build File Contents" /></a>When the file is created, the first thing we need to do is make sure that we edit the UnityBuild.cpp so that it includes <em>all</em> of the other files in the project. Check out the image for the example.

If we attempted to compile at this point, we have all kinds of issues. Everything right now is being included twice. So we need to fix that so that we don't have issues in <em>every</em> configuration. Let's start by fixing up the Unity build project. Select all of the source files <em>except</em> UnityBuild.cpp, and <strong>exclude them</strong> from the project (right-click on them to see the properties). Check out the pics below to see how it's done.
<a href="/uploads/2007/12/unitybuild-08.png" rel="lightbox[unitybuild]" title="Files selected"><img src="/uploads/2007/12/unitybuild-08.thumbnail.png" class="InlineImageLeft" alt="Files selected" /></a>
<a href="/uploads/2007/12/unitybuild-09.png" rel="lightbox[unitybuild]" title="Exclude from build"><img src="/uploads/2007/12/unitybuild-09.thumbnail.png" class="InlineImageLeft" alt="Exclude from build" /></a>
<a href="/uploads/2007/12/unitybuild-10.png" rel="lightbox[unitybuild]" title="Solution with files excluded"><img src="/uploads/2007/12/unitybuild-10.thumbnail.png" class="InlineImageLeft" alt="Solution with files excluded" /></a>
<div style="float: none; clear: both;"></div>
So that this point you've excluded the files from the UnityDebug configuration, and hence the unity configuration <em>should</em> build.

<strong>Note:</strong> if you're implementing this in a large existing source base you may find that the UB doesn't actually compile. You need to bear in mind that the preprocessor is making <strong>one large file</strong> out of all of your source files. Which means global variables, static variables, global functions, etc that appear more than once in the source code will now tread all over each other. Technically you shouldn't have this problem if you're writing "proper" code :) But the harsh reality is that lots of devs copy and paste code, duplicate function names, and all kinds of other horrible things. To get it working, you're going to have to do a bit of housekeeping! But since this is going to affect the source in a positive way, it can only be a good thing!

<a href="/uploads/2007/12/unitybuild-11.png" rel="lightbox[unitybuild]" title="Unity build file excluded"><img src="/uploads/2007/12/unitybuild-11.thumbnail.png" class="InlineImageLeft" alt="Unity build file excluded" /></a>OK, next up let's get the other builds working again. You need to exclude the UnityBuild.cpp file from the build in both the Debug and Release builds (not the UnityDebug configuration). I won't show you how to do that, as it should be obvious. When you're done, you should see similar to the image on the left when you use the Debug or Release configurations from this point on.

You're done! The old builds should work just fine because your UB file isn't included in the configuration. Once you've sorted out the code duplication issues with the UB configuration, you should have a perfectly working configuration.

You may find that the whole process is much easier if you're building from the command line, especially using <a href="http://en.wikipedia.org/wiki/Make_%28software%29" title="make">make</a> on <a href="http://en.wikipedia.org/wiki/%2Anix" title="Unix-like">*nix</a> machines, as you don't have to worry about excluding things from configurations. Instead, you just simply need to pass in a single file to the compiler - the UB file!

So after all that, what kind of improvements should expect to find? Well let me give you a few stats. When the UB was implemented at the game company I worked for, the build time dropped from 55 minutes to <strong>just over 6 minutes</strong>. When I implemented UB in a previous job the build time dropped from 10 minutes to <strong>less than 3 minutes</strong>. When I put them in place at home the build times dropped on average by <strong>60%</strong>.

I'd be very surprised if you didn't notice a huge improvement in your build times if you use this mechanism.

Before I finish up, I need to highlight a few issues around housekeeping. As already stated, these builds are not used to replace the normal compilations. They should sit alongside to make it faster for the developer to do his or her work. This means that you need to make sure that both types of builds are <em>working all the time</em>. Keep your automatic builds using the normal build type, and you'll soon find out if you've not been maintaining your configurations properly.

Make sure that every time you add a new source file you <strong>exclude it from the UB configuration</strong>, and that you <strong>add it to the content of the UnityBuild.cpp file</strong> otherwise it won't be compiled and included in the build properly.

On the whole, UBs have really improved the speed in which I've been able to do things. My wait time for compilations is generally a third of what it was. That, in my view, is worth the "hack" :)

Thanks for reading!

PS. Please let me know of any typos, grammar errors, etc. It was a hefty post to put together and I might have missed a couple of obvious stuff-ups!

<em>EDIT: I have fixed up a few grammar and spelling errors. Thank you Yoann and Bryce :)</em>
<hr/>
<strong>This post is a bit old, and some of the images are missing. While it's still got a valid description of Unity Builds it's not really complete without the piccies. So I've created a full screencast and posted that <a href="/posts/screencast-setting-up-unity-builds/" title="Screencast - Setting up Unity Builds">over here</a> to clear up any confusion. Cheers!</strong>
