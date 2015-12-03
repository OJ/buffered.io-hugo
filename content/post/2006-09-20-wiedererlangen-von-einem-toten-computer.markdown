---
categories:
- Hardware
- Technology
comments: true
date: 2006-09-20T00:00:00Z
title: Wiedererlangen von einem toten Computer
---

Computer failures. We've all been bitten by them, and I'm pretty sure we'll all be bitten by them again. Good quality HDD storage is becoming more common, but despite the advances in technology, our drives still only have a <em>reliable</em> lifetime of a few years. Power supplies are becoming more solid, but they still blow up. Motherboards, while having more features than you poke a stick at, are still susceptable to being killed by something as simple as static electricity at the slightest touch.

The result of losing <em>any</em> part of your computer half-way through a project is pretty devastating, especially if it's the HDD. This isn't just because of the loss of information, but also because of the loss of time that it takes to get things up to speed again. That time includes redoing all the work that was lost <em>and</em> getting a new machine built and ready to go so that development work can recommence.

The purpose of this post is to explore the options reducing the impact of hardware failures on our geek lives. The reason I thought I'd rant was because Danny Ireland recently <a href="http://www.shiftperception.com/blog/posts/how-to-blow-up-2-computers-in-2-weeksthe-the-ballad-of-the-spare-computer/" title="How to blow up 2 computers in 2 weeks">experienced an event like this</a>.

From this point on I'll be using the term <strong>Virtual Machine</strong> (VM) as a meaning for having an operating system running inside a window :) So this might be using <a href="http://www.vmware.com" title="VMware">VMWare</a>, <a href="http://www.microsoft.com/windowsxp/virtualpc/" title="VirtualPC">Microsoft VirtualPC</a>, <a href="http://bochs.sourceforge.net/" title="Bochs">Bochs</a> or something similar.

To start off with, I'm one of those guys who likes to have his computer running at a blistering/optimum speed even if he's not going to be doing anything that requires it :) I know I'm not alone in this world either. As a result, the idea of doing my daily activities inside a VM isn't that appealing. Not just that, but VMs tend to always have some form of refresh and response issues which just frustrate me.

On the flip side of the coin, I don't like the idea of having to reboot every time I want to fire up a new operating system, which is one of the things that VMs nail on the head. Another things is being able to have them all running at once if you really want.

The argument for dual-booting vs VMs has no affect on the issues surrounding a hardware failure, I just thought I'd mention a couple issues surrounding the use of VMs.

The main issue that needs to be covered is the minimal recovery time. Minimal recovery time relies on backups, and backups are always in short supply regardless of how good your backup scheme is, because you simply can't back up real-time (well, you can, but is it something that you want to do given the overheads?). Let's look at the areas of hardware failure and the impact it can have on your work.

<strong><u>Video Card / Monitor</u></strong>

Easy peasy. This has little do no impact on you as neither of these items are responsbile for retaining state. If either one of these kitties dies on you, replacing them should be a doddle. Most companies would (<em>should</em>?) have spares, and getting going again should take no more than a few minutes. If you're good enough, you should still be able to save your work before shutting your PC down to make the changes required :)

<strong><u>Motherboard / CPU</u></strong>

Depending on the method of demise, this may or may not affect your work. Sure, it's going to ditch whatever work you've done that's volatile (ie. stuff you haven't saved yet) as most CPU/motherboard failures result in the computer blue-screening, freezing, or totally shutting down. The time it takes to get the motherboard replaced varies greatly. This is due to the fact that figuring out that the motherboard is the problem is what takes the time. Replacing the mobo isn't a huge task, but it does take a bit of time. CPUs are, again, a doddle to replace, and the time to replace is minimal - as soon as you know it's the CPU that's gone.

<strong><u>Power Supply Unit (PSU)</u></strong>

PSUs die in a similar way to what most drunk drivers do. That is, they waver around and end up crashing, and there's always a chance that they're going to take something else with them! The surges in power can kill any or all of the other bits that are unfortunate enough to be connected to the computer at the time. There's no rule of thumb, you're either luck or you're not. Just pray that it doesn't manage to take your HDD with it when it goes, as it's the only thing in the machine that <em>really</em> hurts when it dies. Dan's post contains some information on his PSU woes :)

<strong><u>Hard Disk Drive (HDD)</u></strong>

This is the real killer. Not only is it capable of bringing your system down, resulting in the loss of all your W.I.P, but it will probably end up taking a stack of other data with it that has already been persisted. This is where things should be put in place to reduce the impact of a failure.

One way of doing this is to use <a href="http://en.wikipedia.org/wiki/Redundant_array_of_independent_disks" title="RAID">RAID</a> to store your information on multiple drives so that if one of the breaks you still have other copies lying around. This works well, and you'd be very unlucky to lose all your disks in one go. That doesn't mean to say that it <em>won't</em> happen (just ask my father-in-law who's literally had a <strong>blast</strong> during a thunderstorm resulting in the loss of not just data, but his entire PC).

Another way is to have all your work stored on a network drive, and have that network drive RAIDed and backed up. This at least puts the focus on the system administrator who should be responsible for a strong backup plan which will allow you to get to the data from the day before at the very least. There's stacks of funky network storage solutions out there (check out Paul's purchase of a <a href="http://pauleastabrook.spaces.live.com/blog/cns!46B5EF0BB06A216B!216.entry?_c11_blogpart_blogpart=blogview&_c=blogpart#permalink" title="2TB should do">2TB NAS</a> as an example). This will increase your network traffic a <em>lot</em> so you'd want to make sure that your network infrastructure can deal with it, especially if you deal in massive amounts of content (eg. like source-art for a computer game). If you deal with GBs of data on a daily basis you'd want to have a <a href="http://en.wikipedia.org/wiki/Storage_Area_Network" title="Storage Area Network">SAN</a> or something similar to handle the load.

<strong><u>Hardware aside - there's more to worry about</u></strong>

If you lose your HDD, you're going to have to rebuild your machine whether you like it or not. To some people this is exciting :) But those people are sick, and are usually recaptured very quickly. To most people, this is a dull and mundane task that they just don't want to undertake. In the case of recovering from a hardware failure, I fit firmly into the latter category. Rebuilding a machine is painfully slow, but there are ways to help get back up to speed quickly:
<ul><li><strong>Imaging/Ghosting</strong> - When you first build your machine, get the OS installed and kill a day installing all the other bits and pieces that you need to do your work (Office, Visual Studio, Firefox, etc), you can use a utility such as <a href="http://en.wikipedia.org/wiki/Ghost_(software)" title="Ghost">Ghost</a> to take a snapshop of your set up and store it to disk. This image can be used to quickly create replica installations on other machines with the same hardware - which is exactly what you want to do when you rebuild your machine. Of course, this comes with the proviso that you have the same hardware when you rebuild. If there's a consistant set of hardware across the organisation, this kind of utility really is a time saver. Unfortunately it's not so fruitful when you have a mixture of hardware set ups.</li><li><strong>Oursourcing</strong> - I'm not a huge advocate of this, but it's still an option. By this I mean either hire another company to make sure that there's a replacement machine ready to rock in the case where something goes horribly wrong, or get a deal going with a company like DELL to just get another machine under warranty within 24 hours.</li></ul>

There are other options too.

The common problem across all these solutions is that you generally can't your environment back up to exactly how it was before the total dismemberment of your machine. It'd be lovely if snapshots of machines can be taken at arbitrary points through the day (without the pain of doing it through Ghost) and restored on arbitrary hardware at the drop of a hat, resulting in, not just your environment but your <em>data</em>, being restored. Yes, this is where those VMs come to the party. You can store your VM anywhere, and run it up on any machine that can boot (and that has a VM player on it).

This sounds so fine and so dandy that it's almost too good to be true - and to a point it is. You have to have some plan of managing the backups of the VM snapshots, and you need to cater for when they need to be copied. Most VM images are quite large due to the amount of software that needs to be installed (imagine how bad it's going to be having to copy a VM of Vista, which without <em>any extra</em> software weighs in at a hefty <strong>9GB</strong>!?).

<strong><u>Conclusion</u></strong>

I like the idea of having my own VM that can follow me from machine to machine. I like the thought of being able to boot multiple operating systems at any time, and interact with them all at once. I also like the idea of getting the most out of my hardware with an installation that only has what it needs installed and running. The problem is that I don't think given the current set of tools available, that you can have both of these at the same time. I'm not saying that the performance of a VM is woeful (far from it), but until I can't notice a difference between playing a game in or out of a VM, I'll probably end up sticking with a normal install.

VMs are great for test environments (reverting to a pre-test state is a two-click process), they're great for persistance of entire operating system state for easy use on multiple machines, and they're even free now thanks to VMware!

Even with all those positives, I think I'd still prefer to stick to a standard OS install for the main operating system and development environment, with VMs as test environments. I would have RAID in each of my machines with shared drives that are backed up over night so that data and more than a day old isn't lost.

At the end of the day, it's down to the user or the copmany to choose what they think fits best for their scenario. I think in Dan's case, he was particularly unlucky. Even if I was in his shoes, I'd stick to what I've just described in the previous paragraph.

What would you guys do? Do you think your company has a strategy that gives you good coverage? I'd like to hear what procedures you guys have in place to cover for this - and I'm sure Dan would like to as well!

PS. For those who care, the title of this blog post <em>should</em> translate to "<em>Recovering from a dead computer</em>".
