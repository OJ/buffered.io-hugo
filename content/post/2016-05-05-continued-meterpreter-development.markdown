---
categories:
- Metasploit
- Meterpreter
- Security
comments: true
date: 2016-05-05T07:38:32+10:00
title: Continued Meterpreter Development
---

Active users of Metasploit will no doubt be aware that Meterpreter is still being actively developed and enhanced by a bunch of people. I'm lucky enough to still be one of them! In this post I just want to cover a few things that have been done to it recently, and to give a bit of visbility of how I am able to continue contributing in the way that I do.

<!--more-->

## Credit where it is due

I love Meterpreter. It's an amazing bit of kit, and I really do love working on it. The people who built the early versions were clearly visionaries whose shoulders we all get to stand on.

I actively contribute to Meterpreter using my own personal time because I think it's continued improvement is important, and I like to help it evolve. One such contribution is the recent [TLV Traffic Obfuscation][] feature which helps to make AV and IDS suffer a little while trying to detect the communications that passes better MSF and Meterpreter.

However, the bulk of the big features that have been built, and continue to be built, are not done on my own time. It's hard to justify weeks of effort while running a [security business][bb].

It's important for you all to know that [Rapid7][] are the ones footing the bill for these major features. They pay me for my time, and they support me through the process of development. Without them, the speed at which these things get delivered would be hindered, and the tool that you all make use of would not move forward at the rate that it does.

Many users of Metasploit believe that everything I'm doing for Meterpreter is done out of my own pocket, and I want to be 100% clear that while I do _some_ work of my own volition, the bulk of it is all thanks to Rapid7 and the Metasploit team. So please give them a squeeze and a beverage next time you see them if you think this work is valuable to you.

It's also important to point out that I am making this statement because I want you to know, and not because Rapid7 want you to know. I am not under duress or instruction here. This is all about giving credit where it's due.

To the folks at Rapid7, especially those in the Metasploit team, I love you all and thank you for your continued support.

## The Powershell Extension

A recent bombshell was dropped in the form of a new extension that allows for direct interaction with a lot-loaded Powershell interpreter. Prior to this extension existing, we were stuck with invoking `powershell.exe` on the command line and passing in scripts. We weren't in a position to interact with it like we were with `cmd.exe` (for example). This was painful, and with so much ... er.. power.. being provided by Powershell, we were finding that more and more people wanted to use it without having to juggle results via `cmd.exe`. There were other issues too:

* Single-shot `powershell.exe` invocations didn't give us the ability to have persistent sessions.
* It wasn't always clear which `powershell.exe` was being invoked.
* It was hard to do anything complex.

The Powershell extension was designed to solve this, and is based on a similar idea that the likes of [Empire][] use, though it's different in some ways. The foundation is built on the work that [Lee][] put into [Unmanaged Powershell][], and so he deserves a huge deal of credit, lots of cuddles, and a noogie or two.

![NOOOOOGIIIIIEEEE!!](/uploads/2016/05/ace-ventura-noogie.jpg)

At this point I'd like to defer to [DarkOperator][] who has already put together a [very good post][darkoperator-powershell-post] that covers the extension in quite a bit of detail. Thank you Carlos, you saved me quite a bit of time! I recommend going over there and reading it in full.

## Reverse Port Forwards

One of the most frequenly requested features for Meterpreter over the last few years has been "reverse port forwards". While we've had the ability to pivot through a Meterpreter session in a forward direction for quite some time, it wasn't possible to go in the other direction.

Having the ability to tunnel a listener "into" a network and have connections pivot out to a local port has quite a lot of value in assessment scenarios. To get around the lack of support in Meterpreter, most people use the likes of [plink][] or a similar tool. The problem with this is that it often requires dropping binaries on the target's disk, which isn't ideal.

As of yesterday, the [reverse port forward][] functionality is now present in Metasploit. It is possible to use the `portfwd` command with the `-R` option to create a reverse port forward. When invoked, Meterpreter opens up a port and listens for connections. When a connection is received, a new channel is created and data is shuffled back and forth through that channel in the same way it does for typical port forwards.

Please go and have a play with it, and if you have any issues, hit me up or open a Github issue.

## Pivots via Named Pipes

Reverse port forwards give us the ability to pivot sessions out of a network from machines that don't have direct internet connectivity. They also reduce noise on the wire a bit by not having extra connections heading to the untrusted Tubes.

The only thing better than this is having the ability to pivot without having to go through obscure ports and custom listeners. Opening a port can also be noisy, and so ideally we would use something else that doesn't require it. This is where [Named Pipes][] come in, which operate over [SMB][] and hence blend in a bit more with existing Windowsy traffic.

This functionality is currently in development; I hope to have a Pull Request submitted very soon. The goal is to have:

* A new transport type called `reverse_named_pipe` that makes use of the Named Pipe functionality that's native to Windows.
* Full support in Stageless Meterpreter.
* Two new stagers, x86 and x64, that stage Meterpreter over Named Pipes.

I would also like to be able to migrate while the Named Pipe transport is active, but I'm still not sure if that will happen yet. There's more research to do here.

It's worth noting that Metasploit itself doesn't have the ability to "be a SMB-compaitible Named Pipe server" (at least yet), and so it's not possible to talk directly to MSF over Named Pipes across port 445. To make use of this feature, an existing Meterpreter session must be present. The new Named Pipe handler forces the user to specify a `SESSION` when the handler is created, as this is where the listener will be created. When the listener is created, the session is set up with ACLs configured to allow `EVERYONE` to connect, which ties the implementation to Windows domains only. This is a short-term limitation but is built this way to allow non-admins to create the Named Pipe. To create NULL sessions, the user would have to be admin with full privileges, as it requires other hacks (such as poking registry keys) in order to work. This will come down the track, but for now we'll be starting with somethig simpler.

While the general use of Named Pipes is rather simple, there are all kinds of use cases in Meterpreter where using Named Pipes as a transport cause heartache. One example is handling cases where pivot points fail, or when `sessions -K` is executed (killing the pivot means we can't kill the client, as the comms channel is gone!). When the work is finished, I intend to document these nuances on the [Wiki][] so that it's clear to all why things are the way they are.

Yes, I know other tools "do this already". Tell me something I don't know.

## That'll do pig. That'll do.

That's all for now. Be sure to keep tabs on both the [Metasploit Repository][] and the [Payloads Repository][] if you're keen to see things as they happen. Otherwise, wait for more blogs here or over on the [Rapid7 community][] site.

Thanks for reading.


  [TLV Traffic Obfuscation]: /posts/tlv-traffic-obfuscation/
  [bb]: https://beyondbinary.io/
  [Rapid7]: https://www.rapid7.com/
  [Empire]: http://www.powershellempire.com/
  [Lee]: https://twitter.com/tifkin_
  [Unmanaged Powershell]: https://github.com/leechristensen/UnmanagedPowerShell
  [noogie]: https://www.urbandictionary.com/define.php?term=noogie
  [DarkOperator]: https://twitter.com/Carlos_Perez
  [darkoperator-powershell-post]: http://www.darkoperator.com/blog/2016/4/2/meterpreter-new-windows-powershell-extension
  [plink]: http://www.chiark.greenend.org.uk/~sgtatham/putty/download.html
  [reverse port forward]: https://github.com/rapid7/metasploit-framework/pull/6753
  [Named Pipes]: https://msdn.microsoft.com/en-us/library/windows/desktop/aa365590(v=vs.85).aspx
  [SMB]: https://en.wikipedia.org/wiki/Server_Message_Block
  [Wiki]: https://github.com/rapid7/metasploit-framework/wiki
  [Metasploit Repository]: https://github.com/rapid7/metasploit-framework
  [Payloads Repository]: https://github.com/rapid7/metasploit-payloads
  [Rapid7 community]: https://community.rapid7.com/welcome

