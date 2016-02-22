---
categories:
- Metasploit
- Meterpreter
- Security
comments: true
date: 2016-02-22T00:11:46Z
title: TLV Traffic Obfuscation
---

As many of you are already aware, Metasploit and Meterpreter talk to each other using a variety of transports. While the transports may vary from session to session, one thing that doesn't vary is the "protocol" that travels over those transports. This information fits a well-known structure, and is referred to as _TLV Packets_ (Type, Length, Value).

Once a session has been estalbished, the TLV traffic that is sent across the wire contains a bunch of very easily recognisable content, and as such can be detected by Antivirus software, or Intrusion Detection Systems.

Recently, I made a change to the way the packets are formed prior to transmission, and this post is intended to explain the detail of how it works.

Warning: It's _really simple_.

<!--more-->

## The Core Problem

Every command that Meterpreter supports is identified by a string. This string is sent in the TLV packet so that Meterpreter can match the command with the appropriate functionality. The [core commands][] contain values that are _very clearly_ Meterpreter-related, such as `core_loadlib` and `core_patch_url`. There are a bunch more in the [stdapi commands][] that look even more suspicious, such as `stdapi_sys_process_execute` and `stdapi_sys_process_memory_allocate`. Clearly, if those strings are visible in some network traffic, you've probably got a Meterpreter session running somewhere. Depending on your perspective, this could be considered a bad thing!

In the case of both `reverse_tcp` and `reverse_https` payloads, this content is encrypted. In the case of `reverse_http` (which isn't used that often) the content is in the clear. However, many environments have AV or IDS software that man-in-the-middles SSL traffic, and so the TLV content is made visible, rendering `reverse_https` as vulnerable as `reverse_http`.

Many defensive solutions actively review traffic on the wire, and so many of the known TLV commands were being picked up through very simple signature-based detection (ie. string pattern matching). It was beyond time to do something about it.

## The "Fix"

Note: "Fix" is in air quotes because this is a game of cat and mouse. We aren't really fixing anything, we're just changing it enough so that the defenders have to level up.

The main goal of the [PR][] that addressed this concern was to make it so that every single TLV packet that was sent down the wire was obfuscated (not encrypted). The chosen approach did the following (for every packet):

1. Generate the TLV packet as per usual.
1. Generate 4 separate non-`NULL` bytes, and combine them into a single 32-bit XOR key.
1. XOR the entire TLV packet with the 4-byte XOR key (cycled to match the entire payload length).
1. Write the XOR key to the socket first.
1. Finally write the XOR-ed packet to the socket.

See, I told you it was simple.

The chosen key size was hard-coded to 4 because of `$REASONS`. I chose to make sure that each byte was not `NULL` because I wanted to see every byte in the payload change.

With this simple obfuscation technique, it was relatively easy to implement both encoding and decoding in MSF and in all of the separate Meterpreter implementations. The net effect is that the TLV packets never\* look the same. Even if the same command is sent down the wire, the TLV packet will have a different XOR key, and hence a different signature.

As far as defense goes, vendors will have to begin parsing the content if they are to continue using signature-based techniques. Or do something smarter. Either way, the ball is now in their court.

I tested this against 4 different AV applications, and 2 different IDSs, using `reverse_http` (deliberately visible). Prior to the change, they all interfered with and caught TLV traffic. After the change, none of them did.

No, I will not name any of them!

## No More RECV

While I was at it, I wanted to remove another very obvious issue that [Didier Stevens had posted about][didier] a while back. Both `reverse_http` and `reverse_https` transports in Meterpreter were polling Metasploit via `POST` requests to determine if there commands to be executed. The _body_ of the request contained a single, hard-coded word: `RECV`. Talk about easy to find!

As of this changeset, Meterpreter now uses `GET` requests, and does not specify any body content. No more `RECV` to be found. Things look way more like "legit" HTTP at this point. I didn't test the impact of this change on the Snort rules that were mentioned in Didier's post, however I'm sure that some changes will be required after this.

As a side note, I got to meet Didier at 44con in 2015. What an awesome dude. `#achievementunlocked`

## Comments and Suggestions

As always, feedback is welcomed and encouraged. I can, however, answer a few simple ones right now.

> This is beyond trivial. You could have done so much more. Why didn't you?

Because this is, for now, more than enough. It was simple, easy to implement, and didn't add that much overhead to the communication process.

> AV and IDS will level up quickly, what will you do then?

They _will_ level up (I hope). I doubt it'll be too quick though. If it is, we have more modifications in the wings ready to rock. As I said, it's a game of cat and mouse.

> Does this also cover the staging process?

No, this implementation is for Meterpreter's TLV traffic only. At this point, the staging process is still prone to being owned by traffic inspection, as `metsrv` is still sent in the raw. Stageless payloads with `metsrv` baked-in are definitely better if you're able to take that option.

> I can do way better!

Yup, you probably can. I look forward to checking out your Pull Request!

## Thanks

Kudos and appreciation goes out to all the fine people who helped, commented, tested and fixed up my crappy work. In random order, they are Brent, Wvu, RageLtMan, Egypt, Hdm, Timwr and zeroSteiner. You're all awesome.

<small>* For small values of 'never'.</small>

  [core commands]: https://github.com/rapid7/metasploit-payloads/blob/master/c/meterpreter/source/server/remote_dispatch_common.c#L18
  [stdapi commands]: https://github.com/rapid7/metasploit-payloads/blob/master/c/meterpreter/source/extensions/stdapi/server/stdapi.c#L21
  [PR]: https://github.com/rapid7/metasploit-framework/pull/6480
  [didier]: http://blog.didierstevens.com/2015/05/11/detecting-network-traffic-from-metasploits-meterpreter-reverse-http-module/
