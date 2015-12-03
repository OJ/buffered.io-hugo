---
categories:
- Security
- Shellcode
comments: true
date: 2014-03-21T22:31:39Z
title: Jumping with Bad Chars
---

During the course of exploit development it is not uncommon to require jumps in your shellcode. The most common case for these jumps is when doing [SEH overwrites][], due to their nature. There are times when the author of the exploit has a hard time performing these jumps due to the fact that only a subset of characters are deemed valid for use by the target application in that particular input field.

In this post I want to briefly cover a few options for performing those jumps in such a scenario.

<!--more-->

### SEH overwrites

SEH overwrites tend to follow the pattern of having an input field overwrite its target buffer and overflow beyond the return address through to the `SEH` pointers. The `SEH` pointer is overwritten with an address that contains a `POP # POP # RET` instruction sequence, and this results in `EIP` being redirected to the `NSEH` pointer address. The `NSEH` pointer address is contained in the `4` bytes immediately preceding the `SEH` pointer.

In order to exploit an SEH overwrite, the attacker need to execute shellcode which will perform some kind of operation on the target machine. It could be a reverse shell, a bind shell or something like a command execution. Regardless of the payload, it's almost certain that `4` bytes is not going to be enough to fit the entire instruction sequence in.

So once control has been gained and `EIP` is pointing to those `4` bytes, the first thing that needs to be done is execution needs to move to an area of memory that is controlled by the attacker via a jump. Depending on the exploit, the attacker may need to jump forwards or backwards.

### The basic JMP

Jumps tend to only require `2` bytes, and so in the case of SEH overwrites it is common to see the `NSEH` entry overwritten with a `2` byte jump followed by `NOP` instructions to fill the gap. See the image below for a synthetic example:

![Basic SEH overwrite JMP](/uploads/2014/03/jmp-1-basicjmp.png)

For the sake of discussion we'll assume that the address at `0x77F7F594` is the `NSEH` pointer location and that `0x77F7F598` is the `SEH` pointer location. The example shows that `NSEH` contains a `JMP SHORT` instruction which moves control forward to the address at `0x77F7FC9C`

Once the SEH has been overwritten and the `POP # POP # RET` sequence has been executed, the `JMP` instruction is hit and `EIP` then gets set to the value immediately following the `SEH` address. We can assume at this point that the attacker has been able to control the area of memory immediately following and hence from there they can perform arbitrary execution.

### Bad characters

It is easy to see that flaws that allow for SEH overwrites can appear in any application, and they do! However, some applications only allow a restricted set of characters to be used for input. As an example, a web server might be vulnerable to a stack buffer overflow when handling the URI of a given request. The problem for the attacker is that characters that are allowed in the URI are very limited. More often than not, non-printable or extended characters are not allowed in the target buffer and hence using them will result in behaviour that avoids or changes the process of exploitation.

In the above example we can see that the `JMP` instruction maps to the bytes `EB 06`, and `EB` is a byte that tends not to make web servers very happy. As a result, using this instruction in the `NSEH` block isn't possible. Instead we need to find other jumps which are bad-character friendly. As a generalisation, if we can stick to instructions made up of bytes that are lower than `7E` we can usually get by. This does come with some caveats, such as spaces, newlines or `/` characters when dealing with web server URIs.

So with this constraint in place, how does the attacker perform the simple jump?

There are a number of _conditional_ jumps available in the x86 instruction set which could be used, but the problem we have with conditional jumps is that we have to make sure that the condition is true before we hit the jump instruction.

A well known example is the `JA` instruction, otherwise known as _Jump if above_, which has the byte code of `77`. This instruction will result in a jump if both the `CF` and `ZF` flags are `0`. To prepare for this the attacker can use the extra two bytes to operate on registers and modify flags.

In our example we will assume that the carry flag is already zero at the time of our jump, but the zero flag is unknown. We can force the zero flag to be `0` by using the `DEC ESP` instruction. Here is what it might look like before execution:

![Flag modification - Before](/uploads/2014/03/jmp-2-cond-before.png)

And after running over the two `DEC ESP` instructions we see this:

![Flag modification - After](/uploads/2014/03/jmp-2-cond-after.png)

As a result the jump is taken and our code continues to execute as we would expect.

There are many other ways that this kind of approach can be used with a mixture of other types of jumps. What I'd like to show now is the way that I like to do it, which is a little different to the common examples around the web.

### What could go wrong?

The approach used above works fine, but isn't necessarily the best. Why is this?

When using `JA` we have made an assumption that `CF` will be `0` all the time when in fact it might not be. If `CF` is ever `1`, then the jump would not be taken. This kind of assumption might apply regardless of which kind of conditional jump we choose to use. Guaranteeing the correct combination of flags in a given scenario _might_ not be possible (though admittedly, it usually is).

For me personally I also have the problem of being stupid. I don't remember all of the combinations of flags required to make a jump work. While I know I can fall back on reference material I would much prefer to have an approach that:

* Doesn't rely on forcing a condition prior to taking the jump.
* Doesn't rely on me having to remember or research combinations of flags.
* Still fits within the often-required `4`-byte block.

As a result, I use something that I like to call the _Jump Net_.

### The Jump Net

The Jump Net isn't anything revolutionary. It's not mind-expanding. It's not something that I have come up with that hasn't been thought of before. However, I feel compelled to write about it because I'm yet to see anyone talk about it. I haven't even seen it in any of the exploits that I've read or researched.

There's no doubt that it's out there and being used by everyone all the time. But here it is, bloggified in all its simplistic glory.

The theory is really simple. We have an unknown state `s`, this could be a flag for example, and a predicate `P` which will test `s` to determine whether or not a jump is made. Those who are savvy with the basics of predicate logic will know that `P(s) || ~P(s) == true`. That is, any boolean value `b` logically `OR`ed with `~b` will result in a value of `true`. In our case `s` is not known (ie. the content of the flag at a given time might not be known), but `P` is known (ie. we are using `JA`, or `JO` for our jump). To determine `~P`, we can just look for the logical opposite of the jump instruction that we have chosen. For example, the "opposite" of `JA` is `JNA` (which is a synonym for `JBE`, or _Jump if below or equal_).

To make use of this knowledge, we just need to pair up two logically opposing jump conditions and have `EIP` pass over them. If the first condition succeeds our jump is taken and we win. If not, the second condition _must_ succeed (it's the inverse of the first after all), and so we still win.

Each conditional short jump just happens to have a very similar instruction byte value to its logical opposite. For example, `JNA` is `76` and `JA` is `77`. So this means that the chances of the characters being _bad_ are slim (but admittedly non-zero).

Armed with this knowledge, we can pair up two jumps which when combined form a "net" of conditions which will always "catch" a positive/true case and perform the jump. Here's an example that uses `JO` and `JNO`:

![The Jump Net](/uploads/2014/03/jmp-3-jojno.png)

When `EIP` is at `0x77F7F594` a test is performed to see if `OF` (the overflow flag) is set to `0`. If it is, then a jump is performed and `EIP` moves to `0x77F7F59C` .

If the jump is not performed then `EIP` moves to `0x77F7F596`, at which point a test is performed to see if `OF` is set to `1`. We can conclude that this jump _must always happen_ because we already know that `OF` wasn't set to `1` which caused the previous jump to fail. As a result `EIP` will move to `0x77F7F59C`, just as the first jump does.

Note how similar the instructions are. If `70` is a valid byte, there's a good chance that `71` is as well. Admittedly this isn't always the case, but we do have quite a few conditional jump pairs to play with, so there's a good chance that at least one of them would work.

### Conclusion

That's literally it. As I promised, it isn't anything that's mind-blowingly insightful. It isn't something that can be applied every time because of bad characters. However it does work most of the time, and it's another handy technique to have in your shellcoding toolbox.


  [SEH overwrites]: https://www.corelan.be/index.php/2009/07/25/writing-buffer-overflow-exploits-a-quick-and-basic-tutorial-part-3-seh/
  [jmpsummary]: http://www.unixwiz.net/techtips/x86-jumps.html
