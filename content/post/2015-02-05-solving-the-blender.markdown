---
categories:
- CTF
- Security
comments: true
date: 2015-02-05T15:27:57Z
title: Solving 'The Blender'
---

This post contains a walk-through of the process required to solve _The Blender_. _The Blender_ was a reverse engineering challenge that I built and submitted to [hyprwired][] for inclusion in the [Kiwicon][] [CTF][]. The challenge wasn't intended to be too mind-boggling, but it turned out that nobody was able to solve it on the day.

I won't deny that this didn't me feel awful _and_ great at the same time! However, I don't want this lying in the depths of history unsolved, so I wanted to show people the story behind the challenge, and how to nail it using IDA and your brain.

Please [download][] it if you would like to follow along.

<!--more-->

## Introduction

I don't know how many points the challenge was worth. But I do know that it was one of the ones used as a tie-breaker challenge on the day. There is no write-up, no hint, nothing. All you know is that this thing has a flag inside it somewhere and you need to fish it out.

First of all, we need to see what this thing is (even though the extension is a bit of a giveaway):
```
$ file the_blender.exe 
the_blender.exe: PE32 executable (console) Intel 80386, for MS Windows
```

OK good, it's not x64 so hopefully the experience of reversing won't be too messy. Let's fire it up to see what it does:

![Crash on run](/uploads/2015/02/blender/00.png)

OK, so that's the way it's going to be! Time to fire up IDA and have a look around. Once the application is loaded and the binary has been parsed, we can see that the entry point to the program is shown.

![Entry point](/uploads/2015/02/blender/01.png)

We can skip over the first call as this just does a bunch of setting up, and we can navigate to the second instruction which is a `JMP`.

![Program setup](/uploads/2015/02/blender/02.png)

This section of code contains more initialisation functionality. However, following the code path a little we can see that after the initial set up there is a call to an unnamed function:

![Interesting call](/uploads/2015/02/blender/03.png)

If we jump into this call, we'll see a bunch of code that actually doesn't call any other functionality except for right at the end. The header of that function looks like this:

![Function header](/uploads/2015/02/blender/04.png)

At this point, we can take an educated guess that this part of the program is the "real" entry point (probably `main`) and from here we can start to analyse what is going on. Let's rename the function and assign it some parameters to make it more readable (using the `Y` shortcut key).

![Fixed function header](/uploads/2015/02/blender/05.png)

Next, we can read a bit of code to see if there are any obvious candidates for causing the application to crash. The first branch in the code stood out like a sore thumb:

![Deliberate crash](/uploads/2015/02/blender/06.png)

This looks like a deliberate NULL pointer dereference. The `xor eax, eax` instruction sets `eax` to `0`, and the next instruction tries to read from the address referenced by `eax`. It's no wonder that the program crashed. The instructions leading up to this are also interesting. Note the instruction `mov eax, large fs:18`, as this is an indicator of a common anti-debugging technique. This instruction gets access to the to the [Thread Information Block][], a structure in Windows that contains some handy information about the current thread. At offset `0x30` from the start of this structure, a pointer can be found which can be used to locate a flag that indicates whether or not the process currently has a debugger attached to it. The instruction `cmp byte ptr [eax+2], 0` performs the check to see if this is actually the case.

Bear in mind that the process crashed _without_ a debugger attached. So at this point it's safe to assume that there are more traps hidden in the code, and this one was just placed here to slow us down. We probably haven't actually found the cause for the initial crash.

We can patch this program a little to stop it from crashing when it _does_ hit this case (later when we are actually debugging) by replacing the `mov eax, [eax]` instruction with `NOP` instructions (2 of them to be precise).

Opening up the "Hex View" allows us to edit the bytes directly.

![Before patching the NULL dereference](/uploads/2015/02/blender/07.png)

These bytes can be changed to `0x90` (after pressing `F2` to enter edit mode):

![After patching the NULL dereference](/uploads/2015/02/blender/08.png)

Once modified, pressing `F2` again leaves edit mode, and the binary now looks patched according to IDA.

![IDA's view of the patched bytes](/uploads/2015/02/blender/09.png)

We need to write this change back to the binary so that when we run it again it doesn't break any more. We go to `Edit`, `Patch Program`, `Apply patches to input file` in the menu, and this presents us with the patch dialog.

![Patch dialog](/uploads/2015/02/blender/0A.png)

The default values that are specified are good enough for our needs, so we can just click OK to have the binary modified. With that done, we know we can expect to see some different behaviour under the context of the debugger, however, we haven't really changed a code path that was referenced when we first accessed the program from outside IDA, which means that it should still crash:

![Another crash on run](/uploads/2015/02/blender/00.png)

As we suspected. Back to IDA we go, and this time we're going to run it under the debugger. I chose the "Local Win32 Debugger" in the debugger toolbar:

![Choosing a debugger](/uploads/2015/02/blender/0C.png)

I then pressed the green triangle/startup thingy and watched IDA break with an error:

![Second crash error message](/uploads/2015/02/blender/0D.png)

Clearly this a divide by zero error. However, before we go patching the binary again, we need to have a better look at what might be causing the problem to see if there are any clues as to what the program needs to do. This is what IDA looks like at the time of the crash:

![Second crash instruction](/uploads/2015/02/blender/0E.png)

Interesting. The code shows that the value of `argc` (ie. the number of arguments passed to the function) is pulled into `ecx`. Later, `ecx` is then decremented before being used as part of `idiv ecx`. As we already know, a program that is called with `0` arguments on the command line still has a single argument given to it: the path/file of the program that is running. Therefore, in this case `argc` is `1`, and hence at the point the `idiv` instruction is called, `ecx` is `0`. This is what causes the issue. Instead of patching, we should just be able to pass in a single argument on the command line and watch this problem go away. Let's do that.

![Still crashing](/uploads/2015/02/blender/0F.png)

Damn! Unfortunately we didn't get any useful output, so back to the debugger we go. First, we go back to IDA and open the `Process options` dialog from the `Debugger` menu. This allows us to specify the command line parameters. I decided to use the easily recognisable value of `ABCD`:

![Process options](/uploads/2015/02/blender/10.png)

Launching the process again shows another meaningful error message:

![Third crash error message](/uploads/2015/02/blender/11.png)

Another divide by zero. After closing the error message dialog we can see the line of code that is causing the error.

![Third crash instruction](/uploads/2015/02/blender/12.png)

The code is breaking almost immediately after the previous section that we avoided by adding a command line parameter. It's clear that `ecx` is being overwritten by something offset from `esi`. Scrolling back up we can see that `esi` actually contains a reference to `argv`:

![ESI points at argv](/uploads/2015/02/blender/13.png)

So if `esi` points at `argv` then that's the same as `argv[0]` (the first argument, which is the name of the program being run). From this, we can infer that `esi+4` points to the second argument, and `esi+8` points to the third argument. The instruction `mov al, [ecx]` is attempting to get access to the first character in the third argument, but if it's not there then things go bad.

What this means is that it's crashing because we aren't giving it enough arguments. We are giving it two (the program name and `ABCD`) and it's expecting another one because it's trying to read the first character from it. Before we race off and update the process options, it's worth looking at the next instruction that contains `mov cl, [ecx+1]`. This is moving the _second_ character of the third argument into `cl`. So we had better make sure that we specify an argument of at least `2` characters.

We're making progress. Before we move on, let's take another look at the code, this time with some comments added by me:

```
push    2Ch              ; push 44 onto the stack
mov     al, [ecx]        ; put the first byte of arg 2 into al
mov     cl, [ecx+1]      ; put the second byte of arg 2 into cl
mov     [ebp+var_31], al ; store the first byte in var_31
movsx   edi, cl          ; mov the second byte into edi (sign extend)
pop     eax              ; pop 44 into eax
div     edi              ; divide eax (44) by edi (second byte)
xor     ecx, ecx         ; clear ecx
mov     ebx, eax         ; copy the result of the divide into ebx
mov     [ebp+var_38], al ; copy the result to a member variable.
```

It seems that the value of `44` has some potential importance here, especially given that it's used in a calculation. The `div` instruction is an unsigned integer divide, so at this point it might be safe to assume that the thing we're using to divide it with needs to be factor of `44`. Thankfully this doesn't leave us with many options. They are: `1`, `2`, `4`, `11`, `22` and `44`. With this knowledge we can modify IDA to add a meaningful name to `var_31` and `var_38` (I used `first_byte` and `input_divisor` respectively).

Looking further down, we see there's then a big loop of "stuff".

![The Blender](/uploads/2015/02/blender/14.png)

No, I don't expect you to be able to read that. But on your own screens, I think you should be able to see that there's a lot of character munging going on. This is probably why it's called _The Blender_ (protip: that's exactly why). Characters are swapped around in what seems to be random - but is in fact a very orderly - manner. Read through the code a bit to see if you can get a feel for it. If you don't feel like it, that's fine, you can take my word for it.

Before we continue, one more thing must be taken into consideration. At various points the code makes use of values offset from `ebp+var_30`. For example:

![Data?](/uploads/2015/02/blender/15.png)

This would indicate that there is a buffer in memory (probably on the stack) being modified as part of this loop. If we return to the top of the function, we'll see some interesting things happening:

![Data setup](/uploads/2015/02/blender/16.png)

The highlighted sections at the bottom show that a bunch of bytes are being written to stack memory starting at `var_30`. The amount of data that is written is:

```
movdqu  [ebp+var_30], xmm0        ; 16 bytes
movdqu  [ebp+var_20], xmm0        ; 16 bytes
mov     [ebp+var_10], 0D1F5E3FEh  ; 4 bytes
mov     [ebp+var_C], 0C3DCEEF5h   ; 4 bytes
mov     [ebp+var_8], 0D7C9C3E3h   ; 4 bytes
```

We have hit the jackpot. The sum of those numbers is `44`, the same as the value that was used further down in the code. At this point we can make the following statements in relative confidence:

1. The program prepares a block of data up front in `var_30`. We can rename this to `secret_data` given that it looks like it's the source and destination of a bunch of "blending".
1. The program expects `argc` to be `3`.
1. The first char in `argv[2]` is used, but we don't know what for yet.
1. The second char in `argv[2]` is used as a divisor for the size of the block of data that is loaded at the start of the function.
1. For the whole block of data to be processed, the divisor must be an factor of `44`.

From this we should be able to create a set of command line parameters that will cause the function to go off and do something without crashing. To do that, I'm going to cheat. Instead of launching it with command line parameters containing non-printable characters, I'm going to set a breakpoint in the debugger and overwrite the values in memory before letting the program continue. The following shows where I set my breakpoint (using `F2`):

![Breakpoint](/uploads/2015/02/blender/17.png)

We have to make sure that we specify two parameters on the command line, so we need to adjust the process options again:

![Run props](/uploads/2015/02/blender/18.png)

Now we kick the debugger off and we get to the breakpoint we set. In the register window the values for `AL` and `CL` have been set to `0x41` and `0x42` as we expected:

![AL and CL](/uploads/2015/02/blender/19.png)

Using the register editor, we modify the value in `CL` so that it is one of the divisors of `44` as we determined earlier. I went with `4` to start with.

![CL set to 0x04](/uploads/2015/02/blender/1A.png)

I decided to let the program run from this point to see what would happen. The result: Nothing. But **it did not crash**. This is a good sign. We're able to get the program to appear quite stable, despite not getting any output. We've delayed the inevitable, and it's now time to try to understand what the program is doing in the middle.

The next thing we should do is get an idea of what is going on with that `first_byte`, if anything at all. Using IDA we can follow the references to this value, and this lands us at the following location:

![first_byte getting used](/uploads/2015/02/blender/1B.png)

The `first_byte` value is loaded into `cl`, and then from there it is `xor` with all `0x2C` bytes of the data in `secret_data`! It looks like there might be some rudimentary encoding going on here. This decoding happens after a bunch of other crazy loops so we can assume that more is at play, but for now we do have something to go on.

As part of the CTF rules, we know that flags take the format of `KIWICON_CTF{...}`. If we assume that some of the characters that come out of the blending code are actually ready to be `xor`ed with the key, then we might be able to extract the correct key by performing the `xor` using those known values, such as `K`.

To do this, we set a breakpoint right at the point where the first `xor` instruction is, and then run the application as we did before. Again, I set the value of `cl` to `04`, but modified `al` so that it contained `4B` (hex for `K`, the first letter in the flag). We let the application run until it hits the breakpoint, and this is what we see:

![Break before xor](/uploads/2015/02/blender/1C.png)

The value in `cl` is still `K`, and it's about to be used in the `xor` operation on the first character in the `secret_data`. We step over the `xor` instruction, by pressing `F7`, and then take a look at the resulting value in memory:

![Potential key extracted](/uploads/2015/02/blender/1D.png)

Running `xor` with the value `K` produced a value of `0xAA` as the key. If that first character was supposed to be `K` at this point, then it means that `0xAA` is the value we need to specify to decode the entire flag. We can restart our process with our updated value and see what the result looks like. This time, instead of setting a breakpoint on the `xor` instruction, we'll set a breakpoint on the call to `_fprintf` near the end of the function.

After doing the rinse-repeat cycle with `al` set to `0xAA` and `cl` set to `0x04`, we run and hit our breakpoint:

![fprintf breakpoint](/uploads/2015/02/blender/1E.png)

This image shows that the `secret_data` buffer pointer is stored in `esi` (clearly about to be used as a source pointer). The value in `ebx` is moved to `edi`, indicating that this is a destination buffer. Given that `ebx` contains the pointer that was returned from a call to `malloc` a few lines before an assumption can be made that the code is about to copy the `secret_data` buffer to a new area of memory allocated with `malloc`. Let's peek at the content of `ebx` at this point to see what it contains:

![Munged flag](/uploads/2015/02/blender/1F.png)

This is looking very good! While it's clear that the flag isn't yet correct, it would appear that the `xor` key has done its job. The value above shows the presence of all the bits of a typical Kiwicon CTF key. However, they're not in the right order.

The first and last values are correct, but it's hard to know what else is. Going back over what we have encountered so far, the only thing that we're not yet sure of is whether we have specified the right value for `cl`. If `cl` is the problem, then using the value `04` clearly is incorrect. We're left with a mini brute-force operation, going through the rest of the potential values for `cl`, those being `1`, `2`, `11`, `22` and `44`. It's probably safe to assume that the really high and really low values aren't going to be what we're looking for. So instead of a straight brute force from lowest to highest, I started with the median: `11` (`0x0B` in hex).

Again, we rinse and repeat, using `0xAA` and `0x0B` for `al` and `cl` respectively. When our breakpoint is hit, we again take a look at `ebx`, and this time ...

![Revealed flag](/uploads/2015/02/blender/20.png)

That looks very flag-like to me!

### Show notes

I did my best to walk through this challenge in the manner I expected people to solve it. Given that I created it, I've obviously get the advantage of knowing what to look for, but I still hope that this walk-through is considered feasible.

I'd like to highlight a few things about the challenge that aren't obvious from experiencing it:

* The data buffer is actually a 44-byte transposed matrix that has been XOR'd with `0xAA`.
* An in-place reverse-transposition is what is happening in the middle of the code.
* The value that goes in `cl` is actually a value that represents the width of the matrix prior to doing the transposition, hence why it has to be a factor of 44.
* The transposition algorithm in action looks like it's "blending" the data together, hence why it's called _The Blender_.
* The reason the flag isn't printed out at the end is because the call to `_fprintf` deliberately passes in `stdin` instead of `stdout` for the `FILE*` parameter. Feel free to attempt to change this as a though experiment so that it does spit out the value into the console.

I hope you enjoyed reading this. I'd like to shout out to the [Kiwicon][] folks, and [hyprwired][] in particular for letting me donate my challenge to their fantastic CTF.

If you'd like to see the source code to _The Blender_, please drop me a line.

Until next time!

  [Kiwicon]: https://kiwicon.org/
  [hyprwired]: https://twitter.com/hyprwired
  [CTF]: https://kiwicon.org/the-con/hamiltr0nno-carrier/
  [download]: /uploads/2015/02/blender/the_blender.zip
  [Thread Information Block]: http://en.wikipedia.org/wiki/Win32_Thread_Information_Block
