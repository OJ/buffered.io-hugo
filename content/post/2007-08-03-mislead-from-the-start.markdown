---
categories:
- Being in the Industry
- Software Development
- WTF
comments: true
date: 2007-08-03T00:00:00Z
title: Misled from the Start
---

I find myself frequently concerned with the lack of ability of a lot of people in our industry. Today I found an example of at least one of the possible reasons why software development professionals turn out to be crap.

They start by reading <a href="http://mezzoscorner.blogspot.com/2007/07/basics-your-first-program.html" title="The Basics - Your first program">tutorials like this</a>.

How can we expect people to become good at what they do if the tutorials they're learning from are like that? If you learn the bad stuff from the outset then you're destined to be writing bad code for many years to come.

<!--more-->

The tutorial above has the following flaws:
<blockquote><p>The compiler takes the code you'll write later on and translates it into binary code which the computer can understand and execute.</p></blockquote>Actually, no it doesn't. The <a href="http://en.wikipedia.org/wiki/Compiler" title="Compiler">compiler</a> translates your source files from one language to another. In this case, the C compiler translates C source code into object files which are relocatable blocks of machine code. At this point none of your code is executable. When the source files are compiled to object files, the <a href="http://en.wikipedia.org/wiki/Linker" title="Linker">linker</a> is then invoked to link the object files into a working executable or library. For more information on this process have a read of <a href="http://www.cprogramming.com/compilingandlinking.html" title="Compiling and Linking">this</a>, as it's much more thorough, and (more importantly) it's correct.
<blockquote><p>If you have a compiler without a built in editor you can just use any text editor.</p></blockquote>Compilers do not have built-in text editors. Compilers have the job of translation. An <a href="http://en.wikipedia.org/wiki/Integrated_development_environment" title="Integrated Development Environment">IDE</a> generally comes with some kind of source code editor. It also makes it easy to invoke the compiler and linker when your source has been modified. Do not get confused between a compiler, a text editor and an IDE.
<blockquote><p>Every C program must have a function called "main".</p></blockquote>No. Every C program that is to be compiled to an executable must have an <a href="http://en.wikipedia.org/wiki/Entry_point" title="Entry point">entry point</a>. In console C programs, the entry point is generally called <strong>main</strong>, but this is configurable with most compilers/linkers/IDEs. With GUI applications, such as <a href="http://en.wikipedia.org/wiki/Win32" title="Windows API">Win32</a>, the default entry point can be different (eg. <a href="http://en.wikipedia.org/wiki/WinMain" title="WinMain">WinMain</a>). If your C code is going to form a library (lib, o, dll, etc) the rules are different.
<blockquote><p>A function is just a container that holds a series of commands that tells your computer what do to.</p></blockquote>This isn't totally wrong, but it's not really right either. <a href="http://en.wikipedia.org/wiki/Function_%28computer_science%29" title="Subroutine">Functions</a> are blocks of callable code which perform a specific task inside your program.
<blockquote><p>The next line is your main function.</p></blockquote>Actually, this line is the function's signature and the beginning of the function's definition. The function body is everything between the <strong>{</strong> and <strong>}</strong> that follows the function's signature.
<blockquote><p>The int in this line can either be left or or changed to another basic datatype.</p></blockquote>This is <strong>very, very wrong!</strong> First of all, 'int' specifies the type of the function's <a href="http://en.wikipedia.org/wiki/Return_value" title="Return statement">return value</a>.

Secondly, the entry point is the first part of your program that is executed, and the operating system expects the result of that function to be meaningful. Passing anything other than an integer back to the operating system is completely wrong. Meaningful return codes should be used instead. Do <strong>not</strong> use anything other than int for your main function's return type.
<blockquote><p>The final line, return 1; Tells the compiler that the program is finished and returns an integer.</p></blockquote>The last line does not return an integer. The last line specifies that the value of '1' should be returned to the caller. This statement would be perfectly legal for other return types such as <em>char</em>. The value that you return must be of the same type that you've specified as your return type in your function signature.

Not just that, but do not use <a href="http://en.wikipedia.org/wiki/Magic_number_%28programming%29#Unnamed_Numerical_Constant" title="Unnamed Numerical Constant">magic numbers</a>. You should use constants that give the code meaning. If you're returning a value to the operating system, you need to understand what that number means. In most cases, the value of 1 indicates that an error has occurred in your program. This means that if your program is called as part of a greater set of commands, the likelihood of the process failing increases because the caller will believe that your program has failed. This return statement not only contains a magic number, but it contains the <em>wrong</em> magic number. If the developer had used the predefined constants that are part of <em>stdlib.h</em> then not only would he have avoided this problem, but his code would also be more portable across platforms and operating systems.
<blockquote><p>printf("string"); is used to output a string onto the console, you can substitute any string into it (as well as variables).</p></blockquote> <strong>Do not use variables as the first parameter to any printf()-like function.</strong> There's no excuse for it. It's lazy, and it exposes a security hole in your program, namely <a href="http://en.wikipedia.org/wiki/Format_string_attack" title="Format string attack">format string attacks</a>.
<blockquote><p>{ and } are used to encapsulate logical blocks of code (such as functions). All of the code inside these braces are grouped together.</p></blockquote> Curley braces define <a href="http://en.wikipedia.org/wiki/Scope_%28programming%29" title="Scope (programming)">scope</a>. It's as simple as that. They don't "group" code together.

<h3>The Point</h3>
Too many people are writing misleading or meaningless tutorials which don't aid in helping people write proper code. They crank out examples without understanding what they're teaching. They don't think about the long term effect on those learning. The result? An industry full of plebs who don't really know how to do their job properly.

In case you didn't notice, it shits me up the wall :)

So, for the sake of clarity, this is what the example should have looked like:
```
#include <stdlib.h>  /* contains EXIT_SUCCESS */
#include <stdio.h>   /* contains printf() */

/* main() is the program's entry point */
int main()
{
  /* write some text to the screen */
  printf( "Hello, World!\n" );

  /* indicate successful completion to the
     operating sytem. On most platforms this
     value is 0. */
  return EXIT_SUCCESS;
}
```

