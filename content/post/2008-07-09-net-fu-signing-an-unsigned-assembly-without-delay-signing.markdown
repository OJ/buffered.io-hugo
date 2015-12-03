---
categories:
- HOWTO
- Microsoft
- Security
- Software Development
- Tips/Tricks
- C#
comments: true
date: 2008-07-09T00:00:00Z
tags:
- .net
- disassemble
- ilasm
- ildasm
- MSIL
- sign
- snk
- strong name
title: '.NET-fu: Signing an Unsigned Assembly (without Delay Signing)'
---

This article is also available in [Italian][].

----
The code-base that I am currently working with consists of a large set of binaries that are all [signed][SignedAssemblies]. The savvy .NET devs out there will know that any assembly that's used/referenced by a signed assembly must _also_ be signed.

This is an issue when dealing with third-party libraries that are not signed. Sometimes you'll be lucky enough to be dealing with vendor that is happy to provide a set of signed assemblies, other times you won't. If your scenario fits the latter (as a recent one did for my colleagues and I), you need to sign the assemblies yourself. Here's how.

<!--more-->

_Note:_ [delay signing][DelaySigning] is not covered in this article.

Scenario 1 - Foo and Bar
------------------------

* `Foo` is the component that you're building which has to be signed.
* `Bar` is the third-party component that you're forced to use that _isn't_.

<img src="/uploads/2008/07/foobar.png" alt="Relationship between Foo and Bar" />

Grab [Bar.dll and project][BarProj] along with [Foo.dll and project][FooBarProj] to see a source sample.

You'll notice _Foo_ has a .snk which is used to sign _Foo.dll._ When you attempt to compile _Foo_ you get the following error message:

> Assembly generation failed -- Referenced assembly 'Bar' does not have a strong name.

We need to sign _Bar_ in order for _Foo_ to compile.

<img src="/uploads/2008/07/step1.jpg" style="float: right; margin-left: 5px; margin-bottom: 2px;" alt="Disassemble Bar" />
### Step 1 - Disassemble Bar ###
We need to open a command prompt which has the .NET framework binaries in the PATH environment variable. The easiest way to do this is to open a Visual Studio command prompt (which is usually under the "Visual Studio Tools" subfolder of "Visual Studio 20XX" in your programs menu). Change directory so that you're in the folder which contains _Bar.dll_.

Use [ildasm][] to disassemble the file using the `/all` and `/out`, like so:

    C:\Foo\bin> ildasm /all /out=Bar.il Bar.dll

The result of the command is a new file, _Bar.il_, which contains a dissassembled listing of _Bar.dll_.

<img src="/uploads/2008/07/step2.jpg" style="float: right; margin-left: 5px; margin-bottom: 2px;" alt="Rebuild and Sign Bar" />
### Step 2 - Rebuild and Sign Bar ###
[ilasm]: http://msdn.microsoft.com/en-us/library/496e4ekx.aspx "MSIL Assembler"
We can now use [ilasm][] to reassemble _Bar.il_ back into _Bar.dll_, but at the same time specify a strong-name key to use to sign the resulting assembly. We pass in the value _Foo.snk_ to the `/key` switch on the command line, like so:
<div style="clear:both;"></div>

    C:\Foo\bin> ilasm /dll /key=Foo.snk Bar.il

    Microsoft (R) .NET Framework IL Assembler.  Version 2.0.50727.1434
    Copyright (c) Microsoft Corporation.  All rights reserved.
    Assembling 'Bar.il'  to DLL --> 'Bar.dll'
    Source file is ANSI

    Assembled method Bar.Bar::get_SecretMessage
    Assembled method Bar.Bar::.ctor
    Creating PE file

    Emitting classes:
    Class 1:        Bar.Bar

    Emitting fields and methods:
    Global
    Class 1 Methods: 2;
    Resolving local member refs: 1 -> 1 defs, 0 refs, 0 unresolved

    Emitting events and properties:
    Global
    Class 1 Props: 1;
    Resolving local member refs: 0 -> 0 defs, 0 refs, 0 unresolved
    Writing PE file
    Signing file with strong name
    Operation completed successfully

_Bar.dll_ is now signed! All we have to do is reopen _Foo_'s project, remove the reference to _Bar.dll_, re-add the reference to the new signed assembly and rebuild. Sorted!

Scenario 2 - Foo, Bar and Baz
-----------------------------

* `Foo` is the component that you're building which has to be signed.
* `Bar` is the third-party component that you're forced to use that _isn't_.
* `Baz` is another third-party component that is required in order for you to use _Bar_.

<img src="/uploads/2008/07/foobarbaz.png" alt="Relationship between Foo, Bar and Baz"/>

Grab [_Baz.dll_ and project][BazProj], [_Bar.dll_ and project][BarBazProj] along with [_Foo.dll_ and project][FooBarBazProj] for a sample source.

When you attempt to build _Foo_ you get the same error as you do in the previous scenario. Bear in mind that this time, **both** _Bar.dll_ and _Baz.dll_ need to be signed. So first of all, follow the steps in **Scenario 1** for both _Bar.dll_ and _Baz.dll_.

Done? OK. When you attempt to build _Foo.dll_ after pointing the project at the new _Bar.dll_ no compiler errors will be shown. Don't get too excited :)

When you attempt to **use** _Foo.dll_ your world will come crashing down. The reason is because _Bar.dll_ was originally built with a reference to an <u>unsigned version</u> of _Baz.dll_. Now that _Baz.dll_ is signed we need to force _Bar.dll_ to reference the **signed** version of _Baz.dll_.

<img src="/uploads/2008/07/step3.jpg" style="float: right; margin-left: 5px; margin-bottom: 2px;" alt="Hack the Disassembled IL" />
### Step 1 - Hack the Disassembled IL ###
Just like we did in the previous steps we need to disassemble the binary that we need to fix. This time, make sure you disassemble the new binary that you created in the previous step (this binary has been signed, and will contain the signature block for the strong name). Once _Bar.il_ has been created using ildasm, open it up in a [text editor][VIM].

Search for the reference to _Baz_ -- this should be located a fair way down the file, somewhere near the top of the actual code listing, just after the comments. Here's what it looks like on my machine:

    .assembly extern /*23000002*/ Baz
    {
      .ver 1:0:0:0
    }

This external assembly reference is missing the all-important public key token reference. Before we can add it, we need to know what the public key token is for _Bar.dll_. To determine this, we can use the [sn.exe][StrongNameTool] utility, like so:

    C:\Foo\bin> sn -Tp Baz.dll

    Microsoft (R) .NET Framework Strong Name Utility  Version 3.5.21022.8
    Copyright (c) Microsoft Corporation.  All rights reserved.

    Public key is
    0024000004800000940000000602000000240000525341310004000001000100a59cd85e10658d
    9229d54de16c69d0b53b31f60bb4404b86eb3b8804203aca9d65412a249dfb8e7b9869d09ce80b
    0d9bdccd4943c0004c4e76b95fdcdbc6043765f51a1ee331fdd55ad25400d496808b792723fc76
    dee74d3db67403572cddd530cadfa7fbdd974cef7700be93c00c81121d978a3398b07a9dc1077f
    b331ca9c

    Public key token is 2ed7bbec811020ec

Now we return to _Bar.il_ and modify the assembly reference so that the public key token is specified. This is what it should look like after modification:

    .assembly extern /*23000002*/ Baz
    {
      .publickeytoken = (2E D7 BB EC 81 10 20 EC )
      .ver 1:0:0:0
    }

Save your changes.

<img src="/uploads/2008/07/step4.jpg" style="float: right; margin-left: 5px; margin-bottom: 2px;" alt="Reassemble Bar" />
### Step 2 - Reassemble Bar ###
This step is just a repeat of previous steps. We are again using ilasm to reassemble _Bar.dll_, but this time from the new "hacked" _Bar.il_ file. We must use the exact same command line as we did previously, and we still need to specify the _Foo.snk_ for signing the assembly. To save you having to scroll up, here it is again:

    C:\Foo\bin> ilasm /dll /key=Foo.snk Bar.il

    Microsoft (R) .NET Framework IL Assembler.  Version 2.0.50727.1434
    Copyright (c) Microsoft Corporation.  All rights reserved.
    Assembling 'Bar.il'  to DLL --> 'Bar.dll'
    Source file is ANSI

    Assembled method Bar.Bar::get_SecretMessage
    Assembled method Bar.Bar::.ctor
    Creating PE file

    Emitting classes:
    Class 1:        Bar.Bar

    Emitting fields and methods:
    Global
    Class 1 Fields: 1;      Methods: 2;
    Resolving local member refs: 3 -> 3 defs, 0 refs, 0 unresolved

    Emitting events and properties:
    Global
    Class 1 Props: 1;
    Resolving local member refs: 0 -> 0 defs, 0 refs, 0 unresolved
    Writing PE file
    Signing file with strong name
    Operation completed successfully

Open up _Foo_'s project, remove and re-add the reference to _Bar.dll_, making sure you point to the new version that you just created. _Foo.dll_ will not only build, but this time it will run!

Disclaimer
----------

"Hacking" third-party binaries in this manner **_may_ breach the license agreement** of those binaries. Please make sure that you are not breaking the license agreement before adopting this technique.

I hope this helps!

[Italian]: http://www.otherbit.com/modules/blog/BlogContent.aspx?ID=174 ".NET-FU : come trasformare in SIGNED un assembly UNSIGNED (senza ricorrere al DELAY SIGNING)"
[SignedAssemblies]: http://msdn.microsoft.com/en-us/library/xc31ft41.aspx "Sign an Assembly with a Strong Name"
[DelaySigning]: http://msdn.microsoft.com/en-us/library/t07a3dye(VS.80).aspx "Delay Signing an Assembly"
[BarProj]: /uploads/2008/07/bar.zip "Project/Binary for Bar"
[FooBarProj]: /uploads/2008/07/foobar.zip "Project/Binary for Foo"
[ildasm]: http://msdn.microsoft.com/en-us/library/f7dy01k1(VS.80).aspx "MSIL Disassembly"
[BazProj]: /uploads/2008/07/baz.zip "Project/Binary for Baz"
[BarBazProj]: /uploads/2008/07/barbaz.zip "Project/Binary for Bar"
[FooBarBazProj]: /uploads/2008/07/foobarbaz.zip "Project/Binary for Foo"
[VIM]: http://www.vim.org/ "VIM - @secretGeek loves it... no really, he does!"
[StrongNameTool]: http://msdn.microsoft.com/en-us/library/k5b5tt23(VS.80).aspx "Strong Name Tool"
