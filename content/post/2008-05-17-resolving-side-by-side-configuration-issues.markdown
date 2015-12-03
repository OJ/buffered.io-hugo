---
categories:
- Microsoft
- Tips/Tricks
- Windows
comments: true
date: 2008-05-17T00:00:00Z
tags:
- configuration
- dll
- error
- investigation
- Windows
- winsxs
title: Resolving Side-by-Side Configuration Issues
---

I've been meaning to blog about this for well over a year now, but for some reason I never got round to it. This came up in conversation the other day with a couple of workmates and it prompted me to revisit the issue.

Have you ever fired up an application on Windows XP and got the following error?
<blockquote><p>The application has failed to start because the application configuration is incorrect. Reinstalling the application may fix this problem.</p></blockquote>
Informative isn't it! What about if you fire up the same application on Windows Vista?
<blockquote><p>The application has failed to start because its side-by-side configuration is incorrect. Please see the application event log for more detail.</p></blockquote>
This does tell us a little bit more about the problem, but not a <em>lot</em> more.

<!--more-->

The fact is that the first error message above is useless, and the second is useless to everyone except those who know all about <a href="http://blog.tiensivu.com/aaron/archives/1306-Demystifying-the-WinSxS-directory-in-Windows-XP,-Vista-and-Server-20032008.html" title="WinSxS">WinSxS</a> (<em>side-by-side</em>). I'm not going to go into detail about WinSxS in this article, but the short description is: <em>it's an attempt at alleviating <a href="http://en.wikipedia.org/wiki/DLL_hell" title="DLL Hell">DLL hell</a></em>.

When a binary component links against a DLL, such as <a href="http://www.microsoft.com/" title="Microsoft">MS</a>'s <a href="http://msdn.microsoft.com/en-us/library/abx4dbyh(VS.80).aspx" title="C Run-Time Libraries">CRT</a>, an entry for that dependant DLL is specified in the component's <a href="http://msdn.microsoft.com/en-us/library/aa375365.aspx" title="Manifests">manifest</a>. This tells Windows that the application can't run without those DLLs being present. If they're not present in WinSxS then the errors above are thrown in the user's face.

To demonstrate the problem, consider the C++ program below.
```
#include <windows.h>
#include <tchar.h>

int WINAPI _tWinMain(HINSTANCE instance, HINSTANCE prevInstance, LPTSTR cmdLine, int cmdShow)
{
  ::MessageBox(NULL,
      _T("This is a text executable that links against a later version of the runtimes."),
      _T("Test EXE"),
      MB_OK);

  return 0;
}
```


Compile this on a machine with <a href="http://msdn.microsoft.com/en-us/vstudio/default.aspx" title="Visual Studio Developer Center">Vis Studio 2008</a> installed and the resulting EXE will be linked against version 9.0 of the CRT.

Here is the result of running this on an XP machine without that runtime installed:

<a href="/uploads/2008/05/xp_fail.png" rel="lightbox[winsxs]"><img src="/uploads/2008/05/xp_fail.png" alt="Windows XP Error" title="XP Error" width="300" height="56" class="aligncenter size-medium wp-image-350" /></a>

Here's the same application running on Vista, again without the runtime installed:
<a href="/uploads/2008/05/vista_fail.png" rel="lightbox[winsxs]"><img src="/uploads/2008/05/vista_fail.png" alt="WinSxS error on Vista" title="Vista Error Message" width="300" height="95" class="aligncenter size-medium wp-image-349" /></a>

Let's now pretend that we don't know why this problem is occuring and attempt to ascertain the reason for the error.

First off, we need to locate the application's manifest. This can be found either in a appname.exe.manifest file, or <em>inside</em> the binary itself. In our case, the manifest is embedded so we need to open up the file in a binary/hex editor (or at least an editor that allows you to view the content of binary files). I used <a href="http://www.vim.org/" title="VIM">VIM</a>, but there are other options such as <a href="http://www.ultraedit.com/" title="UltraEdit">UltraEdit</a> and the free <a href="http://www.softcircuits.com/cygnus/fe/" title="Cygnus Hex Editor">Cynus</a> editor.

Manifest information is usually stored towards the end of the file, so after opening it in your editor of choice, scroll to the end of the file and slowly scroll up. When you reach a section that contains what looks to be <a href="http://www.w3.org/XML/" title="XML">XML</a> then you've probably found it. It usually lies just above a section of padding that looks like this:

    0001ab0: 5041 4444 494e 4758 5850 4144 4449 4e47  PADDINGXXPADDING
    0001ac0: 5041 4444 494e 4758 5850 4144 4449 4e47  PADDINGXXPADDING
    0001ad0: 5041 4444 494e 4758 5850 4144 4449 4e47  PADDINGXXPADDING
    0001ae0: 5041 4444 494e 4758 5850 4144 4449 4e47  PADDINGXXPADDING
    0001af0: 5041 4444 494e 4758 5850 4144 4449 4e47  PADDINGXXPADDING
    0001b00: 5041 4444 494e 4758 5850 4144 4449 4e47  PADDINGXXPADDING
    0001b10: 5041 4444 494e 4758 5850 4144 4449 4e47  PADDINGXXPADDING
    0001b20: 5041 4444 494e 4758 5850 4144 4449 4e47  PADDINGXXPADDING
    0001b30: 5041 4444 494e 4758 5850 4144 4449 4e47  PADDINGXXPADDING
    0001b40: 5041 4444 494e 4758 5850 4144 4449 4e47  PADDINGXXPADDING
    0001b50: 5041 4444 494e 4758 5850 4144 4449 4e47  PADDINGXXPADDING
    0001b60: 5041 4444 494e 4758 5850 4144 4449 4e47  PADDINGXXPADDING
    0001b70: 5041 4444 494e 4758 5850 4144 4449 4e47  PADDINGXXPADDING

The manifest XML usually begins with an <em>assembly</em> tag. In the case of this example, it looks like this:

    0001850: e404 0000 0000 0000 3c61 7373 656d 626c  ........<assembl
    0001860: 7920 786d 6c6e 733d 2275 726e 3a73 6368  y xmlns="urn:sch
    0001870: 656d 6173 2d6d 6963 726f 736f 6674 2d63  emas-microsoft-c
    0001880: 6f6d 3a61 736d 2e76 3122 206d 616e 6966  om:asm.v1" manif
    0001890: 6573 7456 6572 7369 6f6e 3d22 312e 3022  estVersion="1.0"
    00018a0: 3e0d 0a20 203c 7472 7573 7449 6e66 6f20  >..  <trustInfo
    00018b0: 786d 6c6e 733d 2275 726e 3a73 6368 656d  xmlns="urn:schem
    00018c0: 6173 2d6d 6963 726f 736f 6674 2d63 6f6d  as-microsoft-com
    00018d0: 3a61 736d 2e76 3322 3e0d 0a20 2020 203c  :asm.v3">..    <
    00018e0: 7365 6375 7269 7479 3e0d 0a20 2020 2020  security>..
    00018f0: 203c 7265 7175 6573 7465 6450 7269 7669   <requestedPrivi
    0001900: 6c65 6765 733e 0d0a 2020 2020 2020 2020  leges>..
    0001910: 3c72 6571 7565 7374 6564 4578 6563 7574  <requestedExecut
    0001920: 696f 6e4c 6576 656c 206c 6576 656c 3d22  ionLevel level="
    0001930: 6173 496e 766f 6b65 7222 2075 6941 6363  asInvoker" uiAcc
    0001940: 6573 733d 2266 616c 7365 223e 3c2f 7265  ess="false"></re
    0001950: 7175 6573 7465 6445 7865 6375 7469 6f6e  questedExecution
    0001960: 4c65 7665 6c3e 0d0a 2020 2020 2020 3c2f  Level>..      </
    0001970: 7265 7175 6573 7465 6450 7269 7669 6c65  requestedPrivile
    0001980: 6765 733e 0d0a 2020 2020 3c2f 7365 6375  ges>..    </secu
    0001990: 7269 7479 3e0d 0a20 203c 2f74 7275 7374  rity>..  </trust
    00019a0: 496e 666f 3e0d 0a20 203c 6465 7065 6e64  Info>..  <depend
    00019b0: 656e 6379 3e0d 0a20 2020 203c 6465 7065  ency>..    <depe
    00019c0: 6e64 656e 7441 7373 656d 626c 793e 0d0a  ndentAssembly>..
    00019d0: 2020 2020 2020 3c61 7373 656d 626c 7949        <assemblyI
    00019e0: 6465 6e74 6974 7920 7479 7065 3d22 7769  dentity type="wi
    00019f0: 6e33 3222 206e 616d 653d 224d 6963 726f  n32" name="Micro
    0001a00: 736f 6674 2e56 4339 302e 4352 5422 2076  soft.VC90.CRT" v
    0001a10: 6572 7369 6f6e 3d22 392e 302e 3231 3032  ersion="9.0.2102
    0001a20: 322e 3822 2070 726f 6365 7373 6f72 4172  2.8" processorAr
    0001a30: 6368 6974 6563 7475 7265 3d22 7838 3622  chitecture="x86"
    0001a40: 2070 7562 6c69 634b 6579 546f 6b65 6e3d   publicKeyToken=
    0001a50: 2231 6663 3862 3362 3961 3165 3138 6533  "1fc8b3b9a1e18e3
    0001a60: 6222 3e3c 2f61 7373 656d 626c 7949 6465  b"></assemblyIde
    0001a70: 6e74 6974 793e 0d0a 2020 2020 3c2f 6465  ntity>..    </de
    0001a80: 7065 6e64 656e 7441 7373 656d 626c 793e  pendentAssembly>
    0001a90: 0d0a 2020 3c2f 6465 7065 6e64 656e 6379  ..  </dependency
    0001aa0: 3e0d 0a3c 2f61 7373 656d 626c 793e 5041  >..</assembly>PA

In case you don't find this very readable, here it is after extraction/formatting:
```
<assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0">
  <trustInfo xmlns="urn:schemas-microsoft-com:asm.v3">
    <security>
      <requestedPrivileges>
      <requestedExecutionLevel
        level="asInvoker"
        uiAccess="false"></requestedExecutionLevel>
      </requestedPrivileges>
    </security>
  </trustInfo>
  <dependency>
    <dependentAssembly>
      <assemblyIdentity
        type="win32"
        name="Microsoft.VC90.CRT"
        version="9.0.21022.8"
        processorArchitecture="x86"
        publicKeyToken="1fc8b3b9a1e18e3b">
      </assemblyIdentity>
    </dependentAssembly>
  </dependency>
</assembly>
```


The bit we're really interested in is:
```
<dependentAssembly>
  <assemblyIdentity
    type="win32"
    name="Microsoft.VC90.CRT"
    version="9.0.21022.8"
    processorArchitecture="x86"
    publicKeyToken="1fc8b3b9a1e18e3b">
  </assemblyIdentity>
</dependentAssembly>
```


This tells us the exact component and version required for this application to run. You'll notice that it also mentions the processor architecture. In this case, we need to make sure that we have version 9.0.21022.8 of the Visual C Runtimes for x86 installed in the side-by-side folder. The WinSxS folder can be found at <em>%WINDIR%\WinSxS</em>

Inside that folder you'll probably see a stack of subfolders with crazy looking names. The one you would need to have to solve the problem above is called <em>x86_microsoft.vc90.crt_1fc8b3b9a1e18e3b_9.0.21022.8_none_bcb86ed6ac711f91</em>, which as you can see has a name that closely resembles the properties of the <em>assemblyIdentity</em> tag in the XML listed above.

If you can't find the appropriate folder in your WinSxS then you need to download an installer that contains the appropriate components and install it.When installed, the application should run without a problem, and you should get a message like this:
<a href="/uploads/2008/05/vista_success.png" rel="lightbox[winsxs]"><img src="/uploads/2008/05/vista_success.png" alt="The resulting message box shown when the test application\&#039;s side-by-side configuration is correct" title="Successful run of application" width="300" height="102" class="aligncenter size-medium wp-image-351" /></a>

Hope that helps!

<hr/>

<strong>Edit (3rd Jan '09)</strong>: A nifty tool has been built by Kenny Kerr which makes viewing manifest information <em>much</em> easier. Pointer your browser <a href="http://weblogs.asp.net/kennykerr/archive/posts/manifest-view-support-for-dlls.aspx">this way and check it out</a>. It should help when tryinig to resolve this problem.

<!--adsense-->
