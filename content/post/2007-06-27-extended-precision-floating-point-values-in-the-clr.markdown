---
categories:
- CLR
- Tips/Tricks
- C#
comments: true
date: 2007-06-27T00:00:00Z
tags:
- C#
- CLR
- floating point
title: Extended-Precision Floating-Point Values in the CLR
---

While at work today I hit a problem that I've never hit before (which is quite rare these days :) ), and while it was frustrating it was also good to learn about something that I never knew was a problem. If you're having some issues marshalling double-precision floating-point information through managed components to unmanaged components, or you're just interested in learning something new, then read on :)

Let me start by explaining the scenario:<!--more-->

One of our <acronym title="Visual Basic v6">VB6</acronym> applications passes a variant array of 64-bit doubles to a managed C++ component. The managed code does a bit of munging, before passing the same information on to another component which is an <em>un</em>managed C++ DLL written and maintained by a third party. This second DLL is responsible for doing boolean operations on 3D solids. When this component is invoked, the application dies with a nice obscure "screwed-up memory" error. Unfortunately, since the component is closed-source, we needed to pass the information on to the author of the code and wait for a response.

After the author had investigated the problem he informed us that the reason for the crash was that the values that are being passed in are <a href="http://en.wikipedia.org/wiki/Extended_precision" title="Extended Precision">80-bit extended-precision</a> values, and not the expected <a href="http://en.wikipedia.org/wiki/IEEE_754" title="IEEE 754">IEEE</a> <a href="http://en.wikipedia.org/wiki/Double_precision" title="Double Precision">64-bit double-precision</a> values.

Wierd! That didn't make any sense to me. On hearing this I started to investigate where the process was falling down. The original data was 64-bit, so how come it <em>wasn't</em> by the time it hit the final DLL? Something must be converting the 64-bit double-precision values to their 80-bit extended-precision equivalents. I spent a fair bit of time trying to find out as much as I could about the various floating-point representations, and where they're used, but it took me an eternity to locate the information I was searching for.

In the deep, dark depths of the web, I stumbled across this little nugget of information (thanks to <a href="http://www.extremeoptimization.com/resources/Articles/FPDotNetConceptsAndFormats.aspx#standards" title="FP .NET Concepts and Formats">ExtremeOptimization.com</a>):<blockquote><p>... the 'extended' format, for which the IEC 60559 standard defines minimum specifications, and which is used by the floating-point unit on Intel processors, and is also <strong>used internally by the CLR</strong>.</p></blockquote>Ah ha! You bastards :) So we're always dealing with 80-bit values when we use <em>double</em> in the <a href="http://en.wikipedia.org/wiki/Common_Language_Runtime" title="Common Language Runtime">CLR</a>.

Now that we know this, we can take steps to sort it out. In a nutshell, we need to set the <a href="http://en.wikipedia.org/wiki/Floating_point_unit" title="Floating Point Unit">FPU</a> state to force the use of 64-bit values before invoking the unmanaged component. Thankfully there's an API function that we can use to do this, it's called <a href="http://msdn2.microsoft.com/en-us/library/e9b52ceh(VS.80).aspx" title="_controlfp">_controlfp()</a>. In VS 2005, this function is deprecated, so we're forced to use the 'secure' equivalent _controlfp_s(). Here's how you do it:
```
#include <float.h>
.
.
void ManagedClass::ManagedFunction()
{
  double dooby[128];
  .
  unsigned int prevState;
  // set the FPU state to 64-bit
  _controlfp_s( &prevState, _PC_53, MCW_PC);
  .
  // invoke unmanaged DLL - these values wil be 64-bit, not 80-bit
  ExternalFunction( dooby );
  .
  // reset FPU state to previous value
  _controlfp_s( NULL, prevState, MCW_PC);
}
```

So there you have it. I hope this information is useful :)
