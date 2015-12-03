---
categories:
- C#
comments: true
date: 2007-03-02T00:00:00Z
title: .NET System::String to ANSI char*
---

Hi All,

I've been doing a bit of work of late dealing with interops between managed and unmanaged code, and I have nailed a little snippet which shows how convert between a managed .NET String object and a stanard ANSI/C-style string. Check out the following code if you need to do the same:

```
System::String managedString = WHATEVER;
System::IntPtr stringPtr = System::Runtime::InteropServices::Marshal::StringToHGlobalAnsi( myString );
char* unmanagedString = static_cast< char* >( stringPtr.ToPointer() );
// do whatever you want with the unmanaged string ...
CallReallyOldCFunction( unManagedString );
System::Runtime::InteropServices::Marshal::FreeHGlobal( stringPtr );
```

Another option is to use the CString class, but some people might not want to have to deal with the extra overhead or objects. The choice is yours :)
