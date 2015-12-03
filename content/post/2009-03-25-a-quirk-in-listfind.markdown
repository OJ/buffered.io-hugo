---
categories:
- Microsoft
- Software Development
- C#
comments: true
date: 2009-03-25T00:00:00Z
tags:
- .net
- C#
- design
- generics
title: A Quirk in List.Find()
---

Earlier today I was having a chat with a [friend of mine][Jimmy], who lives in Vancouver, about finding items that are stored in [generic Lists][ListT]. He flicked me a code snippet that looked something like this:

```
List<foo> list = new List<foo>();
// .. do some stuff
Foo f = list.Find(delegate(Foo f) { return foo.Name == "Bar"; });
```

Straight away I fired back with an update to the code which used [lambda expressions][] instead, as I'm a fan of how concise they are ;)

<!--more-->

```
List<foo> list = new List<foo>();
// .. do some stuff
Foo f = list.Find(foo => foo.Name == "Bar");
```

My friend ran this code against a data set that he had constructed and found that when the call to [Find()][ListTFind] was made, a [NullReferenceException][] was being thrown. I found this odd as I hadn't seen that before. _list_ was definitely a valid reference and the lambda expression was well-formed as well. So what was wrong?

It turns out that even though _list_ was a valid reference, it **didn't contain any elements**.

How odd! Why would the generic List object throw an exception when the user calls Find() when no elements are present? After a little bit of thinking I thought that I had the answer. I thought to myself:

> What if the List was a container for a [value type][], such as _int_? If you attempt to find a value in an empty list, then the function cannot return _null_ because that isn't valid for value-types! Throwing an exception _does_ make sense!

Isn't it amazing how easy it is to convince yourself of your own greatness? I thought I'd nailed it first go. So I proposed my argument to my friend, who initially was semi-sold on the idea.

Then I thought about it again and managed to convince myself that my apparent "brilliance" was, in fact, a failure. The perfect counter-argument to the above point is:

> What happens when you have a List of ints which _does_ contain elements and you attempt to search for a value that **is not in the list**?

It wasn't immediately obvious. So I tried something to see what would happen:

```
List<int> list = new List<int>(new int[] { 1, 2, 3 });
int i = list.Find(x => x > 3);
// ....
```

So what do you think the value of _i_ is after those first two lines? Yes, you guessed it: **Zero**. Why? Well, duh, it's because [default(T)][defaultT] for integers is Zero!

This is where little alarm bells started to ring in my head. I immediately whipped up an example where this would be considered bad:
```
List<int> list = new List<int>(new int[] { 0, 1, 2, 3 });
int i = list.Find(x => x > 3);
// ....
```

Again, _i_ is Zero when this code is executed, but the result is very misleading. Zero is contained in the collection but doesn't match the predicate, yet Zero is returned because that's the default value for this value-type.

I thought this was a bit of a glaring hole in the design. So I went straight to the [documentation][ListTFind] and found this:

> **Important Note:**
> 
> When searching a list containing value types, make sure the default value for the type does not satisfy the search predicate. Otherwise, there is no way to distinguish between a default value indicating that no match was found and a list element that happens to have the default value for the type. If the default value satisfies the search predicate, use the [FindIndex][] method instead.

This was concerning for a couple of reasons. First of all, the designers have left it up to you to determine that this is the default behaviour. Yes I should be able to come to that conclusion myself, but I didn't until I got bitten :) So shut up! Secondly, you have to check your result value against your predicate _again_ to be sure that it's not dodgey. For example:

```
List<int> list = new List<int>(new int[] { 0, 1, 2, 3 });
int i = list.Find(x => x > 3);
if(i > 3)
{
  // .. valid value, do stuff ..
}
else
{
  // .. no item found
}
```

Do _you_ want to do that? I certainly don't. After a bit of back-and-forth with Jimbo, I thought that the best option for a generic List Find() function would be one that is akin to the good old C++ days. It would look something like this:
```
bool Find<t>(Predicate<t> predicate, ref T output);
```

This would mean that you could change your code to something like the following:
```
int i;
List<int> list = new List<int>(new int[] { 0, 1, 2, 3 });
if(list.Find(x => x > 3, ref i))
{
  // .. valid value, do stuff ..
}
else
{
  // .. no item found, or empty list!
}
```

Note how with this option you could easily support the case for empty lists at the same time. It would be helpful and meaningful. Only when the function returns true can you rely on the output parameter. It's very clear and caters for value-types and reference-types. It'd be easy to implement in an [extension method][] as well. I'd prefer this solution over using FindIndex().

In case it's not obvious, this problem would no doubt exist in all functions on generic objects that attempt to return a single instance of _T_ based on some form of predicate. [FindLast()][ListTFindLast] would be another example.

I'm very keen to know the reasons behind the original design decision. I'm sure that minds far greater than mine parsed that problem and came up with that solution, probably for a very good reason.

What do you guys think?

  [Jimmy]: http://doomkeeper.com/ "Jimmy's Blog"
  [ListT]: http://msdn.microsoft.com/en-us/library/6sh2ey19.aspx "List(T)"
  [lambda expressions]: http://msdn.microsoft.com/en-us/library/bb397687.aspx "Lambda Expressions (C# Programming Guide)"
  [ListTFind]: http://msdn.microsoft.com/en-us/library/x0b5b5bc.aspx "List(T).Find"
  [NullReferenceException]: http://msdn.microsoft.com/en-us/library/system.nullreferenceexception.aspx "System.NullReferenceException"
  [value type]: http://msdn.microsoft.com/en-us/library/34yytbws(VS.71).aspx "Value Types"
  [defaultT]: http://msdn.microsoft.com/en-us/library/xwth0h0d.aspx "default keyword"
  [FindIndex]: http://msdn.microsoft.com/en-us/library/0k601hd9.aspx "FindIndex"
  [extention method]: http://msdn.microsoft.com/en-us/library/bb383977.aspx "Extension Methods (C# Programming Guide)"
  [ListTFindLast]: "http://msdn.microsoft.com/en-us/library/5kthb929.aspx" List(T).FindLast"
