---
categories:
- WTF
comments: true
date: 2006-11-16T00:00:00Z
title: 'WTF: Date Malarky'
---

I don't think I'm going to need to say much about the following code, as <em>anyone</em> with even an <strong>inkling</strong> of the features of the .NET framework would know that it's a joke. See the nugget(s) of genius below:

```
public static void ToStartofCurrDay(ref DateTime aDate)
{
	aDate = aDate.Subtract(aDate.TimeOfDay);
}

public static void ToEndofPrevDay(ref DateTime aDate)
{
	aDate = aDate.Subtract(aDate.TimeOfDay);
	TimeSpan ts = new TimeSpan(1);
	aDate = aDate.Subtract(ts);
}

public static void ToEndofCurrDay(ref DateTime aDate)
{
	aDate = aDate - aDate.TimeOfDay;
	aDate = aDate.AddHours(23);
	aDate = aDate.AddMinutes(59);
	aDate = aDate.AddSeconds(59);
	aDate = aDate.AddMilliseconds(999);
}
```

The thing that annoys me about this is that it was written by the <em>lead/architect</em>! The man who should know the framework like the back of his hand. Scary stuff. It's this kind of thing that inspired my comments about "knowing your tech" in my post about the day job.
