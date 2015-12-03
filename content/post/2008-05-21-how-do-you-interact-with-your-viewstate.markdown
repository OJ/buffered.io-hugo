---
categories:
- ASP.NET
- Software Development
- Tips/Tricks
- C#
comments: true
date: 2008-05-21T00:00:00Z
tags:
- ASP.NET
- C#
- tip
- viewstate
title: How do you Interact with your ViewState?
---

There comes a time in every ASP.NET developer's life when the need arises for information to be persisted into <a href="http://msdn.microsoft.com/en-us/library/ms972976.aspx" title="ViewState">ViewState</a>. For the sake of this post I'm not really interested in the reasons why. What I am interested in is <em>how</em>.

How do you interact with your ViewState?

<!--more-->

I've seen two methods in use in the various codebases that I've worked on. The first that I'll demonstrate is the most common form I've seen.

Let's pretend we have a web form which has a property called <strong>MyMagicValue</strong> which needs to be stored across <a href="http://www.xefteri.com/articles/show.cfm?id=18" title="How postback works in ASP.NET">postbacks</a>.

Most developers would do something like this:
```
protected int MyMagicValue
{
    get
    {
        return (int)ViewState["MyMagicValue"];
    }
    set
    {
        ViewState["MyMagicValue"] = value;
    }
}
```

Information is stored directly in the ViewState hash/dictionary by accessing the ViewState property that exists on every web form/control. In this case there's a danger of things going horribly wrong "get" is invoked before "set"!

The second isn't so common. In fact, the only times I've seen it in a codebase is when I've implemented it myself. This leads me to believe that most of the time developers don't even know that this method exists. Here's the code:
```
private int _myMagicValue;

protected int MyMagicValue
{
    get
    {
        return _myMagicValue;
    }
    set
    {
        _myMagicValue = value;
    }
}

protected override object SaveViewState()
{
    object[] state = new object[2];
    state[0] = _myMagicValue;
    state[1] = base.SaveViewState();

    return state;
}

protected override void LoadViewState(object savedState)
{
    if (savedState != null)
    {
        object[] state = (object[])savedState;

        if (state.Length > 0)
        {
            _myMagicValue = (int)state[0];
            base.LoadViewState(state[1]);
        }
    }
}
```

Quite a bit different from the first version. But which one is better? Which is considered best practice? Which has the better performance?

I haven't done any extensive research into this topic other than chatting to a <a href="http://secretgeek.net/" title="secretGeek">workmate</a> or two. So I wouldn't say that I have hard data to back up what I'm about to say.

It seems to me that the latter implementation is preferrable for the following reasons:
<ol><li>Your page/control contains the usual definitions for container variables, just like most other classes without references to ViewState in every property definition.</li><li>You're not spreading references to ViewState all through your code.</li><li>You're not indexing into the ViewState hash for every since get/set for a given property.</li><li>If you want to add something else to the ViewState you can easily add it in the two overridden functions without affecting other areas of the code.</li><li>You don't have to worry about checking for <em>null</em> in the getters.</li><li>You could easily rip out the implementation and store in another class without a great deal of refactoring or code modification.</li></ol>
The former is preferrable to many coders because:
<ol><li>It's less code to write.</li><li>They don't know that the latter method exists.</li></ol>
So what's the verdict? Which of these is better, or considered "best practice"?

<strong>Edit:</strong> As per my comment below about using <a href="http://msdn.microsoft.com/en-us/library/system.web.ui.pair(VS.80).aspx" title="System.Web.UI.Pair">System.Web.UI.Pair</a>, I thought I would add an example of how this should be used. It's not overly pretty when you get more and more values to store, but I still prefer it to directly manipulating the ViewState object.
```
protected override object SaveViewState()
{
    return new Pair(base.SaveViewState(), _myMagicValue);
}

protected override void LoadViewState(object savedState)
{
    if (savedState != null)
    {
        Pair state = savedState as Pair;
        if(state != null)
        {
            base.LoadViewState(state.First);
            _myMagicValue = (int)state.Second;
        }
    }
}
```

This doesn't look so bad with just a single value, but for each value you add to the ViewState you need another Pair instance.
