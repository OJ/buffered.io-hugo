---
categories:
- ASP.NET
- Microsoft
- Software Development
- WTF
comments: true
date: 2010-10-06T00:00:00Z
tags:
- ASP.NET
- TempData
- Content Advisor
- ASP.NET MVC
title: ASP.NET MVC 2, Random Sign-offs and TempData Loss
---

<a title="MVC" href="/uploads/2010/10/mvc.png" rel="lightbox[contentadvisor]"><img style="margin-left: 5px; margin-bottom: 5px; float: right;" src="/uploads/2010/10/mvc.png" alt="MVC" width="150" /></a>In the last few days I've been working on resolving issues in a production system which runs on <a title="What is ASP.NET MVC" href="http://www.asp.net/mvc/whatisaspmvc">ASP.NET MVC 2</a>. Most of the issues were actually really easy to resolve and the team of developers were able to fix them and deploy to production without too many problems.

Unfortunately, as always, there was one problem in particular that had us scratching our heads and was causing some of us to lose sleep. All over the Internet there were posts of people describing similar symptoms yet none of them revealed a solid answer.

The purpose of this post is to document the issue and the resolution in it's entirety. It's in story form rather than reference form because that's how I felt like writing it :)

<!--more-->

The Issue
---------

First of all, the percentage of users that were experiencing this problem was relatively low, less than 1 in every 400. This is low enough to indicate that we were going to have one hell of a time finding the issue.

So what was the issue?

The **entry-point** to the site was the **sign-in** page. This is the first page that the user sees. In short, users would sign in to the site and be presented with the landing page in the authenticated area of the site. As soon as they attempted to click on _any_ link inside the authenticated area they were **immediately sent back to the sign-in page**.

The Investigation
-----------------

After adding some more logging functionality to the application and talking to one of our users, we were able to see some really odd behaviour. The user would sign-in, again they were presented with their landing page. Before the user clicked on anything else, we could see that the system had recorded that the browser had **already invoked the sign-off functionality**.

Not good. Thankfully our logging was able to point the finger at a particular action on the site which was causing the user to be signed off. For business and security reasons, the sign-in page had some code that detected if the user was already signed in and, if so, would immediately sign them off. The main driver was to prevent users from _thinking_ that they had signed off from the application and then walk away without realising that their session was still available.

This didn't make sense. The user was clearly not returning to the sign-in page, but for some reason the system thought they were and hence was signing them off.

Emulating this at the office was proving to be impossible. We weren't able to reproduce it in _any_ of our environments, and according to our help desk, none of the users claimed to be using any tools, plug-ins or add-ins which may be interfering with their sessions.

We searched the <a href="http://google.com/">usual</a> <a href="http://stackoverflow.com/">haunts</a> for answers, but none came up. A post here and there would get our hopes up, but we would eventually have them dashed after realising that the resolution mentioned was something that was already place. Some of the common suggestions are:

* Make sure that the <a title="Machine Key Explained" href="http://msdn.microsoft.com/en-us/library/ff649308.aspx#paght000007_machinekeyexplained">machine key</a> is the same across all sites in the web farm.
* Make sure that the <a title="ASP.NET &amp; IIS Website Load Balancing" href="http://knol.google.com/k/kishore-gorjala/asp-net-iis-website-load-balancing/3jdbfde3g5y2c/3#">IIS site identifier</a> is the same across all sites in the web farm.
* Make sure that the cookie path was set to `/` instead of having a sub-path like `/mysite`.
* Make sure that the user's browser was accepting cookies.

While point 4 was indeed a problem for some users, it wasn't the golden bullet we were looking for.

We spun the wheels for a few days trying to come up with potential reasons why this could be the case. One of the developers suggested that it could be an issue with applications that operate as download accelerators. This application might scrape the screen when the user has signed in, and in an effort to pre-cache the next potential click it <a title="HTTP Request Methods" href="http://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol#Request_methods">GET</a>s each link that it finds on screen. It turns out that this wasn't, nor couldn't, be the issue: we were not including any links to the sign-on page anywhere in the authenticated area of the site.

The First Shot
--------------

We decided to take a shot in the dark and remove the functionality from the sign-in page which logs the users off. Instead, we present them with a notice which tells them that they are already signed-in. After deploying to production, our users were able to sign in! We were leaping for joy, though frustrated because we didn't know why this was a problem in the first place.

Our joy was short-lived. The application itself provides a series of wizards that the user can utilise to perform certain tasks. _All_ of the users who had problems signing in were having unexpected errors shown when using the wizards. Something was still amiss.

We were at a loss. So we decided to call in the cavalry: our poor users.

The Site Visit
--------------

We got in contact with one user, a lady who had been completely unable to interact with the site since its release, and she graciously accepted our request to pay her a visit and allow us to witness the failure in action. This proved to be a **very** good move as you'll soon see.

After the initial meet and greet, I was given access to our client's network. At first, I wanted to plug my own personal laptop in to the network to see if I could reproduce the problem without having to touch the client's machine. It wasn't to be, as  everything worked perfectly for me.

I requested access to the client's machine and fired up her browser of choice (<a title="Internet Explorer" href="http://www.microsoft.com/windows/internet-explorer/default.aspx">yuck</a>). As soon as I hit the site, I was presented with a dialog asking for a password. Here is what it looked like:

<a title="The Content Advisor Prompt" href="/uploads/2010/10/content-advisor-prompt.png" rel="lightbox[contentadvisor]"><img style="margin-right: 5px; margin-bottom: 5px; float: left;" src="/uploads/2010/10/content-advisor-prompt.png" alt="The Content Advisor Prompt" width="250" /></a>I had never seen this before, so I asked the client what it was. Her response:

>Oh, that's the content advisor that my husband set up. We have that enabled on all our computers so that only me and my husband can get to certain websites.</blockquote>

<a title="Microsoft Content Advisor" href="http://www.microsoft.com/windows/ie/using/howto/security/contentadv/config.mspx">Content Advisor</a>? Why had I never heard of this before? After literally _years_ of building production websites, I had never once encountered this beast. Yet here it was in front of me on screen, like a smart-arse teenager giving me the bird, and according to the dates on some of the articles on the web it has been around for quite a few years.

I proceeded to sign in to the site and attempt to perform an action using the wizard. As expected, it failed miserably. I then asked the client to disable the Content Advisor to see if it made any difference.

It did. The site performed _flawlessly_.

The Resolution
--------------

So the Content Advisor was causing problems. But how? What was it doing behind the scenes that was preventing our users from getting the quality experience that we'd worked so hard to deliver?

Now that we had knowledge of the content advisor our queries to the Interwebs resulting in more revealing posts, such as one posted on <a title="IE's Content Advisor, PICS Ratings and the ASP.NET Flakey of the Day" href="http://www.hanselman.com/blog/IEsContentAdvisorPICSRatingsAndTheASPNETFlakeyOfTheDay.aspx">Scott Hanselman's blog</a>. Here are some key bits of information from that post:

> ... when Content Advisor is OFF, the interaction looks like this:<br/>
> 
> HTTP GET /somefile.aspx<br/>
> RESPONSE 200<br/>
> HTTP GET /somethingelse.aspx (we did a javascript.open)<br/>
> RESPONSE 302 getthisfile.aspx<br/>
> HTTP GET getthisfile.aspx<br/>
> RESPONSE 200<br/>
> 
> But when **Content Advisor is ON**, we see this:<br/>
> 
> HTTP GET /somefile.aspx<br/>
> RESPONSE 200<br/>
> HTTP GET /somethingelse.aspx (we did a javascript.open)<br/>
> **HTTP GET /<br/>
> RESPONSE 200**<br/>
> RESPONSE 302 getthisfile.aspx<br/>
> HTTP GET getthisfile.aspx<br/>
> RESPONSE 200<br/>
> 
> ...<br/>
>
>When the Content Advisor is ON, Internet Explorer will request '/' from a site anytime a new window is opened.

As we can see from the above quote, the Content Advisor hits the root of the site on the user's behalf in an effort to scrape <a title="Platform for Internet Content Selection" href="http://en.wikipedia.org/wiki/Platform_for_Internet_Content_Selection">PICS</a> information about the website.

According to the last point in the above quote, this will happen any time a new window is opened. Unfortunately for us, it goes deeper than that: **the Content Advisor makes the browser request `/` every single time an action is invoked if it can't find sufficient PICS information for the current URL**.

Yes, it's shit, but that's the way it works.

So any users which have the Content Advisor turned on will actually be hitting the site twice for every GET action they perform. This explains why the users were being signed off! Our sign-off code was being executed behind the scenes without the users being aware of it.

But why was it causing unexpected errors when clients were using the wizard? To answer that, we need to know about a little feature of MVC called <a title="TempData @ you've been HAACKED" href="http://haacked.com/tags/TempData/default.aspx">TempData</a>.

For those who don't want to click the above link, TempData is a feature of ASP.NET MVC which allows information to be persisted across post-backs to the server. A classic example is when URI `/Foo` gets invoked, and the action results in a redirect to `/Bar`. But `/Bar` requires some data that got sent to `/Foo`, so the `/Foo` action stores that data in the TempData dictionary which allows `/Bar` to get access to it during processing. It's a nifty feature, and one that we use extensively.

The key piece of information to remember about TempData is that it _only persists data across a single post-back_. That means that if you post back to the server once, TempData is lost unless the target action **explicitly requests for it to stick around**.

This is the deal-breaker. If `/Bar` assumes that TempData contains information from `/Foo` when it fires up then `/Bar` is going to break if the information is _not_ there. Why would it not be there if a redirect happened? That's right, the f$%#ing Content Advisor!

It turns out that every time the Content Advisor hits the root of the site, TempData is cleared. This is obvious in hindsight because the root URI is an <a title="ASP.NET MVC Controller Overview" href="http://www.asp.net/mvc/tutorials/asp-net-mvc-controller-overview-cs">action</a> just like any other anywhere in the whole site. That action gets invoked through the same means and comes with the same caveats ... including that of TempData getting cleared unless we ask it not to. So the chain of events looked like this:

* User invokes a wizard via a simple click.
* Request is set to the server for`/Foo` 
* `/Foo` gets executed and stores information, `Baz`, in TempData so that the next step in the wizard, `/Bar`, can get access to it.
* `/Foo` returns page content to the user.
* The browser receives the page content from the `/Foo` action and the Content Advisor notices a lack of PICS information in the HTTP headers and META tags.
* The Content Advisor forces the browser to make a request to `/` in an attempt to find the missing PICS information.
* The root site action is hit and it renders the sign-on page content.
* At the end of page content generation, the TempData dictionary notices that no requests have been made to retain any of the information across another call, and hence **all of the TempData information is lost**, including `Baz`.
* The user click's "Next" in the wizard, which causes the browser to invoke the `/Bar` action on the server.
* The server invokes `/Bar` and the first thing it does is attempt to pull `Baz` out of TempData. `Baz` ends up being null.
* Game over, Red Rover.

There are quite a few issues that were highlighted as a result, but the key one is this: _you can't assume that your users aren't using some form of Content Advisor which is hitting your server with unexpected requests_. In our case, the resolution was simple. We just had to add a couple of lines to our root action:

```
public ActionResult SignOn()
{
  if(Request.IsAuthenticated)
  {
    // force the TempData dicionary to keep hold
    // of the information it has in case this
    // action is being hit by a Content Advisor.
    TempData.Keep();
  }
  // rest of the action code
  // ...
  return View(...);
}
```

Conclusion
----------

The first question you might ask is: why didn't you just add PICS data to your site to prevent the Content Advisors from behaving that way? The short answer is that you can't guarantee that the Content Advisors will adhere to the "rules", especially given that there don't appear to be any.

<a title="Platform for Internet Content Selection" href="http://en.wikipedia.org/wiki/Platform_for_Internet_Content_Selection">PICS</a> has already been replaced with <a title="Protocol for Web Description Resources" href="http://en.wikipedia.org/wiki/POWDER">POWDER</a>. But POWDER doesn't appear to be used anywhere, and there's very little information around on it. Attempting to support all possible half-baked standards would result in serving up a great deal more content for the sake of a very small subset of users who actually use Content Advisors. It made much more sense to just persist TempData across one more call.

Bear in mind that while the Content Advisor is built-in to the Internet Options area in Windows, it's not specific to Internet Explorer. Both Chrome and Safari are affected by the Content Advisor if it is enabled!

So for anyone out there who is building, or has built, a public-facing website using ASP.NET MVC please bear this in mind. Keep an eye on your TempData usage, make sure that your root action persists your TempData if required, and avoid having that same action sign off your users.

I hope this helps someone :) Comments and feedback greatly appreciated.
