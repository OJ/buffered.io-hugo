---
categories:
- Microsoft
- Security
comments: true
date: 2008-04-12T00:00:00Z
tags:
- brave
- developers
- Microsoft
- Security
- UAC
title: Microsoft want to Annoy you, Cancel or Allow?
---

Before I even started using Vista, I hated <a href="http://technet2.microsoft.com/WindowsVista/en/library/0d75f774-8514-4c9e-ac08-4c21f5c6c2d91033.mspx" title="User Access Control">UAC</a>. I read about it all over the place, and laughed at the stupidity involved in asking users to constantly "cancel or allow" every action they wanted to take. As soon as I was forced to use Vista for work (both on my work laptop and on site with the client) I turned off UAC on both installations.

<!--more-->

UAC has been a nagging frustration for a lot of Vista users, and has also been a <a href="http://www.youtube.com/watch?v=VKM1cAtAdtQ" title="Get a Mac ad">joke to the Apple fans</a> as well:
<object width="425" height="355"><param name="movie" value="http://www.youtube.com/v/VKM1cAtAdtQ&hl=en"></param><param name="wmode" value="transparent"></param><embed src="http://www.youtube.com/v/VKM1cAtAdtQ&hl=en" type="application/x-shockwave-flash" wmode="transparent" width="425" height="355"></embed></object>
Yeah, I laughed too.

But I didn't really give UAC any credit. I didn't think about why it was there, all I thought was that MS were being stupid. That was until a couple of days ago when I was thinking about it (don't ask me why ;) ).

After deliberating on it for a day or so, trying to come up with the motivation behind it, <a href="http://arstechnica.com/news.ars/post/20080411-vistas-uac-security-prompt-was-designed-to-annoy-you.html" title="Vista's UAC security prompt was designed to annoy you">this landed in my RSS feed reader</a> and made it clear. MS are taking a hit in the hope that devs will sort their shit out!

Windows users have had a long history of being administrators of their own machines by default. In general, most users do not login using an account with low levels of privileges. In Windows it has been too much of a pain in the butt to get anything done if you <em>don't</em>. Is that the fault of MS and Windows? Yes it is, but it's not their fault alone.

Looking back through history, developers who write software for Windows haven't really been particularly brilliant when it comes to being mindful of security. This is something that needs to change. Some really basic applications, office applications, editors and even games have require administrator privileges to run. It's no wonder that everyone decides to give themselves an admin account. Why would you want to face constant issues with security when all the things you're trying to use need the elevated access?

The fault lies with the developers. They need to get into the habit of writing software that doesn't require elevated privileges. UAC is part of MS's effort to force the plebian developers to sort their shit out. By the looks of it, it's beginning to work. It's a brave move on MS's part, as pissing off users to force developers to be mindful of security is not necessarily the smartest thing to do. Vista has enough pitfalls as it is without having something that's designed to being annoying popping up in your face all the time.

Let's hope the trend continues. In a perfect world it'd be fine to have something like UAC running all the time, so long as it doesn't appear all the time!

Note for those who care: sorry for the poorly written post :) I couldn't be bothered fixing it up, it's late and it's time for bed!
