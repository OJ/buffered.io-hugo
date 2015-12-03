---
categories:
- Software
comments: true
date: 2010-02-05T00:00:00Z
tags:
- Fail
- sysadmin
title: Truncated Pages
---

Not too long ago I mentioned that I'd <a href="/posts/now-running-nginx/" title="Now Running Nginx">setup and installed Nginx</a> on this server. All seemed well to start off with, then on certain occasions I started to notice that some pages were being served truncated while I was at work. I thought that the issue was work-related, as they have a fairly draconian security policy in place and I thought that it had something to do with severing the connection.

This assumption was proved false a few days ago when I received an email about my <a href="/posts/setting-up-trac-mercurial-and-ssh-on-windows/">Trac/Mercurial/SSH</a> post not rendering properly in someone else's browser. Dammit!

So this time I actually did a bit of research by delving into the log files (yes, I know.. genius isn't it!) and determined that there was indeed a problem. Thankfully it was fairly simple to resolve. For some reason the <em>fastcgi</em> instances were failing to function periodically when attempting to access a folder on disk. In short, some of the temp folders were owned by the right user, and some of them weren't. After modifying the ownership of the appropriate folders, everything seems to be running nicely.

Yet more proof that <a href="/posts/the-admin-is-an-idiot/" title="The Admin is an Idiot">the admin is an idiot</a>.

If after I post this you notice any truncation at all across the site, please let me know! Cheers!
