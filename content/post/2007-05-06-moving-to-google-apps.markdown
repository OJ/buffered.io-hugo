---
categories:
- Google
- Software
comments: true
date: 2007-05-06T00:00:00Z
title: Moving to Google Apps
---

As you all know, I recently moved the Blackapache site to a new server. We had some teething issues, as ya do when making the move, but all is now well. One of the things that I hadn't got round to fixing was my <a href="http://www.spamassassin.org/" title="SpamAssassin">SpamAssassin</a> installation.

I'd already gone through the fun times of setting up email (POP3s, IMAP4s) with <a href="http://www.qmail.org/" title="Qmail">Qmail</a>, which after a bit of time (and a learning curve) was working fine (with SSL, hence the 's' at the end of the names ;) ) and all my users were able to send/receive without an issues. While trying to set it up, I was having issues getting both SpamAssassin and <a href="http://www.clamav.net/" title="Clam AV">Clam AntiVirus</a> working. Both of the daemons would run fine, but I just couldn't quite get the configuration right.

Thankfully, AV isn't so much of an issue as my users all have AV protection on their local machines. Spam, while annoying, can be lived with until I find the time to get the filters working properly - so I basically left these two things alone for a while until I found the time after doing all the other <em>urgent</em> bits.

I finally got round to making an attempt to fix the problem a weekend or so ago, but still couldn't get it to behave. I read forum upon forum, blog upon blog, and readme upon readme, but for some reason the bloody thing just refused to work. I got frustrated, reverted all the settings back to how they were before, and gave up for the day.

During the last week I was prepping myself for another attempt at getting it work, when I heard about <a href="http://www.google.com/a/" title="Google Apps">Google Apps</a>. I was a bit surprised when I first read all the details, because it was the first time I'd heard about it! Being a geek I thought I was pretty good with keeping up to speed, and I was a bit narked with myself for letting this golden nugget slip through the net.

I'm not going to go through the ins and outs of what Google Apps is all about as there are already a <a href="http://www.google.com.au/search?hl=en&q=google+apps" title="Google Apps Search">stack</a> of pages out there already talking about it. What I will say is that I should have made the move earlier!

Not only do I no longer have to worry about keeping my entire email configuration (Qmail with vpopmail and courier imap, SpamAssassin, ClamAV, Horde/SquirrelMail) up to date and patched, I don't even have to have it running. The "precious" resources on the server can now be utilised for other things such as an <a href="http://subversion.tigris.org/" title="SubVersion">SVN</a> install. Each email user has 2GB of space, webmail comes for free and has the fantastic facilities that come with <a href="http://www.gmail.com/" title="Gmail">Gmail</a>. Users can easily configure their own accounts, handle their own password changes, etc. My administration of email has basically dropped to just above zero. The only thing I need to do as an administrator is get the account set up initially, the rest is up to the user.

You can probably tell that I'm quite excited about this. It's great that I don't have to worry about this any more, and I can put more time and effort into other things.  Of course, my excitement only extends as far as email for now, but I'm pretty sure that over time I'll also come to love the added features of <a href="http://calendar.google.com/" title="Google Calendar">Google Calendar</a> and <a href="http://docs.google.com/" title="Google Calendar">Google Calendar</a>.

This service does come with its limitations, and with the possibility of Google Datamining, but for a home server which I use for a blog, having this facility to lighten the admin load is just fantastic.

I highly recommend considering this as an option if you're looking to sort out an email host! Give it a trial, at least for a while, and you may find you're as happy as I am. If you don't like it, you can always revert back to your old setup.
