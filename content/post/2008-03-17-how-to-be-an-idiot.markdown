---
categories:
- Security
comments: true
date: 2008-03-17T00:00:00Z
title: How to be an Idiot
---

I've just done something stupid. I attempted to install a new plugin for Wordpress without verifying the contents of the package. The result? I lost most of the file system under this website. From what I can see in the script, it also attempted to various other nasty things such as deleting files from outside the web root, and emailing certain files to other websites. It's a good job I have file permissions set up so that the web server can't access the file system outside of its root. I'm lucky it didn't attempt to trash the database too!

I've requested a partial restore of content from our web host so that I don't have to go through the pain of adding all the content again. Hopefully it'll be back up soon.

I'm not happy, but I only have myself to blame. Whatever you do, unless you're grabbing from the official Wordpress plugin repo, make sure you check out the contents of the plugin before you attempt to install it!
