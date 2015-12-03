---
categories:
- Software
- Technology
comments: true
date: 2009-09-10T00:00:00Z
title: Now Running Nginx
---

<a href="/uploads/2009/09/slow_snail.jpg" rel="lightbox"><img src="/uploads/2009/09/slow_snail.jpg" alt="clarkson_face" title="clarkson_face" width="150" style="float: right; margin-bottom: 5px; margin-left: 5px;" /></a>For the last couple of years, the server which has powered this site (and a few other sites) has been running the free version of <a href="http://litespeedtech.com/" title="Litespeed">Litespeed</a> web server. After feeling the resource burden of <a href="http://httpd.apache.org/" title="Apache Httpd">Apache</a>, Litespeed was a breath of fresh air! The fact that the "full" version wasn't free didn't bother me, I was happy to stick with the standard edition as it seemed more than capable of handling the meagre traffic that this site generates.

<!--more-->

After a year or so of serving up content, it managed to handle a <a href="http://www.reddit.com/r/programming/comments/6ngfy/an_interesting_little_problem/" title="An Interesting Little Problem">couple</a> <a href="http://www.reddit.com/r/programming/comments/6tsbf/a_better_nub/" title="A better nub">of</a> <a href="http://www.reddit.com/r/haskell/comments/8whp6/pointfree_style_what_is_it_good_for/" title="Point-free style - What is it good for?">Reddit</a> <a href="http://www.reddit.com/r/haskell/comments/8v95i/data_crunching_in_haskell/" title="Data crunching in Haskell">submissions</a> without much of a bother! I was suitably impressed.

But there comes a time when you can't help but wonder what else is out there. For me, that time came a little while ago when I boosted the number of plug-ins that I use on this site. Litespeed took longer to serve pages up and I couldn't help but notice it. I'm certainly not blaming Litespeed, because the obvious solution is to remove the plug-ins! But I didn't want to do that as those plug-ins provide features for this site that I don't want to lose. So my quest to find another web server began.

Then recently I started researching ideas for developing software in Erlang. I've been reading up on the language for quite a while and I'm getting to the point where I intend to build something. My first effort is going to be a web application that uses <a href="http://code.google.com/p/mochiweb/" title="Mochiweb">Mochiweb</a>, <a href="http://bitbucket.org/justin/webmachine/wiki/Home" title="Webmachine">Webmachine</a> and <a href="http://couchdb.apache.org/" title="CouchDB">CouchDB</a>. I know that at some point I'm going to want to have this thing run on the web so that other people can play with it and that would mean that I would have to have a web server available that could also route requests to my Erlang server. Of course, I want it to be <em>quick</em>.

<a href="/uploads/2009/09/clarkson_face.jpg" rel="lightbox"><img src="/uploads/2009/09/clarkson_face.jpg" alt="clarkson_face" title="clarkson_face" width="150" style="float: left; margin-bottom: 5px; margin-right: 5px;" /></a>So after a bit of deliberation and research, I followed the footsteps of some big players and ended up installing <a href="http://nginx.net/" title="Nginx">Nginx</a> (which I believe is pronounced <em>"Engine-X"</em>). I kid you not, this thing is amazing! I'm not going to give a full review of it here because I haven't used it enough to know how good it is. But the speed-up for this site in particular is obvious (at least for me).

My mate Dan's <a href="http://shiftperception.com/blog/" title="Shiftperception">site</a> <strong>absolutely flies</strong>!

On the whole, <a href="http://wiki.nginx.org/Main" title="Nginx wiki">Nginx</a> gets a huge thumbs up from me thus far. If you're looking for a speedy web server that can do reverse proxying, dish up static content like a maniac and handle forwarding of requests to a variety of other servers then you should seriously consider taking a look at Nginx.

Thank you, <a href="http://sysoev.ru/en/" title="Igor Sysoev">Igor</a>!
