---
categories:
- Security
- Technology
- Tips/Tricks
comments: true
date: 2008-08-18T00:00:00Z
tags:
- dns
- Freeware
- Security
title: OpenDNS is Wicked
---

<a title="Use OpenDNS to make your Internet faster, safer, and smarter." href="http://www.opendns.com/share/"><img src="http://images.opendns.com/buttons/use_opendns_155x52.gif" width="155" height="52" style="border:0; float:right; margin-left:5px;margin-bottom;5px" alt="Use OpenDNS" /></a>Over the last couple of weeks the DNS timeouts and lags I've been experiencing at home have made the web experience a little dire. My <a href="http://internode.on.net/" title="Internode">ISP</a> is actually pretty darned good, but for some reason they seem to have glitches with their DNS servers every now and then.

<!--more-->

The last time it happened I wasn't able to get <em>any</em> sites to respond. I ended up popping some manual DNS server entries into my router which I had archived in a "welcome" email that I had received when I first signed up for my Internet account. These worked well for a while, but eventually ended up going offline and so I had to look for another option.

Enter <a href="http://opendns.org/" title="OpenDNS">OpenDNS</a>. A free DNS service with a stack of features that anyone can use. I'll go over a few of them.

First up is <strong>Content filtering</strong>. What a fab idea! Offsite content filtering that stops dodgey and unwanted stuff before it even hits your modem. OpenDNS supports content filtering in a couple of ways which makes it really easy to generally filter out particular sites and content.

<a href="/uploads/2008/08/opendns_content_filtering_levels.png"><img src="/uploads/2008/08/opendns_content_filtering_levels.png" alt="Content Filtering" title="Content Filtering" width="150" style="float:left; margin-right:5px; margin-bottom:5px;" rel="lightbox[opendns]" /></a>The content filtering mechanism is quite extensive. You have the option of choosing a predefined "filtering level", each of which defines a set of site categories which will be filtered out for viewers on your network. The options go from <em>Minimal</em>, which simply blocks known <a href="http://en.wikipedia.org/wiki/Phishing" title="Phishing">phishing</a> sites, through to <em>High</em> which covers everything from porn and illegal activities to video sharing sites.

<a href="/uploads/2008/08/opendns_content_filtering_custom.png"><img src="/uploads/2008/08/opendns_content_filtering_custom.png" alt="Custom Content Filtering" title="Custom Content Filtering" width="150" style="float:right; margin-left:5px; margin-bottom:5px;" rel="lightbox[opendns]" /></a>There is also a <em>Custom</em> level which allows you to choose the categories that you want filtered. This is the long-hand version of the predefined levels mentioned above, which is great as it let's you pick and choose if you want finger-grained control.

<a href="/uploads/2008/08/opendns_site_block.png"><img src="/uploads/2008/08/opendns_site_block.png" alt="Site Blocking" title="Site Blocking" width="150" style="float:left; margin-right:5px; margin-bottom:5px;" rel="lightbox[opendns]" /></a>Lastly, as far as filtering goes, you have the option of allowing or blocking sites as a hard rule. This is handy if you know that a particular site is getting caught in your filter but you know for sure that it's safe. This happens surprisingly often, so having a whitelist is very handy. Being able to block a stack of ad sites before they even hit your browser is also a winner, hence the blacklist feature is great too.

<a href="/uploads/2008/08/opendns_client.png"><img src="/uploads/2008/08/opendns_client.png" alt="OpenDNS Client" title="OpenDNS Client" width="150" style="float:right; margin-left:5px; margin-bottom:5px;" rel="lightbox[opendns]" /></a>Thankfully OpenDNS supports <strong>IP address auto-update</strong>. Over time, you'll no doubt get different IP addresses from your ISP as your DHCP leases expire or when you reconnect your modem to your service. Given that OpenDNS needs to have some way of determining who you are, IP address really is the only way. So to help keep OpenDNS up-to-date with your current IP (and hence, keep applying your filtering rules), there is a client application that you can have running in your system tray which contacts the service periodically and makes sure that it has the latest IP. While this is a great idea it's a bit crap that you have to run a client application. It's a shame that they didn't decide to support the use of another dynamic addressing system such as <a href="http://dyndns.org/" title="DynDns">DynDns.org</a> (lots of modems have built-in support for updating services like DynDns automatically).

<a href="/uploads/2008/08/opendns_customisation.png"><img src="/uploads/2008/08/opendns_customisation.png" alt="Customisation" title="Customisation" width="150" style="float:left; margin-right:5px; margin-bottom:5px;" rel="lightbox[opendns]" /></a>For those who like to make their services feel cosey, OpenDNS supports <strong>customisation</strong> of various bits of functionality. For example, you're able to change the logo picture (this is more of an interest to site admins). You can also modify messages that are displayed when sites are blocked for various reasons. For those of you on home networks, specifying more meaningful messages for those, shall we say, "less technical" family members will no doubt be beneficial in reducing the number of support calls.

<a href="/uploads/2008/08/opendns_shortcuts.png"><img src="/uploads/2008/08/opendns_shortcuts.png" alt="Shortcuts" title="Shortcuts" width="150" style="float:right; margin-left:5px; margin-bottom:5px;" rel="lightbox[opendns]" /></a><strong>Network shortcuts</strong> are another nifty feature. A network shortcut is essentially a bookmark which works across the entire network. All you have to do is specify a name for your shortcut, and the site that it redirects to, and you're done. Once the shortcut has been saved, all you have to do to get to the site is type in the name of the shortcut. When the DNS request is made, OpenDNS looks for any shortcuts by that name that exist in your list, and if found, it will redirect the user to the appropriate site. Groovey!

At first you don't believe it, but OpenDNS is <strong>surprisingly quick</strong>. I wouldn't say that I have conducted a huge set of performance tests and benchmarks, but I would say that it by far outperforms my ISP's DNS servers as far as responsivity is concerned. Not just that, but <strong>it works</strong>. I am yet to see a DNS lookup fail requiring me to retry.

The final redeeming feature of OpenDNS is that it's constantly updated and secure. <a href="http://www.doxpara.com/?p=1185">Flaws in the DNS system</a>, for example, have already been patched. This reduces the chances of you dealing with an insecure DNS server (such as the one sitting at your ISP) and ending up at a site that might be a little unsavoury.

To sum up, I really think OpenDNS is a great service. It's fast, feature-rich and very handy. For anyone with a family/kids or a need/desire to filter out some of the f**ked up content that lives on the web (such as this site ;)), this service is for you. Give it a spin!
