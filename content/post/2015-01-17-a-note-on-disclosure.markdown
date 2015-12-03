---
categories:
- Security
- Disclosure
comments: true
date: 2015-01-17T08:19:03Z
title: A Note on Disclosure
---

In October last year, while conducting an internal assessment for a client in Sydney, I found a vulnerability in a vendor product. The flaw allows for remote code execution on the device, _as the root user_, without requiring authentication. Needless to say, "instant remote root" vulnerabilities are bad. On the scale of bug severity, they're up pretty high. For a device such as this, it doesn't really get any worse.

Once I had a working proof-of-concept which demonstrated the flaw, I made contact with the vendor in an effort to disclose the issue in a secure and responsible manner. I was aware that other options were available, such as handing the issue over to CERT or some other initiative that deals with the pain of disclosure, but I wanted to get first-hand experience of the process, hence I decided to do it myself.

I'm not going to lie, it has been frustrating.
<!--more-->

Disclosure is a hot topic at the moment. Big players such as Google and Microsoft are butting heads over it, the security community is constantly talking about it, and everyone seems to have their own spin on what it should and shouldn't be. While I have my own ideas around what disclosure is and how it should be handled, I am yet to form a really clear view of how it should be dealt with.

Part of the reason I am struggling to "lock it down" is because, in my opinion, not all vendors are equal. Not all software is equal. Not all devices are equal. Not all bugs are equal. To me it doesn't necessarily make sense to treat all disclosures as equal. The hardest part is knowing how long to wait before going public.

Since establishing contact with the vendor in October, I have had a pretty frustrating experience. I made it clear that I had intended to give them 100 days to work through the issues and release a fix, a time frame which I felt was more than reasonable given the severity, the conditions and the need for people to have time to install the fix once released.

That disclosure period ends on the 30th January (less than two weeks from today). So far, I still haven't received confirmation that they have even been able to reproduce the issue.

The purpose of this post is not to go into detail of this case, as that will come later when disclosure is upon us. I'm also intentionally avoiding the discussion of who's right and wrong in the [Google][] vs [Microsoft][] case; other more [seasoned][errata] [security][spacerogue] people have already commented on this. The purpose is instead to talk about a nuance which has led me to extend my original disclosure period; something that will no doubt make some people in the security community shake their heads and curse me for keeping users at risk for longer.

## Extension "Justification"

The initial discussion with the vendor consisted of me attempting to get past front-line support and speak to someone who either cared about the security of their products or was responsible for it (preferably both, in fact!). What's clear is that I totally failed to get hold of the right people.

The people on the other end of the line clearly had no idea how to respond to me, nor did they have a clear path on how to handle the issue. The responses I received were pre-canned, and demonstrated a total lack of understanding.

Fast-forward 2 months to the end of December and OJ was a very sad and frustrated person. The discussion hadn't improved, the vendor had gone silent, and it was clear that no progress was being made.

Given that this was my first attempt at disclosure, I didn't want it to end with a long period of silence and me dropping 0day. I really did want to see things get resolved appropriately. I knew I hadn't been talking to the right people, and I felt that I should make an extra effort to find them.

I put myself in the shoes of those responsible, something I found easy to do given that I'd been a Software Engineer for most of my career, and I asked myself "how would I feel if I was responsible for handling that issue but wasn't given the chance to do so?" I know I'd feel like I was cheated out of the chance to get things fixed within the given time frame. I also know that I would not blame the researcher for pushing ahead with their initial disclosure on the date that they intended to. Instead I would blame my own organisation for the failure.

And so I took to LinkedIn (yes, I know, that's how desperate I was) and hunted down a few vendor employees who had the word "security" in their job title and began emailing them. Given that it was early January by this time I didn't expect an immediate response as most people were probably on holidays. I waited.

A few days later I posted another message on Twitter expressing frustration at the lack of contact from the vendor. I do this kind of thing periodically in the hope that someone who can help would notice it. To cut the story short, someone did, and they helped connect me directly to someone inside the organisation who immediately set up a secure communication channel and was receptive and keen to get things fixed. This was music to my ears. A day or two later I received a response from someone I contacted after trawling LinkedIn. This person was part of the same team as the other person I had recently connected with, and who also expressed a desire to help. This lifted the spirits a great deal.

Fast forward to yesterday: the day I wrote up a document which included graphic detail of the issues I had discovered, suggested remediation approaches, CVE details, a functional "push button, receive bacon" exploit script, and anything else that I could think of that might help them fix the problems. I also included the timeline of events from the day that the issues were discovered. I got to the point where I was almost ready to send it all to my new contact, and then I stalled.

I was staring at the section which detailed the intended timeline for disclosure and I really didn't know what to put in it. There was just 14 days left of the original timeline, and it was clear to me that 14 days would never be enough to address the issues, package up the fixes, test them and release them to the public in time.

Part of me was thinking "I'm sorry, but until now I've done everything I could by the book, and I'm sticking to the timeline I initially proposed." Another part of me was screaming my daily mantra of **"Try to be less of a dick today than you were yesterday."** I can't make other people (or vendors) do this, but I make myself do it. These two conflicting thoughts made it very difficult for me to decide what to do.

I felt that another 100 days was out of the question. Enough time had already been eaten up, and users were still at risk. I wanted to choose a period of time which I felt was fair, giving enough time to fix things while still trying to force them to act quickly. I ended up deciding to extend the disclosure period by another 30 days. That gives the vendor, and my new contact, about 45 days in total to address the problems and release a fix.

I am comfortable with this decision. I made a point of trying to find someone else to help with the problem and I didn't feel it was appropriate to then drop an unreasonable timeline on them, even if it was their own organisation that had let them down. With enough effort and focus, 45 days is definitely achievable.

In closing, the point that I am trying to make here is that while I think "hard and fast" disclosure time lines are in the best interest of basically everyone involved, I do think that there are cases where extensions are valid. At the end of the day, I want to feel like I've done the right thing with the interests of both users and vendors in mind. In this case, I think I have. You might not agree. Feel free to share your thoughts with me in the comments.

If you've made it this far, thanks for reading. Expect to see full disclosure, Metasploit module, and a standalone Python exploit appear on the **1st March 2015**.

## Update 1st March 2015

The disclosure deadline has come and gone and the [advisory has now been released][advisory].


  [Google]: https://code.google.com/p/google-security-research/issues/detail?id=123
  [Microsoft]: http://blogs.technet.com/b/msrc/archive/2015/01/11/a-call-for-better-coordinated-vulnerability-disclosure.aspx
  [errata]: http://blog.erratasec.com/2015/01/a-call-for-better-vulnerability-response.html
  [spacerogue]: http://www.spacerogue.net/wordpress/?p=536
  [advisory]: https://beyondbinary.io/advisory/seagate-nas-rce/
