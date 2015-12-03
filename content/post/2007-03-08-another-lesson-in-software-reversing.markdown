---
categories:
- ASM
- RCE
comments: true
date: 2007-03-08T00:00:00Z
title: Another Lesson in Software Reversing
---

<a href="http://www.iitac.org/" title="IITAC.org"><img src="http://certification.iitac.org/templates/default/images/HeaderIcon.png" class="InlineImageLeft" alt="IITAC.org" /></a>Yes, you can (most probably) consider this to be a fairly regular segment from this point on :) As I said before I've always been partial to RCE, and I don't think I'll ever get sick of it.  Today's installment is another tutorial that I felt shouldn't be published.  The reason is because it's a tutorial on how to solve an example reversing challenge for the <a href="http://certification.iitac.org/goto.php?target=cat_384&client_id=iitac" title="">IITAC online RCE adacemy</a>, and I generally don't think it's a good move to show other people how to do this stuff when they can get certified for it. However, this is just a <em>training</em> example so I think I'm safe.

The challenge consists of a few tasks:<ol><li>Removing a nag-screen</li><li>Finding a hard-coded serial number</li><li>Finding a valid name/serial combination</li><li>Writing a key generator</li></ol>You'll notice from the tutorial that the full source code is included written in 32-bit ASM.  Again, I would appreciate the feedback on the quality from anyone who wishes to give it (that includes you Alister ;) ) as I'm always looking to improve what I write.

You can grab it from <a href="/uploads/2007/03/01-course-splish-2.zip" title="Course 01 - Splish 2 reversing solution">here</a>.
