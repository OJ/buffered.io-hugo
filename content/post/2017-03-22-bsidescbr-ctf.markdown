---
categories:
- Security
- CTF
comments: true
date: 2017-03-22T11:51:10+10:00
title: BSidesCBR CTF Round Up
---

<img style="float: right; margin-left: 5px; margin-bototm: 5px" src="/uploads/2017/03/bsides/bsides-ctf.png" width="200px"/>
[BSides Canberra](http://www.bsidesau.com.au/) for 2017 has just finished up! A cracking 2-day conference hosted by a bunch of infosec folks down here in Australia, and everything went as well as it could have. It was great fun, and the vibe there was really awesome.

If you're here for the details on how to get the CTF challenges running locally, jump to the bottom of the post. Keep reading if you want more information on how things went.

I was ~~stupid~~ nice enough to offer to help out with organising the CTF, along with a bunch of very dear friends. Some of you who have been following along will already know that it took a few months of effort to prepare for. We had weekly meetings, many nights burned building challenges, other nights burned testing challenges, infrastructure stresses and much more along the way. On the day, we managed to get it all up and running, and I can honestly say that I'm not only really happy with how it turned out, I'm super proud and humbled by the efforts of the people I worked with.

## Shoutouts

* [sw1tch](https://twitter.com/__sw1tch__) - This dude is a machine. Not only did he build and prepare a stack of challenges (including trivia, scavenger hunt, and the _famous_ "Mr Robot" series carried on from last year), he also put in a **huge** effort on the day to make the whole room have the right vibe. He prepared lights, TVs, music and more. Without him, the experience would have been a _lot_ more dull. He also made and paid for the CTF stickers, which looked awesome.
* [Peleus](https://twitter.com/0x42424242) - The man who kicked it all off. Despite the pressures faced from various areas of life, he put in a huge effort to get challenges together and playtested a load of them as well. You can thank him for all of the horrible crypto challenges that you failed to break.
* [Rick](https://twitter.com/rickoates) - A quiet, unsung hero. His forensics challenges made people suffer, which is exactly what we wanted. He did a stack of playtesting and even managed to achieve his OSCE certification during the course of preparing this CTF. Kudos to the boy, and much `<3` for the effort he put in.
* [acebond](https://twitter.com/aceb0nd) - A relative newcomer to the security scene, he stepped up when the rest of us started to drop the ball and built a number of reversing and web challenges. It's no easy to work with people like me in such cases, but he made it all happen and did a fantastic job. Huge props to this boy.
* [Joffy](https://twitter.com/Joflixen) - The man who had to keep the infra going! This is no small job, and he was all over it like a cheap suit. Joff is the kind of person that nobody seems to see because he's frantically battling behind the scenes. Without him, we'd have been screwed. He did an awesome job on the day and we owe him a huge thanks for his efforts.
* [Andrew](https://twitter.com/andrew__muller) - The man behind [Ionize](https://twitter.com/1onize), the company that sponsored the CTF and provided a stack of resources (including 4 of the people listed above). Not only did Andrew support the event, he paid for prizes, and even picked up the tab for various "on the fly" issues such as chairs (which we bought from Bunnings on the day because the venue had run out!), power cables, etc that allowed us to keep our competitors powered and comfortable during the event. You'd be hard pressed to find a nicer and more supportive person in the security community.
* [Justin](https://twitter.com/justinsteven) - Our beloved Ginger Ninja who wasn't able to be there with us on the day. He helped playtest a bunch of stuff, and also built the `bogecoin` challenge, which broke a few heads along the way. Love this guy, love his work, love his support.

## Stats and other info

Brief stats for those interested:

* There were `59` challenges created in `10` different categories.
* Of the `59` challenges created, only `3` were left standing at the end of the day.
* A total of `66` teams registered.
* A total of `38` teams submitted at least one flag.
* There were approximately `8500` points up for grabs across all challenges.
* The winners, [Cybears](https://twitter.com/cybearsCTF), scored approximately `5500` points.
* We had `193` unique machines that joined the CTF network.
* The maximum concurrent machines partcipating was `107` at 4:30pm Friday 17th.
* `36` clients were active at the closing of the competition at 3:30pm Saturday 18th.
* Over `55%` of all wireless hardware presented on the network as an Apple device.
* `80.4Gb` of traffic was transferred, despite it being a _closed network_.
* The number of beers bought from CTF Bar over two days was `31`.


We had a [team of year 11 students](https://twitter.com/CodeCadets) compete! Not only did they do really well (4th place) they managed to find 0day in one of the devices that we had on the network. This was a _different_ 0day than the one we had intended to find! How exciting. These young folks now get to experience the ~~pain~~joy of the vendor disclosure process. Well done to those guys.

Cybears, winners of `$500` for coming first, donated all of their winnings to the Code Cadets (the year 11 student team). Huge props to them for the generosity and support for the next generation.

## Get the CTF

As promised, we have put some effort into having the CTF "boxed up" in a way that will let you play with it. Short notes:

* The scavenger hunt, trivia and Mr Robot challenges aren't part of it. Most of these are hard to replicate, especially the dumpster diving and finding 0day in a device we refuse to put online.
* All web, pwnable, reversing, forensics, crypto and misc challenges are included.
* Challenges that require a service running have all been dockerised. To run them, you will need to make sure you have `docker` installed, and also `docker-compose`. If you can't do that all by yourself, don't ask for help, because that's the easy part!
* To get access to the challenges, you'll need to open a command prompt and run the following:

```
$ git clone https://github.com/OJ/bsides-2017-ctf-docker
```

* Have a look at the [main README](https://github.com/OJ/bsides-2017-ctf-docker) first. Each challenge should have it's own README with specific details. Make sure you read the whole README before you start the challenge, to make sure you're not doing the wrong thing.
* Despite dockerising the scoreboard, we can't release it as we wanted, because it's a clusterfuck of shit. I will not be using that thing again. Chances are I will be building my own for next time. Sorry, if you want to submit/confirm flags, you can DM me personally on [the Twits](https://twitter.com/TheColonial).

If you have any issues with any of the challenges, feel free to hit any of us up! We'll do what we can to help.

Thanks to everyone for your support. Until next time!
