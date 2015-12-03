---
categories:
- C#
- Riak
- Mono
- NoSQL
- Open Source
- CLR
comments: true
date: 2011-08-03T00:00:00Z
tags:
- corrugatediron
- dotnet
- csharp
- clr
- mono
- riak
title: CorrugatedIron Update - v0.1.1
---

Last week [JP][] and I released our first Open Source project, [CorrugatedIron][]. The release seemed to be fairly well received by those people who gave it a spin. We've had some good feedback along the way which we'll be evaluating, and no doubt those suggestions and comments will be influencing the future of the library.

In the interim, we wanted to get another version out which sorts out two main issues and that changes the _perceived_ "norm" when building applications with CorrugatedIron. Those issues are listed below. We've also go the first pass of our [Map/Reduce][] documentation ready.

If you're not interested in the detail, head on over to the [download page][downloads] to find out the many ways in which you can get access to the release. Otherwise, please read on!

<!--more-->

## Removal of IoC ##

When we first put together the sample applications we thought that it'd be a good idea to show how these things can be done using what the mainstream .NET developers would use. That is, we decided to wire everything in with IoC. This wasn't because we felt that this is how it **had** to be done, but more to try and give people a level of familiarity. The library that we chose to use for the samples was Unity, for no other reason that "it was there".

This small mistake seemed to give off the impression that we felt that Unity was the best choice of all the IoC containers out there.

This is most definitely **not the case**. We are in no way advocating the use of one IoC container over the other. We honestly don't care which one you want to use. You should use whichever works for you.

To avoid this perception we decided that it would be best to remove references to any IoC container in all samples except for the [one sample][IoCSample] which shows how to use _lots_ of different containers to do the same thing. Hence you should see a _lack_ of IoC containers in our examples from now on. Sorry for any confusion.

## Handling of Client IDs ##

Our first implementation of Client ID generation in CorrugatedIron wasn't a great implementation. We made the decision early on to generate IDs based on some details of the machine that the client was running on (ie. The MAC address of the first functioning NIC on the machine). Our thinking was that we wanted to uniquely identify a client while still allowing the ID to be reused across instances of the application. This might make sense for rich-client applications, but certainly doesn't work well in the web world. In a web environment, each request could come from a different user.

Almost immediately after releasing v0.1.0, [JP][] and I read an email on the [Riak mailing list][mailinglist] which made us rethink our approach. After a bit of discussion, we decided to go with an idea of Jeremiah's which involves the generation of the Client ID when the RiakClient instance is created. This generation can be controlled by the user of the library by specifying a `seed` value.

This gives the user the flexibility of not being concerned about the Client ID if they don't want to be, but can have some control if they do.

## Ease of Configuration ##

The only bit of "constructive criticism" that we received on-masse was via [Hacker News][] and revolved around configuration. The general feeling was that the effort required to configure the library was higher than expected, especially when compared to other libraries. I'd suggest reading the full discussion over on the [Hacker News][] site to read some of the reasons behind the design decisions. But, if you're too lazy (I don't blame you if you are), the short version is this: CorrugatedIron is a .NET library connecting to a clustered, distributed key-value store. A library that does this, while attempting to manage load-balancing across all nodes in the cluster, is going to require some configuration.

One concern in particular resonated with me, and that was the difficulty in getting CorrugatedIron running inside a REPL, such as [FSI][]. The two issues with getting a REPL to work from configuration are:

* The ability to specify the location of the configuration file.
* The number of lines of code it takes to wire things in.

The XML that's required is not going to be changing in the short term. The values that are specified in that configuration are required to make the most of Riak and that's not something we're prepared to compromise on. However, the .NET code required to access it has changed, though the old way of wiring things in still exists for those people who want that level of flexibility.

In short, you can wire-in CorrugatedIron's XML configuration as simply as:

```
var cluster = RiakCluster.FromConfig("riakConfig");
var client = cluster.CreateClient();
```

## That's it! ##

Hopefully this will make your life a little easier while getting CorrugatedIron up and running. We're always keen to hear your feedback, so please [drop us a line][contact] if you have any thoughts, suggestions or issues.

Enjoy!

  [contact]: https://github.com/DistributedNonsense/CorrugatedIron "CorrugatedIron @ Github"
  [FSI]: http://www.fsharphelp.com/Interactive.aspx "F# interactive"
  [Hacker News]: http://news.ycombinator.com/item?id=2799823 "CI on Hacker News"
  [Map/Reduce]: http://corrugatediron.org/documentation/MapReduce.html "Map/Reduce"
  [Corrugatediron]: http://corrugatediron.org/ "CorrugatedIron"
  [JP]: http://facility9.com/ "Jeremiah Peschka"
  [v0.1.1]: https://github.com/DistributedNonsense/CorrugatedIron/tree/v0.1.1 "CorrugatedIron v0.1.1"
  [downloads]: http://corrugatediron.org/downloads.html "Downloads page"
  [IoCSample]: https://github.com/DistributedNonsense/CorrugatedIron.Samples/tree/master/VisualStudio2010/Sample "IoC Sample Project"
