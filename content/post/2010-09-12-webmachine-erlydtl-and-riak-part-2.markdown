---
categories:
- Riak
- Databases
- Functional Programming
- HOWTO
- Erlang
- Webmachine
comments: true
date: 2010-09-12T00:00:00Z
tags:
- web development
- Erlang
- NoSQL
- Webmachine
- Riak
- ErlyDTL
- HAProxy
title: Webmachine, ErlyDTL and Riak - Part 2
---

<img src="/uploads/2010/09/riak-logo.png" alt="Riak Logo" style="float:left;padding-right:5px;padding-bottom:5px;"/>In [Part 1][] of the series we covered the basics of getting the development environment up and running. We also looked at how to get a really simple ErlyDTL template rendering. If you haven't yet gone through Part 1, I suggest you do that now. If you have, read on!

There are a few reasons this series is targeting this technology stack. One of them is **uptime**. We're aiming to build a site that stays up as much as possible. Given that, one of the things that I missed in the previous post was setting up a [load balancer][]. Hence this post will attempt to fill that gap.

<!--more-->

Any serious web-based application will have load-balancing in play somewhere. While not essential during development, it's handy to have a similar set up in the hope that it exposes you to some potential issues you might face when the application reaches production.

There are many high-quality load-balancing solutions out there to choose from. For this series, we shall be using [HAProxy][], which is a common choice amongst developers building scalable web applications. The rest of this post will cover how to set up HAProxy, verifying that the configuration is correct and confirming that it behaves appropriately when nodes in our cluster go down.

Please note the goal is to demonstrate how HAProxy _can_ be configured. When deploying to your production environments please make sure the configuration matches your needs.

### HAProxy installation ###
Let's start by pulling down the latest stable version of HAProxy's source, extracting it and building it. Here's a sample log of what you should expect:

    oj@nix ~/blog $ wget http://haproxy.1wt.eu/download/1.4/src/haproxy-1.4.20.tar.gz

      ... snip ...

    oj@nix ~/blog $ tar -xzf haproxy-1.4.20.tar.gz 

      ... snip ...



At this point we've got the source and we're ready to make. HAProxy requires a parameter in order to build, and this parameter varies depending on your target system:


    oj@nix ~/blog $ cd haproxy-1.4.20
    oj@nix ~/blog/haproxy-1.4.20 $ make

    Due to too many reports of suboptimized setups, building without
    specifying the target is no longer supported. Please specify the
    target OS in the TARGET variable, in the following form:

       make TARGET=xxx

    Please choose the target among the following supported list :

       linux26, linux24, linux24e, linux22, solaris
       freebsd, openbsd, cygwin, custom, generic

    Use "generic" if you don't want any optimization, "custom" if you
    want to precisely tweak every option, or choose the target which
    matches your OS the most in order to gain the maximum performance
    out of it. Please check the Makefile in case of doubts.
    make: *** [all] Error 1



According to [uname][], I'm running Linux Kernel 2.6:

    oj@nix ~/blog/haproxy-1.4.20 $ uname -r
    2.6.31-21-generic


As a result, I'll be passing in **linux26**. Make sure you specify the correct option depending on which system you are running. We'll be building it _and_ installing it so that it can be called from anywhere:

    oj@nix ~/blog/haproxy-1.4.20 $ make TARGET=linux26

        ... snip ...

    oj@nix ~/blog/haproxy-1.4.20 $ sudo make install

       ... snip ...


Simple! We now need to create a configuration for HAProxy which we can use during development. Not surprisingly, HAProxy can be run as a daemon, but it can also be invoked from the command line with a configuration passed as a parameter. For our development, we'll be executing from the command line as this will give us feedback/output on what's going on.

**Note:** If you're using Mac OSX, you can easily use brew to install haproxy with the command `brew install haproxy`

Let's create a file called `dev.haproxy.conf` inside our application directory so that it can be included in our source:

```
# start with the global settings which will
# apply to all sections in the configuration.
global
  # specify the maximum connections across the board
  maxconn 2048
  # enable debug output
  debug

# now set the default settings for each sub-section
defaults
  # stick with http traffic
  mode http
  # set the number of times HAProxy should attempt to
  # connect to the target
  retries 3
  # specify the number of connections per front and
  # back end
  maxconn 1024
  # specify some timeouts (all in milliseconds)
  timeout connect 5000

########### Webmachine Configuration ###################

# here is the first of the front-end sections.
# this is where we specify our webmachine instances.
# in our case we start with just one instance, but
# we can add more later
frontend webfarm
  # listen on port 4000 across all network interfaces
  bind *:4000
  # by default, point at our backend configuration
  # which lists our webmachine instances (this is
  # configured below in another section)
  default_backend webmachines
  # indicate keep-alive
  option http-server-close
  # timeouts
  timeout client 5000

# this section indicates how the connectivity to
# all the instances of webmachine should work.
# Again, for dev there is only one instance, but
# in production there would be more.
backend webmachines
  # we'll specify a round-robin configuration in
  # case we add nodes down the track.
  balance roundrobin
  # enable the "X-Forware-For" header so that
  # we can see the client's IP in Webmachine,
  # not just the proxy's address
  option forwardfor
  # later down the track we'll be making the use
  # of cookies for various reasons. So we'll
  # enable support for this while we're here.
  cookie SERVERID insert nocache indirect
  # list the servers who are to be balanced
  # (just the one in the case of dev)
  server Webmachine1 127.0.0.1:8000
  # timeouts
  timeout server 1200000
  timeout connect 3000

########### Riak Configuration ###################

# We are yet to touch Riak so far, but given that
# this post is going to cover the basics of
# connectivity, we'll cover off the configuration
# now so we don't have to do it later.
frontend dbcluster
  # We'll be using protocol buffers to talk to
  # Riak, so we will change from the default mode
  # and use tcp instead
  mode tcp
  # we're only interested in allowing connections
  # from internal sources (so that we don't expose
  # ourselves to the web. so we shall only listen
  # on an internal interface on port 8080
  bind 127.0.0.1:8080
  # Default to the riak cluster configuration
  default_backend riaks
  # timeouts
  timeout client 1200000

# Here is the magic bit which load balances across
# our four instances of riak which are clustered
# together
backend riaks
  # again, make sure we specify tcp instead of
  # the default http mode
  mode tcp
  # use a standard round robin approach for load
  # balancing
  balance roundrobin
  # timeouts
  timeout server 1200000
  timeout connect 3000
  # list the four servers as optional targets
  # for load balancing - these are what we set
  # up during Part 1. Add health-checking as
  # well so that when nodes go down, HAProxy
  # can remove them from the cluster
  server Riak1 127.0.0.1:8081 check
  server Riak2 127.0.0.1:8082 check
  server Riak3 127.0.0.1:8083 check
  server Riak4 127.0.0.1:8084 check
```


In the configuration above the `backend riaks` section has four server nodes. Each one of them has the `check` option specified. This enables health-checking on the same address and port that the server instance is bound to. If you decided that you didn't want to do health-checking in this manner you easily enable health-checking over HTTP, as Riak has a built-in URI which can be used to validate the state of the node. Change the `backend riaks` section in the configuration to look like this:
```
  ... snip ...

  # enable HTTP health checking using the GET method
  # on the URI "/ping". This URI is part of Riak and
  # can be used to determine if the node is up.
  # We specify that we want to use the GET action, and
  # use the URI "/ping" - this is the RESTful health
  # check URI that comes as part of Riak.
  option httpchk GET /ping
  # list the four servers as optional targets
  # for load balancing - these are what we set
  # up during Part 1. Add health-checking as
  # well so that when nodes go down, HAProxy
  # can remove them from the cluster.

  # change the health-check address of the node to 127.0.0.0:8091
  # which is the REST interface for the first Riak node
  server Riak1 127.0.0.1:8081 check addr 127.0.0.1 port 8091

  # change the health-check address of the node to 127.0.0.0:8092
  # which is the REST interface for the second Riak node
  server Riak2 127.0.0.1:8082 check addr 127.0.0.1 port 8092

  # change the health-check address of the node to 127.0.0.0:8093
  # which is the REST interface for the third Riak node
  server Riak3 127.0.0.1:8083 check addr 127.0.0.1 port 8093

  # change the health-check address of the node to 127.0.0.0:8094
  # which is the REST interface for the third Riak node
  server Riak4 127.0.0.1:8084 check addr 127.0.0.1 port 8094
```


To make sure this is functioning correctly, we need to open two consoles and change our working directory to our `csd` application (for those who have forgotten, `csd` is the application we're building - it was mentioned in [Part 1][]). In console 1:

    oj@nix ~/blog/csd $ sudo haproxy -f dev.haproxy.conf -d
    Available polling systems :
         sepoll : pref=400,  test result OK
          epoll : pref=300,  test result OK
           poll : pref=200,  test result OK
         select : pref=150,  test result OK
    Total: 4 (4 usable), will use sepoll.
    Using sepoll() as the polling mechanism.


This indicates that HAProxy is up and running and waiting for connections. Let's get Webmachine fired up in console 2:

    oj@nix ~/blog/csd $ ./start.sh

        ... snip ...

    =PROGRESS REPORT==== 11-Jul-2012::23:07:27 ===
             application: csd
              started_at: nonode@nohost


Now Webmachine is fired up with our application running. We should be able to hit our page, this time at [localhost][], and see exactly what we saw at the end of [Part 1][].

<img src="/uploads/2010/09/haproxy-validation.png" />

### Verification of HAProxy configuration ###
On the surface it appears that we haven't broken anything. We also need to make sure that any communication with Riak that we have down the track is also functioning. So let's validate that now.

First, we have to make sure that Riak is running. If you have followed [Part 1][] already and your Riak cluster is running then you're good to go. If not, please read [Part 1][] for information on how to install Riak and configure it to run as a cluster of 4 nodes.

Next, let's create 4 new connections and use the [get\_server\_info/1][riakc-getserverinfo] function to find out which node we are connected to. To do this, we'll need to use an Erlang console which has all the Riak dependencies ready to go. It just so happens that when we fired up our Webmachine instance, we got an Erlang console for free. Simply hit the `enter` key and you'll be given a prompt. Notice that when we connect to Riak using the [start_link/2][riakc-startlink] function, we are passing in the IP address and port of the load-balanced cluster instead of one of the running Riak nodes:
```
1> {ok, C1} = riakc_pb_socket:start_link("127.0.0.1", 8080).
{ok,<0.113.0>}
2> riakc_pb_socket:get_server_info(C1).
{ok,[{node,<<"dev2@127.0.0.1">>},
     {server_version,<<"1.1.2">>}]}
3> {ok, C2} = riakc_pb_socket:start_link("127.0.0.1", 8080).
{ok,<0.116.0>}
4> riakc_pb_socket:get_server_info(C2).                     
{ok,[{node,<<"dev3@127.0.0.1">>},
     {server_version,<<"1.1.2">>}]}
5> {ok, C3} = riakc_pb_socket:start_link("127.0.0.1", 8080).
{ok,<0.119.0>}
6> riakc_pb_socket:get_server_info(C3).                     
{ok,[{node,<<"dev4@127.0.0.1">>},
     {server_version,<<"1.1.2">>}]}
7> {ok, C4} = riakc_pb_socket:start_link("127.0.0.1", 8080).
{ok,<0.122.0>}
8> riakc_pb_socket:get_server_info(C4).                     
{ok,[{node,<<"dev1@127.0.0.1">>},
     {server_version,<<"1.1.2">>}]}
```


So we can see that the load balancer has allocated four different connections, each to a different node in the cluster. This is a good sign. So let's kill off one of the nodes:

```
oj@nix ~/blog/riak/dev $ dev3/bin/riak stop
ok
```

In a very short period of time, you should see output in the HAProxy console which looks something like this:

```
WARNING] 192/231028 (33025) : Server riaks/Riak3 is DOWN, reason: Layer4 connection
problem, info: "Connection refused", check duration: 0ms. 3 active and 0 backup
servers left. 0 sessions active, 0 requeued, 0 remaining in queue. 
```

The load balancer noticed that the node has died. Let's make sure it no longer attempts to allocate connections to `dev3`. Note that we call [f()][] in our console before running the same script again, as this forces the shell to forget about any existing variable bindings:
```
9> f().
ok
10> {ok, C1} = riakc_pb_socket:start_link("127.0.0.1", 8080).
{ok,<0.128.0>}
11> riakc_pb_socket:get_server_info(C1).                     
{ok,[{node,<<"dev4@127.0.0.1">>},
     {server_version,<<"1.1.2">>}]}
12> {ok, C2} = riakc_pb_socket:start_link("127.0.0.1", 8080).
{ok,<0.131.0>}
13> riakc_pb_socket:get_server_info(C2).                     
{ok,[{node,<<"dev1@127.0.0.1">>},
     {server_version,<<"1.1.2">>}]}
14> {ok, C3} = riakc_pb_socket:start_link("127.0.0.1", 8080).
{ok,<0.134.0>}
15> riakc_pb_socket:get_server_info(C3).                     
{ok,[{node,<<"dev2@127.0.0.1">>},
     {server_version,<<"1.1.2">>}]}
16> {ok, C4} = riakc_pb_socket:start_link("127.0.0.1", 8080).
{ok,<0.137.0>}
17> riakc_pb_socket:get_server_info(C4).                     
{ok,[{node,<<"dev4@127.0.0.1">>},
     {server_version,<<"1.1.2">>}]}
```


As we hoped, `dev3` is nowhere to be seen. Let's fire it up again:

```
oj@nix ~/blog/riak/dev $ dev3/bin/riak start
```

**Note:** It isn't necessary to tell the node to rejoin the cluster. This happens automatically. Thanks to Siculars (see comment thread) for pointing that out.

HAProxy's console will show you that it has re-established a connection to `dev3`

```
[WARNING] 192/231536 (33025) : Server riaks/Riak3 is UP, reason: Layer4 check passed,
check duration: 0 ms. 4 active and 0 backup servers online. 0 sessions requeued, 0 total in queue.
```

As a final test, let's make sure we see that node get connections when we attempt to connect:
```
18> f().
ok
19> {ok, C1} = riakc_pb_socket:start_link("127.0.0.1", 8080).
{ok,<0.141.0>}
20> riakc_pb_socket:get_server_info(C1).                     
{ok,[{node,<<"dev1@127.0.0.1">>},
     {server_version,<<"1.1.2">>}]}
21> {ok, C2} = riakc_pb_socket:start_link("127.0.0.1", 8080).
{ok,<0.144.0>}
22> riakc_pb_socket:get_server_info(C2).                     
{ok,[{node,<<"dev2@127.0.0.1">>},
     {server_version,<<"1.1.2">>}]}
23> {ok, C3} = riakc_pb_socket:start_link("127.0.0.1", 8080).
{ok,<0.147.0>}
24> riakc_pb_socket:get_server_info(C3).                     
{ok,[{node,<<"dev3@127.0.0.1">>},
     {server_version,<<"1.1.2">>}]}
25> {ok, C4} = riakc_pb_socket:start_link("127.0.0.1", 8080).
{ok,<0.150.0>}
26> riakc_pb_socket:get_server_info(C4).                     
{ok,[{node,<<"dev4@127.0.0.1">>},
     {server_version,<<"1.1.2">>}]}
```

### Wrapping up ###

Excellent. Now that we've got our load-balancer set up in development, we're ready to dive into connecting to Riak from our `csd` application. That will be the topic for the next post in this series.

As always, comments and feedback are welcome and greatly appreciated. Suggestions on improvements and pointers on mistakes would be awesome. To anyone out there who has put HAProxy into production, we would love to hear your comments on your configuration!

**Note:** The code for Part 2 (this post) can be found on [Github][Part2Code].

Other parts in this series: [Part 1][], [Part 3][], [Part 4][], [Part 5][]

  [Part 1]: /posts/webmachine-erlydtl-and-riak-part-1/ "Wembachine, ErlyDTL and Riak - Part 1"
  [Part 3]: /posts/webmachine-erlydtl-and-riak-part-3/ "Webmachine, ErlyDTL and Riak - Part 3"
  [Part 4]: /posts/webmachine-erlydtl-and-riak-part-4/ "Webmachine, ErlyDTL and Riak - Part 4"
  [Part 5]: /posts/webmachine-erlydtl-and-riak-part-5/ "Webmachine, ErlyDTL and Riak - Part 5"
  [HAProxy]: http://haproxy.1wt.eu/ "HAProxy"
  [f()]: http://www.erlang.org/documentation/doc-5.2/doc/getting_started/getting_started.html "Getting started"
  [load balancer]: http://en.wikipedia.org/wiki/Load_balancing_(computing) "Load balancing"
  [localhost]: http://localhost/ "localhost"
  [riakc-startlink]: https://github.com/basho/riak-erlang-client/blob/master/src/riakc_pb_socket.erl#L97 "riakc_pb_socket:start_link/2"
  [riakc-getserverinfo]: https://github.com/basho/riak-erlang-client/blob/master/src/riakc_pb_socket.erl#L181 "riakc_pb_socket:get_server_info/1"
  [uname]: http://en.wikipedia.org/wiki/Uname "uname"
  [Part2Code]: https://github.com/OJ/csd/tree/Part2-20110403 "Source Code for Part 2"
