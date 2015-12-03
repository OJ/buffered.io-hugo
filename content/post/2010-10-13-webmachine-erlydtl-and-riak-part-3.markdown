---
categories:
- Riak
- Databases
- Functional Programming
- HOWTO
- Erlang
- Webmachine
comments: true
date: 2010-10-13T00:00:00Z
tags:
- web development
- Erlang
- NoSQL
- Webmachine
- Riak
- ErlyDTL
- HAProxy
title: Webmachine, ErlyDTL and Riak - Part 3
---

<img src="/uploads/2010/09/riak-logo.png" alt="Riak Logo" style="float:left;padding-right:5px;padding-bottom:5px;"/>For those of you who are new to the series, you may want to check out [Part 1][] and [Part 2][] before reading this post. It will help give you some context as well as introduce you to some of the jargon and technology that I'm using. If you've already read then, or don't want to, then please read on!

This post builds on the previous two, but not without a few little modifications. If you're interested in following along step by step with your own version of the code running, then get yourself a copy of [this changeset][Part2Code] before doing so.

In this post we're going to cover:

1. A slight refactor of code structure to support the "standard" approach to building applications in Erlang using OTP.
1. Building a small set of modules to talk to [Riak][].
1. Creation of some [JSON][] helper functions for reading and writing data.
1. Calling all the way from the [Webmachine][] front-end to Riak to extract data and display it in a browser using [ErlyDTL][] templates.

There are quite a few code snippets in this post as well as output from script executions and `bash` sessions. To avoid confusion, all file listings reference the path to the file that is being modified relative to the root of the project folder.

Be warned, this is a _long_ post :) Get yourself a _shmoke und a pancake_, a glass of your favourite beverage and put some relaxing music on (instrumental is best).

Are you ready? OK, here we go ...

<!--more-->

A Slight Refactor
-----------------

I was ready to embark on this third post a while back but then I sat back and thought about how I might structure things if I were using another set of technologies. Usually I would put another layer between the web tier and the back-end database cluster as opposed to having the web tier talk to the database directly. It didn't make sense to me that this approach would be any different in Erlang.

I had a chat to [two][SJMackenzie] [blokes][MattErbs] that I really respect to get their views, and then I fired off a question to the Basho guys (via the [#riak IRC channel][RiakIRC]). The Basho lads even made the effort to respond to me via the [Riak Recap][] as they weren't available at the time to answer me via IRC (thanks again [Mark][]). All three of them confirmed my thoughts. Here's what appeared in the recap which captures the question and response nicely:

> Q --- I have a Webmachine application which will be talking to Riak. I was going to put application and controller logic in that application and I am wondering if [I] should instead be creating a "core" OTP application with the business style logic in it and have the Webmachine app talk to that app which, in turn, talks to Riak? Is that the general approach that is taken [in Erlang applications]? (from TheColonial via #riak)
> 
> A --- We recommend going with the latter approach. You're better off to create a core app that talks to Webmachine and Riak separately.

Perfect, that makes total sense. Therefore the following describes what I did to modify the code base that I had in order to support this set up. **Any failure** in implementation, structure or understanding is totally my own and in no way reflects on the abilities and advice of those mentioned above who took the time to offer assistance.

Moving on. What we want to end up with is three applications:

<table cellspacing="0">
  <thead>
    <tr>
      <th style="text-align:center;">Application</th>
      <th style="text-align:center;">Structure/Responsibility</th>
      <th style="text-align:center;">Talks to</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Riak</td>
      <td>Bomb-proof data storage and replication.</td>
      <td style="text-align:center">-</td>
    </tr>
    <tr>
      <td>csd_core</td>
      <td>An OTP application that provides an API to a set of logic that deals with the transformation of data from a client through to the database. It should keep the clients ignorant of the data storage medium. It should provide business logic that would be required for any client application to be able to talk to a `csd`/Riak back-end.</td>
      <td style="text-align:center">Riak</td>
    </tr>
    <tr>
      <td>csd_web</td>
      <td>Provide a nice, web-based interface for the user to experience the goal of the Code Smackdown application.</td>
      <td style="text-align:center">csd_core</td>
    </tr>
  </tbody>
</table>

Given that we're going to be using this structure, the "root" folder should actually be fairly clean without any source. Instead, each `csd`-related application should live in its own sub-folder under an `apps` folder and the root should just contain the means to build it and start it. In essence what we'd like to see in the root folder is something like this:

    oj@spawn-link ~/blog/csd $ ls -F
    apps/  dev.haproxy.conf  Makefile  proxy.sh*  rebar*  rebar.config  start.sh*

With that in mind, let's start the surgery.

### Moving csd to csd_web ###

There are two ways to approach this problem. The first is to do a **find and replace**, making sure you cover off file names as well as module names, etc. The second is to simply **recreate the web site from scratch**, copy over any missing files and make any other adjustments manually that may be required.

I preferred the second approach, so that's what I did. First I recreated the web application, which is now called `csd_web` in the `apps` folder:

    oj@spawn-link ~/blog/csd $ mkdir apps && cd apps
    oj@spawn-link ~/blog/csd/apps $ ~/blog/webmachine/scripts/new_webmachine.sh csd_web .
    ==> priv (create)
    Writing /home/oj/blog/csd/apps/csd_web/README
    Writing /home/oj/blog/csd/apps/csd_web/Makefile
    Writing /home/oj/blog/csd/apps/csd_web/rebar.config
    Writing /home/oj/blog/csd/apps/csd_web/rebar
    Writing /home/oj/blog/csd/apps/csd_web/start.sh
    Writing /home/oj/blog/csd/apps/csd_web/ebin/csd_web.app
    Writing /home/oj/blog/csd/apps/csd_web/src/csd_web.erl
    Writing /home/oj/blog/csd/apps/csd_web/src/csd_web_app.erl
    Writing /home/oj/blog/csd/apps/csd_web/src/csd_web_sup.erl
    Writing /home/oj/blog/csd/apps/csd_web/src/csd_web_resource.erl
    Writing /home/oj/blog/csd/apps/csd_web/priv/dispatch.conf
    oj@spawn-link ~/blog/csd/apps $ ls -F
    csd_web/

Next I removed a few files which weren't going to be needed any more. I then copied over `rebar.config`, the ErlyDTL templates and the `csd.app.src` file (which we need to modify):

    oj@spawn-link ~/blog/csd/apps $ cd csd_web
    oj@spawn-link ~/blog/csd/apps/csd_web $ rm README rebar start.sh
    oj@spawn-link ~/blog/csd/apps/csd_web $ cp ../../rebar.config .
    oj@spawn-link ~/blog/csd/apps/csd_web $ cp -R ../../templates .
    oj@spawn-link ~/blog/csd/apps/csd_web $ cp ../../src/csd.app.src ./src/csd_web.app.src

I then edited the `csd_web.app.src` file so that the names were updated (I tidied it up a little and added a version number too):

```
%%-*- mode: erlang -*-
{application, csd_web,
  [
    {description, "The Webmachine component of the Code Smackdown application."},
    {vsn, "0.0.1"},
    {modules, []},
    {registered, []},
    {applications,
      [
        kernel,
        stdlib,
        crypto,
        mochiweb,
        webmachine
      ]
    },
    {mod, {csd_web_app, []}},
    {env, []}
  ]
}.
```

I then opened up `csd_web_resource.erl` and made it look like the original `csd_resource.erl` so that it called the ErlyDTL template:

```
-module(csd_web_resource).
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
  {ok, undefined}.

to_html(ReqData, State) ->
  {ok, Content} = sample_dtl:render([{param, "Slartibartfast"}]),
  {Content, ReqData, State}.
```

`csd_web` is now ready to go. To build it, we need to go back up to the root `csd` folder and adjust the `rebar.config` so that it knows to look in the `apps` sub-folder (thanks to [Andrew][] for [pointing this out][RebarAppsFolder]). We can also remove all the dependencies because that will be taken care of by `csd_web`:

```
%%-*- mode: erlang -*-
{sub_dirs, ["apps/csd_web"]}.
```

Next, I removed all the other left-over stuff in the root folder that wasn't required any more (including the startup script):

    oj@spawn-link ~/blog/csd $ rm -rf README priv src templates start.sh

I then modify the `Makefile` so that it does a couple of other things:

1. Includes a target which builds just the current applications _without_ building the dependencies (this will make builds much quicker most of the time).
1. Includes a target which can start the web application, essentially replacing the original startup script. This target will be dependent on the previous target so that it is always up to date when running the application.
1. Includes targets which can start/stop `HAproxy`.

```
ERL ?= erl
APP = csd

.PHONY: deps

all: deps
  @./rebar compile

app:
  @./rebar compile skip_deps=true

deps:
  @./rebar get-deps

clean:
  @./rebar clean

distclean: clean
  @./rebar delete-deps

webstart: app
  exec erl -pa $(PWD)/apps/*/ebin -pa $(PWD)/deps/*/ebin -boot start_sasl -s reloader -s csd_web

proxystart:
  @haproxy -f dev.haproxy.conf
```

All that is left to do is start `haproxy` and launch the application (make sure `Riak` is running first). These commands need to be done in two different terminal windows. First, start the proxy (note the use of `sudo` so that we can listen on port 80):

    oj@spawn-link ~/blog/csd $ sudo make proxystart
    [2] 1935
    Available polling systems :
         sepoll : pref=400,  test result OK
          epoll : pref=300,  test result OK
           poll : pref=200,  test result OK
         select : pref=150,  test result OK
    Total: 4 (4 usable), will use sepoll.
    Using sepoll() as the polling mechanism.

Then make and start the web application. We have to do a full `make` first time around so that all the dependencies are resolved:

    oj@spawn-link ~/blog/csd $ make && make webstart

       ... snip ...

    =PROGRESS REPORT==== 4-Apr-2011::21:04:18 ===
             application: csd_web
              started_at: nonode@nohost

Now we should be able to hit [localhost][] and see the ErlyDTL template rendered in all its awesome, black-and-white glory:

<img src="/uploads/2010/10/localhost-slartibartfast.png" />

Refactor complete. Now let's start work on our new OTP application which will be responsible for talking to Riak.

If you need a break, now is the time to take it! Go freshen up, take a leak and refill your glass.

Ready to go again? Here we go ...

### Creating the csd_core OTP Application ###

Creation of an OTP-compliant application is another job for [Rebar][] as it comes with a set of templates built-in. Unfortunately those template aren't 100% and hence don't do everything we need to do out of the box. But we shall use them as a starting point:

    oj@spawn-link ~/blog/csd $ mkdir apps/csd_core && cd apps/csd_core
    oj@spawn-link ~/blog/csd/apps/csd_core $ ../../rebar create-app appid=csd_core
    ==> csd_core (create-app)
    Writing src/csd_core.app.src
    Writing src/csd_core_app.erl
    Writing src/csd_core_sup.erl

We have a very simple application shell set up, but we need to do a bit more work to get it ready. First, let's create our base `csd_core.erl` module which is used to fire up our application. For this we will use `csd_web.erl` (the one which is part of our Webmachine application) as a template. Note that I've shuffled things around and removed some things that are not relevant:

```
%% @author OJ Reeves <oj@buffered.io>
%% @copyright 2011 OJ Reeves

%% @doc csd_core startup code

-module(csd_core).
-author('OJ Reeves <oj@buffered.io>').
-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    ensure_started(crypto),
    csd_core_sup:start_link().

%% @spec start() -> ok
%% @doc Start the csd_core server.
start() ->
    ensure_started(crypto),
    application:start(csd_core).

%% @spec stop() -> ok
%% @doc Stop the csd_core server.
stop() ->
    Res = application:stop(csd_core),
    application:stop(crypto),
    Res.
```

Next up, edit `csd_core.app.src` and add some application-specific information:

```

{application, csd_core,
  [
    {description, "Core functionality for the Code Smackdown application."},
    {vsn, "0.0.1"},
    {registered, []},
    {applications,
      [
        kernel,
        stdlib
      ]
    },
    {mod, {csd_core_app, []}},
    {env, []}
  ]
}.
```

We know that we'll be talking to Riak, so we need to make sure we've included the `riakc` (Riak client) dependency. Though I haven't yet talked about it, we'll also be using Mochiweb's [mochijson2][] module to help with handling JSON data, so we shall add this as a dependency to the application. Bear in mind this is already a dependency for the web component of the application, so we're not actually adding a _new_ dependency to the overall application.

We can do this by creating a `rebar.config` in `apps/csd_core` and editing it to contain the following:

```
%%-*- mode: erlang -*-
{deps,
  [
    {mochiweb, "1.5.1", {git, "git://github.com/mochi/mochiweb", {tag, "1.5.1"}}},
    {riakc, ".*", {git, "git://github.com/basho/riak-erlang-client", "HEAD"}}
  ]
}.
```

Then we need to tell `rebar` to build this new application by adjusting the `rebar.config` in the `csd` root folder:

```
%%-*- mode: erlang -*-
{sub_dirs, ["apps/csd_core", "apps/csd_web"]}.
```

Now we have enough to get the `csd_core` application started, even though it doesn't do anything. We just need to adjust our `Makefile` target so that it launches the `csd_core` application as well:

```
ERL ?= erl
APP = csd

.PHONY: deps

all: deps
  @./rebar compile

app:
  @./rebar compile skip_deps=true

deps:
  @./rebar get-deps

clean:
  @./rebar clean

distclean: clean
  @./rebar delete-deps

webstart: app
  exec erl -pa $(PWD)/apps/\*/ebin -pa $(PWD)/deps/\*/ebin -boot start_sasl -s reloader -s csd_core -s csd_web

proxystart:
  @haproxy -f dev.haproxy.conf
```

Then off we go:

    oj@spawn-link ~/blog/csd $ make webstart
    ==> csd_core (compile)
    Compiled src/csd_core_app.erl
    Compiled src/csd_core_sup.erl
    Compiled src/csd_core.erl

       ... snip ...

    =PROGRESS REPORT==== 4-Apr-2011::21:49:27 ===
             application: csd_core
              started_at: nonode@nohost

       ... snip ...

    =PROGRESS REPORT==== 4-Apr-2011::21:49:27 ===
             application: csd_web
              started_at: nonode@nohost

As you can see we now have a system which contains both `csd_core` and `csd_web`. This is great, but `csd_core` needs a lot more work. The intent for this application is to be an [OTP][] application which provides an API to the `csd` logic and back-end database. This means we're going to need to get ourselves a [gen_server][] set up which can handle requests from various clients. Let's do that next.

Thankfully `rebar` comes with a simple template that we can use for creating the `gen_server` behaviour, so we can invoke that from the command line and have it generate the shell for us:

    oj@spawn-link ~/blog/csd/apps/csd_core $ ../../rebar create template=simplesrv srvid=csd_core_server
    ==> csd_core (create)
    Writing src/csd_core_server.erl

We now have a very dumb server ready to go, to make it start with the rest of the application we have to modify `csd_core_sup`, the [supervisor][] and tell it to fire up the server for us:

```
-module(csd_core_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  Server = ?CHILD(csd_core_server, worker),
  Processes = [Server],
  {ok, { {one_for_one, 5, 10}, Processes} }.
```

With this in place we can now start our application again and we should see the new `csd_core_server` appear in the start-up sequence:

    oj@spawn-link ~/blog/csd $ make webstart

       ... snip ...

    =PROGRESS REPORT==== 4-Apr-2011::22:04:04 ===
              supervisor: {local,csd_core_sup}
                 started: [{pid,<0.54.0>},
                           {name,csd_core_server},
                           {mfargs,{csd_core_server,start_link,[]}},
                           {restart_type,permanent},
                           {shutdown,5000},
                           {child_type,worker}]

    =PROGRESS REPORT==== 4-Apr-2011::22:04:04 ===
             application: csd_core
              started_at: nonode@nohost

       ... snip ...

The shell and structure of our application is now in place. We are finally ready to start talking to Riak!

Again, now is the time to have a mini-break if you need one. Grab a _Shigar und a waffle_ and a cup of English Breakfast tea.

Preparing csd_core for Riak connectivity
----------------------------------------

Given that this is the first look at connecting to Riak, we're going to have to set up a little infrastructure to support our needs. As a result, the data itself won't be discussed much for fear of turning this post into something way more epic than originally intended.

So in short, we're interested in storing the idea of a _code snippet_. That is an entity which contains two opposing blobs of code which are being compared. That snippet will have a title. Down the track, more information will be associated with this snippet, such as the author, along with links to a set of comments and votes. For now we'll just focus on storing the bare essentials of the snippet.

### The Snippet ###

As far as our Erlang code is concerned, our snippet is going to be a simple list of properties that we can interact with via the [proplists][] module. This keeps things really simple. To demonstrate what our snippet will look like in code, here is the function that takes a Title, and the two code blobs (called Left and Right) and returns a `snippet` instance. This code goes in a module called `csd_snippet` defined in `src/csd_snippet.erl`:

```
to_snippet(Title, Left, Right) ->
  {snippet,
    [
      {title, Title},
      {left, Left},
      {right, Right}
    ]
  }.
```

Note that the first part of the tuple is the atom `snippet` which I am using to identify the layout of the contents in the second part of the tuple. Down the track we'll have more collections of data in the system than just snippets, and we may want to make sure that the caller doesn't accidentally pass in a `user`, for example, to a function expecting a `snippet`.

It is important at this point to note that, down the track, I will include a `key` property in all of the data objects that are pushed to Riak. This property serves as the identifier for the object in Riak and is stored alongside the rest of the data so that it is easy to relate the in-memory instance back to the stored instance. This value, if not specified, will be inserted automatically when an item is saved via the API functions in `csd_core`. More on this later.

### Formatting Data for Storage in Riak ###

Riak is very flexible in that it will store whatever kind of information you give it. This is good because it means we can cater our data format to whatever needs we have.

In our case, the _easiest_ option would be to store our Erlang terms as binary using [term_to_binary][] as we wouldn't have to think about _anything_ else. We could easily read the data using [binary_to_term][]. Done.

This comes with a set of problems though. For example, if we wanted to [map/reduce][] using JavaScript we wouldn't find it easy to get the data into a format that we could use. Another example would be that the RESTful interface to Riak would be close to useless because **any** non-Erlang client would have to somehow get the data into a meaningful format to work with.

Instead of using binary and throwing Erlang terms straight into Riak, we're going to use [JSON][]. It's very easy to convert to and from JSON in many different languages, and it's very easy to read. We can also easily verify that the data is being stored correctly by querying Riak's RESTful interface directly using [cURL][] or a browser.

In order to store data in JSON format, we're going to enlist the help of [mochijson2][], a library that comes with [Mochiweb][] that makes it a _lot_ easier to deal with JSON than doing everything manually. Given that we're using Webmachine for the front-end (which itself relies on Mochiweb) we already have the dependency available.

Unfortunately we can't just throw our data straight at this module and have it do everything for us. `mochijson2` requires data to be in a certain format before it can encode it to JSON. When decoding _from_ JSON, it converts the data into the same format. Hence, we need the ability to convert our own data format to and from this intermediate data format so that `mochijson2` can deal with it.

We need two functions: `to_json()` and `from_json()`, and we shall define these in a helper module called `csd_json`. This module will live in `csd_core`:

```
-module(csd_json).
-export([from_json/1, from_json/2, to_json/1, to_json/2]).

to_json(PropList) ->
  to_json(PropList, fun(_) -> true end).

to_json(PropList, IsStrFun) ->
  list_to_binary(mochijson2:encode(from_proplist(PropList, IsStrFun))).

from_json(Json) ->
  from_json(Json, fun(_) -> true end).

from_json(Json, IsStrFun) ->
  to_proplist(mochijson2:decode(Json), IsStrFun).
```

You're probably wondering why each of these functions requires the `IsStrFun` parameter (if you're not, you're obviously an experienced Erlanger!). For those who don't know, strings in Erlang are actually lists of integers. This is fantastic as it makes it easy to manipulate strings as if they were lists, but it comes at a small price: it's not possible to determine the difference between a list of integers and a string.

Why is this important? `mochijson2` needs strings to be encoded as binaries, so we need a way to differentiate between integer lists and real strings. My original implementations of both the `to_json()` and `from_json()` functions attempted to figure out if certain fields were strings or not by looking at the content of the list. Not only was the code messy, but it wasn't foolproof. Instead, I made the decision to force the user to provide a callback function which will tell the JSON serialiser if the given property is a string or not. This callback takes a single parameter which is the name (in atom form) of the property and returns a boolean -- `true` indicates that the value is a string, `false` otherwise.

In some cases we might just be happy to encode/decode every single value as a string. Hence, there is an overload to both `to_json()` and `from_json()` which caters for this case. The rest of the code which implments the conversion is listed below. Don't feel that you need to understand the code below, as it's really not the goal of this post. The full source to this module is included in the source link specified at the end of this post.

```
from_proplist(List=[H|_], IsStrFun) when is_tuple(H) ->
  { struct, lists:map(fun(P) -> from_proplist(P, IsStrFun) end, List) };
from_proplist({PropName, ComplexProp=[H|_]}, IsStrFun) when is_tuple(H) ->
  { list_to_binary(atom_to_list(PropName)), from_proplist(ComplexProp, IsStrFun) };
from_proplist({PropName, PropVal}, IsStrFun) ->
  { list_to_binary(atom_to_list(PropName)), to_value(PropName, PropVal, IsStrFun) }.

to_proplist({struct, PropList}, IsStrFun) when is_list(PropList) ->
  lists:map(fun(P) -> to_proplist(P, IsStrFun) end, PropList);
to_proplist({PropName, ComplexProp={struct, _}}, IsStrFun) ->
  { list_to_atom(binary_to_list(PropName)), to_proplist(ComplexProp, IsStrFun) };
to_proplist({PropName, PropVal}, IsStrFun) ->
  PropAtom = list_to_atom(binary_to_list(PropName)),
  { PropAtom, from_value(PropAtom, PropVal, IsStrFun) }.

to_value(PropName, L=[H|_], IsStrFun) when is_list(L) and is_list(H) ->
  lists:map(fun(P) -> to_value(PropName, P, IsStrFun) end, L);
to_value(PropName, L, IsStrFun) when is_list(L) ->
  case IsStrFun(PropName) of
    true -> list_to_binary(L);
    _ -> lists:map(fun(V) -> to_value(PropName, V, IsStrFun) end, L)
  end;
to_value(_, V, _) ->
  V.

from_value(PropName, L, IsStrFun) when is_list(L) ->
  lists:map(fun(P) -> from_value(PropName, P, IsStrFun) end, L);
from_value(PropName, B, IsStrFun) when is_binary(B) ->
  case IsStrFun(PropName) of
    true -> binary_to_list(B);
    _ -> B
  end;
from_value(_, V, _) ->
  V.
```

We are now able to read and write data to and from JSON format. Now we need to use the Riak client to push that into our Riak cluster.

### Setting up the Riak client ###

Basho have done a great job of creating a protocol buffer-based client for use with Riak. The interface is really simple to use. Despite that, we shall create a module which will deal with this for us. This gives us a single point of abstraction of Riak and a place where we can add extra support for our own needs without spreading Riak-specific code all over the source base.

The first problem we need to resolve is: _what do we do with configuration?_

This was a question I initially didn't know how to answer. After a bit of deliberation and a chat with a [respected Erlang sifu][MononcQc] (who has a [fantastic Erlang tutorial site][LearnYouSomeErlang]) I decided to go with a module-based option.

We have our Riak cluster hidden behind the `haproxy` load balancer, and hence we have a single entry-point to connect to. If this entry-point changes, it changes for all of the clients, not just a single client. Therefore, I want the ability to manage a single set of connection information, but I want the ability to update it on the fly without having to restart the `csd_core` application. This is Erlang, after all, and modifying code and configuration on-the-fly is extremely easy. We shall abuse that.

We create a single module, `csd_riak_config.erl`, to contain our configuration which is referenced at start-up. It looks like this:

```
-module(csd_riak_config).
-export([connection_info/0]).

connection_info() ->
  { "127.0.0.1", 8080 }.
```

Pretty simple stuff. Let's use this functionality in our `gen_server`, and carry the configuration through from initialisation to all of the calls that will be made to the Riak server. This requires two simple modifications to the `csd_core_server` module:

```
start_link() ->
  ConnInfo = csd_riak_config:connection_info(),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [ConnInfo], []).

% ...

init([ConnInfo]) ->
  {ok, ConnInfo}.
```

Confiuration is now loaded and is being passed to all of our `gen_server` callbacks. Let's make use of it. `csd_snippet` is the entry point for all snippet-related information, and one of the things that we are going to want to be able to do is write a snippet to Riak. So let's create a code-path that can do that.

#### Writing Data to Riak ####

The first point of call for a client is the OTP interface. Let's create an API call and a call handler to support saving snippets in `csd_core_server`:

```

%% This is a simple function which invokes a call via the gen_sever
%% behaviour.
save_snippet(Snippet) ->
  gen_server:call(?SERVER, {save_snippet, Snippet}, infinity).

%% Handle the case where a caller wants to save a snippet to Riak. We
%% create a connection to Riak and pass that into the snippet handler
%% along with the snippet that needs to be saved. We return the newly
%% saved snippet.
handle_call({save_snippet, Snippet}, _From, ConnInfo) ->
  RiakPid = csd_riak:connect(ConnInfo),
  SavedSnippet = csd_snippet:save(RiakPid, Snippet),
  {reply, SavedSnippet, ConnInfo};
```

Of course, we will need to export the `save_snippet()` function if we want to be able to call it.

You'll notice that we're getting the connection information passed in as the state for the OTP call, and that we're using that to create a connection to Riak via the `csd_riak` module. We shall cover this module in just a minute, but hopefully the interface to this function should make it relatively self-explanatory.

You might be wondering "Why are you creating the Riak client connection here instead of letting the `csd_snippet:save()` function do it by itself. It's a good question. The reason I decided to create the connection as part of OTP call rather than in the data/helper modules is because down the track there will probably be a need to do multiple interactions with Riak in a single call. If we force each of the called modules, such as `csd_snippet`, to establish their own connections then we'd probably have _multiple connections to Riak being created during a single client request_. This isn't what I would like to see happen, so it made sense (in my view) to create the client connection once and reuse it across all modules that are invoked during the request.

With that out of the way, we need to implement the `save()` function in the `csd_snippet` module. Brace yourself:

<span class="filename"></span>
```
save(RiakPid, Snippet={snippet, SnippetData}) ->
  case proplists:get_value(key, SnippetData, undefined) of
    undefined ->
      Key = csd_riak:new_key(),
      NewSnippetData = [{key, Key} | SnippetData],
      RiakObj = csd_riak:create(?BUCKET, Key, to_json_internal(NewSnippetData)),
      ok = csd_riak:save(RiakPid, RiakObj),
      {snippet, NewSnippetData};
    ExistingKey ->
      RiakObj = csd_riak:fetch(RiakPid, ?BUCKET, ExistingKey),
      NewRiakObj = csd_riak:update(RiakObj, to_json_internal(SnippetData)),
      ok = csd_riak:save(RiakPid, NewRiakObj),
      Snippet
  end.
```

On the surface this looks a little complicated, but it's actually very simple. As mentioned earlier in the post, we use a `key` property to store the identifier of the object in Riak. This code supports this notion. It works as follows:

1. **Try to get the value of the `key` from the given list of properties.**
1. **If _not_ found ...**
    1. create a new key using the `new_key()` function in the `csd_riak` module (this will be covered shortly).
    1. Add the `key` to the list of properties for the snippet.
    1. Create a new instance of a Riak object (more on this later) which contains the details of the snippet data to be written, along with the target bucket name and the key of the snippet.
    1. Save the Riak object to the Riak cluster using the specified Riak client connection (Pid), and for now assume that it succeeds.
    1. Return the new set of snippet data with the snippet's key included.
1. **If found ...**
    1. Load the existing data from the Riak cluster into a Riak object.
    1. Update the Riak object with the new data values passed into the function.
    1. Save the Riak object _back_ to the Riak cluster using the specified Riak client connection (Pid), and for now assume that it succeeds.
    1. Return the snippet back to the caller as is.

It's fairly basic functionality which does enough to cater for our needs at this point. Through this one function, we can write new snippet instances to Riak, and we can update them too.

You'll also notice that another function is being called that hasn't been discussed: `to_snippet_internal()`. Rather than try to explain this, let's see the code as it's quite easy to follow:

```
%% exported functions
to_json({snippet, SnippetData}) ->
  to_json_internal(SnippetData).

from_json(SnippetJson) ->
  from_json_internal(SnippetJson).

%% helper functions used internally.
to_json_internal(SnippetData) ->
  csd_json:to_json(SnippetData, fun is_string/1).

from_json_internal(SnippetJson) ->
  {snippet, csd_json:from_json(SnippetJson, fun is_string/1)}.

is_string(title) -> true;
is_string(left) -> true;
is_string(right) -> true;
is_string(_) -> false.
```

As you can see, these are helper functions which call the `csd_json` functions to serialise/deserialise to/from JSON format. The `is_string()` function is the one that is used to tell the JSON functionality which properties are strings and which are not. At the moment, all properties defined on the snippet are string properties. Bear in mind that the `key` property, which is added automatically, is _not_ a string.

All that is left is to see how `csd_riak` deals with the underlying Riak connectivity. Prepare to be underwhelmed!

```
%% @spec connect(connection_info()) -> pid()
%% @doc Create a connection to the specified Riak cluster and
%%      return the Pid associated with the new connection.
connect({IP, Port}) ->
  {ok, RiakPid} = riakc_pb_socket:start_link(IP, Port),
  RiakPid.

%% @spec create(binary, binary, json) -> riakc_obj()
%% @doc Create a new instance of a riak object using the
%%      parameters given. The riak object can then be
%%      persisted to a Riak node/cluster. This overload
%%      assumes that the data passed in is JSON and sets
%%      the MIME type to "application/json" for you.
create(Bucket, Key, JsonData) ->
  create(Bucket, Key, JsonData, "application/json").

%% @spec create(binary, binary, term(), string) -> riakc_obj()
%% @doc Create a new instance of a riak object using the
%%      parameters given. The riak object can then be
%%      persisted to a Riak node/cluster. This overload
%%      takes arbitrary data and requires the user to
%%      specify the mime type of the data that is being
%%      stored.
create(Bucket, Key, Item, MimeType) ->
  RiakObj = riakc_obj:new(Bucket, Key, Item, MimeType),
  RiakObj.

%% @spec fetch(pid(), binary, binary) -> riakc_obj()
%% @doc Fetches a riakc object from a Riak node/cluster
%%      using the connection given.
fetch(RiakPid, Bucket, Key) ->
  RiakObj = riakc_pb_socket:get(RiakPid, Bucket, Key),
  RiakObj.

%% @spec update(riakc_obj(), term()) -> riakc_obj()
%% @doc Updates the stored value for a riakc object with
%%      the new one specified.
update(RiakObj, NewValue) ->
  NewRiakObj = riakc_obj:update_value(RiakObj, NewValue),
  NewRiakObj.

%% @spec get_value(riakc_obj()) -> term()
%% @doc Retrieves the stored value from within the riakc
%%      object.
get_value(RiakObj) ->
  Value = riakc_obj:get_value(RiakObj),
  Value.

%% @spec save(pid(), riakc_obj()) -> {ok, riakc_obj()} | {error | Reason}
%% @doc Saves the given riak object to the specified Riak node/cluster.
save(RiakPid, RiakObj) ->
  Result = riakc_pb_socket:put(RiakPid, RiakObj),
  Result.

%% @spec new_key() -> key()
%% @doc Generate an close-to-unique key that can be used to identify
%%      an object in riak. This implementation is blatantly borrowed
%%      (purloined) from the wriaki source (thanks basho!)
new_key() ->
  { {Yr, Mo, Dy}, {Hr, Mn, Sc} } = erlang:universaltime(),
  {_, _, Now} = now(),
  new_key([Yr, Mo, Dy, Hr, Mn, Sc, node(), Now]).

%% @spec new_key(list()) -> key()
%% @doc Generate an close-to-unique key that can be used to identify
%%      an object in riak using the given list parameter as the stuff
%%      to hash.
new_key(List) ->
  Hash = erlang:phash2(List),
  base64:encode(<<Hash:32>>).
```

Hopefully the code in this module is fairly self-explanatory. It's a very simple API to follow which made it very easy to build. So with this in place, let's fire up the application, create a new snippet and see if it lands in the Riak store:

    oj@spawn-link  ~/blog/csd $ make webstart

       ... snip ...

    =PROGRESS REPORT==== 4-Apr-2011::22:54:55 ===
             application: csd_web
              started_at: nonode@nohost

    1> Snippet = csd_snippet:to_snippet(
    1> "Super composition!",
    1> "(.^) = (.) . (.)",
    1> "(.^) = fmap `fmap` fmap").
    {snippet,[{title,"Super composition!"},
              {left,"(.^) = (.) . (.)"},
              {right,"(.^) = fmap `fmap` fmap"}]}
    2> SavedSnippet = csd_core_server:save_snippet(Snippet).

    PROGRESS REPORT==== 4-Apr-2011::22:57:13 ===
              supervisor: {local,inet_gethost_native_sup}
                 started: [{pid,<0.103.0>},{mfa,{inet_gethost_native,init,[[]]}}]

    =PROGRESS REPORT==== 4-Apr-2011::22:57:13 ===
              supervisor: {local,kernel_safe_sup}
                 started: [{pid,<0.102.0>},
                           {name,inet_gethost_native_sup},
                           {mfargs,{inet_gethost_native,start_link,[]}},
                           {restart_type,temporary},
                           {shutdown,1000},
                           {child_type,worker}]
    {snippet,[{key,<<"B41kUQ==">>},
              {title,"Super composition!"},
              {left,"(.^) = (.) . (.)"},
              {right,"(.^) = fmap `fmap` fmap"}]}

As you can see from the above script dump, a new `key` was generated for us and stored alongside the snippet (it's highlighted in bold). Verifying that the data has persisted is simple. We can hit any of the Riak nodes via its web interface. Let's take a look at **http://localhost:8091/riak/snippet/B41kUQ==** (your URL will have a different key):

<img src="/uploads/2010/10/localhost-verify-write.png" />

Great stuff! For more detail, let's see what cURL has to say:

    oj@spawn-link ~/blog/csd/ $ curl http://localhost:8091/riak/snippet/B41kUQ== -v
    * About to connect() to localhost port 8091 (#0)
    *   Trying ::1... Connection refused
    *   Trying 127.0.0.1... connected
    * Connected to localhost (127.0.0.1) port 8091 (#0)
    > GET /riak/snippet/B41kUQ== HTTP/1.1
    > User-Agent: curl/7.21.0 (x86_64-pc-linux-gnu) libcurl/7.21.0 OpenSSL/0.9.8o zlib/1.2.3.4 libidn/1.18
    > Host: localhost:8091
    > Accept: */*
    > 
    < HTTP/1.1 200 OK
    < X-Riak-Vclock: a85hYGBgzGDKBVIsjOy7jmcwJTLmsTJ8tuc7zpcFAA==
    < Vary: Accept-Encoding
    < Server: MochiWeb/1.1 WebMachine/1.7.3 (participate in the frantic)
    < Link: </riak/snippet>; rel="up"
    < Last-Modified: Mon, 04 Apr 2011 13:13:23 GMT
    < ETag: "6fw7c5v4IPAsf4B5hMHybc"
    < Date: Mon, 04 Apr 2011 13:13:36 GMT
    < Content-Type: application/json
    < Content-Length: 107
    < 
    * Connection #0 to host localhost left intact
    * Closing connection #0
    {"key":"B41kUQ==","title":"Super composition!","left":"(.^) = (.) . (.)","right":"(.^) = fmap `fmap` fmap"}

As you can see, it has not only serialised to JSON properly, but the MIME type has been set correctly as well.

This is all well and good, but we need our code to be able to read from Riak as well. That's up next.

#### Reading Data from Riak ####

We've already covered off what happens at the bottom level when reading data from Riak (see the above code snippet for more info). To enable this functionality at the top level, we simply need to create a `gen_server` call, handle it appropriately and expose a function in the `csd_snippet` module. Let's start at the top level:

```
%% OTP API function to get a snippet based on the key
get_snippet(SnippetKey) ->
  gen_server:call(?SERVER, {get_snippet, SnippetKey}, infinity).

%% handle the call and call the functionality from csd_snippet
handle_call({get_snippet, SnippetKey}, _From, ConnInfo) ->
  RiakPid = csd_riak:connect(ConnInfo),
  Snippet = csd_snippet:fetch(RiakPid, SnippetKey),
  {reply, Snippet, ConnInfo};
```

This code is a bit of a no-brainer. It's very similar to the writing code, but just a bit simpler. Let's see what the `csd_snippet:fetch()` function looks like:

```
fetch(RiakPid, Key) ->
  {ok, RiakObj} = csd_riak:fetch(RiakPid, ?BUCKET, Key),
  SnippetJson = csd_riak:get_value(RiakObj),
  from_json_internal(SnippetJson).
```

This code just pulls a Riak object out of the back-end, extracts is value and deserialises it from JSON to our Erlang `proplist`. Very simple stuff.

We should be able to build this and, via the Erlang console, verify that it functions:

    3> Reloading csd_core_server ... ok.
    3> csd_core_server:get_snippet(<<"B41kUQ==">>).
    {snippet,[{key,<<"B41kUQ==">>},
              {title,"Super composition!"},
              {left,"(.^) = (.) . (.)"},
              {right,"(.^) = fmap `fmap` fmap"}]}

Works like a charm. Now, for the icing on the cake, let's get this rendering in a very simple template in our browser.

### End to End ###

In order to gain access to our data in Riak from the web we need to create a new resource. This resource will respond to any URI of the form `/snippet/<key>`. We shall call this resource `csd_web_snippet_resource` and we'll be putting this in our web application. It looks like this:

```
%% @author OJ Reeves <oj@buffered.io>
%% @copyright 2010 OJ Reeves
%% @doc Webmachine resource that handles snippet-related actions

-module(csd_web_snippet_resource).
-author('OJ Reeves <oj@buffered.io>').

-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
  PathInfo = wrq:path_info(ReqData),
  {ok, SnippetKey} = dict:find(key, PathInfo),
  {snippet, SnippetData} = csd_core_server:get_snippet(list_to_binary(SnippetKey)),
  {ok, Content} = snippet_dtl:render(SnippetData),
  {Content, ReqData, State}.
```

As you can see, this code calls through to the `csd_core_server` to extract the data from the back-end. The value that is used as a key for the snippet is one that is pulled from the URI via Webmachine's `wrq:path_info()` function. This function extracts values from the URI based on the rules in `dispatch.conf` and provides a [dict][] which can be used to lookup the values.

The code also uses a new ErlyDTL template called `snippet`. We'd best add that to the `templates` folder:

```
<!-- TODO : get the templating engine to stop ripping out the inline template code -->
<html>
  <body>
    <h1>Snippet View</h1>
    <h2>{{ "{{ title "}} }}</h2>
    <p>Left: {{ "{{ left "}} }}</p>
    <p>Right: {{ "{{ right "}} }}</p>
  </body>
</html>
```

Finally, we just need to adjust `dispatch.conf` to include the new route handler so that our code gets called:

```
%%-*- mode: erlang -*-
{[], csd_web_resource, []}.
{["snippet", key], csd_web_snippet_resource, []}.
```

Note how `key` is specified alongside `"snippet"`. This means that the path following `snippet/` in the URI will be associated with the `key` atom in the `dict` generated by `wrq:path_info()`.

We're ready to rock. Rebuild, then hit the right URL, **http://localhost/snippet/B41kUQ==** (again, your key will be different), and you should get the following:

<img src="/uploads/2010/10/webmachine-to-riak.png" />

Wrapping up
-----------

Thanks for sticking with me! As you can see there is a little bit of ground-work required if you're interested in producing some form of structure that you can reuse all over your application, but the effort is definitely worth it. Now we have something in place which we can use to store arbitrarily complex `proplists` into Riak in JSON format, we have the ability to talk to Riak (read and write), and we have a proper application structure in place which we can build on.

Please note that the mechanism implemented in this post is quite simple and doesn't cover all cases that will be required before the application is complete. In future posts, this implementation will change to support more of those cases, such as dealing with concurrent updates, handling versions, etc.

Many thanks to those people who took the time out of their busy schedules to review my post before I shared it with the world. Those people shall remain nameless to protect them from any mistakes made in this post (which are solely my own).

As always, comments and feedback is welcomed and greatly appreciated. As are suggestions on improvements, pitfalls and blatant mistakes :)

**Note:** The code for Part 3 (this post) can be found on [Github][Part3Code].

Other parts in this series: [Part 1][], [Part 2][], [Part 4][], [Part 5][]

  [Part 1]: /posts/webmachine-erlydtl-and-riak-part-1/ "Wembachine, ErlyDTL and Riak - Part 1"
  [Part 2]: /posts/webmachine-erlydtl-and-riak-part-2/ "Webmachine, ErlyDTL and Riak - Part 2"
  [Part 4]: /posts/webmachine-erlydtl-and-riak-part-4/ "Webmachine, ErlyDTL and Riak - Part 4"
  [Part 5]: /posts/webmachine-erlydtl-and-riak-part-5/ "Webmachine, ErlyDTL and Riak - Part 5"
  [Erlang]: http://erlang.org/ "Erlang"
  [Webmachine]: http://www.basho.com/developers.html#Webmachine "Webmachine"
  [JSON]: http://json.org/ "JavaScript Object Notation"
  [Part2Code]: https://github.com/OJ/csd/tree/Part2-20110403 "Source code for Part 2"
  [Riak]: http://www.basho.com/developers.html#Riak "Riak"
  [ErlyDTL]: http://github.com/evanmiller/erlydtl "ErlyDTL"
  [SJMackenzie]: http://twitter.com/sj_mackenzie "Stewart Mackenzie on Twitter"
  [MattErbs]: http://twitter.com/MatthewErbs "Matt Erbs on Twitter"
  [RiakIRC]: irc://irc.freenode.com/riak "Riak IRC on Freenode"
  [Riak Recap]: http://lists.basho.com/pipermail/riak-users_lists.basho.com/2010-September/001984.html "Riak Recap"
  [Mark]: http://twitter.com/pharkmillups "Mark Phillips on Twitter"
  [Andrew]: http://twitter.com/andrewtj "AndrewTJ on Twitter"
  [RebarAppsFolder]: http://lists.basho.com/pipermail/rebar_lists.basho.com/2010-October/000246.html "Configuring the Rebar apps folder on Basho list"
  [localhost]: http://localhost/ "localhost web app"
  [Rebar]: http://www.basho.com/developers.html#Rebar "Rebar"
  [mochijson2]: https://github.com/mochi/mochiweb/blob/master/src/mochijson2.erl "Mochiweb's json module"
  [Mochiweb]: https://github.com/mochi/mochiweb "Mochiweb"
  [OTP]: http://en.wikipedia.org/wiki/Open_Telecom_Platform "Open Telecom Platform"
  [gen_server]: http://www.erlang.org/doc/design_principles/gen_server_concepts.html "gen_server behaviour"
  [supervisor]: http://www.erlang.org/doc/design_principles/sup_princ.html "supervisor behaviour"
  [proplists]: http://www.erlang.org/doc/man/proplists.html "proplists"
  [term_to_binary]: http://www.erlang.org/doc/man/erlang.html#term_to_binary-1 "term_to_binary"
  [binary_to_term]: http://www.erlang.org/doc/man/erlang.html#binary_to_term-1 "binary_to_term"
  [map/reduce]: http://en.wikipedia.org/wiki/MapReduce "map/reduce"
  [cURL]: http://curl.haxx.se/ "cURL homepage"
  [MononcQc]: http://twitter.com/mononcqc "Ferd T-H on Twitter"
  [LearnYouSomeErlang]: http://learnyousomeerlang.com/ "Learng you some erlang"
  [dict]: http://www.erlang.org/doc/man/dict.html "Erlang dict"
  [Part3Code]: https://github.com/OJ/csd/tree/Part3-20110405 "Source code for Part 3"
