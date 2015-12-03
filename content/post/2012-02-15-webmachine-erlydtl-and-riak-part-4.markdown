---
categories:
- Riak
- Functional Programming
- HOWTO
- Erlang
- Webmachine
- OAuth
- Twitter
- ErlyDTL
comments: true
date: 2012-02-15T00:00:00Z
tags:
- Riak
- Functional Programming
- HOWTO
- Erlang
- Webmachine
- OAuth
- Twitter
- ErlyDTL
title: Webmachine, ErlyDTL and Riak - Part 4
---

{% img left /uploads/2010/09/riak-logo.png 'Riak Logo' %}For those of you who are new to the series, you may want to check out [Part 1][], [Part 2][] and [Part 3][] before reading this post. It will help give you some context as well as introduce you to some of the jargon and technology that I'm using. If you've already read then, or don't want to, then please read on!

Upon finishing [Part 3][] of the series we were finally able to read data from [Riak][] and see it appear in our web page. This was the first stage in seeing a full end-to-end web application functioning. Of course there is still a great deal to do!

<!--more-->

Agenda
------

In this post we're going to hit a few points of pain:

1. Another slight refactor! We need to manage Riak connections in a smarter way, so we'll do that first.
1. We'll be dealing with more configuration so we'll change the way our application deals with configuration so that it's all in the one spot and a little easier to manage.
1. Add the ability for users to sign in. To keep this simple and avoid the need for users to manage yet another login, we're going to use [OAuth][] and let people sign in with their [Twitter][] accounts.
1. Store a cookie in the user's browser which contains identifying information and an encrypted set of OAuth tokens.

There's little Riak-specific work going on this post as we're focusing on front-end user management. Other than a bit of refactoring the Riak code remains the same as in Part 3. In Part 5 (coming soon) we'll be writing snippets to Riak and associating them to users who have logged into the application via Twitter.

**NOTE**: I'll no longer be using `localhost` in URLs and will instead be using the loopback address, `127.0.0.1`. The main reason is because we'll be interacting with Twitter which requires a "proper" address to be used when setting up. A secondary reason is the use of cookies. If I accidentally leave `localhost` somewhere in the post (or in the images) please let me know.

Again, be warned, this post is a bit of a whopper! So get yourself a drink and get comfortable. Here we go...

Another Slight Refactor
-----------------------

Now that we're at the stage where Riak is going to get used more often we need to do a better job of handling and managing the connections to the cluster. Ideally we should pool a bunch of connections and reuse them across different requests. This reduces the overhead of creating and destroying connections all the time. Initially we're going to make use of Seth's [Pooler][] application (with a slight modification) to handle the pooling of Riak connections for us.

### Fixing HAProxy ###

So now that we have a plan to pool connections, the first thing we need to fix is our load-balancer's configuration. At the moment we have configured [HAProxy][] with the following settings:

```
# now set the default settings for each sub-section
defaults
  .
  .
  # specify some timeouts (all in milliseconds)
  timeout connect 5000
  timeout client 50000
  timeout server 50000
  .
  .
```

As you can see we've forced the timeout of connections which means that every connection that is made to the proxy will be killed off when it has been inactive for a long enough period of time. If you were paying attention to the output in the application console window you'd have seen something like this appear after making a request:

    =ERROR REPORT==== 13-Aug-2011::20:52:01 ===
    ** Generic server <0.99.0> terminating 
    ** Last message in was {tcp_closed,#Port<0.2266>}
    ** When Server state == {state,"127.0.0.1",8080,false,false,undefined,
                                   undefined,
                                   {[],[]},
                                   1,[],infinity,100}
    ** Reason for termination == 
    ** disconnected

    =CRASH REPORT==== 13-Aug-2011::20:52:01 ===
      crasher:
        initial call: riakc_pb_socket:init/1
        pid: <0.99.0>
        registered_name: []
        exception exit: disconnected
          in function  gen_server:terminate/6
        ancestors: [csd_core_server,csd_core_sup,<0.52.0>]
        messages: []
        links: [<0.54.0>]
        dictionary: []
        trap_exit: false
        status: running
        heap_size: 377
        stack_size: 24
        reductions: 911
      neighbours:
        neighbour: [{pid,<0.54.0>},
                      {registered_name,csd_core_server},
                      {initial_call,{csd_core_server,init,['Argument__1']}},
                      {current_function,{gen_server,loop,6}},
                      {ancestors,[csd_core_sup,<0.52.0>]},
                      {messages,[]},
                      {links,[<0.53.0>,<0.99.0>]},
                      {dictionary,[]},
                      {trap_exit,false},
                      {status,waiting},
                      {heap_size,987},
                      {stack_size,9},
                      {reductions,370}]

    =SUPERVISOR REPORT==== 13-Aug-2011::20:52:01 ===
         Supervisor: {local,csd_core_sup}
         Context:    child_terminated
         Reason:     disconnected
         Offender:   [{pid,<0.54.0>},
                      {name,csd_core_server},
                      {mfargs,{csd_core_server,start_link,[]}},
                      {restart_type,permanent},
                      {shutdown,5000},
                      {child_type,worker}]


    =PROGRESS REPORT==== 13-Aug-2011::20:52:01 ===
              supervisor: {local,csd_core_sup}
                 started: [{pid,<0.104.0>},
                           {name,csd_core_server},
                           {mfargs,{csd_core_server,start_link,[]}},
                           {restart_type,permanent},
                           {shutdown,5000},
                           {child_type,worker}]


This is paired up with the following output from the HAProxy console:

    00000010:riaks.srvcls[0009:000a]
    00000010:riaks.clicls[0009:000a]
    00000010:riaks.closed[0009:000a]
    0000000e:webmachines.srvcls[0006:0007]
    0000000e:webmachines.clicls[0006:0007]
    0000000e:webmachines.closed[0006:0007]

These logs from the console clearly indicate that HAProxy is doing exactly what we've told it to do. It's killing off the connections after a period of time.

For a connection pool this is not a good idea. Therefore we need to modify this configuration so that it doesn't kill off connections. Thankfully this is a very simple thing to do! We delete the lines that force `client` and `server` timeouts (I'm commenting the lines out to make it obvious which ones you need to remove):

```
# now set the default settings for each sub-section
defaults
  .
  .
  # specify some timeouts (all in milliseconds)
  timeout connect 5000
  #timeout client 50000
  #timeout server 50000
  .
  .
```

After making this change to the configuration, HAProxy will no longer kill off the connections. Therefore it's up to us to manage them.

### Connection Pooling ###

Given that it is _not_ one of the goals of this series to demonstrate how to create a connection pooling application in Erlang, we're going to use an application that's already out there to do it for us. This application is called [Pooler][]. Out of the box this application does Erlang process pooling, and given that our Riak connections are each Erlang processes, this suits us perfectly.

One thing that I didn't like about the interface to Pooler was that it relied on the caller managing the lifetime of the connection. As a result, I made a small change to the interface in my own [fork][PoolerFork] which I think helps keep things a little cleaner. This application will be making use of this fork.

First up, we need to add another dependency in our `rebar.config` file which will pull this application in from Github at a dependency.

```
%%-*- mode: erlang -*-
{deps,
  [
    {mochiweb, ".*", {git, "git://github.com/mochi/mochiweb", "HEAD"}},
    {riakc, ".*", {git, "git://github.com/basho/riak-erlang-client", "HEAD"}},
    {pooler, ".*", {git, "git://github.com/OJ/pooler", "HEAD"}}
  ]
}.
```

Build the application so that the dependency is pulled and built:

```
oj@hitchens ~/code/csd $ make

   ... snip ... 

Pulling pooler from {git,"git://github.com/OJ/pooler","HEAD"}
Cloning into pooler...
==> pooler (get-deps)

   ... snip ... 

==> pooler (compile)
Compiled src/pooler_app.erl
Compiled src/pooler_pooled_worker_sup.erl
Compiled src/pooler_pool_sup.erl
Compiled src/pooler_sup.erl
Compiled src/pooler.erl

   ... snip ... 
```

Next we need to take the scalpel to `csd_core`. When we first created this application, it was intended to manage all of the interaction with Riak and to manage the intricacies of dealing with snippets and other objects without exposing Riak's inner workings to the `csd_web` application. To do this we put a [gen_server][] in place, called `csd_core_server`, which handled the incoming requests. It internally established connections to Riak and used them without destroying them.

For now, we'll be keeping this `gen_server` in place but we're going to make some modifications to it:

1. We'll start and stop `pooler` when our `csd_core` application starts and stops.
1. We'll change the way configuration is managed and add the configuration for `pooler`.
1. We'll be removing the code that establishes the connections.
1. We'll pass the calls through to Riak using the new `pooler` application.

Let's get to it.

#### Starting and Stopping Pooler ####

Given that we're using `pooler` the first thing we need to do is make sure that it loads and runs when `csd_core` fires up. To do this, we need to modify `csd_core.erl` so that it looks like this:

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
    start_common(),
    csd_core_sup:start_link().

%% @spec start() -> ok
%% @doc Start the csd_core server.
start() ->
    start_common(),
    application:start(csd_core).

%% @spec stop() -> ok
%% @doc Stop the csd_core server.
stop() ->
    Res = application:stop(csd_core),
    application:stop(pooler),
    application:stop(crypto),
    Res.

%% @private
start_common() ->
    ensure_started(crypto),
    ensure_started(pooler).
```

This code will start and stop the `pooler` application along with our application. Exactly what we need!

#### Fixing Configuration ####

Our rudimentary configuration module, `csd_riak_config.erl`, is now obsolete. We're going to remove it and replace it with something a little more complicated which will not only make it easier to handle configuration using Erlang's built-in [configuration][] handling, but we'll add some code which will make it easier to access configuration both in development _and_ once the application has been deployed.

Let's start by creating a new file:

```
[
  {pooler, [
      {pools, [
          [
            {name, "haproxy"},
            {max_count, 30},
            {init_count, 5},
            {start_mfa, {riakc_pb_socket, start_link, ["127.0.0.1", 8080]}}
          ]
        ]}
    ]}
].
```

`pooler` is smart enough to pool connections across multiple nodes. This is quite a nifty feature, but not one that we're making use of because we have HAProxy in place. Therefore, the configuration above is telling Pooler to use just one single node/pool (ie. the proxy), to create 5 connections and to allow up to 30 to be created if required.

The last parameter in the configuration, `start_mfa`, tells `pooler` which module, function and arguments to invoke to create the Erlang process from. In our case we want it to create a pool of Riak client connections, hence why we've specified the `start_link` function in the `riakc_pb_socket` module.

Next we modify our `Makefile` so that when we invoke `make webstart` the configuration is properly included:

```
.PHONY: deps

REBAR=`which rebar || ./rebar`

all: deps compile

compile:
    @$(REBAR) compile

app:
    @$(REBAR) compile skip_deps=true

deps:
    @$(REBAR) get-deps

clean:
    @$(REBAR) clean

distclean: clean
    @$(REBAR) delete-deps

test: app
    @$(REBAR) eunit skip_deps=true

webstart: app
    exec erl -pa $(PWD)/apps/*/ebin -pa $(PWD)/deps/*/ebin -boot start_sasl -config $(PWD)/apps/csd_core/priv/app.config -s reloader -s csd_core -s csd_web

proxystart:
    @haproxy -f dev.haproxy.conf
```

At this point we are able to build and run the application just as we were before. The first thing you'll notice is that the HAProxy console immediately registers 5 new connections:

```
0000004:dbcluster.accept(0005)=0006 from [127.0.0.1:34536]
00000005:dbcluster.accept(0005)=0008 from [127.0.0.1:58770]
00000006:dbcluster.accept(0005)=000a from [127.0.0.1:44734]
00000007:dbcluster.accept(0005)=000c from [127.0.0.1:33874]
00000008:dbcluster.accept(0005)=000e from [127.0.0.1:35815]
```

This is evidence that `pooler` is doing its job and starting with 5 connections. Now that we have this in place, let's get rid of the old configuration:

```
oj@hitchens ~/code/csd $ rm apps/csd_core/src/csd_riak_config.erl 
```

That was easy! We now need to remove any references to this module, thankfully the only module that used was `csd_core_server.erl`, and that's the one we're going to fix up now. After removing references to the configuration, removing connection creation and replacing it with calls to `pooler`, `csd_core_server` now looks like this:

```
-module(csd_core_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, get_snippet/1, save_snippet/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

save_snippet(Snippet) ->
  gen_server:call(?SERVER, {save_snippet, Snippet}, infinity).

get_snippet(SnippetKey) ->
  gen_server:call(?SERVER, {get_snippet, SnippetKey}, infinity).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
  {ok, undefined}.

handle_call({save_snippet, Snippet}, _From, State) ->
  SavedSnippet = pooler:use_member(fun(RiakPid) -> csd_snippet:save(RiakPid, Snippet) end),
  {reply, SavedSnippet, State};

handle_call({get_snippet, SnippetKey}, _From, State) ->
  Snippet = pooler:use_member(fun(RiakPid) -> csd_snippet:fetch(RiakPid, SnippetKey) end),
  {reply, Snippet, State};

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
```

Here you can see we're making use of the [pooler:use_member][] function to easily wrap up the management of the connection's usage lifetime. All traces of the old configuration are gone. We can now rebuild the application using `make`, fire it up using `make webstart` and hit the [same page](http://127.0.0.1/snippet/B41kUQ==) as before resulting in the same content appearing on screen.

We have now successfully removed the old configuration and connection handling code, and we've replaced it with `pooler` to handle a pool of connections to the Riak proxy. The last part of our refactor is around configuration for the front-end web application.

Rewiring Configuration
----------------------

Our configuration is going to get more complicated, so to make sure that we're able to better handle and manage it we're going to set up a similar structure to what we had set up in the `csd_core` application (in the previous section). The first thing we're going to change is the way that the **Webmachine** routes are loaded. Right now, they're stored in `apps/tr_web/priv/dispatch.conf`. This configuration belongs alongside others, so we'll move that to an `app.config` file and re-jig the code to load it from there.

First up, rename the file:

    oj@air ~/code/csd/apps/csd_web/priv $ mv dispatch.conf app.config

Now let's edit it so that it takes the appropriate format:

```
%%-*- mode: erlang -*-
[
  {sasl,
    [
      {sasl_error_logger, {file, "log/sasl-error.log"}},
      {errlog_type, error},
      {error_logger_mf_dir, "log/sasl"},      % Log directory
      {error_logger_mf_maxbytes, 10485760},   % 10 MB max file size
      {error_logger_mf_maxfiles, 5}           % 5 files max
    ]
  },
  {csd_web,
    [
      {web,
        [
          {ip, "0.0.0.0"},
          {port, 8000},
          {log_dir, "priv/log"},
          {dispatch,
            [
              {[], csd_web_resource, []},
              {["snippet", key], csd_web_snippet_resource, []}
            ]
          }
        ]
      }
    ]
  }
].
```

A few things to note here:

1. I've included the `sasl` configuration for later tweaking.
1. the `csd_web` section is named that way so that it is matches the application name. This makes the auto-wiring work.
1. The Webmachine configuration for application is now in a subsection called `web`. Inside this section is the original `dispatch` that we had in our old `dispatch.conf`. This configuration sections takes the _exact_ form that Webmachine expects when we start its process in our supervisor.

At this point we need to go and fiddle with the way Webmachine loads its configuration so that it picks up these details. We'll start by defining a helper which will make it easy to get access to configuration for the `csd_web` application.

```
-module(conf).

-export([get_section/1, get_section/2]).
-export([get_val/2, get_val/3]).

get_section(Name) ->
  get_section(Name, undefined).

get_section(Name, Default) ->
  case application:get_env(csd_web, Name) of
    {ok, V} ->
      V;
    _ ->
      Default
  end.

get_val(SectionName, Name) ->
  get_val(SectionName, Name, undefined).

get_val(SectionName, Name, Default) ->
  case application:get_env(csd_web, SectionName) of
    {ok, Section} ->
      proplists:get_value(Name, Section, Default);
    _ ->
      Default
  end.
```

Configuration helpers are now in place, let's fix the Webmachine loader in `csd_web_sup.erl`.

```
% ... snip ... %
%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
  WebConfig = conf:get_section(web),
  Web = {webmachine_mochiweb,
    {webmachine_mochiweb, start, [WebConfig]},
    permanent, 5000, worker, dynamic},
  Processes = [Web],
  {ok, { {one_for_one, 10, 10}, Processes} }.
% ... snip ... %
```

This little snippet delegates the responsibility of all Webmachine-related stuff to the `app.config` file. Let's include this in our `Makefile` when we start our application.

```
webstart: app
	exec erl -pa $(PWD)/apps/*/ebin -pa $(PWD)/deps/*/ebin -boot start_sasl -config $(PWD)/apps/csd_web/priv/app.config -config $(PWD)/apps/csd_core/priv/app.config -s reloader -s csd_core -s csd_web
```

All we've done here is add another `-config` parameter and pointed it at the new `app.config` file in the `csd_web/src` folder. Fire up the application and it _should_ behave exactly as it did before.

Now that we have our configuration tweaked we have finalised the last of the refactoring tasks (at least for now). It's now time to start designing our user login functionality.

Handling User Logins
--------------------

Handling logins isn't necessarily as simple as it looks. Remember, [Webmachine][] is not a Web application framework, it's a feature-rich tool which helps us build well-behaving RESTful HTTP applications. The idea of a "session" is a (leaky) abstraction that web developers have added to web applications to aid in preventing users from having to manually sign in each time they want to access a resource. This abstraction tends to be handled through cookies.

We'll be doing the same, but given that we don't have anything in place at all we're going to have to come up with our own method for handling authentication of the user via cookies.

Bearing in mind that we'll be making use of Twitter, via OAuth, to deal with the process of authentication, the login process will consist of the following steps:

1. The user clicks a "login via Twitter" button.
1. The server handles the request and negotiates a [request token][] with Twitter using OAuth.
1. The application redirects the user to Twitter on a special URL which contains OAuth request information.
1. The user is asked to sign in to Twitter, if they haven't already during the course of their browser session.
1. Twitter then confirms that the user does intend to sign-in to Code Smackdown using their Twitter credentials, and redirects the user back to the application.
1. If the user approves the process, the application is handed a verification token which is then used to generate an OAuth [access token][] with Twitter. This access token is what is used to allow the user to easily sign in to the application from this point onward.

Prepare yourself, you're about to learn how to do OAuth in Erlang! But before we can do that, we need to register our application with Twitter.

### Creating a new Twitter Application ###

Start by browsing to the [Twitter application registration page][TwitterNewApp] and signing in with your Twitter account credentials. You'll be taken to a page where you can enter the details of the application. Set the **Callback URL** to `http://127.0.0.1:4000/oauth/callback` for now. This points the Twitter redirect traffic back to localhost which will make things easy during development. When it comes time to deploy the application to production you can change this to the proper callback address.

![Creating an application in Twitter][ImgTwitterAppCreate]

Once you've filled out the details you'll being presented with a standard set of OAuth-related bits which we'll be using down the track. I'll of course be using my own registered application name (Code Smackdown) along with the keys. Given these keys are specific to my application and should be kept secret I will not be making them part of the source (sorry).

Once you're registered, we're ready to take the OAuth configuration information from Twitter and plug it into our own configuration. Re-open `csd_web/priv/app.config` and create a new section called `twitter` under the `csd_web` section and add the following

```
% ... snip ... %
  {csd_web,
    [
      % ... snip ... %
      {twitter,
        [
          {consumer_key, "< your application's key goes here >"},
          {consumer_secret, "< your application's secret goes here >"},
          {request_token_url, "https://twitter.com/oauth/request_token"},
          {access_token_url, "https://twitter.com/oauth/access_token"},
          {authenticate_url, "https://twitter.com/oauth/authenticate"},
          {current_user_info_url, "https://twitter.com/account/verify_credentials.json"}
        ]
      }
    ]
  }
% ... snip ... %
```

The first two values come straight from Twitter and would have been given to you upon registering your application. The rest are URLs that we'll be using later on when doing the OAuth handshake.

Now that we've got our configuration locked in we can get started on managing the requests. For this we need to understand how OAuth actually works.

A deep-dive into the ins and outs of OAuth is beyond the scope of this article. I recommend having a read of [this presentation on OAuth][OAuthPresso] which gives a good overview. The rest of this article will fill the gaps as to how it all works.

### Implementing OAuth ###

Using OAuth requires us to invoke HTTP requests to Twitter. We could go through the pain of doing this manually, but instead we're going to use another Open Source utility which has the ability to handle this for us.

[erlang-oauth][] is an Erlang application which makes it easy to deal with OAuth requests and is ideal for what we need to do. Given that it will be a dependency on our application we need it to work nicely with rebar. Out of the box this isn't the case, so I have made a [fork][erlang-oauth-fork] with a topic branch that has rebar-friendliness in it. We'll use this fork and branch in our application.

```
%%-*- mode: erlang -*-
{deps,
  [
    {oauth, ".*", {git, "git://github.com/OJ/erlang-oauth", {branch, "rebarise"}}},
    {webmachine, ".*", {git, "git://github.com/basho/webmachine", "HEAD"}},
    {erlydtl, ".*", {git, "git://github.com/OJ/erlydtl.git", "HEAD"}}
  ]
}.
```

The `erlang-oauth` application requires `ssl` and `public_key` applications to be running for it to function properly, so we need to kick those applications off during start-up. We can do that by editing `csd_web.erl` like so:

```

%% @author OJ Reeves <oj@buffered.io>
%% @copyright 2012 OJ Reeves.

%% @doc csd_web startup code

-module(csd_web).
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
  start_common(),
  csd_web_sup:start_link().

%% @spec start() -> ok
%% @doc Start the csd_web server.
start() ->
  start_common(),
  application:start(csd_web).

%% @spec stop() -> ok
%% @doc Stop the csd_web server.
stop() ->
  Res = application:stop(csd_web),
  application:stop(webmachine),
  application:stop(mochiweb),
  application:stop(public_key), % stop new dependency
  application:stop(ssl),        % stop new dependency
  application:stop(crypto),
  application:stop(inets),
  Res.

start_common() ->
  ensure_started(inets),
  ensure_started(crypto),
  ensure_started(public_key), % start new dependency
  ensure_started(ssl),        % start new dependency
  ensure_started(mochiweb),
  application:set_env(webmachine, webmachine_logger_module, webmachine_logger),
  ensure_started(webmachine),
  ok.
```

Interacting with Twitter now becomes quite simple. To handle talking to Twitter we'll create a new module, called `twitter.erl`, that does the dirty work. Let's take a look at the code then we'll walk through it.

```

%% @author OJ Reeves <oj@buffered.io>
%% @copyright 2012 OJ Reeves

-module(twitter).

-author('OJ Reeves <oj@buffered.io>').

-export([request_access/0, verify_access/3, get_current_user_info/2]).

request_access() ->
  TwitterConf = conf:get_section(twitter),
  RequestTokenUrl = proplists:get_value(request_token_url, TwitterConf),
  {ok, RequestResponse} = oauth:get(RequestTokenUrl, [], consumer(TwitterConf)),
  RequestParams = oauth:params_decode(RequestResponse),
  RequestToken = oauth:token(RequestParams),
  AuthenticateUrl = proplists:get_value(authenticate_url, TwitterConf),
  {ok, oauth:uri(AuthenticateUrl, [{"oauth_token", RequestToken}])}.

verify_access(RequestToken, RequestTokenSecret, Verifier) ->
  TwitterConf = conf:get_section(twitter),
  AccessTokenUrl = proplists:get_value(access_token_url, TwitterConf),
  {ok, AccessResponse} = oauth:get(AccessTokenUrl, [{"oauth_verifier", Verifier}], consumer(TwitterConf), RequestToken, RequestTokenSecret),
  AccessParams = oauth:params_decode(AccessResponse),
  AccessToken = oauth:token(AccessParams),
  AccessTokenSecret = oauth:token_secret(AccessParams),
  {ok, AccessToken, AccessTokenSecret}.

get_current_user_info(AccessToken, AccessTokenSecret) ->
  call_json_service(current_user_info_url, AccessToken, AccessTokenSecret).

% Extract a oauth-formatted consumer tuple from the given Twitter configuration.
consumer(TwitterConf) ->
  ConsumerKey = proplists:get_value(consumer_key, TwitterConf),
  ConsumerSecret = proplists:get_value(consumer_secret, TwitterConf),
  {ConsumerKey, ConsumerSecret, hmac_sha1}.

% Invoke a call to a JSON service on Twitter.
call_json_service(UrlKey, AccessToken, AccessTokenSecret) ->
  TwitterConf = conf:get_section(twitter),
  Url = proplists:get_value(UrlKey, TwitterConf),
  {ok, Response} = oauth:get(Url, [], consumer(TwitterConf), AccessToken, AccessTokenSecret),
  {{ "{{" }}_Version, 200, "OK"}, _Headers, Json} = Response,
  {ok, Json}.
```

This might seem like a lot but there isn't much to it. Here's the run-down:

*   `request_access`: This function is what handles the first step in the OAuth negotiation process. It starts by loading the `twitter` configuration from our `app.config` file. The `twitter` section contains all the URLs we need to talk to Twitter

    First we need to get hole of a _request token_, which is an identifier for an authorisation request that Twitter generates when we first start talking OAuth. We get the `request_token_url` from the configuration and we connect to Twitter, using `oauth:get` to kick the process off. Note the use of the `consumer` function, which simply takes our local `twitter` configuration and populates an `erlang-oauth`-friendly tuple with the details required to make OAuth requests on behalf of our application. This tuple contains our _consumer key_, the _consumer secret_ and the signature method to use. We will always be using `hmac_sha1` as that's what Twitter currently requires.

    Twitter reponds with a payload which includes the generated request token. We take that request token out of the payload and generate an Authentication URL. This URL contains information about the request that we started in the previous steps, along with the `authenticate_url` value loaded from configuration. If you remember back to our configuration you'll see that this `authenticate_url` is one that Twitter told us to use when we first registered our application and it resolves to `https://twitter.com/oauth/authenticate`.

    This URL is returned to the caller and the calling code should redirect the user to this URL so that they can authenticate themselves with Twitter.

*   `verify_access`: This function is what is called after the use has authenticated themselves with Twitter. The function expects both the _request token_ and _request token secret_ so that the result of the request can be validated with Twitter. Twitter also generates a "verifier" value as part of it's authentication process, and this value is what is passed in via the `Verifier` parameter.

    After getting hold of the Twitter configuration an _access token_ URL is generated. This URL contains all the information required to turn the _request token_ into an _access token_. Once generated, this URL is then accessed via `erlang-oauth` and the payload that comes back from Twitter contains both the _access token_ and the _access token secret_. Both of these are required from this point on to make requests to Twitter on behalf of the user.

*   `get_current_user_info`: This is a small helper function which calls to Twitter via `erlang-oauth` and extracts the user details for the user. The payload contains the usual Twitter profile stuff such as Twitter ID, username, bio, tweet count, etc.

Before we take a look at the Webmachine resource that will invoke this functionality, let's take a look at what we'll need to do with the tokens once we've got them.

For now, we are only going to store them, encrypted, in the user's cookie which we'll send down to the browser. This isn't "best practice" when it comes to storage of this kind of information, but for the sake of this blog post it will suffice. Later in the series we'll be doing more with this information and most likely removing some of the information from the cookie.

With this in mind, we need something that is able to write to and read from the user's cookies during a request. This module needs to be able to verify that a user's cookie is valid and that it hasn't expired. When writing and reading the module must also handle the encryption of the sensitive information.

Let's create this new module, called `cookie.erl`, inside `csd_web`. I'll break it up into it's functions so you can see what it's doing.

```

%% @author OJ Reeves <oj@buffered.io>
%% @copyright 2012 OJ Reeves

-module(cookie).

-author('OJ Reeves <oj@buffered.io>').

-export([load_auth/1, store_auth/5]).

-define(AUTH_COOKIE, "__CodeSmackdown__Auth").
-define(AUTH_SALT, "27ed2d041cdb4b8b2702").
-define(AUTH_SECRET, "2d0431cd9bda5ba4b98271edcb2e7102").
-define(AUTH_EXPIRY_DAYS, 7).
-define(ENC_IV, <<207,94,217,158,198,63,132,205,35,187,246,2,56,122,250,33>>).
-define(ENC_KEY,
  <<110,56,121,28,235,159,77,154,160,5,130,210,204,32,26,224,255,86,101,71,61,3,
  66,69,30,39,42,0,116,93,204,99>>).
```

Ignoring the usual headers/setup for the module, we can see a stack of defines. They are:

* `AUTH_COOKIE`: This is the name of the cookie that will live in the browser. If you use a cookie editor you'll see this name appear as the name of the cookie once it's written.
* `AUTH_SALT`: This is a bunch of characters that will be used as a [salt][] for when we're generating the [SHA MAC][] from the user's cookie information.
* `AUTH_SECRET`: This is the key we'll be using when creating a [SHA MAC][] from the data we'll be pushing into the cookie. This is to make sure that the cookie hasn't been tampered with.
* `AUTH_EXPIRY_DAYS`: This is the number of days that the cookie is valid for.
* `ENC_IV`: This is the initialisation vector used when encrypting/decrypting the data in the cookie.
* `ENC_KEY`: This is the key that's used for encrypting/decrypting data that's in the cookie.

Pretty simple stuff. Now let's take a look at a function that does something interesting.

```
load_auth(ReqData) ->
  case wrq:get_cookie_value(?AUTH_COOKIE, ReqData) of
    undefined ->
      {error, no_cookie};
    V ->
      Val = mochiweb_util:unquote(V),
      decode(Val)
  end.
```

`load_auth` is a function which attempts to load authentication information from the cookies stored in the `ReqData` parameter. `ReqData` is the [request data][] that comes from Webmachine. As you can see, the function attempts to read the cookie value from the request data using Webmachine's [wrq][] module. If it fails `undefined` is returned and we know that no cookie has been set. If a value is read, we munge the data into something usable and then attempt to decode it using the `decode` function explained further down.

This function returns either `{ok, <Cookie Information>}` or `{error, <Reason>}`.

```
store_auth(ReqData, Id, Name, Token, TokenSecret) ->
  Value = mochiweb_util:quote_plus(encode(Id, Name, Token, TokenSecret)),
  Options = [
    %{domain, "codesmackdown.com"},
    {max_age, 3600 * 24 * ?AUTH_EXPIRY_DAYS},
    {path, "/"},
    {http_only, true}
  ],
  CookieHeader = mochiweb_cookies:cookie(?AUTH_COOKIE, Value, Options),
  wrq:merge_resp_headers([CookieHeader], ReqData).
```

`store_auth` is the opposite to `load_auth` as it writes the user's information and token data to a cookie. The parameters to this function are:

* `ReqData`: Webmachine's request data.
* `Id`: The user's Twitter ID. We'll be using this as a key later on to retrieve information from Riak.
* `Name`: The user's Twitter user name. We'll use this purely for display.
* `Token` and `TokenSecret`: Token information for making OAuth requests on behalf of this user.

The first thing we do is call `encode` and pass in the last four arguments. This gives us an encrypted blob which we can store in a cookie. We then put down some basic information inside `Options`, including the expiry date. We then use `mochiweb_cookies` to generate a cookie with the name (`AUTH_COOKIE`), value and options.

Lastly we take the generated cookie header and merge that with the headers that already part of `ReqData` and produce a new request data object which is returned to the caller.

```
encode(Id, Name, Token, TokenSecret) ->
  SecretInfo = encrypt({Token, TokenSecret}),
  CookieValue = {Id, Name, get_expiry(), SecretInfo},
  base64:encode(term_to_binary({CookieValue, ?AUTH_SALT, crypto:sha_mac(?AUTH_SECRET, term_to_binary([CookieValue, ?AUTH_SALT]))})).
```

The `encode` function is rather self-explanatory. We start by encrypting the OAuth token information, we then generate a tuple which includes all the data we want to keep, convert it to binary and [base64][] encode it.

```
decode(CookieValue) ->
  {Value={Id, Name, Expire, SecretInfo}, Salt, Sign} = binary_to_term(base64:decode(CookieValue)),
  case crypto:sha_mac(?AUTH_SECRET, term_to_binary([Value, Salt])) of
    Sign ->
      case Expire >= calendar:local_time() of
        true ->
          {Token, TokenSecret} = decrypt(SecretInfo),
          {ok, {Id, Name, Token, TokenSecret}};
        false ->
          {error, expired}
      end;
    _ ->
      {error, invalid}
  end.
```

The `decode` function does a little more than its counterpart as there's validation built-in as well as decrypting. Firstly we do the inverse of the final steps of the `encode` function in that we base64 decode the data into binary and convert the resulting binary back to Erlang terms. We then break this value up into its components.

We then validate that the cookie hasn't been tampered with by calculating the [SHA MAC][] of the data that was retrieved. If this value doesn't match what is expected we indicate that the value is invalid. If the value is valid, we then make sure that the internal cookie value hasn't expired. If it hasn't, we return `{ok, <data>}`.

The rest of the functions are easy to understand, so here they are for the sake of completeness without explanation.

```
get_expiry() ->
  {Date, Time} = calendar:local_time(),
  NewDate = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) + ?AUTH_EXPIRY_DAYS),
  {NewDate, Time}.

encrypt(Value) ->
  crypto:aes_ctr_encrypt(?ENC_KEY, ?ENC_IV, term_to_binary([Value, ?AUTH_SALT])).

decrypt(Value) ->
  [V, ?AUTH_SALT] = binary_to_term(crypto:aes_ctr_decrypt(?ENC_KEY, ?ENC_IV, Value)),
  V.
```

Phew! Now that's out of the way we have some back-end glue which we can use to perform some more interesting tasks. One thing that we really need to do is update the landing page template with something more meaningful than what we have now.

We'll start making use of [ErlyDTL][]'s hierarchical templates and implement a base template which our other templates will also make use of. Here it is in all its simplicity:

```
<!DOCTYPE html>
<html lang="en">
  <head>
    <title>Code Smackdown - {{ "{" }}% block page_title %}{{ "{" }}% endblock %}</title>
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js" type="text/javascript"></script>
    <script src="http://ajax.microsoft.com/ajax/jquery.templates/beta1/jquery.tmpl.min.js" type="text/javascript"></script>
  </head>
  <body>
    {{ "{" }}% block body_content %}{{ "{" }}% endblock %}
  </body>
</html>
```

In this template we've included a couple of Javascript files that we'll be using later on as well as setting up a basic HTML5 page. `page_title` and `body_content` are the two sections that child templates can populate with their own content.

With that, let's go ahead and modify our default template so that it has something a little more meaninful in it:

```
{{ "{" }}% extends 'base.dtl' %}

{{ "{" }}% block page_title %}Landing Page{{ "{" }}% endblock %}

{{ "{" }}% block body_content %}
    <h1>Welcome to Code Smackdown</h1>
    {{ "{" }}% if logged_in %}
    <p>Welcome back {{ "{" }}{ user_name }}.</p>
    {{ "{" }}% else %}
    <p>We require you to sign in via Twitter</p>
    <p><a href="{{ "{" }}{ logon_url }}" title="Sign in with Twitter"><img src="http://si0.twimg.com/images/dev/buttons/sign-in-with-twitter-d.png"/></a><p>
    {{ "{" }}% endif %}
{{ "{" }}% endblock %}
```

Nothing sinister going on here, but there are a couple of things worth noting. The template now looks for a field called `logged_in`, and if it's `true` it renders a paragraph which contains the value in the `user_name` field. If the `logged_in` flag is false a link is provided which points to `logon_url` which ultimately points the user at the Twitter OAuth entry page.

We'll need to pass these values in when we render the template. Let's have a look at the changed section of `csd_web_resource`:

```
% ... snip ... %
to_html(ReqData, State) ->
  Content = case cookie:load_auth(ReqData) of
    {ok, {_, Name, _, _}} ->
      csd_view:home(Name);
    _ ->
      csd_view:home()
  end,
  {Content, ReqData, State}.
% ... snip ... %
```

Yes this is quite a bit different to before. We are calling into our `cookie` module to find out if the user is logged in. If they are logged on we call `csd_view:home` with a single parameter `Name`, if they're not logged on the same function is called without any parameters.

The `csd_view` module is new and was created to abstract the idea of template rendering. All the ErlyDTL handling happens in `csd_view`. Let's take a look at it now.

```
%% @author OJ Reeves <oj@buffered.io>
%% @copyright 2012 OJ Reeves

-module(csd_view).

-author('OJ Reeves <oj@buffered.io>').

-export([home/0, home/1]).

home() ->
  Params = [{logged_in, false}, {logon_url, conf:get_val(urimap, twitter_logon)}],
  {ok, Content} = home_dtl:render(Params),
  Content.

home(Name) ->
  Params = [{logged_in, true}, {user_name, Name}],
  {ok, Content} = home_dtl:render(Params),
  Content.
```

Here we can see the two functions called `home` which were invoked in the `csd_web_resource` module. Most of this module is simple and uninteresting except for the use of `conf:get_val`. Templates need to know about paths when generating URLs in the markup. In our case, we're rendering links which point to internal routes which are specified in the dispatch list. Rather than hard-code URLs in the templates I decided to create another section in the `app.config` called `urimap`. The goal is to have an easy-to-access location for addresses which lives alongside the routes so that the maintainer of the application can update both at the same time should something need to change. Here's what the new section looks like.

```
% ... snip ... %
  {csd_web,
    [
      % ... snip ... %
      {urimap,
        [
          {home, "/"},
          {twitter_logon, "/oauth/request"}
        ]
      },
      % ... snip ... %
```

Accessing a link address is as simple as running `conf:get_val(urimap, <link-id>)`.

At this point we can build and run the application to see what the landing page looks like. To fire up the application you'll need three consoles:

1. One for Riak. Riak has to be running behind the scenes because `Pooler` will connect on start. Only one node is necessary at this point. Run: `/path/to/riak/dev/dev1/bin/riak start`
1. One for HAProxy. Run: `make proxystart`
1. One for the CSD application. Run: `make webstart`

When you browse to [http://127.0.0.1:4000](http://127.0.0.1:4000) you should see the following:

![Landing page when logged off][ImgHomeLoggedOff]

Clicking the link will result in an error at this point, so don't do it yet! We need to implement more resources, but first let's just stick the routes into the dispatch in preparation.

```
{csd_web,
  [
    {web,
      [
        % ... snip ... %
        {dispatch,
          [
            {[], csd_web_resource, []},
            {["snippet", key], csd_web_snippet_resource, []},
            {["oauth", "request"], csd_web_request_resource, []},  % new route
            {["oauth", "callback"], csd_web_callback_resource, []} % new route
          ]
        }
      ]
    },
    % ... snip ... %
```

Easily done. Now let's look at the implementation of the first handler which handles the `/oauth/request` URI, `csd_web_request_resource`.

```
%% @author OJ Reeves <oj@buffered.io>
%% @copyright 2012 OJ Reeves

-module(csd_web_request_resource).

-author('OJ Reeves <oj@buffered.io>').

-export([init/1, resource_exists/2, previously_existed/2, moved_temporarily/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
  {ok, undefined}.

resource_exists(ReqData, State) ->
  {false, ReqData, State}.

previously_existed(ReqData, State) ->
  {true, ReqData, State}.

moved_temporarily(ReqData, State) ->
  {ok, Url} = twitter:request_access(),
  {{ "{" }}{true, Url}, ReqData, State}.
```

This module is quite lightweight, but has a little bit of magic in it that revolves around getting redirects to work. If you're not familiar with how 307 redirects work in Webmachine, take a quick side-glance at my [Redirects with Webmachine][WebmachineRedirects] post.

Back? Ok. The extra line of code in the `moved_temporarily` function is where we invoke `twitter:request_access()` which goes to Twitter.com and gets a request token. The URL generated by this call is then passed back to Webmachine which will tell the caller's browser where to redirect to.

Build the app, fire up it up and click on the "Sign in via Twitter" button and you should see a screen that resembles this (assuming you're already signed in to Twitter):

![Logging into CSD view Twitter][ImgTwitterLogon]

Exciting! We're nearly there. Don't click "Sign In" just yet because we don't yet have the callback set up to handle the result. Let's do that now. Here's the resource:

```
%% @author OJ Reeves <oj@buffered.io>
%% @copyright 2012 OJ Reeves

-module(csd_web_callback_resource).

-author('OJ Reeves <oj@buffered.io>').

-export([init/1, resource_exists/2, previously_existed/2, moved_temporarily/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
  {ok, undefined}.

resource_exists(ReqData, State) ->
  {false, ReqData, State}.

previously_existed(ReqData, State) ->
  {true, ReqData, State}.

moved_temporarily(ReqData, State) ->
  handle_callback(ReqData, State).

handle_callback(ReqData, State) ->
  ReqToken = wrq:get_qs_value("oauth_token", ReqData),
  ReqTokenSecret = wrq:get_qs_value("oauth_token_secret", ReqData),
  Verifier = wrq:get_qs_value("oauth_verifier", ReqData),

  {ok, AccessToken, AccessTokenSecret} = twitter:verify_access(ReqToken, ReqTokenSecret, Verifier),
  {ok, UserInfoJson} = twitter:get_current_user_info(AccessToken, AccessTokenSecret),
  {struct, Json} = mochijson2:decode(UserInfoJson),
  UserId = proplists:get_value(<<"id">>, Json),
  UserName = proplists:get_value(<<"screen_name">>, Json),
  NewReqData = cookie:store_auth(ReqData, UserId, UserName, AccessToken, AccessTokenSecret),

  % TODO: store the 'session' in Riak in an ETS backend

  % TODO: error handlng for when things don't go to plan
  {{ "{" }}{true, conf:get_val(urimap, home)}, NewReqData, State}.

```

The first few parts of this module should look familiar by now. We are overriding the `resource_exists`, `previously_existed` and `moved_temporarility` functions because we're going to be redirecting. For now we're going to assume that the user clicked "Sign In" and that everything went according to plan. Later on we'll worry about handling logon errors.

When `moved_temporarily` is invoked we pass responsibility off to the `handle_callback` function. Here you can see we are taking three parameters out of the query string that Twitter sent through to us. Those parameters are the _request token_, _request token secret_ and the _verifier_. We take those values and pass them down to our `twittter` module to get it to verify the access with Twitter and to generate an _access token/access token secret_ pair. When that comes back we have our token information and we can assume that the user has authenticated via Twitter. At this point we can "log the user on" by storing a cookie, but before we do that we want to get their Twitter ID and Username, so we invoke the `twitter:get_current_user_info` function, passing in the OAuth credentials, which in return gives us a blob of [JSON][] which contains the user's Twitter information.

From that we glean their ID and Username. We then store that information, along with the access token information, in a cookie using `cookie:store_ath` (which we've covered previously) and we get a new request data object out as a result.

Now all we have to do is redirect the user back to the home page and pass on the new request data. Webmachine will take this data and push the cookie to the user's browser, then redirect the user to the `home` entry in the `urimap` section in `app.config`. In effect, we're redirected to the home page as a logged on user.

Ignoring the `TODO` notes (which we'll cover in future posts in this series), we've got ourselves to the point where the application should function end-to-end. Finally.

Compile the application and fire it up! Let's take a look at what happens.

![Hitting the home page prior to logging on][ImgHomeLoggedOff]

![Authenticating with Twitter][ImgTwitterLogon]

![Back home after the redirect with successful sign-on][ImgHomeLoggedOn]

## That's all ... for now ##

Thanks for reading this post. If you managed to make it this far you've done well. In the next post we'll start to do some more meaningful things with our logged on users, such as allowing them to submit code snippets. This is where the end-to-end process becomes interesting.

Comments, feedback and criticisms are as welcome as always.

**Note:** The code for Part 4 (this post) can be found on [Github][Part4Code].

Other parts in this series: [Part 1][], [Part 2][], [Part 3][], [Part 5][]

  [Part 1]: /posts/webmachine-erlydtl-and-riak-part-1/ "Wembachine, ErlyDTL and Riak - Part 1"
  [Part 2]: /posts/webmachine-erlydtl-and-riak-part-2/ "Webmachine, ErlyDTL and Riak - Part 2"
  [Part 3]: /posts/webmachine-erlydtl-and-riak-part-3/ "Webmachine, ErlyDTL and Riak - Part 3"
  [Part 5]: /posts/webmachine-erlydtl-and-riak-part-5/ "Webmachine, ErlyDTL and Riak - Part 5"
  [Part4Code]: https://github.com/OJ/csd/tree/Part4-20120217 "Source code for Part 4"
  [erlang-oauth]: https://github.com/tim/erlang-oauth "erlang-oauth"
  [erlang-oauth-fork]: https://github.com/OJ/erlang-oauth/tree/rebarise "erlang-oauth rebar fork"
  [HAProxy]: http://haproxy.1wt.eu/ "HAProxy"
  [Pooler]: https://github.com/seth/pooler "Pooler"
  [Twitter]: http://twitter.com/ "Twitter"
  [OAuth]: http://oauth.net/ "OAuth"
  [OAuthPresso]: http://www.slideshare.net/leahculver/oauth-open-api-authentication "OAuth overview"
  [TwitterNewApp]: https://dev.twitter.com/apps/new "New Twitter Application"
  [Erlang]: http://erlang.org/ "Erlang"
  [Webmachine]: http://www.basho.com/developers.html#Webmachine "Webmachine"
  [JSON]: http://json.org/ "JavaScript Object Notation"
  [Part 1]: /posts/webmachine-erlydtl-and-riak-part-1/ "Wembachine, ErlyDTL and Riak - Part 1"
  [Part 2]: /posts/webmachine-erlydtl-and-riak-part-2/ "Wembachine, ErlyDTL and Riak - Part 2"
  [Part 3]: /posts/webmachine-erlydtl-and-riak-part-3/ "Wembachine, ErlyDTL and Riak - Part 3"
  [Riak]: http://www.basho.com/developers.html#Riak "Riak"
  [ErlyDTL]: http://github.com/evanmiller/erlydtl "ErlyDTL"
  [Rebar]: http://www.basho.com/developers.html#Rebar "Rebar"
  [mochijson2]: https://github.com/mochi/mochiweb/blob/master/src/mochijson2.erl "Mochiweb's json module"
  [Mochiweb]: https://github.com/mochi/mochiweb "Mochiweb"
  [OTP]: http://en.wikipedia.org/wiki/Open_Telecom_Platform "Open Telecom Platform"
  [cURL]: http://curl.haxx.se/ "cURL homepage"
  [WebmachineRedirects]: http://buffered.io/posts/redirects-with-webmachine/ "Redirects with Webmachine"
  [PoolerFork]: https://github.com/OJ/pooler "OJ's Pooler fork"
  [gen_server]: http://www.erlang.org/doc/man/gen_server.html "gen_server"
  [configuration]: http://www.erlang.org/doc/man/config.html "Erlang configuration"
  [pooler:use_member]: https://github.com/OJ/pooler/blob/master/src/pooler.erl#L125 "use_member"
  [request token]: http://oauth.net/core/1.0/#auth_step1 "Request tokens"
  [access token]: http://oauth.net/core/1.0/#auth_step3 "Access tokens"
  [SHA MAC]: http://en.wikipedia.org/wiki/HMAC "HMAC"
  [salt]: http://en.wikipedia.org/wiki/Salt_(cryptography) "Salt (crypto)"
  [request data]: http://wiki.basho.com/Webmachine-Request.html "Request data"
  [wrq]: http://wiki.basho.com/Webmachine-Request.html "Request data"
  [base64]: http://en.wikipedia.org/wiki/Base64 "Base64"
  [ImgTwitterAppCreate]: /uploads/2012/02/twitter-app-create.png "Twitter app creation"
  [ImgHomeLoggedOff]: /uploads/2012/02/home-loggedoff.png "Home - Logged Off"
  [ImgHomeLoggedOn]: /uploads/2012/02/home-loggedon.png "Home - Logged On"
  [ImgTwitterLogon]: /uploads/2012/02/twitter-logon.png "Twitter - Logon Page"
