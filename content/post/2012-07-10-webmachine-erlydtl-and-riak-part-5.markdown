---
categories:
- Riak
- Functional Programming
- HOWTO
- Erlang
- Webmachine
- ErlyDTL
- BackboneJS
- Bootstrap
comments: true
date: 2012-07-10T00:00:00Z
tags:
- Riak
- Functional Programming
- HOWTO
- Erlang
- Webmachine
- ErlyDTL
- BackboneJS
- Bootstrap
title: Webmachine, ErlyDTL and Riak - Part 5
---

{% img right /uploads/2010/09/riak-logo.png 'Riak Logo' %}Newcomers to the series should first take a look at the previous four parts of the [series][] ([Part 1][], [Part 2][], [Part 3][], [Part 4][]) first to make sure that you're up to speed. Feel free to read on if you feel comfortable with the general concepts in use.

When we finished [Part 4][] we were able to authenticate users using [Twitter][] and [OAuth][], which is great as we can delegate the responsibility of password management to an external entity.

Now that we know who people are, we want them to be able to do something meaningful with their accounts. That's what this post is all about.

<!--more-->

## <a id="agenda"></a>Agenda

So far it has been hard to see what the goal of this application is. Given the piecemeal nature of the posts it's hard to project that vision, especially when the content is quite code-heavy. By the end of this post, we'll not only have a "proper" web application that performs useful functions, we'll be able to see what this "Code Smackdown" thing really is all about.

This post is going to cover the following topics:

1. _[Riak][] Secondary Indexes_ - We'll be using these so that we can link code snippet submissions to the users who submitted them.
1. _[Map/Reduce][MapRed]_ - We're going to end up with data stored in our database and we're going to want to query it. Map/reduce is where it's at!
1. _Form submission handling with [Webmachine][]_ - Users will be able to submit code snippet pairs to the system once they're logged in. They'll also be able to vote on submitted snippets.
1. _Listing of submissions per-user_ - Viewing the submissions for a given user will pull out a list from Riak using the secondary indexes and map/reduce. This will allow a user to see what snippets they've submitted.
1. _Static file serving_ - Our new UI will require the serving of static content. There are quite a few ways to do this, one of which is using a custom Webmachine resource. While in production it's a great idea to use a tool like [Nginx][] for this purpose, but we'll go with the custom Webmachine resource just to keep things a little simpler.
1. _Tidying up of templates/UI_ - Now that we've got some content to render, we'll put together some nicer templates and harness [Twitter Bootstrap][] to make the site a little nicer to look at. You'll notice that the emphasis will drop off from [ErlyDTL][] as we'll be doing more rendering of content on the client side using [Handlebars][] while using [Backbone.js][] for logic, routing and event handling.

Lots of UI work has been done for this post, but most of that work will not be discussed in detail as there's already enough content to get through. As always the source code is available so you can read it and play with it. You'll find the link at the end of the post.

Prior to continuing you should make sure you have the latest version of [Riak][] installed. If you don't, please go and do this now (read [Part 1][] to learn how to build Riak from scratch).

With that ... don your robe and Wizard's hat and let's begin.

## <a id="enabling-secondary-indexes"></a>Enabling Secondary Indexes

As already mentioned we're going to be storing data in Riak and using the [Secondary Index][] feature to make it easier to link data together and do certain types of queries over that data. Given this requirement the first thing we should do is enable secondary indexes on our cluster.

As per the [Riak wiki entry][Secondary Index] ...

> As of version 1.0, Secondary Indexes are enabled by configuring Riak to use the
> ELevelDB backend `riak_kv_eleveldb_backend`. Currently, the ELevelDB backend is the
> only index-capable backend.

So we need to go through our Riak development cluster configuration and make sure that our backend is set up correctly. Before continuing, make sure your cluster is no longer running:

```
riak/dev $ dev1/bin/riak stop
ok
riak/dev $ dev2/bin/riak stop
ok
riak/dev $ dev3/bin/riak stop
ok
```

To modify all the `app.config` files easily we can run one simple command from the `dev` (the parent folder which contains all the dev Riak instances):

```
riak/dev $ vim ./**/app.config
```

This will open [VIM][] with all of the `app.config` files open so that we can easily made the necessary modifications. In each of these files, find the `riak_kv` configuration section and change the backend like so:

```
... snip ...

%% Riak KV config
{riak_kv, [
          %% Storage_backend specifies the Erlang module defining the storage
          %% mechanism that will be used on this node.
          {storage_backend, riak_kv_eleveldb_backend},
          ... snip ...
]}
... snip ...
```

Done. Don't forget to make sure your dev cluster is running again before you continue:

```
riak/dev $ dev1/bin/riak start
riak/dev $ dev2/bin/riak start
riak/dev $ dev3/bin/riak start
```

## <a id="schema-design"></a>Schema Design

Before we get going with any more of the implementation, we need to consider the design of the "schema" we're going to use when storing our data in Riak. We want our users to be able to:

1. Submit snippets to the system.
1. See a list of snippets they have submitted to the system (and down the track see other lists using filters).
1. Vote for the left- or right- hand snippets to indicate which they prefer.
1. See that they have voted for a snippet before and be reminded of which one they voted for.

At the centre of this data there is the _snippet_. The snippet has the following fields:

* `title`: A simple descriptive label.
* `left`: One way of performing a function in a given language.
* `right`: Another way of performing the same function in a given language (which may not be the same as the language used for `left`).
* `created`: A date/time when the snippet was created/submitted.
* `key`: A key/ID which identifies the snippet.

These fields will be stored as a blob of JSON.

We also need to store with the snippet an identifier for the user that submitted it. Rather than storing this with the payload, we are instead going to create a secondary index which contains this information. We can then use this index to query the store to find out the snippets a user has submitted. This index will be called `userid`.

In future posts we will probably include more indexes and/or fields, but for the functionality we're aiming to build for this post these fields are sufficient.

Next we need to store votes. In a typical RDMBS this problem is well-known and the solutions out there are also well-known. In a KV store this isn't necessarily the case. What I propose in this post is _a possible way_ of solving this problem. I do not in any way claim that this is _the best way_. With this, this is what we're going to do...

A `vote` needs to keep track of who submitted it along with the snippet it was put against. It also needs to have an indication of whether the user preferred the left or right hand side of the snippet. When these votes are stored, we also want to be able to query them in such a way so that, for a given snippet, we can quickly count the number of votes and which way those votes went. This is quite important as the tallying of the votes and displaying them on screen is a key part of the idea behind the application.

To identify a vote the key needs to be made up of both the `userid` of the person who submitted it and the `key` of the snippet the vote. Therefore, for the `vote` bucket we'll create keys in the format: *userid-snippetkey*

While this makes sense, it doesn't make it easy for us to figure out which votes went against which snippets. To do this, we'll create a secondary index on the vote which contains the snippet key. This will give us a faster way of finding votes that relate to a key while still keeping the votes separate in the bucket. We can then do a map/reduce over the index and pull out the votes.

Originally I had pondered the idea of having another secondary index which contained the vote direction (`left` or `right`) and doing multiple map/reduces over the data to count the items. This seemed silly to me. I didn't think it made sense to invoke two map/reduce jobs when I could do the same thing with one. As a result, I decided to put the direction of the vote inside the vote payload itself as this can be used during a single map/reduce job to total both `left` and `right` votes. Down the track the user is going to want to be able to look at what they've voted on (as part of a history timeline), so we'll also add a `userid` index.

Finally, we are going to need to store some more meaningful information about a user for future use, so we'll create a `user` bucket and store some metadata for each user with their Twitter ID as the key.

Here's a visual of what we should end up with:

{% img /uploads/2012/07/part5-db-schema.png 'CSD Schema' %}

Now that we have the basics of the schema out of the way, the first thing we should do is adjust our Riak module to include the new features we'll need to do with secondary indexing and map/reduce.

## <a id="handling-2i"></a>Handling 2i in csd\_riak

In Riak secondary indexes (2i) are stored as extra metadata alongside the Riak object. Two types of indexes are currently support: `integer` and `binary`. These index types are indicated using a naming convention, such that `integer` indexes are suffixed with `_int` and `binary` indexes are suffixed with `_bin`. Secondary indexes are stored as a key/value pair tuple inside the `index` section of the meta data.

So to start with let's define a few index-specific macros and helper functions.

```
% ... snip ...

-define(INDEX_KEY, <<"index">>).
-define(INDEX_SUFFIX_INT, <<"_int">>).
-define(INDEX_SUFFIX_BIN, <<"_bin">>).

% ... snip ...

index(int, Name) ->
  iolist_to_binary([Name, ?INDEX_SUFFIX_INT]);
index(bin, Name) ->
  iolist_to_binary([Name, ?INDEX_SUFFIX_BIN]).

% ... snip ...
```

Here the `index` function is a simple function which allows us to generate an index name based on a type and a name. This can be called like so: `IndexName = index(int, "userid").` - We'll make use of this in other areas, including the `csd_riak_mr` module which we'll cover off shortly.

Next we'll define some functions which make it easier to add indexes to Riak objects.

```
% ... snip ...

set_index(RiakObj, Type, Name, Value) ->
  Meta = riakc_obj:get_update_metadata(RiakObj),
  Index = case dict:find(?INDEX_KEY, Meta) of
    error -> [];
    {ok, I} -> I
  end,
  NewIndex = dict:to_list(dict:store(index(Type, Name), value(Value), dict:from_list(Index))),
  riakc_obj:update_metadata(RiakObj, dict:store(?INDEX_KEY, NewIndex, Meta)).

set_indexes(RiakObj, Indexes) ->
  Meta = riakc_obj:get_update_metadata(RiakObj),
  Index = case dict:find(?INDEX_KEY, Meta) of
    error -> [];
    {ok, I} -> I
  end,
  UpdatedIndexes = lists:foldl(fun({T, N, V}, I) ->
        dict:store(index(T, N), value(V), I)
    end,
    dict:from_list(Index), Indexes),
  NewIndex = dict:to_list(UpdatedIndexes),
  riakc_obj:update_metadata(RiakObj, dict:store(?INDEX_KEY, NewIndex, Meta)).

get_index(RiakObj, Type, Name) ->
  Meta = riakc_obj:get_metadata(RiakObj),
  Indexes = dict:fetch(?INDEX_KEY, Meta),
  IndexKey = index(Type, Name),
  Value = binary_to_list(proplists:get_value(IndexKey, Indexes)),
  case Type of
    int -> list_to_integer(Value);
    bin -> Value
  end.

% ... snip ...
```

The first function, `set_index`, is used to update a Riak object instance and include a single new index of a certain type. This function gets existing _update metadata_ (different to "normal" metatdata in that this is what will be used to update the object when saved) and then attemps to retrieve the `index` section of that data. If found adds the new index value to the list of indexes. If it's not found then the new indexes is simply inserted into an empty list. This information is then written into a new Riak object via the `riakc:update_metadata/2` function.

This code can be called like so: `NewObj = csd_riak:set_index(RiakObj, int, "userid", 12345).`

This code converts between lists and dictionaries because I want existing index values to be overwritten with the new values.

The second function, `set_indexes`, is an extended version of `set_index` in that it allows you to set more than one key at a time. Instead of a single type/name/value combination it accepts a list of tuples of `{type, name, value}`.

The third function, `get_index`, is a helper function which is designed to get the value of certain index. Note how this function accesses _existing_ metadata via `get_metadata/1` rather than `get_update_metadata/1`. This is due to us being interested in an existing index, not in one that is about to be updated when we next save. While we're here, we do a converstion of the value to an integer if the index type is an integer.

This code can be called like so: `UserId = csd_riak:get_index(RiakObj, int, "userid").`

Last of all you may have noticed that a couple of these functions are calling another function called `value/1`. It looks like this:

```
% ... snip ...

%% ------------------------------------------------------------------
%% Private Function Definitions
%% ------------------------------------------------------------------

value(V) when is_list(V) ->
  list_to_binary(V);
value(V) ->
  V.
```

As you can see this is an internal function which is there to help make sure that values are in the right format when being stored as an index.

With the 2i interface now taken care of, let's take a look at what we need to do for map/reduce.

## <a id="supporting-mapreduce"></a>Supporting Map/Reduce in csd\_riak

As you're already aware, Riak's map/reduce interface requires a set of _inputs_, one or more _map_ phases and zero or more _reduce_ phases. We could manually construct each of these components each time we want to execute a map/reduce job but that doesn't quite feel right to me. Instead, I prefer to have a "usable" module that helps construct properly-formed map/reduce jobs to reduce the risk of the caller doing the wrong thing. Callers of our modules shouldn't have to know about the format of Riak's map/reduce interface in order to use it. So we'll provide a helper module which wraps this up.

Before we look at the code, bear in mind that this module supports enough functionality to provide what is needed for the application so far. Down the track extra features will be added to support other ways of doing map/reduce, but for now they are beyond the scope of this version of the application.

With that, let's take a look at the code.

```
-module(csd_riak_mr).
-author('OJ Reeves <oj@buffered.io>').

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    create/0,
    run/2,
    add_input_index/5,
    add_map_js/2,
    add_map_js/3,
    add_map_js/4,
    add_reduce_js/2,
    add_reduce_js/3,
    add_reduce_js/4,
    add_reduce_sort_js/2,
    add_reduce_sort_js/3
  ]).

%% ------------------------------------------------------------------
%% Private Record Definitions
%% ------------------------------------------------------------------

-record(mr, {
    in_ind = undefined,
    %% TODO: when the need arises add support for other inputs
    %% including {bucket, key} and {bucket, key, arg}.
    phases = []
  }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Create a mew map/reduce job instance.
create() ->
  #mr{}.

run(RiakPid, #mr{in_ind=Input, phases=P}) ->
  % phases are pushed in reverse, so reverse them before using them
  Phases = lists:reverse(P),
  Result = riakc_pb_socket:mapred(RiakPid, Input, Phases),
  Result.

%% @doc Creates a map/reduce Input phase for a secondary index input.
add_input_index(MR=#mr{}, Bucket, Type, Index, Value) when is_integer(Value) ->
  add_input_index(MR, Bucket, Type, Index, integer_to_list(Value));
add_input_index(MR=#mr{}, Bucket, Type, Index, Value) when is_list(Value) ->
  add_input_index(MR, Bucket, Type, Index, list_to_binary(Value));
add_input_index(MR=#mr{}, Bucket, Type, Index, Value) when is_binary(Value) ->
  MR#mr{
    in_ind = {index, Bucket, csd_riak:index(Type, Index), Value}
  }.

%% @doc Creates a map/reduce Map phase from raw JS source. This overload
%%      defaults Keep to true and Arg to none.
add_map_js(MR=#mr{}, JsSource) ->
  add_map_js(MR, JsSource, true).

%% @doc Creates a map/reduce Map phase from raw JS source. This overload
%%      defaults Arg to none.
add_map_js(MR=#mr{}, JsSource, Keep) ->
  add_map_js(MR, JsSource, Keep, none).

%% @doc Creates a map/reduce Map phase from raw JS source.
add_map_js(MR=#mr{phases=P}, JsSource, Keep, Arg) ->
  MR#mr{
    phases = [{map, {jsanon, JsSource}, Arg, Keep}|P]
  }.

%% @doc Creates a map/reduce Reduce phase from raw JS source. This overload
%%      defaults Keep to true and Arg to none.
add_reduce_js(MR=#mr{}, JsSource) ->
  add_reduce_js(MR, JsSource, true).

%% @doc Creates a map/reduce Reduce phase from raw JS source. This overload
%%      defaults Keep to true.
add_reduce_js(MR=#mr{}, JsSource, Keep) ->
  add_reduce_js(MR, JsSource, Keep, none).

%% @doc Creates a map/reduce Reduce phase from raw JS source.
add_reduce_js(MR=#mr{phases=P}, JsSource, Keep, Arg) ->
  MR#mr{
    phases = [{reduce, {jsanon, JsSource}, Arg, Keep}|P]
  }.

%% @doc Creates a map/reduce Reduce sort phase using Riak's built in sort function
%%      using the specified comparison function written in raw JS. This overload
%%      defaults Keep to true.
add_reduce_sort_js(MR=#mr{}, CompareFun) ->
  add_reduce_sort_js(MR, CompareFun, true).

%% @doc Creates a map/reduce Reduce sort phase using Riak's built in sort function
%%      using the specified comparison function written in raw JS.
add_reduce_sort_js(MR=#mr{phases=P}, CompareFun, Keep) ->
  MR#mr{
    phases = [{reduce, {jsfun, <<"Riak.reduceSort">>}, CompareFun, Keep}|P]
  }.
```

Many of you will probably be able to digest this code without explanation. But just in case there's a little bit of confusion:

* `#mr` is an internally defined record which will accumulate a set of inputs and phases to execute against riak. This is internal so that external callers are "forced" to use the module to construct a map/reduce job.
* The `create/0` function simply creates an instance of a `#mr` record that the user can start to add map/reduce details to.
* Each of the `add_*` functions is used to add an input or a phase to to a `#mr` record. For this version of the application we're use JavaScript for our map/reduce phases. Functions that deal with JavaScript tend to have `_js` as a suffix.
* The `add_reduce_sort_js/3` function is one example of where we're using an internal Riak javascript reduce function. This function sorts elements during the reduce phase and uses a user-defined JavaScript function passed in as an argument to the phase.
* The `run/2` function executes the map/reduce job in Riak and returns the result.

This module makes use of the `csd_riak:index/2` function which helps create well-formed index names. This is used when constructing index inputs.

That's map/reduce taken care of (for now). With the guts of boilerplate Riak interaction taken care of, let's have a look at our approach to data storage.

## <a id="goodby-csdcoreserver"></a>Goodbye `csd_core_server`

When I first started working on this application I created `csd_core_server` with the intent of using it as a bridge between the application and Riak. This module, implemented as a [gen_server][], would have been responsible for handling and managing a pool of connections to Riak.

This concern has changed given that we are now using [Pooler][] to solve this problem for us. As a result, the idea of having a `gen_server` doesn't really make sense. Instead it makes more sense to have a module which handles interacting with [Pooler][] so that other areas of the application don't need to know it's there.

`csd_core_server` has now been removed and replaced with another module called `csd_db`. This new module is not a `gen_server`, it is simple a plain module which exposes an interface to the database.

Abstraction purists might argue that this is a positive as it gives us the ability to swap our database out for something else and the consumers of `csd_db` wouldn't even know. This might be true, but that's not really the goal. The goal is to put all the [Pooler][] interaction in a single spot.

Rather than show the module here in its entirety, we'll break it up into chunks: snippets, users and votes. Each of these chunks will be looked at when we dive into storage of those individual bits of data. To give an idea of the purpose that it serves see the following diagram:

{% img /uploads/2012/07/part5-db-modules.png 'Database Module Interaction' %}

The modules on the left invoke functions on `csd_db` which then invokes functions on the respective store modules passing in an extra parameter which is a `RiakPid` so that the store modules can talk to Riak. Simple!

Since `csd_db` is just a helper that we'll be using across all modules, let's take a look at it first.

```
-module(csd_db).
-author('OJ Reeves <oj@buffered.io>').

%% --------------------------------------------------------------------------------------
%% API Function Exports
%% --------------------------------------------------------------------------------------

-export([get_snippet/1, save_snippet/1, list_snippets/1]).
-export([get_user/1, save_user/1]).
-export([get_vote/1, save_vote/1, vote_count_for_snippet/1, vote_count_for_snippet/2]).

%% --------------------------------------------------------------------------------------
%% Snippet API Function Definitions
%% --------------------------------------------------------------------------------------

save_snippet(Snippet) ->
   pooler:use_member(fun(RiakPid) -> csd_snippet_store:save(RiakPid, Snippet) end).

get_snippet(SnippetKey) ->
  pooler:use_member(fun(RiakPid) -> csd_snippet_store:fetch(RiakPid, SnippetKey) end).

list_snippets(UserId) ->
  pooler:use_member(fun(RiakPid) -> csd_snippet_store:list_for_user(RiakPid, UserId) end).

%% --------------------------------------------------------------------------------------
%% User API Function Definitions
%% --------------------------------------------------------------------------------------

get_user(UserId) ->
  pooler:use_member(fun(RiakPid) -> csd_user_store:fetch(RiakPid, UserId) end).

save_user(User) ->
  pooler:use_member(fun(RiakPid) -> csd_user_store:save(RiakPid, User) end).

%% --------------------------------------------------------------------------------------
%% Vote API Function Definitions
%% --------------------------------------------------------------------------------------

get_vote(VoteId) ->
  pooler:use_member(fun(RiakPid) -> csd_vote_store:fetch(RiakPid, VoteId) end).

save_vote(Vote) ->
  pooler:use_member(fun(RiakPid) -> csd_vote_store:save(RiakPid, Vote) end).

vote_count_for_snippet(SnippetId) ->
   pooler:use_member(fun(RiakPid) ->
        csd_vote_store:count_for_snippet(RiakPid, SnippetId)
    end).

vote_count_for_snippet(SnippetId, UserId) ->
  pooler:use_member(fun(RiakPid) ->
        csd_vote_store:count_for_snippet(RiakPid, SnippetId, UserId)
    end).

```

The pattern we're applying should now be obvious. Each function just proxies the call to another module which takes all the source parameters plus a `pid` which can be used to talk to Riak.

With that out of the way, let's dive into what the individual modules do.

## <a id="storing-snippets"></a>Storing Snippets

Until now we've only ever stored snippets and we haven't really done anything complicated with them. The earlier versions of our `csd_snippet` module, the one which encapsulated the snippet functionality, contained methods which covered two concerns: construction/creation of the snippet and storing/retrieval of snippets. Rather than continuing to mix concerns, we're going to break this module up into two: `csd_snippet` and `csd_snippet_store`. The aim is for the former to act like an API to the snippet functionality. This is the one that will be invoked from our web application. The latter will be invoked by the former in the cases where data needs to be written to or read from the data store.

Hopefully now you can see where this fits into the diagram shown above. `csd_snippet` is paired with `csd_snippet_store` and `csd_db` is used as a bridge between the two which provides the connections to Riak.

### <a id="csd_snippet"></a>`csd_snippet` module

`csd_snippet` has changed drastically since we last looked at it, so let's go through the module bit by bit as it currently stands.

```
% ... snip ...

%% --------------------------------------------------------------------------------------
%% Internal Record Definitions
%% --------------------------------------------------------------------------------------

-record(snippet, {
    user_id,
    key,
    title,
    left,
    right,
    created
  }).

% ... snip ...
```

The `snippet` record is an internal container for all the data we need when dealing with a single snippet. Some of this information is stored in Riak as part of the payload, other detail is stored as an index. More to come on this later.

```
% ... snip ...

%% --------------------------------------------------------------------------------------
%% API Function Definitions
%% --------------------------------------------------------------------------------------

to_snippet(Title, Left, Right, UserId) ->
  #snippet{
    user_id = UserId,
    key = csd_riak:new_key(),
    title = Title,
    left = Left,
    right = Right,
    created = csd_date:utc_now()
  }.

% ... snip ...
```

`to_snippet/4` allows construction of snippets from basic information: `title`, `left`, `right` and `user_id`. Behind the scenes we determine the current date/time in UTC format (details of this function coming later) and store that alongside the snippet in the `created` field. We also generate a new (hopefully unique) key for the snippet at the same time.

```
% ... snip ...

fetch(SnippetKey) when is_list(SnippetKey) ->
  fetch(list_to_binary(SnippetKey));
fetch(SnippetKey) when is_binary(SnippetKey) ->
  csd_db:get_snippet(SnippetKey).

save(Snippet=#snippet{}) ->
  csd_db:save_snippet(Snippet).

list_for_user(UserId) ->
  csd_db:list_snippets(UserId).

% ... snip ...
```

These three functions are the "main" functions, so to speak. That is, the main opertions that are done with snippets are fetching, saving and listing. Each one of them simply passes the call on to `csd_db` to invoke functions on `csd_snippet_store` with a Riak connection. Details of what those functions do are coming shortly.

```
% ... snip ...

set_user_id(Snippet=#snippet{}, UserId) ->
  Snippet#snippet{
    user_id = UserId
  }.

get_user_id(#snippet{user_id=UserId}) ->
  UserId.

get_key(#snippet{key=Key}) ->
  Key.

set_key(Snippet=#snippet{}, NewKey) ->
  Snippet#snippet{
    key = NewKey
  }.

% ... snip ...
```

The functions listed above are basic get and set operations for certain pieces of information that live within the snippet. When storing snippets, we need to be able to set a secondary index value for `user_id` and given that the structure of the snippet is hidden to all outside of the `csd_snippet` module this function is required to expose the user's id.

At this point it might not be as obvious as to why we need to provide the ability to set a key on the snippet, but this will come clear later on when we look at [snippet submission](#snippet-submission).

```
% ... snip ...

to_json(#snippet{key=K, title=T, left=L, right=R, created=C}) ->
  Data = [{key, K}, {title, T}, {left, L}, {right, R}, {created, C}],
  csd_json:to_json(Data, fun is_string/1).

from_json(SnippetJson) ->
  Data = csd_json:from_json(SnippetJson, fun is_string/1),
  #snippet{
    key = proplists:get_value(key, Data),
    title = proplists:get_value(title, Data),
    left = proplists:get_value(left, Data),
    right = proplists:get_value(right, Data),
    created = proplists:get_value(created, Data)
  }.

% ... snip ...
```

The above functions are obviously used to convert snippets to and from JSON. These are used when passing snippets to the browser or for storing them in the database.

```
% ... snip ...

%% --------------------------------------------------------------------------------------
%% Private Function Definitions
%% --------------------------------------------------------------------------------------

is_string(title) -> true;
is_string(left) -> true;
is_string(right) -> true;
is_string(created) -> true;
is_string(_) -> false.

```

This last function exists as a helper function during conversion between Erlang proplists and JSON format and is used to highlight those values which are intended to be strings.

That covers off the interface to the snippet "schema", but it doesn't show how an individual snippet ends up in the database. Let's take a look at the code in the storage module `csd_snippet_store`.

### <a id="csd_snippet_store"></a>`csd_snippet_store` module

To start with, let's look at the module header including some handy defines which we'll need to dive into a little bit.

```
-module(csd_snippet_store).
-author('OJ Reeves <oj@buffered.io>').

-define(BUCKET, <<"snippet">>).
-define(USERID_INDEX, "userid").
-define(LIST_MAP_JS, <<"function(v){var d = Riak.mapValuesJson(v)[0]; return [{key:d.key,title:d.title,created:d.created}];}">>).
-define(REDUCE_SORT_JS, <<"function(a,b){return a.created<b.created?1:(a.created>b.created?-1:0);}">>).

% ... snip ...
```

The first two defines are obvious. The next two are much more interesting. Here we can see some JavaScript code that we're going to be using during map/reduce phases when searching for snippets. Given that the code above isn't that nice to read, let's expand it out to see what it's doing:

```
function(v)
{
    var d = Riak.mapValuesJson(v)[0];
    return [{ key: d.key, title: d.title, created: d.created }];
}
```

Map functions in Riak take up to 3 values:

1. The value being mapped over. If the map phase is the first of the phases then this value will be the full object pulled from Riak.
1. The key data associated with the value. This is the (optional) value that is passed in alongside the key in the input phase.
1. A value passed into the map phase which remains consistent for each value that is mapped over.

In our case, we're only interested in the first argument, the value that is coming out of Riak. We're also only interested in the contents of the value itself. We use the built-in function `Riak.mapValuesJson` to pull out the value as JSON. From that value we're only interested in the `key`, the `title` and the `created` properties. The map function much produce a list of values, so we return this new JSON object wrapped in a list.

It's not yet obvious, though it will be, but this is the function that will be used when we list all of the snippets that a single user has submitted. Next up is the reduce phase:

```
function(a, b)
{
    return a.created < b.created ? 1 : (a.created > b.created ? -1 : 0);
}
```

Those of you familiar with Riak will have noticed that this function doesn't look like a typical reduce function. In Riak the reduce phase functions have the same signature as map functions. The function shown above does not fit this description.

In our reduce phase for listing a user's snippets, we're only interested in sorting the snippets by the date in which they were submitted (most recent first). The function above takes two snippets and returns the result of the comparison based on the date. This function is used in conjunction with another built-in function, `Riak.reduceSort`. We pass in our sort comparison to the reduce phase as the argument to the phase and the built-in will execute it for each required comparison to make the resulting list of values ordered correctly.

With that out of the way, let's take a look at the first of the Erlang functions which fetches a single snippet based on its key.

```
% ... snip ...

%% --------------------------------------------------------------------------------------
%% API Function Exports
%% --------------------------------------------------------------------------------------

-export([save/2, fetch/2, list_for_user/2]).

%% --------------------------------------------------------------------------------------
%% API Function Definitions
%% --------------------------------------------------------------------------------------

fetch(RiakPid, Key) ->
  case csd_riak:fetch(RiakPid, ?BUCKET, Key) of
    {ok, RiakObj} ->
      SnippetJson = csd_riak:get_value(RiakObj),
      Snippet = csd_snippet:from_json(SnippetJson),
      UserId = csd_riak:get_index(RiakObj, int, ?USERID_INDEX),
      {ok, csd_snippet:set_user_id(Snippet, UserId)};
    {error, Reason} ->
      {error, Reason}
  end.

% ... snip ...
```

The first thing you'll notice is that the first parameter to the function is the `RiakPid` which we will use to talk to Riak. The second parameter is the `Key` (identifier) of the snippet. The function calls `csd_riak:fetch` which attempts to pull a Riak object out of Riak using the key as the id for the object to read.

If that succeeds then a valid Riak object is returned. This contains all the detail of the object as it is stored in Riak including meta data. At this point we're only interested in two things:

1. The value stored in the object (which should be the snippet data in JSON format).
1. The value of the `user_id` index which identifies the person who created the snippet.

These two values are pulled from the Riak object and are used to construct a valid snippet instance which is then returned to the caller.

We're also interested in listing snippets for a given user, so let's take a look at the code for that:

```
% ... snip ...

list_for_user(RiakPid, UserId) ->
  MR1 = csd_riak_mr:add_input_index(csd_riak_mr:create(), ?BUCKET, int,
    ?USERID_INDEX, UserId),
  MR2 = csd_riak_mr:add_map_js(MR1, ?LIST_MAP_JS, false),
  MR3 = csd_riak_mr:add_reduce_sort_js(MR2, ?REDUCE_SORT_JS),

  Result = case csd_riak_mr:run(RiakPid, MR3) of
    {ok, [{1, List}]} -> List;
    {ok, []} -> []
  end,
  {ok, Result}.

% ... snip ...
```

Here's where we are first using our new map/reduce module to help construct a valid map/reduce job which pulls out the list of snippets. The first line of the function is specifying that we're interested in all values in `?BUCKET` (the snippet bucket) which have an `int` index called `?USERID_INDEX` (the index of the submitting user's id) that is the same as the specified `UserId` passed into the function. We then take this job and add a JavaScript map phase where we pass in the `?LIST_MAP_JS` (details of which we have just seen above). Notice that we pass in `false` as the last parameter as we're not interested in returning the results of this phase from the query, we just want those values passed to the next phase.

The last of the phases is a JavaScript reduce phase that uses Riak's sorting functionality. We pass in `?REDUCE_SORT_JS` which causes the sort to happen in reverse chronological order.

With our map/reduce constructed, we execute this in Riak and check the result. The first thing to note is that we're currently not checking for errors. Right now we want the process to crash should an error occur. Later in the series we'll be looking a bit more at error handling, but for the purpose of this post it's out of scope.

The two patterns we do check for cover the two cases that may arise in normal use. When the map/reduce job runs and succeeds, the result will be in the format: `{ok, [{<phase number>, <results>}]}`. Phase numbers are zero-based. The list that is returned will only contain the results that we asked Riak to keep.

Given these conditions we can see that if the map/reduce job runs and extracts results, we can expect to see a list with one element in it which is the result of the reduce phase. Matching this to `{ok, [{1, List}]}` gives us direct access to the results in the `List` value.

If, however, there isn't any data in Riak that matches the query Riak will return no results for the phase. Hence we also need to match against this case, `{ok, []}`, and return an empty list which implies that there aren't any entries.

Now that listing snippets for the user is done, let's look at the save functionality.

```
% ... snip ...

save(RiakPid, Snippet) ->
  Key = csd_snippet:get_key(Snippet),
  case csd_riak:fetch(RiakPid, ?BUCKET, Key) of
    {ok, RiakObj} ->
      NewRiakObj = csd_riak:update(RiakObj, csd_snippet:to_json(Snippet)),
      persist(RiakPid, NewRiakObj, Snippet);
    {error, notfound} ->
      RiakObj = csd_riak:create(?BUCKET, Key, csd_snippet:to_json(Snippet)),
      persist(RiakPid, RiakObj, Snippet)
  end.

%% --------------------------------------------------------------------------------------
%% Private Function Definitions
%% --------------------------------------------------------------------------------------

persist(RiakPid, RiakObj, Snippet) ->
  UserId = csd_snippet:get_user_id(Snippet),
  UpdatedRiakObj = csd_riak:set_index(RiakObj, int, ?USERID_INDEX, UserId),
  ok = csd_riak:save(RiakPid, UpdatedRiakObj),
  {ok, Snippet}.

```

Let's start by looking at the `persist` function as this is invoked in two spots inside the `save` function. As you can see when the snippet is persisted we take the id of the user who submitted it and add a new index to the Riak object which contains this value. We then push the object into the store. Easy peasy!

The `save` function is also quite self-explanatory. It first attempts to fetch an existing object from Riak using the snippet's key as the identifier. If the value exists, this value is updated with the new snippet information. If it doesn't exist, a new Riak object is created. Both of these code paths call the `persist` function to finish the job of storing the snippet.

We're done! That's the full story of snippet storage. Let's launch the application and play with storing snippets.

```
$ make webstart
... snip ...

1> S = csd_snippet:to_snippet("The Basics", "var x = 1;", "int x = 1;", 12345).
{snippet,12345,<<"AIUWiw==">>,"The Basics","var x = 1;",
         "int x = 1;",<<"2012-06-05T21:28:20.314Z">>}
2> csd_snippet:save(S).
{ok,{snippet,12345,<<"AIUWiw==">>,"The Basics","var x = 1;",
             "int x = 1;",<<"2012-06-05T21:28:20.314Z">>}}
3> csd_snippet:fetch("AIUWiw==").
{ok,{snippet,12345,<<"AIUWiw==">>,"The Basics","var x = 1;",
             "int x = 1;","2012-06-05T21:28:20.314Z"}}
```

One thing you'll notice here is that we've added a snippet for a user with an Id of `12345`. This user _does not exist_ in Riak. In Riak you can add an index for a particular value but there is no way of adding the equivalent of a foreign key in the RDBMS world.

While we're here, let's see what Riak gives is when we talk directly to it via curl:

```
$ curl -i http://127.0.0.1:8091/riak/snippet/AIUWiw==
HTTP/1.1 200 OK
X-Riak-Vclock: a85hYGBgzGDKBVIcMRuuc/nPTpqdwZTImMfK8Our00m+LAA=
x-riak-index-userid_int: 12345
Vary: Accept-Encoding
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)
Link: </riak/snippet>; rel="up"
Last-Modified: Tue, 05 Jun 2012 21:28:26 GMT
ETag: "5E5aOqpuUZa30DpSVytdn7"
Date: Tue, 05 Jun 2012 21:37:09 GMT
Content-Type: application/json
Content-Length: 117

{"key":"AIUWiw==","title":"The Basics","left":"var x = 1;","right":"int x = 1;","created":"2012-06-05T21:28:20.314Z"}%   
```

You can see that the detail we're getting matches that which we pulled straight out of our application, including the `X-riak-index-userid_int` header which contains the Id of the user the submitted the snippet.

Everything looks in order. Next let's handle storage of votes.

## <a id="storing-votes"></a>Storing Votes

Storage of a snippet is a great thing, but it is ultimatley meaningless of people can't indicate which one they prefer. What we need to be able to do is provide the ability to vote so that users of the site can see which side of the snippet users feel is the best.

We've already discussed the approach that we're going to take. Let's dive into the code.

### <a id="csd_vote"></a>`csd_vote` module

```
-module(csd_vote).
-author('OJ Reeves <oj@buffered.io>').

%% --------------------------------------------------------------------------------------
%% API Function Exports
%% --------------------------------------------------------------------------------------

-export([
    to_vote/3,
    fetch/2,
    save/1,
    get_user_id/1,
    get_which/1,
    get_id/1,
    get_id/2,
    to_json/1,
    from_json/1,
    get_snippet_id/1,
    count_for_snippet/1,
    count_for_snippet/2,
    random_votes/2
  ]).

%% --------------------------------------------------------------------------------------
%% Internal Record Definitions
%% --------------------------------------------------------------------------------------

-record(vote, {
    user_id,
    snippet_id,
    which,
    time
  }).

-record(count, {
    left,
    right,
    which
  }).

% ... snip ...
```

Here we can see that we're following a similar pattern to what we did with snippets. We have an internal `vote` record which indicates which user submitted the vote, which snippet the vote is for, which side of the snippet they voted for (`"left"` or `"right"`) and a timestamp. Hopefully there's nothing in here that will surprise anyone.

The next record, `count`, is a little more interesting. It will make more sense after we see where it is used, but in short the purpose of this record is to group the results of a map/reduce job which counts the number of votes for a given snippet and which side the votes were for. If the search is conducted by a known (ie. logged-in user) the record will also indicate which side of the snippet they voted for (if any).

```
% ... snip ...

%% --------------------------------------------------------------------------------------
%% API Function Definitions
%% --------------------------------------------------------------------------------------

to_vote(UserId, SnippetId, Which="left") when is_integer(UserId) ->
  to_vote_inner(UserId, SnippetId, Which);
to_vote(UserId, SnippetId, Which="right") when is_integer(UserId) ->
  to_vote_inner(UserId, SnippetId, Which).

% ... snip ...
```

`to_vote` is a simple function that is used to create an instance of a vote. The interface of this function is designed to stop callers from submitting vote for anything other than `"left"` or `"right"`, as this wouldn't make sense to the system. This function calls an internal version which is defined a bit later on.

```
% ... snip ...

count_for_snippet(SnippetId) ->
  {ok, {L, R}} = csd_db:vote_count_for_snippet(SnippetId),
  {ok, #count{
    left = L,
    right = R,
    which = ""
  }}.

count_for_snippet(SnippetId, UserId) ->
  {ok, {L, R, W}} = csd_db:vote_count_for_snippet(SnippetId, UserId),
  {ok, #count{
    left = L,
    right = R,
    which = W
  }}.

% ... snip ...
```

These two functions are the magic that makes the vote counting tick. They both essentially do the same thing but for one small difference. The first, `count_for_snippet/1` takes a single parameter which is the Id of the snippet. It make a call to the `csd_db` module to kick off a map/reduce job in Riak. The result is a pair of values, `{L, R}`, where `L` is the total number of votes for the left side of the snippet and `R` is the total number for the right side. This search is done outside of the context of a known user. The result of a call to this function is a record which doesn't have a meaningful value for the `which` record member.

The second function, `count_for_snippet/2`, is the same as the first except that it also takes the identifier of the user that is conducting the search. This version of the function also calls a counterpart in `csd_db`, but the result is different in that it also contains the side of the snippet which that particular user voted for. This `which` value will be either `"left"`, `"right"` or `""`. If it's `""` then that indicates that the user hasn't voted on this snippet.

Next up we have the standard serialisation functions.

```
% ... snip ...

to_json(#vote{time=T, which=W, snippet_id=S, user_id=U}) ->
  csd_json:to_json([
      {time, T},
      {user_id, U},
      {snippet_id, S},
      {which, W}],
    fun is_string/1);

to_json(#count{left=L, right=R, which=W}) ->
  csd_json:to_json([
      {left, L},
      {right, R},
      {which, W}],
    fun is_string/1).

% ... snip ...
```

By now these functions should be self-explanatory, so we'll kick on to something more interesting.

```
% ... snip ...

fetch(UserId, SnippetId) when is_integer(UserId) ->
  csd_db:get_vote(get_id(UserId, SnippetId)).

save(Vote=#vote{}) ->
  csd_db:save_vote(Vote).

get_user_id(#vote{user_id=U}) ->
  U.

get_which(#vote{which=W}) ->
  W.

get_snippet_id(#vote{snippet_id=S}) ->
  S.

get_id(#vote{user_id=U, snippet_id=S}) ->
  get_id(U, S).

get_id(UserId, SnippetId) when is_integer(UserId) ->
  iolist_to_binary([integer_to_list(UserId), "-", SnippetId]).

% ... snip ...
```

The functions are also rather rudimentary and fit the usual pattern that we're applying across our application. The one thing to note here is that a vote doesn't have its own identifer that is generated. Instead, the key that is used to identify a vote in the `vote` bucket is a combination of the user's Id and the snippet's Id.

At this point the requirement for the accessor functions won't be clear. Keep them in mind, we'll cover them off a bit later when we look at the code that's closer to the UI.

Next up here's a typical deserialisation function.

```
% ... snip ...

from_json(Json) ->
  List = csd_json:from_json(Json, fun is_string/1),
  #vote{
    time = proplists:get_value(time, List),
    user_id = proplists:get_value(user_id, List),
    snippet_id = proplists:get_value(snippet_id, List),
    which = proplists:get_value(which, List)
  }.

% ... snip ...
```

Nothing too stellar here either. After converting the JSON back into a proplist, we're just poking the the values into our `vote` record.

```
% ... snip ...

random_votes(SnippetId, NumVotes) ->
  random:seed(erlang:now()),
  lists:map(fun(_) ->
        Which = case random:uniform(99999999) rem 2 of
          0 -> "left";
          _ -> "right"
        end,
        V = to_vote(random:uniform(99999999), SnippetId, Which),
        save(V) end, lists:seq(1, NumVotes)),
  ok.

% ... snip ...
```

The `random_votes` function is something that I decided to put in to simulate larger numbers of votes. Given that the system isn't live, I wanted to be able to generate votes for a given snippet so that I could see the affect on the UI. Leaving this function in makes sense for the benefit of my awesome reader(s) so they can see the effect themselves. Ultimately it doesn't belong in the _production_ version.

Now for the last two functions in the module.

```
% ... snip ...

%% --------------------------------------------------------------------------------------
%% Private Function Definitions
%% --------------------------------------------------------------------------------------

to_vote_inner(UserId, SnippetId, Which) ->
  #vote{
    user_id = UserId,
    snippet_id = SnippetId,
    time = csd_date:utc_now(),
    which = Which
  }.

is_string(time) -> true;
is_string(which) -> true;
is_string(snippet_id) -> true;
is_string(_) -> false.

```

`to_vote_inner` is a simple function called by `to_vote` at the top of the module. It's there just to reduce code duplication. `is_string` is the classic helper function which tells the JSON serialiser/deserialiser which values are strings and which aren't.

We're done with the handling module, next we need to dive into how these are stored.

### <a id="csd_vote_store"></a>`csd_vote_store` module

This module follows the same pattern as the snippet storage module. Let's take a look at the code, starting with the defines. This is where we start to get into more interesting map/reduce jobs.

```
-module(csd_vote_store).
-author('OJ Reeves <oj@buffered.io>').

-define(BUCKET, <<"vote">>).
-define(SNIPPET_INDEX, <<"snippetid">>).
-define(USER_INDEX, <<"userid">>).
-define(COUNT_VOTE_MAP_JS, <<"function(v){var d=Riak.mapValuesJson(v)[0];if(d.which===\"left\"){return[[1,0]];}return[[0,1]];}">>).
-define(COUNT_VOTE_RED_JS, <<"function(vals,arg){if(vals.length===0){return[[0,0]];}return[vals.reduce(function(a,v){return[a[0]+v[0],a[1]+v[1]];})];}">>).
-define(COUNT_VOTE_USER_MAP_JS, <<"function(v,k,a){var d=Riak.mapValuesJson(v)[0];var which=d.user_id===a?d.which:\"\";if(d.which===\"left\"){return[[1,0,which]];}return[[0,1,which]];}">>).
-define(COUNT_VOTE_USER_RED_JS, <<"function(vals,arg){if(vals.length===0){return[[0,0,\"\"]];}return[vals.reduce(function(a,v){return[a[0]+v[0],a[1]+v[1],a[2].length>0?a[2]:v[2]];})];}">>).

% ... snip ...
```

The first few -- `BUCKET`, `SNIPPET_INDEX` and `USER_INDEX` -- speak for themselves and probably don't need explanation. The rest of them do. These are all JavaScript map/reduce job phases condensed into single strings. Let's expand them out.

```
function(v) {
  var d = Riak.mapValuesJson(v)[0];
  if (d.which === "left") {
    return [[1, 0]];
  }

  return [[0, 1]];
}
```

As the name of the snippet suggests, this is the map phase of the job which performs a count. The first line of this function is extracting the value of the JSON object out of the Riak object as we have done in the past in other phases. Remember that each vote contains a value which indicates which side of the snippet the vote counts towards. This function checks to see which side a given vote and returns two values. Each value that is parsed in this map phase will result in either `[[1, 0]]` or `[[0, 1]]`. We'll see how this is useful after taking a look at the reduce phase.

```
function(vals, arg) {
  if (vals.length === 0) {
    return[[0, 0]];
  }

  return [vals.reduce(function(a, v) {
      return [a[0] + v[0], a[1] + v[1]];
    }
  )];
}
```

When this reduce phase is run we check the existing list of values from any previous reductions and if there aren't any we default to `[[0, 0]]`. This acts as the seed for our accumulation of values. Otherwise, we reduce across all the values that are given, which will come in the form `[[L0, R0], [L1, R1], ... [Ln, Rn]]`. During our reduction we simply add the two values in the arrays together based on index, resulting in us totalling both the number of `left` and `right` votes at the same time. We return the result again as another array of values.

When the reduce is finished we end up with a single nested array in the form `[[L, R]]` where `L` is the total number of votes cast for the `left` side and `R` is the total for the `right`.

With these two phases we now have a map/reduce job which is able to tally up all the votes for a given snippet and tell is which side was voted for.

Next up is a bit of boilerplate with the vote fetch function:

```
% ... snip ...

%% --------------------------------------------------------------------------------------
%% API Function Exports
%% --------------------------------------------------------------------------------------

-export([fetch/2, save/2, count_for_snippet/2, count_for_snippet/3]).

%% --------------------------------------------------------------------------------------
%% API Function Definitions
%% --------------------------------------------------------------------------------------

fetch(RiakPid, VoteId) ->
  case csd_riak:fetch(RiakPid, ?BUCKET, VoteId) of
    {ok, RiakObj} ->
      VoteJson = csd_riak:get_value(RiakObj),
      Vote = csd_vote:from_json(VoteJson),
      {ok, Vote};
    {error, Reason} ->
      {error, Reason}
  end.

% ... snip ...
```

There shouldn't be anything new about this fetch function at this stage. It's the same as what we've done for the snippet loader, but specific to votes. Let's take a look at something a little more interesting.

```
% ... snip ...

count_for_snippet(RiakPid, SnippetId) ->
  MR1 = csd_riak_mr:add_input_index(csd_riak_mr:create(), ?BUCKET, bin,
    ?SNIPPET_INDEX, SnippetId),
  MR2 = csd_riak_mr:add_map_js(MR1, ?COUNT_VOTE_MAP_JS, false),
  MR3 = csd_riak_mr:add_reduce_js(MR2, ?COUNT_VOTE_RED_JS),
  case csd_riak_mr:run(RiakPid, MR3) of
    {ok, [{1, [[Left, Right]]}]} -> {ok, {Left, Right}};
    Error -> Error
  end.

% ... snip ...
```

In the first line of the function we are creating a new map/reduce job. This job is passed into the `add_input_index` function which adds an input to the job with an index. This index is a _binary_ index (`bin`) called `?SNIPPET_INDEX` and we're passing in the value of `SnippetId` which will tell Riak to pull out all vote entries that have a secondary index which contains the key of the snippet.

We then add another phase to this job that contains the javascript function that maps over the votes and counts them. Here we're not interested in pulling the results of the phase so we're passing in `false` in as the last parameter. Finally we add our last phase, which is the reduce phase that counts up all the votes.

Upon executing the map/reduce job we there are a number of possible results. Just like we saw with the map/reduce job in the `snippet` module, we are able to pattern match directly against the exact format of the result because we are only expecting a single phase result.

If the result comes out in this format we just return a tuple that contains the counts for the `left` and `right` sections. If not, we just return whatever it was that came out of Riak (which should be an error).

Next we'll see a very similar function with a very slight difference.

```
% ... snip ...

count_for_snippet(RiakPid, SnippetId, UserId) ->
  MR1 = csd_riak_mr:add_input_index(csd_riak_mr:create(), ?BUCKET, bin,
    ?SNIPPET_INDEX, SnippetId),
  MR2 = csd_riak_mr:add_map_js(MR1, ?COUNT_VOTE_USER_MAP_JS, false, UserId),
  MR3 = csd_riak_mr:add_reduce_js(MR2, ?COUNT_VOTE_USER_RED_JS),
  case csd_riak_mr:run(RiakPid, MR3) of
    {ok, [{1, [[Left, Right, Which]]}]} -> {ok, {Left, Right, Which}};
    Error -> Error
  end.

% ... snip ...
```

This function differs from the previous `count_for_snippet` only in that it accepts another parameter, `UserId`, which indicates the Id of the user initiating the query. The phases are included in the same way, but the functions invoked are `UserId`-aware. The result varies from before in that it returns another parameter in the reduce phase result, `Which`. This value indicates which side the user voted for, if at all.

Lastly, we're back to a bit more boilerplate.

```
% ... snip ...

save(RiakPid, Vote) ->
  VoteId = csd_vote:get_id(Vote),
  UserId = csd_vote:get_user_id(Vote),
  SnippetId = csd_vote:get_snippet_id(Vote),

  case csd_riak:fetch(RiakPid, ?BUCKET, VoteId) of
    {ok, _RiakObj} ->
      {error, "User has already voted on this snippet."};
    {error, notfound} ->
      RiakObj = csd_riak:create(?BUCKET, VoteId, csd_vote:to_json(Vote)),
      Indexes = [
        {bin, ?SNIPPET_INDEX, SnippetId},
        {int, ?USER_INDEX, UserId}
      ],

      NewRiakObj = csd_riak:set_indexes(RiakObj, Indexes),
      ok = csd_riak:save(RiakPid, NewRiakObj),
      {ok, Vote}
  end.
```

Here you can see the indexes being added when the item is being saved. Those indexes are the most important part, otherwise the vote won't be counted.

The only other thing that is really worth mentioning here is there is validation that the user hasn't already voted for a given snippet. My design choice here was to make it so that people can't change their mind. I reserve the right to change _my_ mind on this design later.

Let's save a vote and execute a map/reduce job to find votes to make sure our functionality works.

```
1> V = csd_vote:to_vote(12345, "ABCDE", "left").
{vote,12345,"ABCDE","left",<<"2012-07-02T09:37:21.332Z">>}
2> csd_vote:save(V).
{ok,{vote,12345,"ABCDE","left",
          <<"2012-07-02T09:37:21.332Z">>}}
3> csd_vote:count_for_snippet("ABCDE").
{ok,{count,1,0,<<>>}}
4> csd_vote:count_for_snippet("ABCDE", 12345).
{ok,{count,1,0,<<"left">>}}
```

Excellent. We can see that storage is working and that when we do map/reduce with and without the `UserId` specified we get the expected results.

That's votes done. The last thing we're going to store is a bit of user information.

## <a id="storing-users"></a>Storing Users

At this point in the game we're not interesting in too much stuff with respect to the user. We're putting this in place now because down the track we will be storing more. To start with we're just going to track the user's Twitter name, their Twitter ID (which we'll use as their ID in our system too) and the date in which they joined CSD.

### <a id="csd_user"></a>`csd_user` module

This should be routine by now. Let's take a look at the file as a whole.

```
-module(csd_user).
-author('OJ Reeves <oj@buffered.io>').

%% --------------------------------------------------------------------------------------
%% API Function Exports
%% --------------------------------------------------------------------------------------

-export([
    get_id/1,
    get_name/1,
    fetch/1,
    save/1,
    to_user/2,
    from_json/1,
    to_json/1]).

%% --------------------------------------------------------------------------------------
%% Internal Record Definitions
%% --------------------------------------------------------------------------------------

-record(user, {
    id,
    name,
    joined
  }).

%% --------------------------------------------------------------------------------------
%% API Function Definitions
%% --------------------------------------------------------------------------------------

to_user(Id, Name) ->
  #user{
    name = Name,
    id = Id,
    joined = csd_date:utc_now()
  }.

get_id(#user{id=Id}) ->
  Id.

get_name(#user{name=Name}) ->
  Name.

fetch(Id) ->
  csd_db:get_user(Id).

save(User=#user{}) ->
  csd_db:save_user(User).

to_json(#user{name=N, id=T, joined=J}) ->
  csd_json:to_json([{name, N}, {id, T}, {joined, J}], fun is_string/1).

from_json(UserJson) ->
  User = csd_json:from_json(UserJson, fun is_string/1),
  #user{
    id = proplists:get_value(id, User),
    name = proplists:get_value(name, User),
    joined = proplists:get_value(joined, User)
  }.

%% --------------------------------------------------------------------------------------
%% Internal Function Definitions
%% --------------------------------------------------------------------------------------

is_string(name) -> true;
is_string(joined) -> true;
is_string(_) -> false.
```

This entire module fits the pattern that we have already applied to both the `snippet` and `vote` functionality. Rather than waste more characters in this post I'm going to assume that you guys are able to digest this without any explanation. Ping me a comment below if you get stuck.

So what does the storage bit look like?

### <a id="csd_user_store"></a>`csd_user_store` module

It looks like this!

```
-module(csd_user_store).
-author('OJ Reeves <oj@buffered.io>').

-define(BUCKET, <<"user">>).

%% --------------------------------------------------------------------------------------
%% API Function Exports
%% --------------------------------------------------------------------------------------

-export([fetch/2, save/2]).

%% --------------------------------------------------------------------------------------
%% API Function Definitions
%% --------------------------------------------------------------------------------------

fetch(RiakPid, UserId) when is_integer(UserId) ->
  fetch(RiakPid, integer_to_list(UserId));
fetch(RiakPid, UserId) when is_list(UserId) ->
  fetch(RiakPid, list_to_binary(UserId));
fetch(RiakPid, UserId) when is_binary(UserId) ->
  case csd_riak:fetch(RiakPid, ?BUCKET, UserId) of
    {ok, RiakObj} ->
      UserJson = csd_riak:get_value(RiakObj),
      User = csd_user:from_json(UserJson),
      {ok, User};
    {error, Reason} ->
      {error, Reason}
  end.

save(RiakPid, User) ->
  IntId = csd_user:get_id(User),

  % Id is int, so we need to conver to a binary
  UserId = list_to_binary(integer_to_list(IntId)),

  case csd_riak:fetch(RiakPid, ?BUCKET, UserId) of
    {ok, _RiakObj} ->
      % user already exists, we don't need to save anything.
      {ok, User};
    {error, notfound} ->
      NewRiakObj = csd_riak:create(?BUCKET, UserId, csd_user:to_json(User)),
      ok = csd_riak:save(RiakPid, NewRiakObj),
      {ok, User}
  end.
```

User management is really easy at this stage. We're doing basic store and retrieve operations without any real complexity. After seeing the `vote` and `snippet` functionality I'm fairly certain that you'll be more than comfortable with this code.

For brevity I'm going to skip going through a sample of storing/retrieving users via the Erlang shell and move on to something completely new. But first...

## <a id="take-a-breath"></a>Take a Breath

Phew! That was quite a bit to take in. Thanks for reading this far. Posts this long do take a bit of effort to get through. If you're not scared yet you should be as we've now only covered the back-end stuff. We've still got the Webmachine end to deal with. There's a bit to cover here as too, so fill that glass back up, do some Pilates and when you're refreshed come back and dive into the next section.

Ready? Good. Here we go.

## <a id="save-user-on-login"></a>Saving User on Login

Now that we have the ability to store the details of a user the first thing we're going to do is make a call to this new functionality when a user signs in successfully. For that we need to edit the `csd_web_callback_resource` module in the `csd_web` application. This is the module that is invoked when Twitter responds via OAuth. For the most part the module is the same, except for one function which looks like this:

```
% ... snip ...

moved_temporarily(ReqData, State) ->
  ReqToken = wrq:get_qs_value("oauth_token", ReqData),
  ReqTokenSecret = wrq:get_qs_value("oauth_token_secret", ReqData),
  Verifier = wrq:get_qs_value("oauth_verifier", ReqData),

  {ok, AccessToken, AccessTokenSecret} = twitter:verify_access(ReqToken, ReqTokenSecret, Verifier),
  {ok, UserInfoJson} = twitter:get_current_user_info(AccessToken, AccessTokenSecret),
  {struct, Json} = mochijson2:decode(UserInfoJson),
  UserId = proplists:get_value(<<"id">>, Json),
  UserName = proplists:get_value(<<"screen_name">>, Json),
  NewReqData = cookie:store_auth(ReqData, UserId, UserName, AccessToken, AccessTokenSecret),

  User = csd_user:to_user(UserId, UserName),   %% -- new functionality
  {ok, _} = csd_user:save(User),               %% -- new functionality

  % TODO: error handlng for when things don't go to plan
  {{ "{" }}{true, conf:get_val(urimap, home)}, NewReqData, State}.
```

The two new lines are highlighted with comments. You can see we're just creating a new user record by specifying the id and password, and then persisting this to Riak through the `csd_user` module. Simple stuff! User details will now be persisted when the user successfully signs in. We're not yet handling the case where the user decides not to sign in, or if the process fails, but we'll get to that in a future post.

## <a id="post-sign-in"></a>Post Sign-In

To briefly recap, when a user hits our site for the first time want to ask them to sign in. Once they have done so, we know who they are and we want to show a different view. the `moved_temporarily` function above redirects them back to this page after a successful sign in. Given that we have the ability to find out who they are, we need to respond differently on the home page view. When a recognised user signs in we're going to show them a landing page with a list of the snippets that they have submitted. To do this, we're going to need to know their Twitter Id, as that's what we're using to identify the owner of a snippet.

We need to make a very slight adjustment to our main `csd_web_resource` module so that we extract the user's Id at the same time as their name.

```
% ... snip ...

to_html(ReqData, State) ->
  Content = case cookie:load_auth(ReqData) of
    {ok, {UserId, Name, _, _}} ->       %% -- this is what we changed
      csd_view:home(UserId, Name);
    _ ->
      csd_view:home()
  end,
  {Content, ReqData, State}.

% ... snip ...
```

In the [last post][Part4] we had already stored a few details about the user in their auth cookie, but we were only extracting their name. To get their Id as well we just needed to change our pattern match from `{_, Name, _, _}` to `{UserId, Name, _, _}`. We then pass this extra detail into the call to `csd_view:home` so that we can utilise that down the track. This new parameter needs to be handled by `csd_view` so let's take a look at the changes there.

```
% ... snip ...

home(UserId, Name) ->
  Params = [{logged_in, true}, {user_id, UserId}, {user_name, Name}],
  {ok, Content} = home_dtl:render(Params),
  Content.

% ... snip ...
```

The only difference here is that we're now passing `{user_id, UserId}` down to the template renderer as well as other detail. This means the view can do something useful with it. We'll go over that a bit later when we cover off the UI, but for now let's take a look at what happens when the user's "profile" page is rendered.

## <a id="user-profile"></a>User Profile

The user profile page is what is displayed when the user signs in or comes back to the site while their cookie is still valid. This page contains a list of snippets that the user submitted listed in reverse chronological order. As a sneak preview, this is what we're striving for:

{% img /uploads/2012/07/part5-user-profile.png 'User profile page' %}

Please excuse my obvious test data, but you should get the idea. Each snippet listed on the page is shown as a link which gives the user direct access to the page specific to that snippet. So let's take a look at the resource code which provides the data for this view. This is an entirely new module called `csd_web_user_detail_resource`.

I've trimmed out some of the usual cruft for brevity and am showing just the interesting bits. The full source is available on Github and linked at the bottom of the post.

```
% ... snip ...

content_types_provided(ReqData, State) ->
  Types = [
    {"application/json", to_json}
  ],
  {Types, ReqData, State}.

% ... snip ...
```

Why is this interesting? Because the resource will only serve JSON. The JSON is accessed via Ajax in the view and rendered in a custom template in the browser. More on this detail a bit later.

```
% ... snip ...

to_json(ReqData, State) ->
  PathInfo = wrq:path_info(ReqData),
  {ok, UserId} = dict:find(user_id, PathInfo),

  % We need to render a username, but don't hit the DB
  % if the user is the same as the one looking at the
  % page.
  UserName = case cookie:load_auth(ReqData) of
    {ok, {UserId, Name, _, _}} ->
      Name;
    _ ->
      {ok, UserInfo} = csd_user:fetch(UserId),
      list_to_binary(csd_user:get_name(UserInfo))
  end,

  {ok, Snippets} = csd_snippet:list_for_user(UserId),
  UserData = {struct, [
      {user_name, UserName},
      {snippets, Snippets}
    ]},
  Json = mochijson2:encode(UserData),
  {Json, ReqData, State}.

```

Here you can see that we're getting the id of the user from the URI. From the browser we're hitting this resource via a URI which takes the form of `/userdetail/<user-id>`, so we access the request data and pull the id out from the path information.

The next bit of code needs a bit of background information. When a user views a profile page the goal was to render the user's name on screen. If a user goes to their own profile page it makes more sense to not render the user's name but instead make it more personal. To do this we pass in the Twitter name of the current user as well as the user that is being viewed back to the JavaScript that made the call to the resource. If those values are the same then the view can be rendered differently.

As a result there was a need to find out who is viewing the page. So what we do is access the authentication information in the request and directly pattern match against the `UserId` that we pulled from the URI. If we get a match, then we return the name of the current user directly. This means that we can avoid going to the database as we already know the name, but if the user being viewed is different we go to Riak to pull out the name of the user.

Once we have the user name, we then list all the snippets for the user and combine those two bits information into a blob of JSON before returning this to the browser.

So now that the browser has the payload it can render the view that we saw above, including links to the snippets. What happens when a snippet is viewed? Let's a look now.

## <a id="snippet-view"></a>Snippet View

Snippet viewing is the most interesting part of the site so far (in my opinion). So before we dive into the code, let's see what it looks like when we open a snippet.

{% img /uploads/2012/07/part5-snippet-view.png 'The Snippet View' %}

Hopefully this screenshot will finally give you a vivid image as to what this application is all about. A snippet has two sides which do similar things in slightly different wants. Votes are cast by the users of the site to indicate which option they prefer. At the bottom you can see the current tally of votes, the side with the most votes is rendered in green and the side with the least is rendered in red. Both sides are rendered in blue if the the vote count is even.

When a user has voted for a given snippet, the view changes to look like this:

{% img /uploads/2012/07/part5-voted-snippet-view.png 'The Voted Snippet View' %}

When the user returns to the same snippet down the track, the view looks like this:

{% img /uploads/2012/07/part5-voted-snippet-view-return.png 'The Voted Snippet View on return' %}

While we're at it, let's take a look at the view when a user is _not_ signed in:

{% img /uploads/2012/07/part5-snippet-unknown-user.png 'The Voted Snippet View on return' %}

As you can see there are a few things going on here:

1. The main content of the snippet has to be loaded.
1. The count of votes for the snippet has to be loaded.
1. If the user is not logged in, show the vote count without any buttons which allow the user to vote.
1. If the user is logged in and hasn't yet voted, show the vote buttons.
1. Otherwise show the vote buttons.
1. When the user votes, post a vote to the server, show a confirmation message and update the vote count on screen. When the vote count is updated, the numbers should reflect any additional votes that have been cast while the user has been viewing the page.

Let's see what the snippet loading resource looks like (again, with boring stuff ommitted).

```
% ... snip ...

content_types_provided(ReqData, State) ->
  Types = [
    {"application/json", to_json}
  ],
  {Types, ReqData, State}.

% ... snip ...
```

Like before we're only providing JSON versions of the content. The client is responsible for the generation and handling of markup.

```
% ... snip ...

to_json(ReqData, State) ->
  PathInfo = wrq:path_info(ReqData),
  {ok, SnippetKey} = dict:find(key, PathInfo),
  {ok, Snippet} = csd_snippet:fetch(list_to_binary(SnippetKey)),

  {ok, Count} = case cookie:load_auth(ReqData) of
    {ok, {UserId, _, _, _}} ->
      csd_vote:count_for_snippet(SnippetKey, UserId);
    _ ->
      csd_vote:count_for_snippet(SnippetKey)
  end,

  Json = iolist_to_binary([
      "{\"snippet\":",
      csd_snippet:to_json(Snippet),
      ",\"count\":",
      csd_vote:to_json(Count),
      "}"
    ]),

  {Json, ReqData, State}.
```

The first part of this function is doing the same thing as with the user profile page. It's getting the Id of the snippet being viewed from the URI, which is in the form `/snippet/<snippet-id>`. Once this has been extracted, the body of the snippet is pulled out of Riak.

After this we then take a look to see if the user is logged in via the auth cookie. If the user is logged in we invoke the vote counting functionality with the `UserId` as a parameter so that the map/reduce job can find which side they voted for. If the user isn't known, then the other version of the vote count is executed which doesn't rely on the user's Id.

When these two bits of information have been pulled from Riak we combine them (in a rather rudimentary fashion) into a blob of JSON and return that to the client.

Again it's worth noting that we aren't handling the case where the snippet isn't found (ie. returning a [404][Http404]). We'll be covering this off in a future blog post.

From this blob of JSON the client-side code is able to infer quite a bit and can update the display to show the appropriate views depending on the state of the user and the votes. So how exactly do we handle the submission of a vote? Let's look at that now.

```
% ... snip ...

-record(state, {
    user_data = undefined
  }).

% ... snip ...

init([]) ->
  {ok, #state{}}.

allowed_methods(ReqData, State=#state{}) ->
  {['POST'], ReqData, State}.

% ... snip ...
```

When processing vote submissions we use the user information in more that one of the Webmachine overloads, so rather than pull it out of the auth cookie each time we store it in the `State` blob that is threaded through each of the functions we overload. The above code declares the record that we're using and shows that for each request we create a new one when the resource is initialised.

The other take-away from this bit of code is that we only accept `POST`s.

```
% ... snip ...

is_authorized(ReqData, State=#state{}) ->
  case cookie:load_auth(ReqData) of
    {ok, UserData} ->
      {true, ReqData, State#state{user_data=UserData}};
    _ ->
      {false, ReqData, State}
  end.

% ... snip ...
```

Given that we're processing the submission of votes we require that the user is signed in. If a non-authorised request comes in we want to return a [401][Http401]. If the user is signed in, we allow the processing to continue but we also store the user data in `State` so that it can be used in `process_post`.

```
% ... snip ...

process_post(ReqData, State=#state{user_data={UserId, _, _, _}}) ->
  FormData = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
  SnippetId = proplists:get_value("snippet", FormData),
  Which = proplists:get_value("which", FormData),
  Vote = csd_vote:to_vote(UserId, SnippetId, Which),
  {ok, _} = csd_vote:save(Vote),
  {ok, Count} = csd_vote:count_for_snippet(SnippetId, UserId),
  Json = csd_vote:to_json(Count),
  NewReqData = wrq:set_resp_header("Content-type", "application/json", wrq:set_resp_body(Json, ReqData)),
  {true, NewReqData, State}.
```

The first thing you'll notice here is that `process_post` requires a pattern-match against valid `user_data` to extract the `UserId`. If it doesn't match the process will crash. This is ok because we shouldn't ever reach this function unless the user is authorised anyway.

The body of the function is made up of a few simple steps. We parse out the content of the `POST` using `parse_qs` from Mochiweb's `mochiweb_util` module and then from that we extract `"snippet"` and `"which"` values which indicate the key of the snippet and the side of the snippet the user voted for (respectively). A new vote is then created using the extract form information and the `UserId` pulled from the authentication cookie.

This new vote is then pushed into Riak via `csd_vote:save`. You'll notice we're not interested in the content of the result other than making sure that it succeeded (ie. the first part of the return tuple is `ok`).

To facilitate the requirement that after voting the UI should be not only refreshed with that user's vote it should also contain any other votes that have been cast while the user has been on the page, we make a call to get the count of votes for the snippet. This is what we pass back to the UI so that it can be displayed.

When I first came across the `process_post` overload in Webmachine I was a little bit confused as to why the return value of the function wasn't the same as others like `to_html` or `to_json`. You'll notice that this function instead returns `true` to indicate that the `POST` has been processesd, but the setting of the content type and the body is done in a different way. It does actually make sense given that processing posts doesn't fit the same flow as with, say, a `GET`. The content type that is returned could be anything (including nothing). In our case we're returning JSON, so we use the `wrq` API to set the content of the response.

When done, we pass in the new request information as part of the return value and Webmachine does the rest. On the client side, the vote submission response is handled by the JavaScript and the vote count is updated inline (with a little animated effect, which I'll show you shortly).

We are now able to submit votes to snippets. That's all well and good, but we need to be able submit snippets before we can vote on them. Let's look at that process now.

## <a id="snippet-submission"></a>Code Snippet Submission

The first thing to note here is that we're going to use a different resource to handle submissions. This is because we want to keep our code clean. Given that we're also going to be handling POSTs on a URI _without_ a snippet key, we can easily handle this in our URI dispatch without having to resort to using guards.

Let's wade through the important bits of `csd_web_snippet_submit_resource` now.

```
% ... snip ...

%% --------------------------------------------------------------------------------------
%% Internal Record Definitions
%% --------------------------------------------------------------------------------------

-record(state, {
    user_data = undefined,
    key
  }).

is_authorized(ReqData, State=#state{}) ->
  case cookie:load_auth(ReqData) of
    {ok, UserData} ->
      {true, ReqData, State#state{user_data=UserData}};
    _ ->
      {false, ReqData, State}
  end.

% ... snip ...
```

When processing snippet submissions there are a few details we need to keep track of along the way. Firstly we need to make sure that the request is authorised, but we'll also need to use that User's information when creating the snippet so rather than process the authorisation cookie twice, we'll carry the detail along as part of the request state.

Secondly we're going to need to create a key for the new snippet since we're handling posts. In a true RESTful fashion, we should return the location of the new snippet in the `Location` response header. Given that this key is used in two Webmachine overloads we'll keep track of the key as well.

Moving on!

```
% ... snip ...

init([]) ->
  {ok, #state{}}.

content_types_accepted(ReqData, State=#state{}) ->
  Types = [
    {"application/x-www-form-urlencoded", process_form}
  ],
  {Types, ReqData, State}.

allowed_methods(ReqData, State=#state{}) ->
  {['POST'], ReqData, State}.

post_is_create(ReqData, State=#state{}) ->
  {true, ReqData, State}.

% ... snip ...
```

Those of you more familiar with Webmachine will note that when processing posts you don't actually have to override the `content_types_accepted` function and can instead simply provide an implementation of `process_post`. This is fine for when you're not interested in creating resources. But if you are interested in creation of resources, as we are, then we need to take a different path through Webmachine's state machine by implementing `post_is_create` and returning `true` as the result. We also need to define a function, which we call `process_form`, which can be invoked for form posts.

Given that we've returned `true` from `post_is_create`, we also need to provide an implementation for `create_path`, like so:

```
% ... snip ...

create_path(ReqData, State=#state{}) ->
  Key = csd_riak:new_key(),
  Path = "/snippet/" ++ binary_to_list(Key),
  {Path, ReqData, State#state{key=Key}}.

% ... snip ...
```

Here you can see that we're generating a new key for the snippet data. We generate a new path, which will contain the location of the new snippet once created, and we also save the key in `State` so that it can be used later.

Finally, all we need to do is implement `process_form` which pulls the form apart and stores the snippet data in Riak.

```
% ... snip ...

process_form(ReqData, State=#state{}) ->
  % get the detail from the form
  FormData = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
  Snippet = to_snippet(FormData, State),
  {ok, SavedSnippet} = csd_snippet:save(Snippet),
  Key = csd_snippet:get_key(SavedSnippet),

  % Return the key of the snippet as the payload
  NewBody = wrq:set_resp_body(Key, ReqData),
  NewReqData = wrq:set_resp_header("Content-type", "text/plain", NewBody),
  {true, NewReqData, State}.

%% --------------------------------------------------------------------------------------
%% Private Function Definitions
%% --------------------------------------------------------------------------------------

to_snippet(FormData, #state{key=Key, user_data={UserId, _, _, _}}) ->
  Title = proplists:get_value("title", FormData),
  Left = proplists:get_value("left", FormData),
  Right = proplists:get_value("right", FormData),
  Snippet = csd_snippet:to_snippet(Title, Left, Right, UserId),
  csd_snippet:set_key(Snippet, Key).
```

Processing the form is just the same as when we did it for votes. We tease the form apart into a dictionary and call our own `to_snippet` function which accesses the dictionary to get the important bits of the snippet detail. At the same time, it utilises the key and the Id of the user to create a proper snippet object, which is returned to the caller.

Once this has been done, the snippet is pushed into Riak and we return the new key of the snippet in plain text format back to the client. The reason I chose this approach was so that the JavaScript on the client could simply redirect to a URI based on that key. JSON parsing on the client isn't needed as a result.

With that done, we're down to the last resource modification before we wire up dispatches and cover the UI.

## <a id="logging-off"></a>Logging off

This isn't part of the core workflow but it's a nice feature to have as it makes the site feel a little more rounded/polished. We want users to be able to sign off if they want to. We need to be able to handle a `POST` without a body, and remove the user's authentication information cookie by forcing it to expire.

Firstly we need to make this slight adjustment in the `cookie` module like so:

```
% ... snip ...

remove_auth(ReqData) ->
  store_auth_cookie(ReqData, "", -1).

% ... snip ...

store_auth(ReqData, Id, Name, Token, TokenSecret) ->
  Value = mochiweb_util:quote_plus(encode(Id, Name, Token, TokenSecret)),
  store_auth_cookie(ReqData, Value, 3600 * 24 * ?AUTH_EXPIRY_DAYS).

%% --------------------------------------------------------------------------------------
%% Private Function Definitions
%% --------------------------------------------------------------------------------------

store_auth_cookie(ReqData, Value, Expiry) ->
  Options = [
    %{domain, "codesmackdown.com"},
    {max_age, Expiry},
    {path, "/"},
    {http_only, true}
  ],
  CookieHeader = mochiweb_cookies:cookie(?AUTH_COOKIE, Value, Options),
  wrq:merge_resp_headers([CookieHeader], ReqData).

% ... snip ...
```

We've created a helper function, `store_auth_cookie`, which does as it says. It stores an auth cookie in the response based on the given `Value` of the cookie and the `Expiry`. This used to be part of the `store_auth` function, but we've pulled it out into a method that can be reused. The `store_auth` function now calls this function when creating the authentication cookie like we used to. We also make a call via the `remove_auth` function, which sets the body of the token to a blank string and sets the expiry to -1 which forces the cookie to expire immediately when it hits the browser.

With this out of the way, we need to expose a resource which invokes it. Here it is without the boring bits included.

```
% ... snip ...

allowed_methods(ReqData, State) ->
  {['POST'], ReqData, State}.

process_post(ReqData, State) ->
  NewReqData = cookie:remove_auth(ReqData),
  {true, NewReqData, State}.
```

Simple right? Handle a `POST` in the usual fashion and remove the authentication token during processing. It doesnt' get easier than that.

## <a id="serving-static-content"></a>Serving Static Content

We're now at a point where we're going to be serving up some content straight from disk. This includes CSS files, JavaScript source files and HTML templates. There are a large number of ways in which we can do this.

For the sake of this blog series we're going to keep this functionality within the application. If this application were to make it to production, this approach wouldn't be used. As great as Webmachine is, serving static content at break-neck speeds isn't one of its strong points. Instead it would be better to use something which is good at this kind of thing. [Nginx][] is a good example, but there are many others.

The static file serving resource is not something that I wrote (though I've tweaked it a bit). I blatantly purloined it from somewhere on the web (quite a while ago I might add) and I can no longer find a reference to it. If anyone out there recognises it, please let me know and I shall give proper kudos/credit to the original author.

This resource works, but I say again it's not something that should be used in production.  Here it is in its entirety.

```
-module(csd_web_static_resource).
-author('OJ Reeves <oj@buffered.io>').

%% --------------------------------------------------------------------------------------
%% API Function Exports
%% --------------------------------------------------------------------------------------

-export([
    init/1,
    allowed_methods/2,
    resource_exists/2,
    content_types_provided/2,
    provide_content/2,
    file_exists/2
  ]).

%% --------------------------------------------------------------------------------------
%% Required Includes
%% --------------------------------------------------------------------------------------

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("kernel/include/file.hrl").

%% --------------------------------------------------------------------------------------
%% Record definitions
%% --------------------------------------------------------------------------------------

-record(context, {docroot, fullpath, fileinfo, response_body}).

%% --------------------------------------------------------------------------------------
%% API Function Definitions
%% --------------------------------------------------------------------------------------

init([ContentDir]) ->
  {ok, App}= application:get_application(),
  PrivDir = code:priv_dir(App),
  SourceDir = filename:join([PrivDir, ContentDir]),
  {ok, #context{docroot=SourceDir}}.

allowed_methods(ReqData, Context) ->
  {['HEAD', 'GET'], ReqData, Context}.

resource_exists(ReqData, Ctx) ->
  {true, ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
  Path = wrq:disp_path(ReqData),
  Mime = webmachine_util:guess_mime(Path),
  {[{Mime, provide_content}], ReqData, Ctx}.

provide_content(ReqData, Context) ->
  % if returns {true, NewContext} then NewContext has response_body
  case Context#context.response_body of
    undefined ->
      case file_exists(Context, wrq:disp_path(ReqData)) of
        {true, FullPath} ->
          {ok, Value} = file:read_file(FullPath),
          {Value, ReqData, Context#context{response_body=Value}};
        false ->
          {error, ReqData, Context}
      end;
    _Body ->
      {Context#context.response_body, ReqData, Context}
  end.

file_exists(Context, Path) ->
  FullPath = get_full_path(Context, Path),
  case filelib:is_regular(filename:absname(FullPath)) of
    true ->
      {true, FullPath};
    false ->
      false
  end.

get_full_path(Context, Path) ->
  Root = Context#context.docroot,
  Result = case mochiweb_util:safe_relative_path(Path) of
    undefined ->
      undefined;
    RelPath ->
      FullPath = filename:join([Root, RelPath]),
      case filelib:is_dir(FullPath) of
        true ->
          filename:join([FullPath, "index.html"]);
        false ->
          FullPath
      end
  end,
  Result.
```

There's a lot here to cover, but most of it could be understood quit easily by following the code path.

To sum it up this is how it works. The resource is configured in the dispatch list and in that list a location is specified as a parameter. This location is the folder in which the files will be located. This value is passed into the `init` function so that the resource knows the root folder to search the files for.

When a request is made the resource attempts to guess the [MIME][] type based on the file extension using a built-in Mochiweb function. It then attempts to load the file from disk and if found it returns the file content as the body of the response.

With that out of the way, now is the perfect time to wire in all the new resources in the dispatch list.

## <a id="updating-dispatch"></a>Updating Dispatch

We need to modify our `app.config` which contains our dispatch list so that it correctly routes all the URIs to the appropriate resources. Let's take a look at the updated list

```
% ... snip ...

  {csd_web,
    [
      {web,
        [
          % ... snip ...
          {dispatch,
            [
              {[], csd_web_resource, []},
              {["css", '*'], csd_web_static_resource, ["www/static/css"]},
              {["js", '*'], csd_web_static_resource, ["www/static/js"]},
              {["views", '*'], csd_web_static_resource, ["www/static/views"]},
              {["img", '*'], csd_web_static_resource, ["www/static/img"]},
              {["snippet"], csd_web_snippet_submit_resource, []},
              {["snippet", key], csd_web_snippet_resource, []},
              {["vote"], csd_web_vote_submit_resource, []},
              {["userdetail", user_id], csd_web_user_detail_resource, []},
              {["logoff"], csd_web_logoff_resource, []},
              {["oauth", "request"], csd_web_request_resource, []},
              {["oauth", "callback"], csd_web_callback_resource, []}
            ]}
        ]
      }
    ]
  }

% ... snip ...
```

The first entry is as it was before, as are the last two. There are 4 routes which use `csd_web_static_resource` to handle different URIs that point to static files on disk. This allows us to have URIs like `"/js/csd.js"` and `"css/site.css"` without us having to add another path (such as `"/static/js/csd.js"` to each). All our static content has been placed under the `apps/csd_web/priv/` and each of the static routes is relative to this folder.

The rest of the routes map directly to handlers based on a common-sense URI which should now make sense based on what we've implemented in this post.

All that we have left to discuss is the new, fandangled user interface.

## <a href="user-interface"></a>User Interface

I stand by what I said on Twitter..

> The problem with Twitter bootstrap is that everything
> now looks like Twitter bootstrap.

Despite this, I'm using Twitter bootstrap for the UI because I'm terrible at design and this was the easiest thing to use which makes me look non-terrible (though I'm sure I may have managed to make bootstrap terrible too).

The goal of this series is to cover server-side programming of web applications using an Erlang technology stack. Heavy user-interface development is beyond the scope for this already lengthy blog post, so I won't be diving into the implementation. What I will say is:

1. The front-end is quite JavaScript heavy and uses [Backbone.js][] to handle routing, models and view rendering.
1. URIs make use of the hashtag quite a bit so that links can still be used to access particular snippets directly.
1. For client-side template rendering I'm using [Handlebars][] which gives me really simple and relatively quick JavaScript template management.
1. Handlebar templates are loaded via ajax calls as required.
1. [jQuery][] is used heavily. What a surprise.

So with all this in mind, and with the [source of the UI readily available][UiSource] for your review, check out the application in action via this little video. It shows the sign-in process, user profile view, adding of new snippets and voting on existing snippets.

Feel free to go to [Vimeo](http://vimeo.com/45499170) and watch it full screen.

<iframe src="http://player.vimeo.com/video/45499170" width="500" height="331" frameborder="0" webkitAllowFullScreen mozallowfullscreen allowFullScreen></iframe> <p><a href="http://vimeo.com/45499170">Code Smackdown</a> from <a href="http://vimeo.com/thecolonial">OJ Reeves</a> on <a href="http://vimeo.com">Vimeo</a>.</p>

That should finally give you an idea of what we're trying to achieve. It's far from perfect and there's plenty more to be done which will see us through another number of parts in this series.

## <a id="known-issues"></a>Known Issues

In the interest of keeping it real it's worth highlighting a few things that we need to address as we progress through development.

* Some IDs that are generated might come out with slashes in them. When this happens the site is unable to render the page for the snippet. The easy solution to this is to replace all slashes with something else but that's not really what I'd like to do. In a future post we're going to revamp the ID generation part of the application to use something a little more sensible (learning opportunities there for everyone, most of all me).
* I was recently contacted by a nice bloke by the name of _Juan Felipe Garcia Catalan_ who had done me the honour of following [Part 4][] in fine detail. He had decided to try the implementation OAuth with another provider an found that it didn't work. It appears that Twitter's OAuth functions slightly differently. A future post will address this problem so that the OAuth integration works with other OAuth providers too. Thank you Juan for letting me know!
* The sign in process doesn't handle cases where OAuth fails or the user says "no" to signing in.
* In general, handling failures isn't covered. This will happen over the course of future posts.

## <a id="finished"></a>Finished!

This post, to date, is the longest one I've written. Thanks for sticking with me. I hope you've learned something or at least enjoyed reading. Please let me know in the comments if I've made any mistakes. If you have ideas on how to better implement anything I'm all ears and would love to hear them. Feel free to point out my crappy typos, grammar issues etc as well.

Thanks again. Until next time!

**Note:** The code for Part 5 (this post) can be found on [Github][Part5Code].

Other parts in this series: [Part 1][], [Part 2][], [Part 3][], [Part 4][]

  [Part 1]: /posts/webmachine-erlydtl-and-riak-part-1/ "Wembachine, ErlyDTL and Riak - Part 1"
  [Part 2]: /posts/webmachine-erlydtl-and-riak-part-2/ "Wembachine, ErlyDTL and Riak - Part 2"
  [Part 3]: /posts/webmachine-erlydtl-and-riak-part-3/ "Wembachine, ErlyDTL and Riak - Part 3"
  [Part 4]: /posts/webmachine-erlydtl-and-riak-part-4/ "Wembachine, ErlyDTL and Riak - Part 4"
  [Part5Code]: https://github.com/OJ/csd/tree/Part5-20120830 "Source code for Part 5"
  [Twitter]: http://twitter.com/ "Twitter"
  [OAuth]: http://oauth.net/ "OAuth"
  [Erlang]: http://erlang.org/ "Erlang"
  [Webmachine]: http://www.basho.com/developers.html#Webmachine "Webmachine"
  [JSON]: http://json.org/ "JavaScript Object Notation"
  [Riak]: http://www.basho.com/developers.html#Riak "Riak"
  [ErlyDTL]: http://github.com/evanmiller/erlydtl "ErlyDTL"
  [Rebar]: http://www.basho.com/developers.html#Rebar "Rebar"
  [mochijson2]: https://github.com/mochi/mochiweb/blob/master/src/mochijson2.erl "Mochiweb's json module"
  [Mochiweb]: https://github.com/mochi/mochiweb "Mochiweb"
  [OTP]: http://en.wikipedia.org/wiki/Open_Telecom_Platform "Open Telecom Platform"
  [cURL]: http://curl.haxx.se/ "cURL homepage"
  [WebmachineRedirects]: http://buffered.io/posts/redirects-with-webmachine/ "Redirects with Webmachine"
  [wrq]: http://wiki.basho.com/Webmachine-Request.html "Request data"
  [MapRed]: http://wiki.basho.com/MapReduce.html "Riak Map/Reduce"
  [series]: http://buffered.io/series/web-development-with-erlang/ "Web Development with Erlang"
  [Nginx]: http://nginx.org/ "Nginx"
  [Twitter bootstrap]: http://twitter.github.com/bootstrap/ "Twitter Bootstrap"
  [Handlebars]: http://handlebarsjs.com/ "Handlebars templating"
  [Backbone.js]: http://documentcloud.github.com/backbone/ "Backbone.js"
  [Secondary Index]: http://wiki.basho.com/Secondary-Indexes.html "Secondary Indexes in Riak"
  [VIM]: http://www.vim.org/ "VIM"
  [gen_server]: http://www.erlang.org/doc/man/gen_server.html "Erlang gen_server"
  [Pooler]: https://github.com/OJ/pooler "Pooler"
  [jQuery]: http://www.jquery.com/ "jQuery"
  [UiSource]: https://github.com/OJ/csd/tree/Part5-20120830/apps/csd_web/priv/www/static "User Interface Source"
