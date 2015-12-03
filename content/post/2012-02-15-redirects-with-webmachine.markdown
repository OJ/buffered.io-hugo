---
categories:
- Webmachine
- Software Development
comments: true
date: 2012-02-15T00:00:00Z
title: Redirects with Webmachine
---

[Webmachine][] is currently my favourite tool for building websites. I've been lucky enough to use it on a few things now, some commercially some personally. While working on my [Erlang web development series][ErlSeries] I had to handle the case where URLs redirected to other URLs. I found some basic documentation on this but wasn't able to find a canonical example of how to do it.

After a big of digging through blog posts and speaking to people on IRC I figured out how it was done. The goal of this post is to show how it's done for 301 (permanent) and 307 (temporary) redirects.

<!--more-->

You're probably already aware that Webmachine's goal is to make easy to build _well behaved HTTP applications_. It does this by enforcing a workflow which guides you through the HTTP process. Your job, as the developer, is to build **resources** which override certain behaviours depending on what you want to achieve.

In order to get your resource to return the appropriate redirect error codes, you need to provide custom implementations of three of four functions. Two of them are common to both 301 and 307 redirects, the developer must choose between the other two depending on the type of redirect required.

## `resource_exists` ##

This function is the first of the functions that is called by the Webmachine pipeline and expects the results in the standard format of `{<result>, ReqData, State}`. The `<result>` needs to be a boolean atom (ie. `true` or `false`). The default behaviour of this function returns `{true, ReqData, State}`. To start off the process of redirecting, this function needs to tell Webmachine that the resource doesn't exist by instead returning `{false, ReqData, State}`.

```
resource_exists(ReqData, State) ->
  {false, ReqData, State}.
```

## `previously_existed` ##

This function tells Webmachine if the resource had, for some reason, existed in the past. The return value of this function takes the same format as the `resource_exists` function, but the default behaviour returns `{false, ReqData, State}` which tells Webmachine that the resource never existed. This results in a 404 (not found) error being returned to the caller.

This isn't the behaviour we want, so instead we need to override the function and instead return `{true, ReqData, State}`.

```
previously_existed(ReqData, State) ->
  {true, ReqData, State}.
```

## `moved_temporarily` - for 307 redirects ##

If you're looking to get a 307 redirect, you need to override this function. The default return value of this function is the same as `previously_existed` which isn't going to give us what we need. Instead we need to tell Webmachine that there is another URL which should be used instead. We do this by returning `{{ "{" }}{true, <URL>}, ReqData, State}`. The URL that is returned is what Webmachine will pass back to the user along with the 307 redirect code.

```
moved_temporarily(ReqData, State) ->
  {{ "{" }}{true, "http://buffered.io"}, ReqData, State}.
```

Do not override this function if you're looking to do a permanent redirect.

This is what happens if you hit a resource with this behaviour using [cURL][].

    oj@air ~/ $ curl -v http://localhost:8000/temporary
    * About to connect() to localhost port 8000 (#0)
    *   Trying 127.0.0.1... connected
    * Connected to localhost (127.0.0.1) port 8000 (#0)
    > GET /temporary HTTP/1.1
    > User-Agent: curl/7.21.4 (universal-apple-darwin11.0) libcurl/7.21.4 OpenSSL/0.9.8r zlib/1.2.5
    > Host: localhost:8000
    > Accept: */*
    > 
    < HTTP/1.1 307 Temporary Redirect
    < Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)
    < Location: http://buffered.io/
    < Date: Wed, 15 Feb 2012 09:05:16 GMT
    < Content-Type: text/html
    < Content-Length: 0
    < 
    * Connection #0 to host localhost left intact
    * Closing connection #0

## `moved_permanently` - for 301 redirects ##

If you're aiming to deliver a 301 redirect then this is the function you should override. Again the default return value is the same as `previously_existed` (just like `moved_temporarily`) and we need to override it with the same detail as in `moved_temporarily`).

```
moved_permanently(ReqData, State) ->
  {{ "{" }}{true, "http://buffered.io"}, ReqData, State}.
```

Do not override this function if you're looking to do a temporary redirect.

This is what happens if you hit a resource with this behaviour using [cURL][].

    oj@air ~/code/riak $ curl -v http://localhost:8000/permanent
    * About to connect() to localhost port 8000 (#0)
    *   Trying 127.0.0.1... connected
    * Connected to localhost (127.0.0.1) port 8000 (#0)
    > GET /permanent HTTP/1.1
    > User-Agent: curl/7.21.4 (universal-apple-darwin11.0) libcurl/7.21.4 OpenSSL/0.9.8r zlib/1.2.5
    > Host: localhost:8000
    > Accept: */*
    > 
    < HTTP/1.1 301 Moved Permanently
    < Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)
    < Location: http://buffered.io/
    < Date: Wed, 15 Feb 2012 09:09:03 GMT
    < Content-Type: text/html
    < Content-Length: 0
    < 
    * Connection #0 to host localhost left intact
    * Closing connection #0

## That's all folks ##

For once it's short and sweet. I hope you find it useful.

[cURL]: http://curl.haxx.se/ "cURL"
[Webmachine]: http://webmachine.basho.com/ "Webmachine"
[ErlSeries]: http://buffered.io/series/web-development-with-erlang/ "Web development with Erlang"
