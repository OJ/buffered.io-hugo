---
categories:
- Merb
- Software Development
comments: true
date: 2008-04-06T00:00:00Z
tags:
- Merb
- tiny
- web development
title: Web Application Development with Merb, MySQL and Tiny
---

I've been thinking about building a web application or two for a while, but I haven't yet found the time to invest. I don't think this is going to change very soon either. I'm not the kind of person who usually likes to get stuck into something unless I know I've got a couple of hours up my sleeve so that I feel I can get something done, and this has been preventing me from doing anything because I don't ever feel like I have a couple of hours spare. Having a child means that you could have as little as 5 minutes before all hell breaks loose.

Instead of whinging about it, I've decided to go against my usual grain and take those 5 minute chunks as a chance to do something. This is the first step :)

<!--more-->

So, while I'm not going to talk about the web application that I'm going to be writing, I will be covering aspects of the development along with the tools and frameworks I've decided to use.

I've decided to give <a href="http://merbivore.com/" title="Merb">Merb</a> a whirl as the base framework for writing my application, which works off the <a href="http://www.ruby-lang.org/" title="Ruby">Ruby</a> language. I'll also look into using <a href="http://code.macournoyer.com/thin/" title="Tiny - Yet another web server">Tiny</a> as the web server, but that'll be decided down the track. I'll be using <a href="http://www.mysql.com/" title="MySQL">MySQL</a> for the database back-end. I'll also be using <a href="http://www.selenic.com/mercurial/" title="Mercurial">Mercurial</a> for my VCS, and may well push to an <a href="http://subversion.tigris.org/" title="Subversion">SVN</a> repo on my server for backups and releases.

All of my development will be done using <a href="http://www.vim.org/" title="VIM">VIM</a> as my text editor.

Don't ask me to justify my choices for any of the above :) I can't be arsed. Suffice is to say that those were chosen as a compromise on speed, ease of development, time to market, etc.

Since Merb is a new framework, I thought I'd read up on some tutorials. Unfortunately, most of the tutorials are totally out of date and don't work with the latest version of Merb. So I thought it'd be good to give back a little and write an updated tutorial on how I got Merb going with Tiny.

To start with we need to download and install a bunch of stuff. I'm using Windows for my dev environment (yeah, shut up, I can't hear your insults anyway ;)), so bear in mind that you might have to use 'sudo' when installing gems, and you might need to use 'apt-get', 'emerge' or 'dpkg' depending on which platform you're using.

Anyway, these are the steps that I took to get my environment ready:
<ol>
<li>Download and install MySQL for your platform (again, stacks of help on the web already).</li>
<li>Download and install Ruby for your platform (there are stacks of how-tos out there to help with this). If you're using Windows, make sure that you add the ruby 'bin' folder to your PATh environment variable so that you can invoke it from anywhere. Also make sure that you have enabled <a href="http://www.rubygems.org/" title="RubyGems">RubyGems</a>.</li>
<li>Open a command prompt.</li>
<li>Install Merb by typing: <em>gem install merb</em></li>
<li>Install Tiny by typing: <em>gem install tiny</em></li>
<li>Install your text editor of choice if you haven't already.</li>
</ol>

You should now be ready to go as far as your environment is concerned. Next up, let's get a basic Merb application running. Start by creating a "test" application using the command:

```
merb-gen app test
```

This should create a folder called "test" with a collection of subfolders which contain the basic Merb application. Now, to make sure that everything is working, we should invoke our merb application with the "tiny" server adapter. First, change the directory to the application directory, and the invoke merb with the appropriate adapter using the following commands:

```
cd test
merb -a thin
```

If all goes well you should see something like this appear in your console window:

```
~ Loaded DEVELOPMENT Environment...
~ Compiling routes...
~ Using 'share-nothing' cookie sessions (4kb limit per client)
~ Using Thin adapter
```

<a href="/uploads/2008/04/merb-empty-app.png" rel="lightbox[merb]"><img src="/uploads/2008/04/merb-empty-app.png" alt="Default Merb application render" title="Empty Merb Application" style="float: left; padding-right: 3px; padding-bottom: 3px;" width="120"/></a>Next, browse to <a href="http://localhost:4000">http://localhost:4000</a> and you should see something like the image on the left.

If you can see that then you're ready to rock. If you can't then you need to make sure that you've set up and installed everything properly. You should also make sure that your firewall software isn't blocking port 4000, otherwise nothing will come up!

So the application works. The more perceptive of you may have realised that there's an error displayed on screen that says:<blockquote><p>Exception:
No routes match the request, /</p></blockquote>
In Merb, just like Rails, there is a router which tells the framework which controller and action to invoke based on the URL's given. The error is telling us that it doesn't know which controller and action to invoke when we browse to the root of the site. That's no surprise because we haven't yet created a controller/action, let alone pointed the root of the site at it.

Let's do that now. Since this is a test application, let's just get a basic page rendered. Go back to your command window, make sure that your current path is still the "test" folder, then invoke the following:

```
merb-gen controller home
```

This should give us a controller skeleton ready to edit. Of the generated files, the ones we're interested in are:

```
app/controllers/home.rb
app/views/home/index.html.erb
```

The content of the controller is very basic, and should look something like the following:
```
class Home < Application

  def index
    render
  end

en
```

This is simply telling the framework to render the default view associated with the action. That is, render that which is stored in the home/index.html.erb file. The default content looks like this:
```
You're in index of the Home
```

<a href="/uploads/2008/04/merb-default-home.png" rel="lightbox[merb]"><img src="/uploads/2008/04/merb-default-home.png" alt="This is the default content of an action which is associated with a controller." title="Default Merb index action render" width="120" style="float: right; padding-left: 3px; padding-bottom: 3px" /></a>If you killed off the merb server, fire it up again and browse to <a href="http://localhost:4000/home">http://localhost:4000/home</a>, you should see the site as shown on the right.

Finally, let's make this the default location for the application when the user browses the root of the application. We do this by modifing the router configuration located in:

    config/router.rb

Open this file in your text editor, and uncomment the second-last line of the file which allows you to match the root folder to a controller and action. Specify "home" as the controller, and leave "index" specified as the action. Your router.rb file should look like this:
```
Merb.logger.info("Compiling routes...")
Merb::Router.prepare do |r|
  # RESTful routes
  # r.resources :posts

  # This is the default route for /:controller/:action/:id
  # This is fine for most cases.  If you're heavily using resource-based
  # routes, you may want to comment/remove this line to prevent
  # clients from calling your create or destroy actions with a GET
  r.default_routes

  # Change this for your home page to be available at /
  r.match('/').to(:controller => 'home', :action =>'index')
end
```

Fire up your application by browsing to <a href="http://localhost:4000">http://localhost:4000</a> again, you should be greeted with the same screenshot shown above.

Well done! You've set up a merb application using tiny, and have managed to create a very basic site with the root folder mapped to a controller action.

My next blog post in this little series will cover the creation of a database to use as the back-end of the application, along with a model which loads from the database and a basic view to render that model's information.

Feedback welcome!
