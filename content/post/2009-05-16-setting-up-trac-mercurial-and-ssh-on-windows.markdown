---
categories:
- HOWTO
- Software
- Software Development
- Tips/Tricks
- Windows
- Mercurial
comments: true
date: 2009-05-16T00:00:00Z
tags:
- Windows
- Mercurial
- hg
- ssh
- trac
title: Setting up Trac, Mercurial and SSH on Windows
---

<strong>WARNING - This blog post is long :)</strong>
<em>This post has been edited since it was published. Please see the end of the article for any notes/modifications</em>

<h3>Some Background Info</h3>
I had the need to do this for work recently. It was nothing short of a right royal pain in the butt. It was such a pain, in fact, that I have decided to document what I had to do to get it working so that other poor unfortunates will feel less pain if they have to do this themselves.

Almost regardless of the company and the software I'm working on, I use <a href="http://www.selenic.com/mercurial/" title="Mercurial">Mercurial</a> for source code control. For the work I am doing at the moment, I was also using hg because the company I am involved with is relatively new and they hadn't yet sorted out a plan for version control or <acronym title="Application Lifecycle Management">ALM</acronym>. It was working quite well and I was pushing all my changes to my <a href="http://www.google.com.au/url?q=http://www.buffalotech.com/products/network-storage/terastation/terastation-iii/&ei=fioOSuLvGpu8swPl56zqAg&sa=X&oi=smap&resnum=1&ct=result&cd=2&usg=AFQjCNHXKMchy5tR-a-jMOFAbtxoWzfedA" title="Terastation">NAS box</a> to make sure I had other copies backed up, etc. I was <em>living the dream</em> :)

<!--more-->

The problem, though, was that this setup was working fine for me as a sole developer, but wasn't solving the problem for the other developers. There are two other people involved in development, and until now most of the work they have been doing has been in a different area to the bits I have been doing. Sharing and merging code was done by email patches. Those developers weren't using version control as far as I'm aware.

So given the volume of changes and the number of merges that were happening, it was well past time to get something proper in place that we could all use. The original plan was to go with an installation of <a href="http://en.wikipedia.org/wiki/Team_Foundation_Server" title="Team Foundation Server">TFS</a> to handle all of these issues for us. Personally, I am not a huge fan of TFS. The experience I've had with it so far hasn't been great. It also has a shocking effect on the speed of my IDE (I seem to have no choice but to use integration with the IDE, which again isn't great).

To cut this part of the story short, we had issues getting TFS to work. We tried installing a few times, tweaking here and there, but we never managed to get it to work. I know I could have called a TFS guru, such as <a href="http://stevennagy.spaces.live.com/" title="Steven Nagy">Snagy</a> to pick his brains and perhaps ask him nicely to help out, but he's a busy man and there's no guarantee that we wouldn't have to start again from scratch. I wasn't really up for that, and neither was my boss.

So I told him that I thought it might just be easier to get a server running with Mercurial for the version control, and <a href="http://trac.edgewall.org/" title="The Trac Project">Trac</a> for issue tracking and project "stuff". Not only would it mean that I could just push my local repository to the server and have the entire history transferred (TFS would have been latest version only) but it would save us a substantial cost in license fees, remove the need for VS integration, and everyone could be up and running with a <acronym title="Distributed Version Control System">DVCS</acronym> in no time.

My boss, being the legend he is, gave me the thumbs up! Awesome.

So the rest of this post is dedicated to what was involved in getting things set up. If you're not interested, then go read <a href="/?random" title="Random post">something else</a> :)

<h3>The need for a bit of security</h3>
Needless to say, the <acronym title="Intellectual Property">IP</acronym> that we're building is something that my employer is rather protective of! And rightly so. Hence it won't come as a surprise to hear that security of this information is very important. I decided that having Mercurial run over plan old HTTP would be a bad idea. I was keen to have it tunnel via SSH, and have all clients authenticate using their own private keys. Hence, getting an SSH server was going to be part of the setup.

<h3>Web server</h3>
IIS is already installed on the server, and running another web server seemed like overkill. I didn't want to expose <a href="http://trac.edgewall.org/wiki/TracStandalone" title="Trac Standalone">tracd</a> directly over the web as it doesn't support SSL, so I wanted to get it running under IIS instead. This added another little bit of complexity to the install.

<h3>What to download</h3>
There were quite a few bits that I needed to download to get this working, they're listed below. At first it might not be obvious as to why these things are needed, just trust me :)

First, server-side components:
<ol>
<li>Python 2.5.4 - Get this version, not anything earlier or later if you want this guide to work! (<a href="http://www.python.org/ftp/python/2.5.4/python-2.5.4.msi" title="Download Python">download</a>)</li>
<li>Setuptools (<a href="http://pypi.python.org/packages/2.5/s/setuptools/setuptools-0.6c9.win32-py2.5.exe#md5=602d06054ec1165e995ae54ac30884d7" title="Download setuptools">download</a>)</li>
<li>ClearSilver (<a href="http://www.clearsilver.net/downloads/win32/clearsilver-0.10.4-py2.5-win32.egg" title="Download ClearSilver">download</a>)</li>
<li>PySqlite (<a href="http://initd.org/pub/software/pysqlite/releases/2.4/2.4.1/pysqlite-2.4.1.win32-py2.5.exe" title="Download PySqlite">download</a>)</li>
<li>flup (<a href="http://www.saddi.com/software/flup/dist/flup-0.5-py2.5.egg" title="Download flup">download</a>)</li>
<li>Trac installer v0.11.4 (<a href="http://ftp.edgewall.com/pub/trac/Trac-0.11.4.win32.exe" title="Download Trac">download</a>)</li>
<li>Mercurial (<a href="http://mercurial.berkwood.com/binaries/Mercurial-1.2.1.exe" title="Download Mercurial">download</a>)</li>
<li>Subversion (<a href="http://www.collab.net/servlets/OCNDirector?id=CSVN1.6.2WINC" title="Download Subversion">download</a>)</li>
<li>Apache Tomcat AJP Isapi filter (<a href="http://www.apache.org/dist/tomcat/tomcat-connectors/jk/binaries/win32/jk-1.2.28/isapi_redirect-1.2.28.dll" title="Download isapi filter">download</a>)</li>
<li>Junction (<a href="http://technet.microsoft.com/en-us/sysinternals/bb896768.aspx" title="Download Junction">download</a>)</li>
<li>CopSSH (<a href="http://sourceforge.net/project/downloading.php?group_id=69227&filename=Copssh_2.1.0_Installer.zip&a=22655277" title="Download CopSSH">download</a>)</li>
<li>Windows Server 2003 Resource Kit (<a href="http://www.microsoft.com/downloads/details.aspx?FamilyID=9D467A69-57FF-4AE7-96EE-B18C4790CFFD&displaylang=en" title="Download resource kit">download</a>)</li>
</ol>

Next, client-side:
<ol>
<li>Mercurial (<a href="http://mercurial.berkwood.com/binaries/Mercurial-1.2.1.exe" title="Download Mercurial">download</a>)</li>
<li>PuTTy (<a href="http://the.earth.li/~sgtatham/putty/latest/x86/putty-0.60-installer.exe" title="Download PuTTy">download</a>)</li>
</ol>
Quite a lot isn't it :)

<h3>Server: Setting up the Python Environment</h3>
<ol>
  <li>Execute the Python installer. This by default installs Python to C:\python25.</li>
  <li>Modify the environment variables so that the Python install folder is included in the PATH.</li>
  <li>Execute the Setuptools installer.</li>
  <li>Execute the PySqlite installer.</li>
  <li>Install the downloaded python eggs:</li>
  <ol>
    <li>Open a command prompt</li>
    <li>Navigate to the folder where your .egg files are downloaded to.</li>
    <li>in the command line, type: <strong>easy_install [filename.egg]</strong>.</li>
    <li>Do this for ClearSilver and flup.</li>
  </ol>
</ol>
At this point, you should have a working installation of Python 2.5.4 with the required eggs and plug-ins.
<h3>Server: Setting up Mercurial</h3>
<ol>
  <li>Execute the Mercurial installer. By default this installs to %programfiles%\Mercurial. Feel free to change this if you like.</li>
  <li>During the installation, the installer will ask if you want to include the install folder in the system PATH. Make sure you click "yes", otherwise you'll have to do it manually afterwards.</li>
</ol>
The Mercurial client is now installed. Later on when installing Trac and getting it to work with Mercurial, I had issues because the Mercurial doesn't install the Mercurial bindings for Python. No only that, trying to use <em>easy_install</em> to install it didn't work because I didn't have Visual Studio 2003 installed on the server (nor did I want to install it!). It turns out that you can get round the issue by doing this:
<ol>
  <li>Go to the Mercurial installation folder using Windows Explorer.</li>
  <li>Locate the file <strong>Library.zip</strong>.</li>
  <li>Open this file with an application like <a href="http://rarsoft.com/" title="WinRAR">WinRAR</a> or <a href="http://www.7-zip.org/" title="7-zip">7-zip</a> (using the built-in Windows zip functionality doesn't work!).</li>
  <li>Inside that archive there is a folder called <em>mercurial</em>. Extract this folder to a disk.</li>
  <li>Open another Windows Explorer and navigate to your Python installation directory (eg. C:\Python25). Open up the <em>Lib</em> sub0folder (that's <em>lib</em>, not <em>lib<strong>s</strong></em>).</li>
  <li>Copy or move the extracted <em>mercurial</em> folder to the <em>lib</em> folder.</li>
</ol>
Done! You've just "installed" the Mercurial bindings for Python. Later down the track, Trac will not crap out when you try and use it!
<h3>Server: Setting up TracMercurial</h3>
Given that we're using Mercurial for the back-end version control, we're going to need to set up and install the Mercurial plugin for Trac. This is where the need for <acronym title="Subversion">SVN</acronym> comes in. Yes, it's ironic that you need SVN to get something working with Mercurial! Just do it, ok!?
<ol>
  <li>Execute the Subversion installer and let it go through with the default installation.</li>
  <li>Make sure that the path to the SVN binaries is included in the system PATH environment variable.</li>
  <li>Open up a command prompt and change directory to a temporary location.</li>
  <li>run the command: <strong>svn co http://svn.edgewall.com/repos/trac/sandbox/mercurial-plugin-0.11</strong> - this gets the right version of the plug-in from source.</li>
  <li>run the command: <strong>cd mercurial-plugin-0.11</strong></li>
  <li>run the command: <strong>python setup.py bdist_egg</strong> - this creates an installable python egg.</li>
  <li>run the command: <strong>python setup.py install</strong> - this installs the plug-in to the global location.</li>
</ol>
The TracMercurial plug-in is now installed. We'll need to configure it slightly when we've installed Trac. That information is listed in the next section.

Next we need to create a repository which will be used to house our source code. We'll pretend we're creating a project called "Slartibartfast".
<ol>
  <li>Create a new folder on the file system somewhere meaningful, such as <strong>C:\Repos</strong>.</li>
  <li>Open a command window, and change directory to that folder.</li>
  <li>create a folder for your project: <strong>mkdir Slartibartfast</strong>.</li>
  <li>Change to that folder: <strong>cd Slartibartfast</strong>.</li>
  <li>Initialise the repository: <strong>hg init .</strong> (note the '.' at the end).</li>
</ol>
Your repository is now ready for code!

<h3>Server: Setting up Trac</h3>
We'll start with the basic install:
<ol>
  <li>Execute the Trac installer.</li>
  <li>Default installation settings are fine.</li>
  <li>It will be installed to the <em>scripts</em> folder under the Python installation folder.</li>
</ol>
At this point it's a good idea to also add the <em>scripts</em> folder to the system PATH. This gives us the ability to run Python scripts from anywhere.

Next up, let's create a Trac project for our Slartitbartfast project.
<ol>
  <li>Create a folder to house your Trac projects, say <strong>C:\Trac</strong></li>
  <li>Open a command window and change directory to your Trac projects folder.</li>
  <li>Create a folder for your new project: <strong>mkdir Slartibartfast</strong></li>
  <li>Change to that folder: <strong>cd Slartibartfast</strong>.</li>
  <li>Start the Trac administration application to start an interactive setup session: <strong>trac-admin . initenv</strong></li>
</ol>
  You need to fill out some values as it asks for them. Here's a sample interactive session that I did as a test run:
 
```
C:\Trac\Slartibartfast>trac-admin . initenv
Creating a new Trac environment at C:\Trac\Slartibartfast

Trac will first ask a few questions about your environment
in order to initialize and prepare the project database.

 Please enter the name of your project.
 This name will be used in page titles and descriptions.

Project Name [My Project]> Slartibartfast

 Please specify the connection string for the database to use.
 By default, a local SQLite database is created in the environment
 directory. It is also possible to use an already existing
 PostgreSQL database (check the Trac documentation for the exact
 connection string syntax).

Database connection string [sqlite:db/trac.db]>

 Please specify the type of version control system,
 By default, it will be svn.

 If you don't want to use Trac with version control integration,
 choose the default here and don't specify a repository directory.
 in the next question.

Repository type [svn]> hg

 Please specify the absolute path to the version control
 repository, or leave it blank to use Trac without a repository.
 You can also set the repository location later.

Path to repository [/path/to/repos]> C:\Repos\Slartibartfast

Creating and Initializing Project
 Installing default wiki pages

**
** craploads of import statements go here **
**

---------------------------------------------------------------------
Warning: couldn't index the repository.

This can happen for a variety of reasons: wrong repository type,
no appropriate third party library for this repository type,
no actual repository at the specified repository path...

You can nevertheless start using your Trac environment, but
you'll need to check again your trac.ini file and the [trac]
repository_type and repository_path settings in order to enable
the Trac repository browser.


---------------------------------------------------------------------
Project environment for 'Slartibartfast' created.

You may now configure the environment by editing the file:

  C:\TracProjects\Slartibartfast\conf\trac.ini

If you'd like to take this new project environment for a test drive,
try running the Trac standalone web server `tracd`:

  tracd --port 8000 C:\TracProjects\Slartibartfast

Then point your browser to http://localhost:8000/Slartibartfast.
There you can also browse the documentation for your installed
version of Trac, including information on further setup (such as
deploying Trac to a real web server).

The latest documentation can also always be found on the project
website:

  http://trac.edgewall.org/

Congratulations!
```

You may notice the message: <em>Warning: couldn't index the repository.</em>
Don't be too concerned at this point, as we need to do a bit more to make sure that the Mercurial stuff is working.

Now we should test to make sure we're able to start the Trac daemon and see if the project site has been set up. We do that by executing the statement they suggested in the output:
<strong>tracd --port 8000 C:\TracProjects\Slartibartfast</strong>
Then we browse to <a href="http://localhost:8000">http://localhost:8000</a> to see if it works! If all goes well you should see a project listing. Click on Slartibartfast and you should see something like this:

<a href="/uploads/2009/05/trac_1.png"><img src="/uploads/2009/05/trac_1.png" alt="Trac Project Page - with error." title="Trac Project Page - with error." width="640" height="347" /></a>

Trac is telling us why it's not able to index the repository, it's because we haven't enabled the plug-in yet. Let's do that now.

Open up the <em>Trac.ini</em> file which is located inside the <em>conf</em> folder under the main project folder (eg <em>C:\Trac\Slartibartfast\conf\Trac.ini</em>).

First, add the following section to the ini file:

```
[hg]
# -- Show revision number in addition to the changeset hash
show_rev = yes

# -- Changeset hash format
node_format = short
# hex:   Show the full SHA1 hash 
# short: Show a shortened hash for the changesets
```

Then enable the Mercurial plugin by adding the following:

```
[components]
tracext.hg.backend.csetpropertyrenderer = enabled
tracext.hg.backend.hgdefaultpropertyrenderer = enabled
tracext.hg.backend.hgextpropertyrenderer = enabled
tracext.hg.backend.mercurialconnector = enabled
```

<em>Note:</em>Make sure that the <kbd>[hg]</kbd> and <kbd>[components]</kbd> sections don't already exist. If they do, then add the respective lines to the existing sections rather than creating new ones. For a fresh install, these sections shouldn't already exist, so you should be safe to add them.

Kill the tracd instance by press CTRL+C, then restart it (press the up arrow, then enter). Refresh your browser window and this time you should see this:

<a href="/uploads/2009/05/trac_2.png"><img src="/uploads/2009/05/trac_2.png" alt="Trac Project Page - without error." title="Trac Project Page - without error." width="640" height="372" /></a>

Excellent, our project is up and our repository is able to be read. We're done!

<h3>Server: Setting up the Trac Daemon as a Service</h3>
The tracd needs to run as a service. This is to make sure it's always running in a way that IIS can reference it. Given that we're later going to be hooking this up to IIS, we need to make sure it uses the <a href="http://tomcat.apache.org/connectors-doc/ajp/ajpv13a.html" title="AJP Reference">AJP</a> protocol. Here are the required steps:
<ol>
  <li>Install the Windows 2003 resource kit.</li>
  <li>Open a command window, and navigate to where the resource kit binaries are installed. On my system that was at <em>C:\Program Files\Windows Resource Kits\Tools</em></li>
  <li>Create a service called "Trac" by executing the following command: <strong>InstSrv Trac "C:\Program Files\Windows Resource Kits\Tools\SrvAny.exe"</strong> - (note the use of the full path is necessary for this to work).</li>
  <li>Next we need to hack the registry a little to pass the right parameters and have the tracd started.
<ol>
  <li>Open regedit.</li>
  <li>Go to <strong>HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Services\Trac</strong></li>
  <li>Create a new sub-key called <strong>Parameters</strong></li>
  <li>Inside this new sub-key, create two new values:
  </li>
<ol>
  <li><strong>Application</strong> of type String. Set the value to the full path of <em>python.exe</em>. Eg <strong>C:\python25\python.exe</strong></li>
  <li><strong>AppParameters</strong> of type String. Set the value to <strong>C:\python25\Scripts\tracd-script.py --port 8009 C:\Trac\Slartibartfast</strong> - (note the change of port number, it's there for a reason we'll cover shortly).</li>
</ol>
</ol>
<li>Open the services tool from the Control Panel, find the <strong>Trac</strong> service and start it up! Navigate to <a href="http://localhost:8009">http://localhost:8009</a> to make sure it still works.</li>
</ol>
If you get the same site as you did before, then all is going well. If not, then you revisit the steps to make sure you haven't missed anything out and that your pointing to the right location of the trac project.

Now that we know this works, we need to change the <strong>AppParameters</strong> value in the registry so that Trac fires up using the appropriate protocol. Edit the value, and append the following to the end of the existing value: <strong>--protocol=ajp</strong> - this forces the use of AJP. AJP's default port is 8009, which is why we chose this value in the first place.

Restart the Trac service. If you attempt to browse to the site now you won't get anything meaningful, but it's ready to be talked to from IIS, which brings us to the next step.

<h3>Server: Setting up Apache Tomcat ISS ISAPI Filter in IIS 6.</h3>
To get IIS to talk to Trac, we need to add an ISAPI filter that's able to translate to the AJP protocol. This is what we'll be doing in this section.

First, we prepare the filter for use:
<ol>
  <li>Create a folder to store the ISAPI file. I called mine <strong>C:\AJPConnector</strong>.</li>
  <li>Copy the Apache Tomcat AJP ISAPI filter (isapi_redirect-1.2.28.dll) into the folder you created in the previous step, and rename it to <strong>isapi_redirect.dll</strong>.</li>
  <li>Create a new text file in the same folder called <strong>isapi_redirect.properties</strong>. Open the file with a text editor and add the following content:

```
# Configuration file for the ISAPI Redirector

# The path to the ISAPI Redirector Extension, relative to the website
# This must be in a virtual directory with execute privileges
extension_uri=/AJPConnector/isapi_redirect.dll

# Full path to the log file for the ISAPI Redirector
log_file=C:\AJPConnector\isapi_redirect.log

# Log level (debug, info, warn, error or trace)
log_level=info

# Full path to the workers.properties file
worker_file=C:\AJPConnector\workers.properties

# Full path to the uriworkermap.properties file
worker_mount_file=C:\AJPConnector\uriworkermap.properties
```

</li>
<li>Create another new text file in the same folder called <strong>workers.properties</strong>. Open it, and add this content:

```
# Define 1 real worker
worker.list=trac
# Set properties for trac (ajp13)
worker.trac.type=ajp13
worker.trac.host=localhost
worker.trac.port=8009
worker.trac.socket_keepalive=0
```
</li>
<li>Create <em>another</em> new text file in the same folder called <strong>uriworkermap.properties</strong>. Open it and add the following:

```
/Slartibartfast*=trac
```

You'll notice that the project name is what I've used here. Down the track if you want more projects, you need to add more lines to this file.</li>
</ol>

Next we need to install the ISAPI filter in IIS so that it can get used. Feel free to do this on the default website if you like. I created another website in IIS running on a different port, but you don't have to. Any time I refer to <em>the website</em> I'm referring to the site you have decided to install the filter on.

We need to enable the filter as a web service extension first. To do that, we follow these steps:
<ol>
  <li>Open up the IIS manager.</li>
  <li>Right-click on "Web Service Extensions" and select "Add a new web service extension".</li>
  <li>In the resulting dialog box, click the "Add..." button and specify the file <strong>C:\AJPConnector\isapi_redirect.dll</strong>. Click "OK".</li>
  <li>Specify the name "AJPConnector" and click "OK".</li>
</ol>

Next we need to add the filter to the website.
<ol>
  <li>In IIS manager right-click on the website and view the properties.</li>
  <li>Select the "ISAPI Filters" tab.</li>
  <li>Click "Add".</li>
  <li>Give the filter the name "AJPConnector".</li>
  <li>Specify the full path to the dll: <strong>C:\AJPConnector\isapi_redirect.dll</strong>.</li>
  <li>Click "OK".</li>
  <li>Click on the "Home Directory" tab.</li>
  <li>Make sure the "Execute Permissions" is set to "Scripts and Executables".</li>
  <li>Make a note of the application pool that the web site is running under! You'll need this in a minute.
  <li>Click "OK" to close the dialog.</li>
</ol>

For the ISAPI filter to function, we need to make sure that IIS has access to the folder which the binary is stored in. To do this we need to know which account the website is running under.
<ol>
  <li>In IIS manager open the list of Application Pools.</li>
  <li>Choose the application pool that your website is running under. Rick-click and select properties.</li>
  <li>Click on the "Identity" tab. This will show you the name of the account that your site is running under.</li>
  <li>Browse to <strong>C:\AJPConnector</strong>, right-click and select "Properties", then choose the "Security" tab.</li>
  <li>Click "Add" and type in the name of the account that you found listed in the "Identity" tab in the application pool properties. You can also use the search feature if you want to.</li>
  <li>When the user has been recognised, click "OK".</li>
  <li>Select the user in the list at the top of the dialog, and make sure that they have "Full Control" selected in the list at the bottom.</li>
  <li>Click on the "Advanced" button.</li>
  <li>Select the check box that says "Replace permission entries on all child objects with entries shown here that apply to child objects". Then press "OK". This will apply the permissions to all child objects in the folder.</li>
  <li>Click "OK" to close the dialog box.</li>
</ol>

You may have noticed that in the <strong>isapi_redirect.properties</strong> file we specified an <em>extension_uri</em> property. This property specifies the path to use to get to the <strong>isapi_redirect.dll</strong> file via IIS. Notice how it's also a relative path from the root folder. Hence we need to create a virtual directory which maps to the same path.
<ol>
  <li>In IIS manager right-click on the website and select "New" then "Virtual Directory...".</li>
  <li>Fill out the wizard making sure you use the name "AJPConnector" for the directory name, and point it at <strong>C:\AJPConnector</strong>. The site also needs to be able to execute programs, not just scripts.</li>
</ol>

Restart IIS. The quickest way to do this is to type <strong>iisreset</strong> into a command prompt or into the "Run" dialog.

It's best at this point to make sure that the ISAPI filter has been loaded. To do this, simply open up the IIS manager again and browser to the sites "ISAPI Filters" tab. If all is working, then you should see something like this:

<img src="/uploads/2009/05/iis_isapi.png" alt="Working IIS ISAPI Filter" title="Working IIS ISAPI Filter" />

Notice the green arrow. If it's red and pointing downwards, then something is wrong. You need to make sure you have followed the above steps correctly. Double-check the security of the folders and make sure executables are enabled on your site.

If all is well then we can test it! Make sure your Trac service is running, then open a browser and navigate to your local IIS website. You should see the Trac Project website for Slartibartfast appear in your browswer. Yay!

It's now very easy to change IIS to function over HTTPS instead of HTTP. That's beyond the scope of this article as there are hundreds of blog entries and how-tos out there already.

<h3>Server: Setting up CopSSH</h3>
This is the final step! We need to give people access to the Mercurial repositories over SSH. For that we need to set up a functional SSH server and give them access to the repository path. On the surface these both seem like easy jobs. Unfortunately setting up SSH servers on Windows isn't pleasant and pointing everyone's home folders at a certain repository is not something that comes out of the box in Windows.

What we need to do is use the CopSSH installer to help with the "easy" installation of SSH. Then we need to use a program like Junction to provdie symbolic-link style functionality to the user's folders.

First up, run the CopSSH installer program. Follow all of the prompts and feel free to use the default installation. That was easy wasn't it! It should install an SSH server service for you. I found it quite hard to locate in the Service Management dialog, but eventually found it under the name "Openssh SSHD".

CopSSH comes with utilities which help you manage users. Those utilities are all in the programs menu and are easy to use. For me, those utilities weren't enough, because I knew that I was going to have to make sure that the folder links were set up on creation of user accounts and removed when those accounts were deleted.

It's a good move at this point to download Junction, if you haven't already, and put it in an easy to reach location. Feel free to add it to it's own folder or to system32. It's up to you. I'll assume you've put it in <strong>C:\tools</strong>.

I then created two scripts. The first lets you add users. It looks like this:

```
REM AddUser.bat
"C:\Program Files\ICW\Bin\copsshadm.exe" --command activateuser --user %1 --shell /bin/bash --passphrase %2
C:\tools\junction.exe "C:\Program Files\ICW\home\%1\Slartibartfast" C:\Repos\Slartibartfast
```

The first line of this file executes the user activation feature of CopSSH. It creates a new user from a local security login (so the users need to be accessible in Active Directory or in the local security set up). It creates a home folder for them and also creates a public/private key pair which it stores in their home folder as well. The keys have the passphrase that is passed to the script on the command line and will be used by the users to SSH in and commit changes to the repository. The public key for the user is added to the server as a recognised key which can be used to log in so we don't have to specify it manually.

The second line of the script executes the Junction program and links a folder called "Slartibartfast", located in the user's home folder, to the folder where the repository is stored.

To execute the script simply run: <strong>adduser.bat [username] [passphrase]</strong>.
Please note that during my testing, I wasn't able to use a passphrase that had spaces. This isn't due to a bug in the script. Even if you replace <em>--passphrase %2</em> with <em>--passphrase "%2"</em> it still doesn't work. It also doesn't work in the user interface tools. This appears to be a bug in CopSSH. This isn't a big issue though, just make sure you don't have spaces in your passphrase and you should be fine.

The second script is for removing users, it looks like this:

```
REM RemUser.bat
C:\tools\junction.exe -d "C:\Program Files\ICW\home\%1\Slartibartfast"
"C:\Program Files\ICW\Bin\copsshadm.exe" --command deactivateuser --user %1
```

It's obvious what this does. First it removes the symbolic link in the user's folder, then it uses CopSSH's command line tools to remove them as a valid user from the SSH server.

Before you assume that everything at this point is ready to rock, you may need to make sure that your firewall is updated so that the SSH port, 22, is open.

Congratulations, you are now finished with the server set up. Now let's get the clients working. Before you go to the next step, make sure you add a user!

<h3>Client: Setting up PuTTy</h3>
The first bit is easy, make sure that you've installed the PuTTy program and added the install path to the system's PATH environment variable.

Take a copy of the user's private key. This is generated by CopSSH and should be sitting in the user's home folder on the server. The key is usually called <strong>[username].key</strong>. Put the key on your local machine. We need to convert this key into a format that PuTTy can use.
<ol>
  <li>Fire up the puttygen.exe utility. It comes with the PuTTy program. Simply type "puttygen" into the Run dialog box and it should come up if you've added the install folder to the system PATH.</li>
  <li>Click "Load" and select the private key file you copied from the server.</li>
  <li>Enter the user's passphrase into the dialog that appears and press "OK".</li>
  <li>You should get a message stating ...
  
```
Successfully imported foreign key
(OpenSSH SSH-2 private key).
To use this key with PuTTY, you need to
use the "Save private key" command to
save it in PuTTY's own format.
```

Just press "OK".</li>
<li>Click "Save private key" and save the key in a safe location on your machine.</li>
</ol>
Your key can now be used by PuTTy.

When using SSH, you are going to have to have to specify the passphrase each time you connect to the server. This can get annoying quickly especially if you do a lot of commits. Instead of typing this in manually every time, I prefer to use the pageant.exe utility that also comes with PuTTy. This takes charge of handling the key passphrase for you.
<ol>
  <li>From the command line or Run dialog, type <strong>pageant</strong>. You see a rather bland dialog appear.</li>
  <li>Click "Add key". Browse to the folder where you have stored your new .ppk (the PuTTy version of your key) and select the key.</li>
  <li>When prompted, specify the passphrase and click "OK".</li>
  <li>You should now see an item appear in the list box. This is your private key. Pageant is now in memory and handling that key.</li>
  <li>Close pageant, but you should still see it running in the system tray.</li>
</ol>
I also find it helpful to add <em>pageant.exe</em> to my start-up folder so that it is always running when I log in to my machine. You may choose to do the same.

<h3>Client: Setting up Mercurial</h3>
If you haven't already, make sure you install Mercurial on the client machine using the default settings. Make sure you tell the installer to add the Mercurial path to the system PATH.

The last step of configuration for the client is to tell Mercurial to use the PuTTy tools when using SSH. Mercurial can be configured by a user-specific configuration file called <em>.hgrc</em>. On Windows it can also be called <em>Mercurial.ini</em>. The file is located in your home folder. If you don't know what your home folder is, simply open a command prompt and type <strong>echo %USERPROFILE%</strong> - this will tell you the path.

If you haven't set up your configuration yet, then chances are the configuration file doesn't exist. So you'll have to create it. Create a file call either <em>.hgrc</em> or <em>Mercurial.ini</em> in your home folder manually, and open it in a text editor. Here is what part of mine looks like:

```
[ui]
username = OJ Reeves <put your email here in the angle brackets>
editor = vim
ssh = plink -ssh -i "C:/path/to/key/id_rsa.ppk" -C -agent
```

The last line is the key and this is what you need to make sure it set properly. We are telling Mercurial to use the <em>plink</em> program. This also comes with PuTTy and is a command-line version of what the PuTTY program itself does behind the scenes. We also add a few parameters:
<ul>
  <li><em>-ssh</em> : Indicates that we're using the SSH protocol.</li>
  <li><em>-i "file.ppk"</em> : Specifies the location of the private key file we want to use to log in to the remote server. Change this to point to your local putty-compatible ppk private key. Make sure you user forward-slashes for the path separators as well!</li>
  <li><em>-C</em> : This switch enables compression.</li>
  <li><em>-agent</em> : This tells <em>plink</em> to talk to the <em>pageant</em> utility to get the passphrase for the key instead of asking you for it interactively.</li>
</ul>
The client is now ready to rock!

<h3>Finale</h3>
It took a while, but we got there in the end. Let's give our new-found Mercurial SSH server a spin. This should be easy as opening a command prompt at the appropriate location on disk and typing:

<strong>hg clone ssh://url.to.your.server.com/Slartibartfast</strong>

If everything has been configured properly, you should see Mercurial create a local folder called <strong>Slartibartfast</strong> which contains the repository. Of course, there won't be much in it because you've only created it recently! But you should be able to start using Mercurial to commit changes and push the committed changesets to the server just by running <strong>hg push</strong>.

<h3>The End</h3>
It took a while to figure most of this stuff out despite the documentation that exists on the web. It also took a bit of time to write this up! I hope that someone out there finds it useful.

The funny thing about all this, is that despite the pain, I found it easier to set up than TFS!

Feedback is always wanted and welcome. I hope it helps :)

<hr />
<strong>Edit 11/06/2009 :</strong> I've been using this set up for a little while and I'm now aware of an issue that I'm not currently able to get around (moreso because I can't be bothered trying to!). The issue is that you have <em>any</em> files added to the Trac system which have file names containing characters which change when URL encoded (such as spaces, which become %20 for example) the you won't be able to view them through the web interface. That goes for files attached to wiki pages, and sources files viewed in the source browser. If you have a file called "My Doc.doc" behind the scenes this setup results calls being made with the file name "My%20Doc.doc", which doesn't exist! Just be warned. If you're going to use this setup, don't add files to your source or to the wiki that contain spaces or odd characters. :)
<hr />
<strong>Edit 26/06/2009 :</strong>
Have a look at <a href="/posts/setting-up-trac-mercurial-and-ssh-on-windows/#comment-20599851" title="Jeremy's comment">Jeremy's comment</a> for a resolution to this escaping problem. Thanks for that mate!
