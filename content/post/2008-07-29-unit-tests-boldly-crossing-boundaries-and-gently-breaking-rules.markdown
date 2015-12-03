---
categories:
- Guest posts
- Software Development
comments: true
date: 2008-07-29T00:00:00Z
tags:
- unit testing
title: 'Unit Tests: Boldly Crossing Boundaries and Gently Breaking Rules'
---

For the first time ever, OJ's rants has a guest blogger! Long term friend and highly-respected geek, RobG has put together an interesting piece on something that's close to the hearts of most Geeks - Unit testing. This is his first post, and I hope it won't be the last.

Without further ado, here's Rob!

<!--more-->

Firstly, a bit of context is required: I recently read <a href="http://haacked.com/">Phils</a> post regarding <a href="http://haacked.com/archive/posts/unit-test-boundaries.aspx">Unit Test Boundaries</a>, and wrote a decent comment there asking him a question or two that were never answered by him (or anyone else for that matter). Similar questions were also being asked by other users also left unanswered, and it seems that my follow-up comment there (meaning to answer some of those) was too long to be a comment, directly resulting in this article instead!

Its highly likely that this article wont make much sense without reading the <a href="http://haacked.com/archive/posts/unit-test-boundaries.aspx">original post</a> first (so please do that), followed by Chris comment (question), which Ive copied below in case it disappears for some reason:
<blockquote cite="Chris B.">Chris B.
Alright...clearly there are a lot of sharp people here, so I'd like to ask everyone's opinion for the utterly perfect testing scenario for a couple of simple database operations.

I need to test whether a customer which exists loads successfully from the database, loaded by a unique id. Do I preload the database in a setup script for the test harness, or do I have an entire db filled with test data from a backup, or what?

I need to test whether a customer is saved successfully to the database. When I'm done, what's the best way to clean up? Do I abort a transaction context, or throw away the entire db (I've done that before), or what? What's the best way to verify that the customer is saved successfully? Direct queries or what?

I'm amenable to the theoretical argument that this article makes, but it seems that pragmatically the kind test we're saying you shouldn't write is exactly the test that is most likely to reveal problems (in my experience). I'd love to hear some other ideas.</blockquote>
Just like Chris, I'm all for the theory, but as he pointed out, pragmatically, it's a lot more like wading through thick syrup. He asked for some guidance, so I thought Id tell you what I usually do, and what has worked for me. It may or may not work for you, but at least you get to make that call: If youre a TDD practitioner, youll have hit the crux of the problem that we all end up facing eventually with unit testing: You've had to make a conscious choice to let your unit test reach into actual (not mock) data-land, because no matter how much you try and persuade yourself, you don't actually believe that your database will in fact return that customer record you're asking for until you've been able to test it for real  and I mean <em>really real</em>!

So what I've done to solve this is setup a new test project for these (crossing the line) tests. This is so that you can run them independently of the <em>faster</em> unit tests. I call them Datastore or Repository tests or something of that nature. Then you have problem number two hit you in the face like a frying pan - <strong>How can I make these tests repeatable?</strong> I know that if I add a new customer to the database in one unit test, I'm going to need some way of restoring the database to its previous clean state right? I can't have <em>"TestCustomer1"</em> running around in the database after I'm done with him, so I need to get rid of all that test data somehow.

I've heard various strategies suggested in the past - some of which involve dropping and recreating the database each time. I can't help thinking that's like taking out a sledgehammer to smash an ant, but it definitely still works. I've also tried other options where you make sure any test data you insert has specific identifying properties like negative ID values (won't help if you're using GUIDs though), or prefix/suffix specific column values with something like <em>"test_"</em> or <em>"!test"</em> and running a cleanup script at the end that clears down any lagging data. This has worked pretty well in the past, and if you stick to the rules, the cleanup script can require very little maintenance and can be quite dynamic - but still, <em>I've always wanted a more elegant solution.</em>

<strong>Finally, it hit me.</strong> It's the solution I've been using for a few projects now, and it has worked very well. Only one problem - you have to break another cardinal rule of unit testing. <em>Hrmm...OK I hear you say, what the hell, we've already crossed the line, lets see if the ogre flinches when we spit in its eye?</em>

The rule we're breaking now is that of <em>only testing ONE thing in a unit test</em>. Well, I don't like to think of it as <em>breaking</em> - more like <em>gently taking apart</em> - that implies you're being very careful about when you do it - and you've thought it through and not done it by accident.

The unit test name gives it away completely:
```
ShouldCreateAndDeleteACustomer()


At this point, I can see many people wince violently while sucking air quickly through their teeth  but let me explain. It stands to reason that you're going to implement the basic CRUD functions into your data strategy right? So why not exercise a couple of them at a time? Well, this is where my own warning bells go off! It's a bit like walking the tight-rope, it's dangerous, but if you're careful and develop enough skill, you can get to the other side without doing any damage.

Now that you've gotten over the initial shock, let me explain what the test does. For this one, you're going to exercise the <em>Create, Retrieve and Delete</em> functions in CRUD, and all of this can be done in a TDD fashion. First you exercise the code that creates the customer:
```
Assert.True(customerRepository.SaveCustomer(customer));
```

This line of code will fail (RED) because you haven't written your data access code yet right...<em>right</em>?! I use LinqToSql, but ultimately you've just got to write enough code (SPROC or otherwise) that adds the record to the table (GREEN) so you can verify it with your eyes with a Query Analyser of sorts - then <strong>delete the row manually.</strong>

Remember, you're still (<em>hopefully</em>) programming against an interface (<em>ICustomerRepository</em> for example), so it's the same call that you would make from a Mock (Moq rocks!), and you can still use your favourite IoC Container (mine is StructureMap) for dependency injection - but instead, this time you're hitting the actual database. My <em>SaveCustomer()</em> methods usually handle Create as well as Update depending on the ID field (I normally use GUIDs), but I'm trying to keep this simple.

Next you exercise the retrieval code:
```
Assert.NotNull(customerRepository.GetCustomers.WithId(customer.ID));
```

In case youre wondering, <em>WithId</em> is an extention method to <em>IQueryable</em> because LinqToSql utilises deferred execution, and <em>WithId</em> triggers the actual DB call.
Again, this will fail (RED) because you havent written your data access code yet right?! You write the code (as I said, I use LinqToSql, but you can use a call to a SPROC if you like), and <strong>viola</strong> it's (GREEN), but the record is still in the database - so you <strong>delete it manually</strong>.

Finally, you exercise the code that deletes the record:
```
Assert.True(customerRepository.DeleteCustomer.WithId(customer.ID));
```

Once again, this will fail (RED) because you haven't written the code yet right?! (Is this getting old yet?). So you write the code to delete the record and the test goes (GREEN).

If you're still a bit sceptical about your delete operation, you can run the retrieval code again and this time <em>Assert.Null</em> instead.

Now what you've done in summary is test your Create, Retrieve, Delete and Retrieve (failure) cases in one test. The downside is that you've tested more than one operation in a single test, but the upside is you've left the database perfectly neat and tidy. With a bit of extra work, you can add a few more calls (hey...why stop there?) to include the "Update" function from CRUD, and what you'll have is a neat unit test (10 to 15 lines of code) that exercises the entire "Customer" entity in your repository, and leaves no traces behind.

<strong>Now for a word of warning</strong> - try and make sure that, even if you can't obey the <em>rules</em> of unit testing, that you at least obey the <em>spirit</em> of unit testing. By that, I mean keep the footprint of your test small and specific. If you're working with the Customer entity - stick to that. If you're working with the Order entity, stick to that. Don't go writing tests that start messing with joins between the two. <strong>THAT'S WHAT YOUR "PROPER"</strong> unit tests are for - testing behaviour should not come into these - or else you're heading for a world of pain. Your mocks and interfaces will take care of behaviour. These data tests are purely there to prove that your data methods do what they say they do - <em>for real!</em>

I hope that helps Chris and any other people looking for elegant solutions to sticky problems. If it does, I might consider blogging some more of the pragmatic techniques that have worked for me and my teams in follow up posts.

Thats it from me for now  over and out!
