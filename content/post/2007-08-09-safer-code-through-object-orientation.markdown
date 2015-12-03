---
categories:
- Software Development
- C#
comments: true
date: 2007-08-09T00:00:00Z
title: Safer Code through Object-Orientation
---

In my current position I spend a lot of time battling against a fairly poorly-written C++ code base. The code, while <em>technically</em> written in C++, is actually more of a C-like "splat" with a few classes thrown in. Since I began working on this project I've seen many cases where proper <a href="http://en.wikipedia.org/wiki/Object-oriented_programming" title="Object-oriented programming">object-orientation</a> would have made a drastic improvement to the quality of the code.

And it's these cases which are the inspiration for this blog post.

<!--more-->

Objects are not only good for encapsulation and making more readable/maintainable code. They can also be used to improve <em>code safety</em>. Rather than try and explain through words what I mean by 'code safety', let's work through a couple of examples.

<h3>Example 1: Data Integrity in a Multithreaded Application</h3>
Let's say that we're working on a <a href="http://en.wikipedia.org/wiki/Thread_(computer_science)" title="Threading">multithreaded</a> application. We have a set of data that is constantly read and written across the different threads. We obviously don't want to have different threads writing to the dataset at the same time, as this may cause all kinds of issues with the data's integrity. We want to put a mechanism in place to ensure that the data's integrity is never put at risk. We can do this simply by 'locking' the sections of code that perform the data writes via a synchronisation technique such as a <a href="http://en.wikipedia.org/wiki/Critical_section" title="Critical section">critical section</a>, a <a href="http://en.wikipedia.org/wiki/Semaphore_(programming)" title="Semaphore">semaphore</a> or a <a href="http://en.wikipedia.org/wiki/Mutex" title="Mutex">mutex</a>. For the sake of this example, we'll use a critical section. We'll wrap it up in an object for easy use. Let's pretend that we have a C++ critical section class that looks like this:
```
class CriticalSection
{
public:
  CriticalSection();
  ~CriticalSection();

  bool Lock();
  bool Unlock();

  // ...
};
```


To use the critical section object, all we need to do is this:
```
CriticalSection importantDataLocker;
// ...
if( importantDataLocker.Lock() )
{
  // do stuff to important data
  // ...

  importantDataLocker.Unlock();
}
```


Easy enough isn't it! We'll use this code in our data writer object, so that we can make sure that all of our threads are playing nicely while writing the data. During the course of our data writing, we might want to do some other processing that is not related to writing data. Even though this task isn't related to writing the data out, we still retain the lock so that we can finish off the job after without having to unlock and relock. So we may have some code that looks like this:

```
bool DataWriter::WriteFunkyData( FunkyData* data )
{
  if( m_dataLocker.Lock() )
  {
    WriteABitOfData( data );
    DoSomeProcessing( data );
    WriteSomeMoreData( data );
    // ...
    m_dataLocker.Unlock();
    return true;
  }
  return false;
}
```


Not really rocket science is it? The above code makes sure that all the code between <em>Lock()</em> and <em>Unlock()</em> is only run when no other threads are requesting a lock (or own a lock) on the critical section.

Unfortunately we have a problem. In a non-perfect world (like the one we live in) there's always a chance that somewhere between the call to <em>Lock()</em> and the call to <em>Unlock()</em> an error might occur. An exception may be thrown. The code might just <em>return;</em>. If that's the case, the critical section will <strong>not be unlocked</strong> because the <em>Unlock()</em> function won't be called before the function finishes.

The result? A critical section that's permanently locked, and all future <em>Lock()</em> requests will either hang or fail. This is not ideal. If we were using system-wide mutexes we'd do some serious damage! Cleary this isn't acceptable.

Most <a href="http://en.wikipedia.org/wiki/Procedural_programming" title="Procedural programming">procedural</a> programmers will adjust the code in the following manner:

```
bool DataWriter::WriteFunkyData( FunkyData* data )
{
  if( m_dataLocker.Lock() )
  {
    if( !WriteABitOfData( data ) )
    {
      m_dataLocker.Unlock();
      return false;
    }

    try
    {
      DoSomeProcessing( data );
    }
    catch(...)
    {
      m_dataLocker.Unlock();
      return false;
    }

    if( !WriteSomeMoreData( data ) )
    {
      m_dataLocker.Unlock();
      return false;
    }
    // ...
    m_dataLocker.Unlock();
    return true;
  }
  return false;
}
```


Some of you might be laughing at the above code. And rightly so! But don't think that it's uncommon because it's <strong>not</strong>. As you can imagine, the code gets bigger, nastier and tougher to maintain. As the function increases in size, code duplication increases and the chance of making mistakes increases. What if a new maintenance coder decides to early-out without unlocking? You're still stuck with the same problem.

Further use of object-orientation is what's required to solve this problem. Let's use and abuse a couple of the core features of objects: <a href="http://en.wikipedia.org/wiki/Constructor_%28computer_science%29" title="Constructor">construction</a> and <a href="http://en.wikipedia.org/wiki/Destructor_%28computer_science%29" title="Destructor">destruction</a>:<ul><li>When an object is instantiated, it's <em>constructor</em> is called.</li><li>When an object is destroyed, it's <em>destructor</em> is called</li></ul>For those of you who don't know, objects are destroyed in two ways:<ol><li>When they go out of scope.</li><li>When they are <a href="http://en.wikipedia.org/wiki/Operator_delete" title="Operator delete">deleted</a>.</li></ol>
Each function in C++ has it's own scope. As soon as the function is finished, the scope is no longer valid and any local variables are cleaned up. This happens when ...<ul><li>... program execution reaches the end of the function.</li><li>... a <em>return</em> statement is reached.</li><li>... a exception is thrown that is not caught.</li></ul>
If there was an object that was able to lock the critical section when it's constructed and unlock the same critical section when it's destroyed, we could use it to make sure that the critical section is unlocked <strong>regardless</strong> of the way in which the function is exited. That was a bit of a mouthful, so to demonstrate the point have a look at the following code:
```
class CriticalSectionLocker
{
public:
  CriticalSectionLocker( CriticalSection& cs )
  : m_criticalSection( cs )
  {
    m_isLocked = m_criticalSection.Lock();
  }
  ~CriticalSectionLocker()
  {
    if( m_isLocked )
    {
      m_criticalSection.Unlock();
    }
  }
  bool IsLocked()
  {
    return m_isLocked;
  }
private:
  bool m_isLocked;
  CriticalSection& m_criticalSection;
};
```


If we use this object instead of the verbose and repetitive method shown above, our code would look like this:

```
bool DataWriter::WriteFunkyData( FunkyData* data )
{
  CriticalSectionLocker csl( m_dataLocker );
  if( csl.IsLocked() )
  {
    if( !WriteABitOfData( data ) )
    {
      return false;
    }

    DoSomeProcessing( data );

    if( !WriteSomeMoreData( data ) )
    {
      return false;
    }
    // ...
    return true;
  }

  return false;
}
```


This code is substantially easier to maintain. As soon as <em>csl</em> goes out of scope, the critical section will be unlocked automatically via the destructor. This will happen on early-outs <em>and</em> when exceptions are thrown. Job done!

<h3>Example 2: Flag/Value Toggling</h3>
Developers have been known to write code which relies on object state to function differently. While this might not be considered 'best practice', it's fairly common out in the wild. There are times where a developer may want to temporarily toggle a flag, or change the value of a variable, and change it back before the rest of the code is run. Here's an example (it's not real-life, but it should give you the idea):
```
void Person::GotToParty( NightClub* nc )
{
  if( !nc->AllowsEntry( this ) )
  {
    // temporarily pretend we're older
    // so we can go clubbing!
    int backupAge = m_Age;
    m_Age = 25;

    if( nc->AllowsEntry( this ) )
    {
      this->Party( nc, HARD_BUT_SUBTLE );
    }

    // best restore our age now
    m_Age = backupAge;
  }
  else
  {
    this->Party( nc, HARD );
  }
}
```


Don't laugh, we've all been there! :)

It should be obvious that the code above suffers from the same shortcomings as the previous example. It's not resiliant against early-outs or exceptions. A similar type of construct/destruct mechanism can be used to reset the original value of a variable in a much safer way. An example class might look like this:
```
template< typename T >
class ValueToggler
{
public:
  ValueToggler( T& variableToToggle, T tempValue )
  : m_toggleVar( variableToToggle ),
    m_originalValue( variableToToggle )
  {
    m_toggleVar = tempValue;
  }

  ~ValueToggler()
  {
    m_togglerVar = m_originalValue;
  }
private:
  T& m_toggleVar;
  T m_originalValue;
};
```


The object above is templated so that it can be used with a variety of types (eg. int, double, float). Using an object such as this, the example program code would change to the following:
```
void Person::GotToParty( NightClub* nc )
{
  if( !nc->AllowsEntry( this ) )
  {
    ValueToggler<int> vt( m_Age, 25 );

    if( nc->AllowsEntry( this ) )
    {
      this->Party( nc, HARD_BUT_SUBTLE );
    }
  }
  else
  {
    this->Party( nc, HARD );
  }
}
```


Again, the code is smaller and a lot more bulletproof. The value of <em>m_Age</em> will be restored no matter what happens inside the function.

<h3>Conclusion and Final Thoughts</h3>
The above samples should show you how easy it is to use objects to help you write more defensive code. Though some people (usually the procedural programmers ;) ) would say that this kind of code is a hack, it's my view that they're wrong. The code is clear, easy to maintain, and very safe.

<em>Disclaimer:</em> The code above hasn't been compiled, so it may contain errors. The point of the post is to show you the idea behind using objects for safety. Ideal implementations are beyond the scope of the article.

Thoughts and feedback are always welcome.
