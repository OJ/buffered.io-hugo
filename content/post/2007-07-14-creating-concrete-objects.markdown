---
categories:
- HOWTO
- Software Development
- C#
comments: true
date: 2007-07-14T00:00:00Z
title: Creating Concrete Objects
---

Being a fan of <a href="http://en.wikipedia.org/wiki/Object-oriented_programming" title="Object-oriented Programming">OOP</a>, I tend to write a lot of object-oriented code. Coming up with a meaningful object model that behaves in an appropriate way is just as important as having a meaningful interface to your objects. A concrete object is an object that actually behaves in the manner you'd expect without any wierd side-effects, and has the same kind of attributes that you'd expect of a primitive data type.

Creating concrete data objects/classes is a good thing to do, as it reduces the probability of bugs, and crazy side-effects. It's also an important first step in writing intuitive code - which will be the topic of a later blog post.

<!--more-->

I'd like to quote one of my lecturers that I learned from during my time at <a href="http://www.uts.edu.au/" title="University of Technology, Sydney">UTS</a>...<blockquote><p>If in doubt, do as the ints do.</p></blockquote>I realise the meaning of this point isn't obvious on the surface, but with some example code it'll all become clear.

<h3>An example concrete data type - a 3D Vector class</h3>
Let's say you're writing some code to do some 3D rendering, and you're in need of a class that can handle the functionality and behaviour of Vectors in 3D space. The first and most obvious thing that you need to handle are x, y and z coordinates. Let's start with a very basic Vector3 class:
```
class Vector3
{
public:
  float X;
  float Y;
  float Z;
};
```

At this point you've got an object that gives public access to its internal workings. Can we say that's what the int datatype does? Are you able to mess around with its inner workings? No, you're not. Not just that, but the idea of public member variables breaks the whole notion of <a href="http://en.wikipedia.org/wiki/Information_hiding" title="Information Hiding">encapsulation/information hiding</a>. We need to hide the internal workings, but in doing so we remove the ability to set and get the values on the object. We need to expose some functions that will allow us to do that. Our improved version of the code might look like this:
```
class Vector3
{
private:
  // change the variable names to make it obvious that
  // they are member variables, not local or global.
  float m_X;
  float m_Y;
  float m_Z;

public:
  // getters
  float GetX();
  float GetY();
  float GetZ();

  // setters
  void SetX( float x );
  void SetY( float y );
  void SetZ( float z );
};
```

This is an improvement, as we now have control over the inner workings of the object without exposing the implementation to external classes. Now let's say that we want to be able to construct a new Vector3 object through a variety of ways, ie. in exactly the same way we can with int. For example:
```
// this is what we can do with ints:
int a( 1 );
int b = a;
int c( b );
int d, e;
d = e = c;

// we want do the same kind of things with our
// own class
Vector3 a( 1.0, 0.0, -1.0 );
Vector3 b = a;
Vector3 c( b );
Vector3 d, e;
d = e = c;
```

We need to expose some options for construction and assignment. So our next iteration might look like this:
```
class Vector3
{
private:
  // change the variable names to make it obvious that
  // they are member variables, not local or global.
  float m_X;
  float m_Y;
  float m_Z;

public:
  // construction - default to zero if nothing is passed in
  Vector3( float x = 0.0, float y = 0.0, float z = 0.0 );
  Vector3( Vector3& v );

  // assigment operator
  void operator=( Vector3& v );

  // getters
  float GetX();
  float GetY();
  float GetZ();

  // setters
  void SetX( float x );
  void SetY( float y );
  void SetZ( float z );
};
```

The above interface should do what we need it to... or should it? A more careful examination will reveal that it doesn't actually behave exactly as you'd expect. The copy constructor takes a reference to another Vector3, which <em>could be modified</em> inside the copy constructor. We have the same issue with our assignment operator. Integers do not behave this way, so we need to modify our interface a bit more to tidy it up:
```
class Vector3
{
private:
  // change the variable names to make it obvious that
  // they are member variables, not local or global.
  float m_X;
  float m_Y;
  float m_Z;

public:
  // construction - default to zero if nothing is passed in
  Vector3( float x = 0.0, float y = 0.0, float z = 0.0 );
  Vector3( const Vector3& v );

  // assigment operator
  void operator=( const Vector3& v );

  // getters
  float GetX();
  float GetY();
  float GetZ();

  // setters
  void SetX( float x );
  void SetY( float y );
  void SetZ( float z );
};
```

We've now made sure that the Vector3 class does not modify anything that doesn't belong to it, which again is how the integers behave. But there is still something missing. The assignment operator doesn't allow for chaining (eg. int a = b = c = d;) just like the ints do, so we need to make a slight adjustment to the overload:
```
// return a const reference to ourself
const Vector3& operator=( const Vector3& v );
```

There, much better.

We're now at the stage where we want to be able to add/subtract/multiply/divide vectors together, but before we start overloading the operators we should look at the way integers behave when they go through the same operations:
```
int a, b, c, d;
a = b; // b doesn't change, a does.
a * b; // both a and b do not change.
a = b * c; // both b and c do not change, but a does.
a = ( b + c ) * d; // a is the only thing that changes
a += b / c; // again, a is the only thing that changes.
```

It's obvious from this that when we overload the operators, we only modify the content of the object if it is on the left hand side of one of the assignment operators. Our interface should make this obvious:
```
class Vector3
{
private:
  // change the variable names to make it obvious that
  // they are member variables, not local or global.
  float m_X;
  float m_Y;
  float m_Z;

public:
  // construction - default to zero if nothing is passed in
  Vector3( float x = 0.0, float y = 0.0, float z = 0.0 );
  Vector3( const Vector3& v );

  // assigment operators - not const because the current object
  // needs to change
  const Vector3& operator=( const Vector3& v );
  const Vector3& operator+=( const Vector3& v );
  const Vector3& operator-=( const Vector3& v );
  const Vector3& operator*=( const Vector3& v );
  const Vector3& operator/=( const Vector3& v );

  // other operators - all const functions to make sure that the
  // internal state of the object doesn't get modified when the
  // function is called. Separate temporary instances of Vector3
  // objects are created and  returned when executed.
  Vector3 operator+( const Vector3& v ) const; // v1 + v2
  Vector3 operator-( const Vector3& v ) const; // v1 - v2
  Vector3 operator/( const Vector3& v ) const; // v1 * v2
  Vector3 operator*( const Vector3& v ) const; // v1 / v2

  // getters
  float GetX();
  float GetY();
  float GetZ();

  // setters
  void SetX( float x );
  void SetY( float y );
  void SetZ( float z );
};
```

The class is starting to take shape, but there are some other functions missing that are an integral part of any Vector class: normalise(), dot() and cross().

The normalise function is used to create a <a href="http://en.wikipedia.org/wiki/Unit_vector" title="Unit Vector">unit vector</a> (ie. a vector with a length of 1.0). But the question here is: when calling the function, should the object be modified, or should it return a copy of the Vector with a unit length? Let's look at the difference in function signatures:
```
// this function would modify the object directly
void Normalise();
// this function would return a new vector that is normalised
Vector3 Normalise() const;
```

Making a decision like this can be a bit of a pain in the butt, but we're fortunate in this case because we can deduce what should be done! Generally when dealing with normalised vectors, we tend to retain a reference to the normal while dealing with a stack of other vectors. So another instance of a vector is used alongside the other vectors. Let's look at how this might be done with both above functions if we had a vector that we wanted to reuse, but get a normalise version of at the same time:
```
// this is the vector we want to keep as is, but need a
// normalised copy of
Vector3 someVector;

// here's how we'd do it with the first option:
Vector3 normal1 = someVector;
normal1.Normalise();

// here's how we'd do it with the second option:
Vector3 normal2 = someVector.Normalise();
```

In my view, the second option is easier to read, and is a bit more intuitive. Not just that, but it's less code! So based on this, we'll utilise the second version of the function. Our class now looks like this:
```
class Vector3
{
private:
  // change the variable names to make it obvious that
  // they are member variables, not local or global.
  float m_X;
  float m_Y;
  float m_Z;

public:
  // construction - default to zero if nothing is passed in
  Vector3( float x = 0.0, float y = 0.0, float z = 0.0 );
  Vector3( const Vector3& v );

  // assigment operators - not const because the current object
  // needs to change
  const Vector3& operator=( const Vector3& v );
  const Vector3& operator+=( const Vector3& v );
  const Vector3& operator-=( const Vector3& v );
  const Vector3& operator*=( const Vector3& v );
  const Vector3& operator/=( const Vector3& v );

  // other operators - all const functions to make sure that the
  // internal state of the object doesn't get modified when the
  // function is called. Separate temporary instances of Vector3
  // objects are created and  returned when executed.
  Vector3 operator+( const Vector3& v ) const; // v1 + v2
  Vector3 operator-( const Vector3& v ) const; // v1 - v2
  Vector3 operator/( const Vector3& v ) const; // v1 * v2
  Vector3 operator*( const Vector3& v ) const; // v1 / v2

  // getters
  float GetX();
  float GetY();
  float GetZ();

  // setters
  void SetX( float x );
  void SetY( float y );
  void SetZ( float z );

  // helpers
  Vector3 Normalise() const;
};
```

So, next up we need the dot() function, which gives us the <a href="http://en.wikipedia.org/wiki/Dot_product" title="Dot Product">dot product</a> of two vectors. The dot product is a single value which represents the angle between the two vectors, so that's what the function should return. Since the dot product requires two vectors, it would make sense for the function to be given a reference to the secont vector that is part of the dot product equation. <strong>None</strong> of the objects should be modified at all during the course of the function, so we should make that obvious in the function signature. Our class now looks like this:
```
class Vector3
{
private:
  // change the variable names to make it obvious that
  // they are member variables, not local or global.
  float m_X;
  float m_Y;
  float m_Z;

public:
  // construction - default to zero if nothing is passed in
  Vector3( float x = 0.0, float y = 0.0, float z = 0.0 );
  Vector3( const Vector3& v );

  // assigment operators - not const because the current object
  // needs to change
  const Vector3& operator=( const Vector3& v );
  const Vector3& operator+=( const Vector3& v );
  const Vector3& operator-=( const Vector3& v );
  const Vector3& operator*=( const Vector3& v );
  const Vector3& operator/=( const Vector3& v );

  // other operators - all const functions to make sure that the
  // internal state of the object doesn't get modified when the
  // function is called. Separate temporary instances of Vector3
  // objects are created and  returned when executed.
  Vector3 operator+( const Vector3& v ) const; // v1 + v2
  Vector3 operator-( const Vector3& v ) const; // v1 - v2
  Vector3 operator/( const Vector3& v ) const; // v1 * v2
  Vector3 operator*( const Vector3& v ) const; // v1 / v2

  // getters
  float GetX();
  float GetY();
  float GetZ();

  // setters
  void SetX( float x );
  void SetY( float y );
  void SetZ( float z );

  // helpers
  Vector3 Normalise() const;
  double Dot( const Vector3& v ) const;
};
```

Note that the function is marked as const to imply that the object will <em>not</em> have a different state when it's called, and the parameter is a const reference to imply that the parameter will not be modified as well.

Finally, we need a function which will determine the <a href="http://en.wikipedia.org/wiki/Cross_product" title="Cross Product">cross product</a> of two vectors (commonly used to determine the vector that is perpendicular to two input vectors. The cross product equation takes two vectors and results in another vector, which again implies that the two input vectors do not change. Based on this implication, our function signature should be easy to deduce. While we're there, let's just chuck in a cheeky length() function which will give us the magnitude of the vector. Our class now looks like this:
```
class Vector3
{
private:
  // change the variable names to make it obvious that
  // they are member variables, not local or global.
  float m_X;
  float m_Y;
  float m_Z;

public:
  // construction - default to zero if nothing is passed in
  Vector3( float x = 0.0, float y = 0.0, float z = 0.0 );
  Vector3( const Vector3& v );

  // assigment operators - not const because the current object
  // needs to change
  const Vector3& operator=( const Vector3& v );
  const Vector3& operator+=( const Vector3& v );
  const Vector3& operator-=( const Vector3& v );
  const Vector3& operator*=( const Vector3& v );
  const Vector3& operator/=( const Vector3& v );

  // other operators - all const functions to make sure that the
  // internal state of the object doesn't get modified when the
  // function is called. Separate temporary instances of Vector3
  // objects are created and  returned when executed.
  Vector3 operator+( const Vector3& v ) const; // v1 + v2
  Vector3 operator-( const Vector3& v ) const; // v1 - v2
  Vector3 operator/( const Vector3& v ) const; // v1 * v2
  Vector3 operator*( const Vector3& v ) const; // v1 / v2

  // getters
  float GetX();
  float GetY();
  float GetZ();

  // setters
  void SetX( float x );
  void SetY( float y );
  void SetZ( float z );

  // helpers
  Vector3 Normalise() const;
  double Dot( const Vector3& v ) const;
  double Length() const;
  Vector3 Cross( const Vector3& v ) const;
};
```

The interface to our class now looks fine.. except for one small omission. Const objects are read-only objects, and hence reading values from a const object should be legal. On the flip side, when we read a given x, y, or z value from a vector the vector shouldn't be modified at all. So with that in mind, we should mark each of the getter function as const as well:
```
class Vector3
{
private:
  // change the variable names to make it obvious that
  // they are member variables, not local or global.
  float m_X;
  float m_Y;
  float m_Z;

public:
  // construction - default to zero if nothing is passed in
  Vector3( float x = 0.0, float y = 0.0, float z = 0.0 );
  Vector3( const Vector3& v );

  // assigment operators - not const because the current object
  // needs to change
  const Vector3& operator=( const Vector3& v );
  const Vector3& operator+=( const Vector3& v );
  const Vector3& operator-=( const Vector3& v );
  const Vector3& operator*=( const Vector3& v );
  const Vector3& operator/=( const Vector3& v );

  // other operators - all const functions to make sure that the
  // internal state of the object doesn't get modified when the
  // function is called. Separate temporary instances of Vector3
  // objects are created and  returned when executed.
  Vector3 operator+( const Vector3& v ) const; // v1 + v2
  Vector3 operator-( const Vector3& v ) const; // v1 - v2
  Vector3 operator/( const Vector3& v ) const; // v1 * v2
  Vector3 operator*( const Vector3& v ) const; // v1 / v2

  // getters
  float GetX() const;
  float GetY() const;
  float GetZ() const;

  // setters
  void SetX( float x );
  void SetY( float y );
  void SetZ( float z );

  // helpers
  Vector3 Normalise() const;
  double Dot( const Vector3& v ) const;
  double Length() const;
  Vector3 Cross( const Vector3& v ) const;
};
```

We're done! Our class looks complete (enough), so let's pump out a definition for the functions (just for the sake of clarity).
```
Vector3::Vector3( float x, float y, float z )
title: m_X( x ),
  m_Y( y ),
  m_Z( z )
{
  // don't need to do anything in the body of the
  // function because we're using the initialisation
  // lists instaed.
}

Vector3::Vector3( const Vector3& v )
title: m_X( v.m_X ),
  m_Y( v.m_Y ),
  m_Z( v.m_Z )
{
  // don't need to do anything in the body of the
  // function because we're using the initialisation
  // lists instaed.
}

const Vector3& Vector3::operator=( const Vector3& v )
{
  // check for self-assignment
  if( this != &v )
  {
    m_X = v.m_X;
    m_Y = v.m_Y;
    m_Z = v.m_Z;
  }

  // return a reference to ourselves for chaining
  return *this;
}

const Vector3& Vector3::operator+=( const Vector3& v )
{
  m_X += v.m_X;
  m_Y += v.m_Y;
  m_Z += v.m_Z;

  return *this;
}

const Vector3& Vector3::operator-=( const Vector3& v )
{
  m_X -= v.m_X;
  m_Y -= v.m_Y;
  m_Z -= v.m_Z;

  return *this;
}

const Vector3& Vector3::operator*=( const Vector3& v )
{
  m_X *= v.m_X;
  m_Y *= v.m_Y;
  m_Z *= v.m_Z;

  return *this;
}

const Vector3& Vector3::operator/=( const Vector3& v )
{
  m_X /= v.m_X;
  m_Y /= v.m_Y;
  m_Z /= v.m_Z;

  return *this;
}

Vector3 Vector3::operator+( const Vector3& v ) const
{
  return Vector3( m_X + v.m_X, m_Y + v.m_Y, m_Z + v.m_Z );
}

Vector3 Vector3::operator-( const Vector3& v ) const
{
  return Vector3( m_X - v.m_X, m_Y - v.m_Y, m_Z - v.m_Z );
}

Vector3 Vector3::operator/( const Vector3& v ) const
{
  return Vector3( m_X / v.m_X, m_Y / v.m_Y, m_Z / v.m_Z );
}

Vector3 Vector3::operator*( const Vector3& v ) const
{
  return Vector3( m_X * v.m_X, m_Y * v.m_Y, m_Z * v.m_Z );
}

float Vector3::GetX() const
{
  return m_X;
}

float Vector3::GetY() const
{
  return m_Y;
}

float Vector3::GetZ() const
{
  return m_Z;
}

void Vector3::SetX( float x )
{
  m_X = x;
}

void Vector3::SetY( float y )
{
  m_Y = y;
}

void Vector3::SetZ( float z )
{
  m_Z = z;
}

Vector3 Vector3::Normalise() const
{
  double length = Length();
  if( length == 0.0 )
  {
    // return a zero vector if it's got zero length
    return Vector3();
  }
  double invLength = 1.0 / length;
  return Vector3( m_X * invLength, m_Y * invLength, m_Z * invLength );
}

double Vector3::Dot( const Vector3& v ) const
{
  return  m_X * v.m_X + m_Y * v.m_Y + mZ * v.mZ;
}

double Vector3::Length() const
{
  return sqrt( m_X * m_X + m_Y * m_Y + mZ * mZ );
}

Vector3 Vector3::Cross( const Vector3& v ) const
{
  return Vector3( m_Y * v.m_Z - m_Z * v.m_Y,
                  m_Z * v.m_X - m_X * v.m_Z,
                  m_X * v.m_Y - m_Y * v.m_X );
}
```


Now that we have a functional class, we should have no problem using it in all of the above scenarios without any crazy side-effects. The class <em>should</em> be concrete! :)

Disclaimer: This code hasn't been compiled, and might not run without a few tweaks. I wrote this off the top of my head while sitting in front of the TV! Comments, questions and flames are welcome.
