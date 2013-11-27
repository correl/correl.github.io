---
title: Learning Functional Programming, Part One
author: Correl Roush
layout: post
permalink: /2012/04/09/learning-functional-programming-part-one/
categories:
  - Programming
tags:
  - functional
  - python
---

## Part One: Lambdas? In my Python?

Over the past few months, I've decided to take a stab at learning some
functional programming. I'd been doing python for a few years (and
completely falling in love with it), and so I'd been exposed to a few
functional concepts it offers - primarily higher-order functions and list
comprehensions, both of which allow for very clear, concise and powerful code.
Since that's where I started my journey, that's where my post will begin as
well.

<!--more-->

### Functions are objects, too

Having graduated to python from PHP and C/C++, perhaps the biggest new thing to
wrap my head around (besides readable code, whitespace-as-syntax,
[programming being fun again](http://xkcd.com/353/), and all that), is that in
python, functions (and classes!) are objects, just like anything else. They
can still be defined in the usual way, but they can also be assigned, passed
as arguments, even modified and replaced like any other value or object in your
program. 

```python
def do_a():
    print "Doing something"

do_b = do_a

do_b()

# Prints "Doing something"
```

Functions themselves no longer require formal definitions, either, they can be
created *[anonymously](http://en.wikipedia.org/wiki/Anonymous_function)*:

```python
my_send = lambda person, thing: send(person.upper(), thing, subject="Check this out!")
ucase_people = map(lambda name: name.upper(), ["Joe", "Mary", "Zach"])
```


### Abstracting behaviour

You'll find you can now start abstracting away common idioms. For
example, you probably very often find yourself looping over some list of items,
performing some set of actions on them, or passing them to some other function
or method:

```python
people = ["Joe", "Chris", "Matt", "Jennifer"]
for person in people:
    u_person = person.upper()
    send(person, super_fun_thing)
```

Instead of that, you could have a function that takes a list as one argument,
and a function to apply to each item in it as another:

```python
def dostuff(action, things):
    result = []
    for thing in things:
        result.append(action(thing))
    return result

dostuff(send, people)
```

The above example is actually just a simple definition of one of the most
common higher-order functions,
[map](http://docs.python.org/library/functions.html#map), which python already
provides for you. Another particularly useful higher-order function is
[filter](http://docs.python.org/library/functions.html#filter) which, given a
function that returns true of false if its criteria are met by the passed item,
will return the subset of the passed list that satisfy the filtering function:

```python
stuff = ["My notes.txt", "Matt's notes.txt",  "My music.pls"]
my_stuff = filter(lambda s: s.startswith("My "), stuff)

# my_stuff = ["My notes.txt", "My music.pls"]
```

[List comprehensions](http://docs.python.org/tutorial/datastructures.html#list-comprehensions)
provide a cleaner, easier to read way to perform mapping and/or filtering on a
list:

```python
stuff = ["My notes.txt", "Matt's notes.txt",  "My music.pls"]

my_stuff = [file for file in stuff if file.startswith("My ")]
# ["My notes.txt", "My music.pls"]

upper_stuff = [file.upper() for file in stuff]
# ["MY NOTES.TXT", "MATT'S NOTES.TXT", "MY MUSIC.PLS"]

music = [file.upper() for file in stuff if file.endswith(".pls")]
# ["MY MUSIC.PLS"]
```


### Tip of the iceberg

This is just a very small taste of functional programming concepts. Later, I'll
introduce a couple of functional languages, and explain what sets them apart
from object-oriented and imperative programming languages.
