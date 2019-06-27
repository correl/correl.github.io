+++
title = "Coders at Work"
author = ["Correl Roush"]
date = 2015-01-28T00:00:00-05:00
keywords = ["emacs", "org-mode", "themes"]
tags = ["programming", "books"]
draft = false
+++

A few days before leaving work for a week and a half of flying and
cruising to escape frigid Pennsylvania, I came across a [Joe Armstrong
quote](#orgfd943c5) during my regularly scheduled slacking off on twitter and Hacker
News. I'd come across a couple times before, only this time I noticed
it had a source link. This led me to discovering (and shortly
thereafter, buying) Peter Seibel's "[Coders at Work -- Reflections on
the Craft of Programming](http://www.codersatwork.com/)". I loaded it onto my nook, and off I went.

The book is essentially a collection of interviews with a series of
highly accomplished software developers. Each of them has their own
fascinating insights into the craft and its rich history.

While making my way through the book, I highlighted some excerpts
that, for one reason or another, resonated with me. I've organized and
elaborated on them below.


## Incremental Changes {#incremental-changes}

<a id="org739bd9a"></a>

> I've seen young programmers say, "Oh, shit, it doesn't work," and then
> rewrite it all. Stop. Try to figure out what's going on. **Learn how to
> write things incrementally so that at each stage you could verify it.**<br />
> -- Brad Fitzpatrick

I can remember doing this to myself when I was still relatively new to
coding (and even worse, before I discovered source control!). Some
subroutine or other would be misbehaving, and rather than picking it
apart and figuring out what it was I'd done wrong, I'd just blow it
away and attempt to write it fresh. While I _might_ be successful,
that likely depended on the issue being some sort of typo or missed
logic; if it was broken because I misunderstood something or had a bad
plan to begin with, rewriting it would only result in more broken
code, sometimes in more or different ways than before. I don't think
I've ever rewritten someone else's code without first at least getting
a firm understanding of it and what it was trying to accomplish, but
even then, breaking down changes piece by piece makes it all the
easier to maintain sanity.

I do still sometimes catch myself doing too much at once when building
a new feature or fixing a bug. I may have to fix a separate bug that's
in my way, or I may have to make several different changes in various
parts of the code. If I'm not careful, things can get out of hand
pretty quickly, and before I know it I have a blob of changes strewn
across the codebase in my working directory without a clear picture of
what's what. If something goes wrong, it can be pretty tough to sort
out which change broke things (or fixed them). Committing changes
often helps tremendously to avoid this sort of situation, and when I
catch myself going off the rails I try to find a stopping point and
split changes up into commits as soon as possible to regain
control. Related changes and fixes can always be squashed together
afterwards to keep things tidy.


## Specifications & Documentation {#specifications-and-documentation}

<a id="org5faa2e2"></a>

> **Many customers won't tell you a problem; they'll tell you a
> solution.** A customer might say, for instance, "I need you to add
> support for the following 17 attributes to this system. Then you have
> to ask, 'Why? What are you going to do with the system? How do you
> expect it to evolve?'" And so on. You go back and forth until you
> figure out what all the customer really needs the software to
> do. These are the use cases.<br />
> -- Joshua Bloch

Whether your customer is your customer, or your CEO, the point stands:
customers are _really bad_ at expressing what they want. It's hard to
blame them, though; analyzing what you really want and distilling it
into a clear specification is tough work. If your customer is your
boss, it can be intimidating to push back with questions like "Why?",
but if you can get those questions answered you'll end up with a
better product, a better _understanding_ of the product, and a happy
customer. The agile process of doing quick iterations to get tangible
results in front of them is a great way of getting the feedback and
answers you need.

<a id="org37ca046"></a>

> The code shows me what it _does_. It doesn't show me what it's
> supposed to do. I think the code is the answer to a problem.
> **If you don't have the spec or you don't have any documentation, you have to guess what the problem is from the answer. You might guess wrong.**<br />
> -- Joe Armstrong

Once you've got the definition of what you've got to build and how
it's got to work, it's extremely important that you get it
documented. Too often, I'm faced with code that's doing something in
some way that somebody, either a customer or a developer reading it,
takes issue with, and there's no documentation anywhere on why it's
doing what it's doing. What happens next is anybody's guess. Code
that's clear and conveys its intent is a good start towards avoiding
this sort of situation. Comments explaining intent help too, though
making sure they're kept up to date with the code can be
challenging. At the very least, I try to promote useful commit
messages explaining what the purpose of a change is, and reference a
ticket in our issue tracker which (hopefully) has a clear accounting
of the feature or bugfix that prompted it.


## Pair Programming {#pair-programming}

<a id="org2b23bda"></a>

> ... **if you don't know what you're doing then I think it can be very
> helpful with someone who also doesn't know what they're doing.** If you
> have one programmer who's better than the other one, then there's
> probably benefit for the weaker programmer or the less-experienced
> programmer to observe the other one. They're going to learn something
> from that. But if the gap's too great then they won't learn, they'll
> just sit there feeling stupid.<br />
> -- Joe Armstrong

Pairing isn't something I do much. At least, it's pretty rare that I
have someone sitting next to me as I code. I **do** involve peers while
I'm figuring out what I want to build as often as I can. The tougher
the problem, the more important it is, I think, to get as much
feedback and brainstorming in as possible. This way, everybody gets to
tackle the problem and learn together, and anyone's input, however
small it might seem, can be the key to the "a-ha" moment to figuring
out a solution.


## Peer Review {#peer-review}

<a id="org02f3602"></a>

> **I think an hour of code reading is worth two weeks of QA.** It's just
> a really effective way of removing errors. If you have someone who is
> strong reading, then the novices around them are going to learn a lot
> that they wouldn't be learning otherwise, and if you have a novice
> reading, he's going to get a lot of really good advice.<br />
> -- Douglas Crockford

Just as important as designing the software as a team, I think, is
reviewing it as a team. In doing so, each member of the team has an
opportunity to understand _how_ the system has been implemented, and
to offer their suggestions and constructive criticisms. This helps the
team grow together, and results in a higher quality of code overall.
This benefits QA as well as the developers themselves for the next
time they find themselves in that particular bit of the system.


## Object-Oriented Programming {#object-oriented-programming}

<a id="orgfd943c5"></a>

> I think the lack of reusability comes in object-oriented languages,
> not in functional languages.
> **Because the problem with object-oriented languages is they've got all this implicit environment that they carry around with them. You wanted a banana but what you got was a gorilla holding the banana and the entire jungle.**<br />
> -- Joe Armstrong

A lot has been written on why OOP isn't the great thing it claims to
be, or was ever intended to be. Having grappled with it myself for
years, attempting to find ways to keep my code clean, concise and
extensible, I've more or less come to the same conclusion as Armstrong
in that coupling data structures with behaviour makes for a terrible
mess. Dividing the two led to a sort of moment of clarity; there was
no more confusion about what methods belong on what object. There was
simply the data, and the methods that act on it. I am still struggling
a bit, though, on how to bring this mindset to the PHP I maintain at
work. The language seems particularly ill-suited to managing complex
data structures (or even simple ones -- vectors and hashes are
bizarrely intertwined).


## Writing {#writing}

<a id="orgec22cec"></a>

> You should read _[Elements of Style]_ for two reasons: The first is
> that a large part of every software engineer's job is writing
> prose. **If you can't write precise, coherent, readable specs, nobody
> is going to be able to use your stuff.** So anything that improves your
> prose style is good. The second reason is that most of the ideas in
> that book are also applicable to programs.<br />
> -- Joshua Bloch

<a id="org80f7fc6"></a>

> **My advice to everybody is pretty much the same, to read and write.**<br />
> ...<br />
> Are you a good Java programmer, a good C programmer, or whatever? I
> don't care. I just want to know that you know how to put an algorithm
> together, you understand data structures, and you know how to document
> it.<br />
> -- Douglas Crockford

<a id="org81f1dbf"></a>

> This is what literate programming is so great for --<br />
> **I can talk to myself. I can read my program a year later and know
> exactly what I was thinking.**<br />
> -- Donald Knuth

The more I've program professionally, the clearer it is that writing
(and communication in general) is a very important skill to
develop. Whether it be writing documentation, putting together a
project plan, or whiteboarding and discussing something, clear and
concise communication skills are a must. Clarity in writing translates
into clarity in coding as well, in my opinion. Code that is short, to
the point, clear in its intention, making good use of structure and
wording (in the form of function and variable names) is far easier to
read and reason about than code that is disorganized and obtuse.


## Knuth {#knuth}

<a id="org5a4529f"></a>

> I tried to make familiarity with Knuth a hiring criteria, and I was
> disappointed that I couldn't find enough people that had read him. In
> my view,
> **anybody who calls himself a professional programmer should have read
> Knuth's books or at least should have copies of his books.**<br />
> -- Douglas Crockford

<a id="org9e64397"></a>

> ... Knuth is really good at telling a story about code. When you read
> your way through _The Art of Computer Programming_ and you read your
> way through an algorithm, he's explained it to you and showed you some
> applications and given you some exercises to work, and **you feel like
> you've been led on a worthwhile journey.**<br />
> -- Guy Steele

<a id="orgaa47f8d"></a>

> At one point I had _[The Art of Computer Programming]_ as my monitor
> stand because it was one of the biggest set of books I had, and it was
> just the right height. That was nice because it was always there, and
> I guess then I was more prone to use it as a reference because it was
> right in front of me.<br />
> -- Peter Norvig

I haven't read any of Knuth's books yet, which is something I'll have
to rectify soon. I don't think I have the mathematical background
necessary to get through some of his stuff, but I expect it will be
rewarding nonetheless. I'm also intrigued by his concept of literate
programming, and I'm curious to learn more about TeX. I imagine I'll
be skimming through [TeX: The Program](http://brokestream.com/tex-web.html) pretty soon now that I've
finished Coders at Work :)
