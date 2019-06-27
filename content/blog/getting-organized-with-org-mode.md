+++
title = "Getting Organized with Org Mode"
author = ["Correl Roush"]
date = 2014-11-25T00:00:00-05:00
keywords = ["emacs", "org-mode", "themes"]
tags = ["emacs", "org-mode", "git", "graphviz"]
draft = false
+++

<img src="/images/org-mode-unicorn-logo.png" alt="Org Mode logo" style="float: right" />

I've been using Emacs Org mode for nearly a year now. For a while I
mostly just used it to take and organize notes, but over time I've
discovered it's an incredibly useful tool for managing projects and
tasks, writing and publishing documents, keeping track of time and
todo lists, and maintaining a journal.


## Project Management {#project-management}

Most of what I've been using [Org mode](http://orgmode.org/) for has been breaking down large
projects at work into tasks and subtasks. It's really easy to enter
projects in as a hierarchy of tasks and task groupings. Using
[Column View](http://orgmode.org/worg/org-tutorials/org-column-view-tutorial.html), I was able to dive right into scoping them individually
and reporting total estimates for each major segment of work.

{{< figure src="/images/emacs-projects.png" alt="Example projects org file" >}}

Because Org Mode makes building and modifying an outline structure
like this so quick and easy, I usually build and modify the project
org document while planning it out with my team. Once done, I then
manually load that information into our issue tracker and get
underway. Occasionally I'll also update tags and progress status in
the org document as well as the project progresses, so I can use the
same document to plan subsequent development iterations.


## Organizing Notes and Code Exercises {#organizing-notes-and-code-exercises}

More recently, I've been looking into various ways to get more
things organized with Org mode. I've been stepping through
[Structure and Interpretation of Computer Programs](http://sarabander.github.io/sicp/) with some other
folks from work, and discovered that Org mode was an ideal fit for
keeping my notes and exercise work together. The latter is neatly
managed by [Babel](http://orgmode.org/worg/org-contrib/babel/intro.html), which let me embed and edit source examples and
my excercise solutions right in the org document itself, and even
export them to one or more scheme files to load into my
interpreter.


## Exporting and Publishing Documents {#exporting-and-publishing-documents}

Publishing my notes with org is also a breeze. I've published
project plans and proposals to PDF to share with colleagues, and
exported my [SICP notes](https://github.com/correl/sicp) to html and [dropped them into a site](http://sicp.phoenixinquis.net/) built
with [Jekyll](http://jekyllrb.com/). Embedding graphs and diagrams into exported documents
using [Graphviz](http://www.graphviz.org/), [Mscgen](http://www.mcternan.me.uk/mscgen/), and [PlantUML](http://plantuml.sourceforge.net/) has also really helped with
putting together some great project plans and documentation. A lot of
great examples using those tools (and more!) can be found [here](http://home.fnal.gov/~neilsen/notebook/orgExamples/org-examples.html).


## Emacs Configuration {#emacs-configuration}

While learning all the cool things I could do with Org mode and Babel,
it was only natural I'd end up using it to reorganize my [Emacs
configuration](https://github.com/correl/dotfiles/tree/master/.emacs.d). Up until that point, I'd been managing my configuration
in a single init.el file, plus a directory full of mode or
purpose-specific elisp files that I'd loop through and load. Inspired
primarily by the blog post, ["Making Emacs Work For Me"](http://zeekat.nl/articles/making-emacs-work-for-me.html), and later by
others such as [Sacha Chua's Emacs configuration](http://pages.sachachua.com/.emacs.d/Sacha.html), I got all my configs
neatly organized into a single org file that gets loaded on
startup. I've found it makes it far easier to keep track of what I've
got configured, and gives me a reason to document and organize things
neatly now that it's living a double life as a [published document](https://github.com/correl/dotfiles/blob/master/.emacs.d/emacs.org) on
GitHub. I've still got a directory lying around with autoloaded
scripts, but now it's simply reserved for [tinkering and sensitive
configuration](https://github.com/correl/dotfiles/blob/master/.emacs.d/emacs.org#auto-loading-elisp-files).


## Tracking Habits {#tracking-habits}

Another great feature of Org mode that I've been taking advantage
of a lot more lately is the [Agenda](http://orgmode.org/manual/Agenda-Views.html). By defining some org files as
being agenda files, Org mode can examine these files for TODO
entries, scheduled tasks, deadlines and more to build out useful
agenda views to get a quick handle on what needs to be done and
when. While at first I started by simply syncing down my google
calendars as org-files (using [ical2org.awk](http://orgmode.org/worg/code/awk/ical2org.awk)), I've started
managing TODO lists in a dedicated org file. By adding tasks to
this file, scheduling them, and setting deadlines, I've been doing
a much better job of keeping track of things I need to get done
and (even more importantly) _when_ I need to get them done.

{{< figure src="/images/emacs-org-agenda.png" alt="Agenda view snippet" >}}

This works not only for one-shot tasks, but also [habits and other
repetitive tasks](http://orgmode.org/manual/Tracking-your-habits.html). It's possible to schedule a task that should be
done every day, every few days, or maybe every first sunday of a
month. For example, I've set up repeating tasks to write a blog
post at least once a month, practice guitar every two to three
days, and to do the dishes every one or two days. The agenda view
can even show a small, colorized graph next to each repeating task
that paints a picture of how well (or not!) I've been getting
those tasks done on time.


## Keeping a Journal and Tracking Work {#keeping-a-journal-and-tracking-work}

The last thing I've been using (which I'm still getting a handle
on) is using [Capture](http://orgmode.org/manual/Capture.html) to take and store notes, keep a journal, and
even [track time on tasks at work](http://orgmode.org/manual/Clocking-work-time.html).

```emacs-lisp
(setq org-capture-templates
      '(("j" "Journal Entry" plain
         (file+datetree "~/org/journal.org")
         "%U\n\n%?" :empty-lines-before 1)
        ("w" "Log Work Task" entry
         (file+datetree "~/org/worklog.org")
         "* TODO %^{Description}  %^g\n%?\n\nAdded: %U"
         :clock-in t
         :clock-keep t)))

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
```

For my journal, I've configured a capture template that I can use
to write down a new entry that will be stored with a time stamp
appended into its own org file, organized under headlines by year,
month and date.

For work tasks, I have another capture template configured that
will log and tag a task into another org file, also organized by
date, which will automatically start tracking time for that
task. Once done, I can simply clock out and check the time I've
spent, and can easily find it later to clock in again, add notes,
or update its status. This helps me keep track of what I've gotten
done during the day, keep notes on what I was doing at any point
in time, and get a better idea of how long it takes me to do
different types of tasks.


## Conclusion {#conclusion}

There's a lot that can be done with Org mode, and I've only just
scratched the surface. The simple outline format provided by Org mode
lends itself to doing all sorts of things, be it organizing notes,
keeping a private or work journal, or writing a book or technical
document. I've even written this blog post in Org mode! There's tons
of functionality that can be built on top of it, yet the underlying
format itself remains simple and easy to work with. I've never been
great at keeping myself organized, but Org mode is such a delight to
use that I can't help trying anyway. If it can work for me, maybe it
can work for you, too!

There's tons of resources for finding new ways for using Org mode, and
I'm still discovering cool things I can track and integrate with it. I
definitely recommend reading through [Sacha Chua's Blog](http://sachachua.com/blog/), as well as
posts from [John Wiegley](http://newartisans.com/2007/08/using-org-mode-as-a-day-planner/). I'm always looking for more stuff to try
out. Feel free to drop me a line if you find or are using something
you think is cool or useful!
