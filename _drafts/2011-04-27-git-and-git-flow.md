---
title: Git and Git-Flow
author: Correl Roush
layout: post
categories:
  - Programming
---

## Experiences learning and using git and git-flow

About a year ago, I succeeded in switching our development team from CVS to Git, and none of us have looked back since.

### Why Git over SVN?

Our biggest issue developing with CVS was branching. We needed a way to develop multiple features without fear of stepping on each others toes or dreading a disastrous, manual merge process at the end.

Both Subversion and Git work with *changesets*. By defining a commit as a snapshot of the repository at a given point of time rather than a change to a particular file in it, you get a single changeset representing all the files created, modified and deleted that comprise a change to your software.

Both systems also succeed in making branching easy. Unfortunately, that's the end of the usefulness of Subversion. When merging branches in subversion, the snapshots of the current states of branches A and B are compared, and then the sum of the changes are merged in, potentially causing a pile of conflicts.

Rather than blindly merging all changes between points A and B, Git is aware of each individual changeset in each branch, and merges each *changeset* into the target branch. Because it knows what changes have been applied where, it's better able to avoid conflicts, and changes that do conflict will be easier to resolve as you get to deal with them one at a time.

### So now we've got branches, how do we manage them?

With the tool choice out of the way, it was time to figure out a workflow. Rather than try and work out a problem that may have already been solved, I got to googling and came across [A successful Git branching model](http://nvie.com/posts/a-successful-git-branching-model/) by Vincent Driessen.
