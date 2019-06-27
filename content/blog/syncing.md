+++
title = "Keeping Files And Configuration In Sync"
author = ["Correl Roush"]
date = 2015-04-20T00:00:00-04:00
keywords = ["emacs", "org-mode", "themes"]
tags = ["git"]
draft = false
+++

I have a few computers I use on a daily basis, and I like to keep the
same emacs and shell configuration on all of them, along with my org
files and a handful of scripts. Since I'm sure other people have this
problem as well, I'll share what I'm doing so anyone can learn from
(or criticise) my solutions.


## Git for configuration and projects {#git-for-configuration-and-projects}

I'm a software developer, so keeping things in git just makes sense
to me. I keep my org files in a privately hosted git repository, and
[Emacs](https://www.gnu.org/software/emacs/) and [Zsh](http://www.zsh.org/) configurations in a [public repo on github](https://github.com/correl/dotfiles). My blog is
also hosted and published on github as well; I like having it cloned
to all my machines so I can work on drafts wherever I may be.

My [.zshrc](https://github.com/correl/dotfiles/blob/master/.zshrc) installs [oh-my-zsh](https://github.com/robbyrussell/oh-my-zsh) if it isn't installed already, and sets
up my shell theme, path, and some other environmental things.

My [Emacs configuration](https://github.com/correl/dotfiles/blob/master/.emacs.d/emacs.org) behaves similarly, making use of John
Wiegley's excellent [use-package](https://github.com/jwiegley/use-package) tool to ensure all my packages are
installed if they're not already there and configured the way I like
them.

All I have to do to get running on a new system is to install git,
emacs and zsh, clone my repo, symlink the files, and grab a cup of
tea while everything installs.


## Bittorrent sync for personal settings & books {#bittorrent-sync-for-personal-settings-and-books}

For personal configuration that doesn't belong in and/or is too
sensitive to be in a public repo, I have a folder of dotfiles and
things that I sync between my machines using [Bittorrent Sync](https://www.getsync.com/). The
dotfiles are arranged into directories by their purpose:

```text
[correlr@reason:~/dotenv]
% tree -a -L 2
.
├── authinfo
│   └── .authinfo.gpg
├── bin
│   └── .bin
├── emacs
│   ├── .bbdb
│   └── .emacs.local.d
├── mail
│   ├── .gnus.el
│   ├── .signature
├── README.org
├── .sync
│   ├── Archive
│   ├── ID
│   ├── IgnoreList
│   └── StreamsList
├── tex
│   └── texmf
├── xmonad
│   └── .xmonad
└── zsh
    └── .zshenv
```

This folder structure allows my configs to be easily installed using
[GNU Stow](https://www.gnu.org/software/stow/) from my `dotenv` folder:

```text
stow -vvS *
```

Running that command will, for each file in each of the directories,
create a symlink to it in my home folder if there isn't a file or
directory with that name there already.

Bittorrent sync also comes in handy for syncing my growing [Calibre](http://calibre-ebook.com/) ebook
collection, which outgrew my [Dropbox](https://www.dropbox.com/) account a while back.
