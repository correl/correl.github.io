---
title: Transmission, RSS and XBMC
author: Correl Roush
layout: post
permalink: /2011/04/27/transmission-rss-and-xbmc/
categories:
  - Programming
tags:
  - bittorrent
  - htpc
  - linux
  - python
  - transmission
  - xbmc
format: standard
---
I'm a huge fan of [XBMC](http://www.xbmc.org/). My pc (currently running Ubuntu 10.04) has taken root in my
living room, piping all my movies and tv shows straight to my HDTV.

While my pc is set up as a DVR using [MythTV](http://www.mythtv.org) to record shows off my FIOS box, it tends to be a little unreliable, which can suck when it's time to catch up on Daily Show and Colbert episodes.
I've had [Transmission](http://www.transmissionbt.com/) set up for a while for all my torrenting needs, and
I've even written an [XBMC script to manage torrents](https://github.com/correl/Transmission-XBMC), so I got to looking for
tools to track tv show torrent rss feeds.

<!--more-->

My first stop was [TED](http://ted.nu/). TED worked well enough, but would occasionally hang.
Since it's a GUI java app running in the taskbar, it would require me to dig
out my mouse and break out of full screen XBMC to fiddle with it. I eventually
got tired of dealing with TED and went back to prodding Myth.

Recently I've been itching to reliably watch my shows again, so I checked around
for a simple command-line utility to track rss feeds and download torrents.
Finding none, I loaded up vim and threw together a python script to handle it
all for me.

I also have another, simple script from when I was using TED (or just manually
downloading shows) which looks at completed torrents, compares their names with
the folders in my TV directory, and moves the shows into them for XBMC to see.

A couple cron jobs and a few rss feeds later, and I've got all my shows
automatically delivered straight to XBMC for my lazy evening viewing pleasure.

### trss.py
[Download](https://github.com/correl/trss/raw/master/trss.py)

```
Usage:
    trss.py add <rss-url> [<recent-items>]
        Adds an RSS feed to follow
        rss-url:        Full URL to the RSS feed
        recent-items:   (Optional) number of recent items to queue
                        for downloading
    trss.py remove <index>
        Remove an RSS feed
        index:          Numeric index of the feed to remove as
                        reported by the list command
    trss.py list
        Displays a list of followed feeds

    trss.py download
        Fetch all feeds and download new items

    trss.py set [<setting> [<value>]]
        Set or view configuration settings
        Call without any arguments to list all settings and their values
        Call with a setting and no value to see the current value for that setting

        Currently, the only used setting is 'download_dir', which allows you to set
        a directory to store all retrieved torrents, such as a directory your
        torrent application watches for new downloads. If 'download_dir' is not set,
        the current directory will be used.
```

### transmission-tv.py
```python
#!/usr/bin/python
import os
import re

import transmissionrpc

TV_PATH = '/media/Gaia/Video/TV/'

class TVShowCollection:
	def __init__(self, path):
		self.path = path
		self.shows = os.listdir(path)
		self.patterns = [[s.lower().replace(' ', '.?'), s] for s in sorted(self.shows, key=len, reverse=True)]
	def match(self, filename):
		for pattern, show in self.patterns:
			if re.findall(pattern, filename.lower()):
				return show
		return None

def move(self, ids, location):
	"""Move torrent data to the new location."""
	self._rpc_version_warning(6)
	args = {'location': location, 'move': True}
	self._request('torrent-set-location', args, ids, True)

if float(transmissionrpc.__version__) < 0.4:
	# The move function is not present in versions 0.3 and older
	transmissionrpc.Client.move = move

collection = TVShowCollection(TV_PATH)
client = transmissionrpc.Client()

torrents = client.info()
for i, torrent in torrents.iteritems():
	status = torrent.status
	if status not in ['seeding', 'stopped']:
		continue
	show = collection.match(torrent.name)
	if show is None:
		continue
	path = '{0}{1}/'.format(TV_PATH, show)
	if torrent.downloadDir.startswith(path):
		continue
	print 'Found {0} torrent \'{1}\' in show \'{2}\', moving...'.format(status, torrent.name, show)
	result = client.move(i, path)
	if status == 'seeding':
		print 'Re-starting torrent to continue seeding'
		client.start(i)
```
