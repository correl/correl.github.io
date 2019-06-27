+++
title = "How Does The Phillips Hue Wake-Up Feature Work?"
author = ["Correl Roush"]
date = 2018-03-13T00:00:00-04:00
keywords = ["emacs", "org-mode", "themes"]
tags = ["home-automation"]
draft = false
+++

I recently got myself a set of Phillips Hue White and Color Ambiance
lights. One of the features I was looking forward to in particular
(besides playing with all the color options) was setting a wake-up
alarm with the lights gradually brightening. This was pretty painless
to get set up using the phone app. I'm pretty happy with the result,
but there's certainly some things I wouldn't mind tweaking. For
example, the initial brightness of the bulbs (at the lowest setting)
still seems a bit bright, so I might want to delay the bedside lamps
and let the more distant lamp start fading in first. I also want to
see if I can fiddle it into transitioning between some colors to get
more of a sunrise effect (perhaps "rising" from the other side of the
room, with the light spreading towards the head of the bed).

Figuring out how the wake-up settings that the app installed on my
bridge seemed a good first step towards introducing my own
customizations.

Information on getting access to a Hue bridge to make REST API calls
to it can be found in the [Hue API getting started guide](https://www.developers.meethue.com/documentation/getting-started).


## My wake-up settings {#my-wake-up-settings}

My wake-up is scheduled for 7:00 to gradually brighten the lights with
a half-hour fade-in each weekday. I also toggled on the setting to
automatically turn the lights off at 9:00.

<style>.org-center { margin-left: auto; margin-right: auto; text-align: center; }</style>

<div class="org-center">
  <div></div>

![](/images/Screenshot_20180313-182434.png) ![](/images/Screenshot_20180313-182438.png)

</div>


## Finding things on the bridge {#finding-things-on-the-bridge}

The most natural starting point is to check the schedules. Right off
the bat, I find what I'm after:


### The schedule ... {#the-schedule-dot-dot-dot}

```http
GET http://bridge/api/${username}/schedules/1
```

```js
{
  "name": "Wake up",
  "description": "L_04_fidlv_start wake up",
  "command": {
    "address": "/api/xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx/sensors/2/state",
    "body": {
      "flag": true
    },
    "method": "PUT"
  },
  "localtime": "W124/T06:30:00",
  "time": "W124/T10:30:00",
  "created": "2018-03-11T19:46:54",
  "status": "enabled",
  "recycle": true
}
```

This is a recurring schedule item that runs every weekday at 6:30. We
can tell this by looking at the `localtime` field. From the
documentation on [time patterns](https://www.developers.meethue.com/documentation/datatypes-and-time-patterns#16%5Ftime%5Fpatterns), we can see that it's a recurring time
pattern specifying days of the week as a bitmask, and a time (6:30).

<div class="table-caption">
  <span class="table-number">Table 1</span>:
  Unraveling the weekday portion
</div>

| `0MTWTFSS`                  |
|:----------------------------|
| `01111100` (124 in decimal) |

Since this schedule is enabled, we can be assured that it will run,
and in doing so, will issue a `PUT` to a sensors endpoint, setting a
flag to true.


### ... triggers the sensor ... {#dot-dot-dot-triggers-the-sensor-dot-dot-dot}

```http
GET http://bridge/api/${username}/sensors/2
```

```js
{
  "state": {
    "flag": false,
    "lastupdated": "2018-03-13T13:00:00"
  },
  "config": {
    "on": true,
    "reachable": true
  },
  "name": "Sensor for wakeup",
  "type": "CLIPGenericFlag",
  "modelid": "WAKEUP",
  "manufacturername": "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
  "swversion": "A_1801260942",
  "uniqueid": "L_04_fidlv",
  "recycle": true
}
```

The sensor is what's _really_ setting things in motion. Here we've got
a [generic CLIP flag sensor](https://www.developers.meethue.com/documentation/supported-sensors#clipSensors) that is triggered exclusively by our
schedule. Essentially, by updating the flag state, we trigger the
sensor.


### ... triggers a rule ... {#dot-dot-dot-triggers-a-rule-dot-dot-dot}

```http
GET http://bridge/api/${username}/rules/1
```

```js
{
  "name": "L_04_fidlv_Start",
  "owner": "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
  "created": "2018-03-11T19:46:51",
  "lasttriggered": "2018-03-13T10:30:00",
  "timestriggered": 2,
  "status": "enabled",
  "recycle": true,
  "conditions": [
    {
      "address": "/sensors/2/state/flag",
      "operator": "eq",
      "value": "true"
    }
  ],
  "actions": [
    {
      "address": "/groups/1/action",
      "method": "PUT",
      "body": {
        "scene": "7GJer2-5ahGIqz6"
      }
    },
    {
      "address": "/schedules/2",
      "method": "PUT",
      "body": {
        "status": "enabled"
      }
    }
  ]
}
```

Now things are happening. Looking at the conditions, we can see that
this rule triggers when the wakeup sensor updates, and its flag is set
to `true`. When that happens, the bridge will iterate through its
rules, find that the above condition has been met, and iterate through
each of the actions.


### ... which sets the scene ... {#dot-dot-dot-which-sets-the-scene-dot-dot-dot}

The bedroom group (`/groups/1` in the rule's action list) is set to
the following scene, which turns on the lights at minimum brightness:

```http
GET http://bridge/api/${username}/scenes/7GJer2-5ahGIqz6
```

```js
{
  "name": "Wake Up init",
  "lights": [
    "2",
    "3",
    "5"
  ],
  "owner": "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
  "recycle": true,
  "locked": true,
  "appdata": {},
  "picture": "",
  "lastupdated": "2018-03-11T19:46:50",
  "version": 2,
  "lightstates": {
    "2": {
      "on": true,
      "bri": 1,
      "ct": 447
    },
    "3": {
      "on": true,
      "bri": 1,
      "ct": 447
    },
    "5": {
      "on": true,
      "bri": 1,
      "ct": 447
    }
  }
}
```


### ... and schedules the transition ... {#dot-dot-dot-and-schedules-the-transition-dot-dot-dot}

Another schedule (`/schedules/2` in the rule's action list) is enabled
by the rule.

```http
GET http://bridge/api/${username}/schedules/2
```

```js
{
  "name": "L_04_fidlv",
  "description": "L_04_fidlv_trigger end scene",
  "command": {
    "address": "/api/xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx/groups/0/action",
    "body": {
      "scene": "gXdkB1um68N1sZL"
    },
    "method": "PUT"
  },
  "localtime": "PT00:01:00",
  "time": "PT00:01:00",
  "created": "2018-03-11T19:46:51",
  "status": "disabled",
  "autodelete": false,
  "starttime": "2018-03-13T10:30:00",
  "recycle": true
}
```

_This_ schedule is a bit different from the one we saw before. It is
normally disabled, and it's time pattern (in `localtime`) is
different. The `PT` prefix specifies that this is a timer which
expires after the given amount of time has passed. In this case, it is
set to one minute (the first 60 seconds of our wake-up will be spent
in minimal lighting). Enabling this schedule starts up the timer. When
one minute is up, another scene will be set.

This one, strangely, is applied to group `0`, the meta-group including
all lights, but since the scene itself specifies to which lights it
applies, there's no real problem with it.


### ... to a fully lit room ... {#dot-dot-dot-to-a-fully-lit-room-dot-dot-dot}

```http
GET http://bridge/api/${username}/scenes/gXdkB1um68N1sZL
```

```js
{
  "name": "Wake Up end",
  "lights": [
    "2",
    "3",
    "5"
  ],
  "owner": "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
  "recycle": true,
  "locked": true,
  "appdata": {},
  "picture": "",
  "lastupdated": "2018-03-11T19:46:51",
  "version": 2,
  "lightstates": {
    "2": {
      "on": true,
      "bri": 254,
      "ct": 447,
      "transitiontime": 17400
    },
    "3": {
      "on": true,
      "bri": 254,
      "ct": 447,
      "transitiontime": 17400
    },
    "5": {
      "on": true,
      "bri": 254,
      "ct": 447,
      "transitiontime": 17400
    }
  }
}
```

This scene transitions the lights to full brightness over the next 29
minutes (1740 seconds), per the specified `transitiontime` (which is
specified in deciseconds).


### ... which will be switched off later. {#dot-dot-dot-which-will-be-switched-off-later-dot}

Finally, an additional rule takes care of turning the lights off and
the wake-up sensor at 9:00 (Two and a half hours after the initial
triggering of the sensor).

```http
GET http://bridge/api/${username}/rules/2
```

```js
{
  "name": "Wake up 1.end",
  "owner": "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
  "created": "2018-03-11T19:46:51",
  "lasttriggered": "2018-03-13T13:00:00",
  "timestriggered": 2,
  "status": "enabled",
  "recycle": true,
  "conditions": [
    {
      "address": "/sensors/2/state/flag",
      "operator": "eq",
      "value": "true"
    },
    {
      "address": "/sensors/2/state/flag",
      "operator": "ddx",
      "value": "PT02:30:00"
    }
  ],
  "actions": [
    {
      "address": "/groups/2/action",
      "method": "PUT",
      "body": {
        "on": false
      }
    },
    {
      "address": "/sensors/2/state",
      "method": "PUT",
      "body": {
        "flag": false
      }
    }
  ]
}
```

Unlike the first rule, this one doesn't trigger immediately. It has an
additional condition on the sensor state flag using the special `ddx`
operator, which (given the timer specified) is true **two and a half
hours after** the flag has been set. As the schedule sets it at 6:30,
that means that this rule will trigger at 9:00, turn the lights off in
the bedroom, and set the sensor's flag to `false`.


## Where to go from here {#where-to-go-from-here}

The wake-up config in the phone app touched on pretty much every major
aspect of the Hue bridge API. Given the insight I now have into how it
works, I can start constructing my own schedules and transitions, and
playing with different ways of triggering them and even having them
trigger each other.

If I get around to building my rolling sunrise, I'll be sure to get a
post up on it :)
