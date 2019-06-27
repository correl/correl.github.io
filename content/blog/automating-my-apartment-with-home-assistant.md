+++
title = "Automating My Apartment With Home Assistant"
author = ["Correl Roush"]
date = 2019-06-27T18:13:00-04:00
keywords = ["emacs", "org-mode", "themes"]
tags = ["home-automation"]
draft = false
+++

A while ago, I [posted about]({{< relref "hue-wake-up.md" >}}) my experiments with the Phillips Hue API
to create an automated morning sunrise effect. The end result was
nice, but all that mucking about with their HTTP APIs was a hassle any
time I wanted to tweak something. I wanted to define what I wanted in
a more declarative style, and have all the API calls managed behind
the scenes. [Home Assistant](https://www.home-assistant.io/) allowed me to do exactly that, and more.

While the Home Assistant docs are geared heavily towards setting up a
raspberry pi appliance to run everything 24/7, I don't own one, and I
already have a server going. I opted instead to get the home assistant
server running using [Docker](https://www.home-assistant.io/docs/installation/docker/), and setting up a git repository to hold
my configuration.


## A Brand New Day {#a-brand-new-day}

Setting up my sunrise was actually _really_ easy. I already had the
scenes I wanted from my [previous attempt]({{< relref "hue-wake-up.md" >}}), so it was just a matter of
codifying them in the YAML config. I split them into four scenes - a
start (dawn) and end (daylight) pair for the standing lamp at the wall
beyond the foot of the bed, and a pair for the two nightstand lights.
The end scenes include the transition time to fade in (30 minutes).

```yaml
scene:
  - name: Dawn Sun
    entities:
      light.standing_lamp:
        state: on
        brightness: 1
        xy_color: [0.6042, 0.3739]
  - name: Dawn Daylight
    entities:
      light.correls_nightstand:
        state: on
        brightness: 1
        xy_color: [0.2376, 0.1186]
      light.stephanies_nightstand:
        state: on
        brightness: 1
        xy_color: [0.2376, 0.1186]
  - name: Sunrise Sun
    entities:
      light.standing_lamp:
        state: on
        transition: 1800
        brightness: 254
        xy_color: [0.3769, 0.3639]
  - name: Sunrise Daylight
    entities:
      light.correls_nightstand:
        state: on
        transition: 1800
        brightness: 203
        xy_color: [0.2698, 0.295]
      light.stephanies_nightstand:
        state: on
        transition: 1800
        brightness: 203
        xy_color: [0.2698, 0.295]
```

Breaking them apart this way means I can trigger the "sun" first for a
splash of orange, then start up the nightstand "daylight" lights a
little bit later! This worked out well, too, since even at the lowest
brightness, having them turn on right at the start when the room is
totally dark had a tendency to jolt me awake. Staggering them produces
a much gentler effect. Scripting all of this took very little work...

```yaml
script:
  sunrise:
    alias: Sunrise
    sequence:
      - service: scene.turn_on
        data:
          entity_id: scene.dawn_sun
      - service: scene.turn_on
        data:
          entity_id: scene.sunrise_sun
      - delay:
          seconds: 180
      - service: scene.turn_on
        data:
          entity_id: scene.dawn_daylight
      - service: scene.turn_on
        data:
          entity_id: scene.sunrise_daylight
```

... and the end result really is quite pleasant:

<style>.org-center { margin-left: auto; margin-right: auto; text-align: center; }</style>

<div class="org-center">
  <div></div>

![](/ox-hugo/ha-lights-1.png)
![](/ox-hugo/ha-lights-2.png)
![](/ox-hugo/ha-lights-3.png)

</div>

That just leaves the automation, which fires a half an hour before the
_actual_ sunrise, so long as the lights aren't already on and somebody
is home (using a binary sensor I defined elsewhere based on phones
detected in the house plus an override toggle).

```yaml
automation:
  - alias: Sunrise
    action:
      - service: script.sunrise
        data: {}
    trigger:
      - platform: sun
        event: sunrise
        offset: '-00:30:00'
    condition:
      - condition: state
        entity_id: binary_sensor.occupied
        state: 'on'
      - condition: state
        entity_id: group.bedroom_lights
        state: 'off'
```

I later extended the automation with some configuration inputs, which
tie into some new triggers and conditions. I added a "latest start
time" to make sure it always gets me up in time for me to get ready
for work, and an option to disable the wake-up on weekends.

```yaml
input_select:
  sunrise_days:
    name: Days to wake up
    options:
      - Every Day
      - Weekdays
    initial: Every Day
    icon: mdi:weather-sunset
input_datetime:
  sunrise_time:
    name: Latest start time
    has_date: false
    has_time: true
    initial: '06:30'
automation:
  - alias: Sunrise
    action:
      - service: script.sunrise
        data: {}
    trigger:
      - platform: sun
        event: sunrise
        offset: '-00:30:00'
      - platform: template
        value_template: >-
          {{ states('sensor.time') == (
               states.input_datetime.sunrise_time.attributes.timestamp
               | int | timestamp_custom('%H:%M', False)
             )
          }}
    condition:
      - condition: state
        entity_id: binary_sensor.occupied
        state: 'on'
      - condition: state
        entity_id: group.bedroom_lights
        state: 'off'
      - condition: or
        conditions:
          - condition: state
            entity_id: input_select.sunrise_days
            state: Every Day
          - condition: and
            conditions:
              - condition: state
                entity_id: input_select.sunrise_days
                state: Weekdays
              - condition: time
                weekday:
                  - mon
                  - tue
                  - wed
                  - thu
                  - fri
```

Sprinkle in some groups, and I've got a nice panel in my Home
Assistant UI to manage everything:

{{< figure src="/images/ha-sunrise-ui.png" caption="Figure 1: The completed sunrise panel" >}}


## Keep It Down! {#keep-it-down}

Determined to find more things to automate, I realized that since I
have my TV audio going through a Sonos sound bar, I could very easily
automate the rather annoying ritual of leaping for the app on my phone
to turn on night mode when a movie I'm watching is getting explodey
and I realize it's a bit late in the evening to be shaking my
neighbor's walls.

```yaml
automation:
  - alias: Toggle Sonos night mode
    action:
      - service: media_player.sonos_set_option
        entity_id: media_player.den
        data_template:
          night_sound: >-
            {{ now().hour >= 22 }}
    trigger:
      - platform: time
        at: '22:30:00'
      - platform: time
        at: '08:00:00'
```

Boom. Happier neighbors, and I can fall asleep in front of movies
without worry!

Just because I could, I also added some configurability to this
automation as well. The logic got a bit tricky, since I wanted to
configure a window that crosses a 24-hour boundary. I also added a
binary sensor so I could see when night mode was enabled from Home
Assistant.

```yaml
automation:
  - alias: Toggle Sonos night mode
    action:
      - service: media_player.sonos_set_option
        entity_id: media_player.den
        data_template:
          night_sound: >-
            {% set start = states.input_datetime.sonos_nightmode_start.attributes %}
            {% set end = states.input_datetime.sonos_nightmode_end.attributes %}
            {% set now_ = (now().hour, now().minute, now().second) %}
            {% set start_ = (start.hour, start.minute, start.second) %}
            {% set end_ = (end.hour, end.minute, end.second) %}
            {% if start_ > end_ -%}
              {{ now_ >= start_ or now_ < end_ }}
            {%- else -%}
              {{ now_ >= start_ and now_ < end_ }}
            {%- endif -%}
    trigger:
      - platform: template
        value_template: "{{ states('sensor.time') == (states.input_datetime.sonos_nightmode_start.attributes.timestamp | int | timestamp_custom('%H:%M', False)) }}"
      - platform: template
        value_template: "{{ states('sensor.time') == (states.input_datetime.sonos_nightmode_end.attributes.timestamp | int | timestamp_custom('%H:%M', False)) }}"
sensor:
  - platform: time_date
    display_options:
      - time
input_datetime:
  sonos_nightmode_start:
    name: Start Night Mode
    has_date: false
    has_time: true
    initial: '22:30'
  sonos_nightmode_end:
    name: End Night Mode
    has_date: false
    has_time: true
    initial: '08:00'
binary_sensor:
  - platform: template
    sensors:
      den_night_mode:
        friendly_name: Sonos Den Night Mode
        value_template: >-
          {{ state_attr('media_player.den', 'night_sound') }}
```

And, voilà, a dashboard for my speakers, which I pretty much never
need to look at anymore!

{{< figure src="/images/ha-sonos-ui.png" >}}


## But Wait, There's More! {#but-wait-there-s-more}

It's a too much to cover in a single blog post, but there's plenty
more going on in my config. Over time, I've tweaked and added to my
device tracking to make sure Home Assistant knows when someone's home.
I set up some text-to-speech to announce the weather in the morning,
and welcome the first person to get home. I even re-purposed an old
phone as a webcam so I can check on the cat while I'm out. My config
is on my personal gitlab server, feel free to check it out and see if
there's anything there you can use or learn from:
<http://git.phoenixinquis.net/correlr/home-assistant>
