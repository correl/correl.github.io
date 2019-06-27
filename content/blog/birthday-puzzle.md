+++
title = "Birthday Puzzle"
author = ["Correl Roush"]
date = 2015-04-18T00:00:00-04:00
keywords = ["emacs", "org-mode", "themes"]
tags = ["programming", "prolog"]
draft = false
+++

This logic puzzle has been floating around the internet lately. When I
caught wind of it, I thought it would be a great exercise to tackle
using Prolog. I'm not especially good with the language yet, so it
added to the challenge a bit, but it was a pretty worthwhile
undertaking. When I got stumped, I discovered that mapping out the
birthdays into a grid helped me visualize the problem and ultimately
solve it, so I've included that with my prolog code so you can see how
I arrived at the answer.


## The Puzzle {#the-puzzle}

Albert and Bernard have just met Cheryl. “When is your birthday?”
Albert asked Cheryl. Cheryl thought for a moment and said, “I won’t
tell you, but I’ll give you some clues”. She wrote down a list of
ten dates:

-   May 15, May 16, May 19
-   June 17, June 18
-   July 14, July 16
-   August 14, August 15, August 17

“One of these is my birthday,” she said.

Cheryl whispered in Albert’s ear the month, and only the month, of
her birthday. To Bernard, she whispered the day, and only the
day. “Can you figure it out now?” she asked Albert.

Albert: “I don’t know when your birthday is, but I know Bernard
doesn’t know, either.”

Bernard: “I didn’t know originally, but now I do.”

Albert: “Well, now I know, too!”

_When is Cheryl’s birthday?_


## The Solution {#the-solution}


### The Dates {#the-dates}

To start off, i entered each of the possible birthdays as facts:

```prolog
possible_birthday(may, 15).
possible_birthday(may, 16).
possible_birthday(may, 19).
possible_birthday(june, 17).
possible_birthday(june, 18).
possible_birthday(july, 14).
possible_birthday(july, 16).
possible_birthday(august, 14).
possible_birthday(august, 15).
possible_birthday(august, 17).
```

And here they are, mapped out in a grid:

|    | May | June | July | August |
|----|:---:|:----:|:----:|:------:|
| 14 |     |      | X    | X      |
| 15 | X   |      |      | X      |
| 16 | X   |      | X    |        |
| 17 |     | X    |      | X      |
| 18 |     | X    |      |        |
| 19 | X   |      |      |        |


### Albert's Statement {#albert-s-statement}

> I don’t know when your birthday is,...

Albert only knows the month, and the month isn't enough to uniquely
identify Cheryl's birthday.

```prolog
month_is_not_unique(M) :-
    bagof(D, possible_birthday(M, D), Days),
    length(Days, Len),
    Len > 1.
```

> ... but I know Bernard doesn’t know, either.

Albert knows that Bernard doesn't know Cheryl's
birthday. Therefore, the day alone isn't enough to know Cheryl's
birthday, and we can infer that the month of Cheryl's birthday does
not include any of the unique dates.

```prolog
day_is_not_unique(D) :-
    bagof(M, possible_birthday(M, D), Months),
    length(Months, Len),
    Len > 1.

month_has_no_unique_days(M) :-
    forall(possible_birthday(M,D),
           day_is_not_unique(D)).
```

Based on what Albert knows at this point, let's see how we've
reduced the possible dates:

```prolog
part_one(M,D) :-
    possible_birthday(M,D),
    month_is_not_unique(M),
    month_has_no_unique_days(M),
    day_is_not_unique(D).
```

```text
Results = [ (july, 14), (july, 16), (august, 14), (august, 15), (august, 17)].
```

So the unique days (the 18th and 19th) are out, as are the months
that contained them (May and June).

|    | July | August |
|----|:----:|:------:|
| 14 | X    | X      |
| 15 |      | X      |
| 16 | X    |        |
| 17 |      | X      |


### Bernard's Statement {#bernard-s-statement}

> I didn’t know originally, but now I do.

For Bernard to know Cheryl's birthday, the day he knows must be
unique within the constraints we have so far.

```prolog
day_is_unique(Month, Day) :-
    findall(M, part_one(M, Day), [Month]).
part_two(Month, Day) :-
    possible_birthday(Month, Day),
    day_is_unique(Month, Day).
```

```text
Results = [ (july, 16), (august, 15), (august, 17)].
```

Both July and August contain the 14th, so that row is out.

|    | July | August |
|----|------|--------|
| 15 |      | X      |
| 16 | X    |        |
| 17 |      | X      |


### Albert's Second Statement {#albert-s-second-statement}

> Well, now I know, too!

Albert's month must be the remaining unique month:

```prolog
month_is_not_unique(Month, Day) :-
    findall(D, part_two(Month, D), [Day]).
part_three(Month, Day) :-
    possible_birthday(Month, Day),
    month_is_not_unique(Month, Day).
```

```text
Results = [ (july, 16)].
```

August had two possible days, so it's now clear that the only
possible unique answer is July 16th.

|    | July |
|----|:----:|
| 15 |      |
| 16 | X    |
| 17 |      |


### Cheryl's Birthday {#cheryl-s-birthday}

```prolog
cheryls_birthday(Month, Day) :-
    part_three(Month, Day).
```

```text
Month = july,
Day = 16.
```

So, there we have it. Cheryl's birthday is July 16th!

|    | July |
|----|:----:|
| 16 | X    |