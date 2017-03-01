# spirits

![Spirit of Christmas](http://vignette1.wikia.nocookie.net/christmasspecials/images/4/4e/Scrooge_sim.jpg)

The ghosts of past, present and future.

A personal, private twitter-type thing for keeping track of events and notes by
way of "entries". Also a kind of goofy exercise in interface, memory,
time management and natural language processing. So lisp.

Entries are like twitter "tweets". Past events are events with a definite
timestamp, future events are events with a flexible (uncertain) time.

```
(past) => all the past events

(past "last time x happened") => search for an event

(present) => todo, inventory, open trades

(present "Lorem ipsum") => add an entry, timestamped to now

(future) => events to come, todos with deadlines

(future "Event in two weeks") => add an entry that implies an upcoming event
```

Each time an action is taken, check future events for expiration.

## Hashtags

Hashtags should provide context where none exists. Might make for more
convenient searches in some cases.

## Natural language queries

"Last time I bought diapers" should be straightforward.

Down-casing and synonym replacement for index building. Manual "rule" coding for
now. Stemming is a dead end for now.

"Need diapers"

"Got diapers, tortilla chips and salsa" takes those off of the need list

## Caveat Lector

This is not serious business. I'm storing all the entries and events in a big
list, because there's little reason not to. If I were magically able to enter 50
of these a day, I'd still have fewer than 200,000 entries 10 years from
now.
