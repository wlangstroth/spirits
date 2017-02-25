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

(past (:property "blah")) => search some property of a past event

(present) => todo, inventory, etc.

(present "Lorem ipsum") => add an entry, timestamped to now

(future) => events to come, todos with deadlines

(future "Lorem ipsum") => add an upcoming event
```

Each time an action is taken, check future events for expiration.

## Format and tags

Any tags should be hashtags. When an entry is created, it should be parsed into
keywords and applied to a hash table (as index) anyway. That way, keyword search
is fairly easy. Hashtags should be a way of providing context where none exists.

## Natural language queries

"Last time I bought diapers" should be straightforward.

Stopword removal, down-casing, stemming and synonym replacement for index
building. Manual "rule" coding for now.

"Need diapers"

"Got diapers, tortilla chips and salsa" takes those off of the need list
