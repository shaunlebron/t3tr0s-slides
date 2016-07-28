# Interactive Guide to Tetris in ClojureScript

__[>> View Slide Here](http://shaunlebron.github.io/t3tr0s-slides)__

[![screen](http://fat.gfycat.com/ForthrightKindIggypops.gif)](http://shaunlebron.github.io/t3tr0s-slides)

These are interactive slides that I put together for a [Houston.js meetup
party](http://www.meetup.com/houston-js/events/198371042/) celebrating Tetris's
30th birthday.

## Setup

For viewing, serve the `public/` directory with something like:

```
$ cd public
$ python -m SimpleHTTPServer
```

For developing, use [npm-cljs](https://github.com/shaunlebron/npm-cljs):

- `cljs watch dev`
- <http://localhost:8000/dev.html>

For minified release build:

- `cljs build min`
- <http://localhost:8000/>

## Why

The presentation is intended to conclude our work on
[T3TR0S](https://github.com/imalooney/t3tr0s).  You can read our devblog there
to see how we built the game in small increments.

## Want more code?

This repo presents the essence of our game.  But you can see full
implementations below:

- [t3tr0s-full](http://github.com/imalooney/t3tr0s) fully-featured multiplayer
- [t3tr0s-bare](http://github.com/shaunlebron/t3tr0s-bare) simple version, easier to read

## Thanks

Thanks to Chris Granger for his [live-cljs
demo](https://github.com/ibdknox/live-cljs), which served as the styling base
for this presentation.

And thanks to the T3TR0S team:

- Elaine Looney
- Luis Gutierrez
- Chris Oakman
- Brett Darnell
- Phil Gambling
