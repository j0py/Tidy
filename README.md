# Tidy
Tidal Cycles syntax for SuperCollider

With Tidy you can use the Tidal Cycles syntax (functions and mininotation) directly inside the Interpreter of SuperCollider. I am a great fan of the syntax and "tidyness" of Tidal Cycles, but if i use something then i want to be able to dig into the source if i think i have found a bug. Also i like to be able to extend or improve it. I had a look at the Haskell language, but could not understand it easily and so decided to try and implement it in SuperCollider classes directly.

My journey so far resulted in a few classes:
- JSSamples: to easily manage my samples
- JSMini: converts mininotation to a cycle of steps
- Pmini: a pseudo pattern that delivers \dur, \str and \num keys to your Pbind, using mininotation as input.
- JSTidy: hooks into ProxySpace / NodeProxy and enables the functions syntax almost like functions in Tidal Cycles. Only a few functions have yet been implemented.

After attendeding the ICLC2023 in Utrecht i want to share this on GitHub.  
Eventually it will become one Quark called "Tidy".

## Usage example

```
// start SuperCollider and then:

"setup.scd".loadRelative

JSSamples.load("samples")

p = ProxySpace.push(s).config(70, 4)

// set up a "mix" using proxies:

~out.play
~raw.fx { |in| in } > "out 1"
~delay = { 3 / 4 / ~tempo }
~lfo = { SinOsc.ar(0.1) }
~room.fx(\eliverb) > "raw 0.5"
~comb.fx(\comb, [\delay, ~delay, \pan, ~lfo, \dec, 4]) > "raw 0.8"

// start layers of sound:

~a < "off 0.125" |+ "note 3" | "note 0 -3 6 8" - "buf 1 2" - "snd sn" - "raw 0.1" - "comb 0.01"

~b < "buf 2" - "snd k2" - "raw 0.2"

~c < "jux" - "rev" | "note [0 2 4 6 9]/10" - "def atone" - "raw 0.02" - "room 0.02" - "legato 2"

// end everything (8 seconds fadeout)

p.hush(8)
```

## Quark content:

- samples.sc (explain, usage)

- mininotation.sc (explain, usage, features supported, inner working)

- pmini.sc (explain, usage)

### Usage:
```
(
Pbindef(\x).clear;
Pbindef(\x,
	\instrument, \playbuf,
	[\dur, \num], Pmini("[[3 ~] 5(3,8)]/2"),
	\bufnum, Pfunc({ |e| JSSamples.bufnum(\kit1, e.num) ? \rest }),
).play;
)
```

- tidy.sc (explain, usage, making a mix, SCLang interpreter limitations, Tidal Cycles functions supported)

## Roadmap

Current version v1.0.0, 2023-04-25

