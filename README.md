# Tidy
Tidal Cycles syntax for SuperCollider

With Tidy you can use the Tidal Cycles syntax (functions and mininotation) directly "inside" the Interpreter of SuperCollider. I am a great fan of the syntax and "tidyness" of Tidal Cycles and i want to use it together with NodeProxies in ProxySpace. The proxies create the "mix", and the cycles trigger the sounds.

My journey so far (05-2023) has resulted in a few classes:
- JSSamples: easily manage samples
- JSMini: convert mininotation to a cycle of steps
- Pmini: a pseudo pattern that delivers \dur, \str and \num keys to a Pbind, using mininotation as input.
- JSTidy: hooks into ProxySpace and enables a functions syntax almost like in Tidal Cycles. Only a few functions have yet been implemented; there's a lot of them!

I start each class with my initials ("JS") so as not to conflict with any other classnames.  
Pmini is an exception though, as patterns must start with a "P".

After attendeding the ICLC2023 in Utrecht i want to share this on GitHub.  
Eventually it will become one Quark called "Tidy".

## Usage example

```
// start SuperCollider and then:

s.boot

JSSamples.load("samples")

p = ProxySpace.push(s).config(70, 4)

// set up a "mixer" using proxies:

~out.play
~raw.fx { |in| in } > "out 1"
~delay = { 3 / 4 / ~tempo }
~lfo = { SinOsc.ar(0.1) }
~room.fx(\eliverb) > "raw 0.5"
~comb.fx(\comb, [\delay, ~delay, \pan, ~lfo, \dec, 4]) > "raw 0.8"

// trigger sounds, and send them to "inputs" of the mixer:

~a < "off 0.125" |+ "note 3" | "note 0 -3 6 8" - "buf 1 2" - "snd sn" - "raw 0.1" - "comb 0.01"

~b < "buf 2" - "snd k2" - "raw 0.2"

~c < "jux" - "rev" | "note [0 2 4 6 9]/10" - "def atone" - "raw 0.02" - "room 0.02" - "legato 2"

// end everything (8 seconds fadeout)

p.hush(8)
```

## Quark content:

- samples.sc

- mini.sc

- tidy.sc

- pmini.sc

## JSSamples

JSSamples makes it easy to load folders containing WAV samples into memory, so that they can be used for triggering sounds.

```
JSSamples.load("mysamples")
```
This will load samples from folder "mysamples", which should be in the same folder as the file where the load statement above is in. Absolute paths not (yet) supported.

The "mysamples" folder is expected to have subfolders containing WAV files.  
Subfolders usually have names like "kick", "snare", etc but can have any name.

The WAV files of subfolder "animals" will end up in the global Library storage and can be reached using:

```
Library.at(\samples, \animals, index)
```
The ```\samples``` symbol is hard-coded, the ```\animals``` symbol is called the "bank" containing the samples, the ```index``` is just a numerical index of each sample.

Usually, you need the Buffer object or the bufnum of the Buffer object for a given bank/index combination.  
The "buf" and "bufnum" methods of JSSamples do that:
```
JSSamples.buf(\animals, 12)
// or:
JSSamples.bufnum(\animals, 12)
```


## JSMini

TODO: explain, usage, inner working

The first thing i started working on is the mini-notation.

At the moment (05-2023), the following is supported:
- s:n syntax, where "s" is the "bank" of the sound, and "n" the index
- ```[1 2 3 4]``` nesting multiple notes into one
- ```<1 2 3 4>``` alternate nested notes for each next cycle
- the ```/2.11``` and ```*3.2``` to make notes longer or shorter
- the ```~``` is a rest (when followed by a space) or a nodeproxy
- the ```_``` prolonges the previous note
- ```{1 2 3 4}%5``` polyrhythm
- ```1(3,8,2)``` euclydian rhythm (but.. the numbers cannot (yet) be patterns)
- ```2!4``` repeats a node n times

Usage:
```
x = JSMini("1 2 <3 6> 4")

x.next_cycle
x.next_cycle
```
Each call to "next_cycle" will return a JSMiniCycle object, containing JSMiniStep objects. A JSMiniStep object has the following values:
- "on" : time in cycles at which the step starts, usually between 0 and 1
- "dur" : duration of the step in cycles
- "string" : the string portion of a note (from the "ss:n" format)
- "number" : the number portion of a note
- "type" : can be "note", "rest" or "space". only internally used.

The example above should result in cycles "1 2 3 4" and "1 2 6 4".

The JSMiniCycle with its steps is used in Pmini and JSTidy to do their thing.

The ```logroot``` method will log the internal tree that has been parsed to the post window.  
The pattern "1 2 <3 6> 4" will result in this "tree":

```
JSMiniRoot 0/0/0 "" 0 (0,0,0)                
--JSMiniNote 0/0/0 "1" 0 (0,0,0)             
--JSMiniNote 0/0/0 "2" 0 (0,0,0)             
--JSMiniTurns 0/0/0 "" 0 (0,0,0)             
----JSMiniNote 0/0/0 "3" 0 (0,0,0)           
----JSMiniNote 0/0/0 "6" 0 (0,0,0)           
--JSMiniNote 0/0/0 "4" 0 (0,0,0)             
```
For every node in the tree, you see it's class name, followed by:  
- values for slower, faster, repeat
- the string and number part
- the euclid numbers (x,y,z)

The ```log(n)``` method will log the first n cycles to the post window.  
```JSMini("1 2 <3 6> 4").log``` looks like this:
```
cycle 0
on     dur    str num                        
0.0    0.25   "1" 0                          
0.25   0.25   "2" 0                          
0.5    0.25   "3" 0                          
0.75   0.25   "4" 0                          
cycle 1                                      
on     dur    str num                        
0.0    0.25   "1" 0                          
0.25   0.25   "2" 0                          
0.5    0.25   "6" 0                          
0.75   0.25   "4" 0                          
cycle 2                                      
on     dur    str num                        
0.0    0.25   "1" 0                          
0.25   0.25   "2" 0                          
0.5    0.25   "3" 0                          
0.75   0.25   "4" 0     
```
These functions can be used to test is JSMini is working as expected, and if not, where it may go wrong.

### Inner working

Things can get pretty complicated if you can do things like "1 2 3/5.11 4".  
The third step should play a lot more slow than steps 1 2 and 4. What JSMini does is, it reserves a quarter of the cycle for each step, and if a step wants to play slower, then it may do so within the time that has been reserved for it.  

So if for example step 3 is like "3/2", then in the first quarter cycle, it will trigger at the beginning of that quarter. But it will not fit in that quarter, so the remaining time (not fitting in the quarter) is calculated, and remembered in the step object in the parse-tree. The next time this step get's a chance to play in a quarter cycle, it will first check if there still is remaining time to wait out. So first this waiting time is "consumed", and when that has happened, then the step will trigger once again. This can happen at any speed, so "1 2 3/8.4362 4" is perfectly possible (but could you dance to it? :).

Playing faster works the same: the quarter cycle is filled with triggered steps until it has been filled completely. It's like filling a bucket (quarter of a step) with water (time). When there is water left over after the bucket is full, it is kept for the next bucket.

The given pattern string is parsed to a tree of objects, and then the root of that tree is asked for the next cycle. The root uses it's children to get their cycles and so on. The different types of objects in the tree (SC classes) go about differently when asked got their steps. This way, the nesting, alternating and such has been implemented.

The "_" results in a Space node object in the tree. The duration of this step is added to the duration of the previous one, which will then last longer. This works within one cycle, but also over to the next cycle.
Consider a pattern like ```"<_ 1> 2 _ 3"```: every other cycle, step "3" lasts 1/2 step.

Another thing i encountered with the alternating steps is, that you cannot use the cycle number to select the alternative.I did that at first using a modulo (%) operation: for 2 alternatives, use ```cycle % 2```.  
If you do that, then a pattern like ```"1 2 <3 <4 5>> 6"``` will result in "1 2 3 6", "1 2 5 6", "1 2 3 6" etc, but never "1 2 4 6". This because "<4 5>" will only be selected when the cycle number is odd, and within this step, "5" will thus always be selected.  
In JSMini, the JSMiniTurns object in the tree counts "turns" by itself (and only for itself). And uses that for the modulo operation. And then you will get "1 2 3 6", "1 2 4 6", "1 2 3 6", "1 2 5 6", etc. Fun, isn't it?

## JSTidy

TODO: explain, usage, making a mix, SCLang interpreter limitations, Tidal Cycles functions supported

## Pmini

TODO: explain

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

## Roadmap

Current version v1.0.0, 2023-04-25

