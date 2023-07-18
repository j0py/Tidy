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

~a < "off 0.125" |+ "note 3" | "note 0 -3 6 8" - "buf 1 2" - "snd sn" - "~raw 0.1" - "~comb 0.01"

~b < "buf 2" - "snd k2" - "~raw 0.2"

~c < "jux" - "rev" | "note [0 2 4 6 9]/10" - "def atone" - "~raw 0.02" - "~room 0.02" - "legato 2"

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

The first thing i started working on is the mini-notation.

You can find the full specification for this in the TidalCycles website: http://tidalcycles.org/docs/reference/mini_notation/ . At this time (07-2023) everything is supported, except the "marking your feet" option where you can use a "." to create groups instead of "[]" brackets.

Earlier this year (05-2023) i did not have patternable euclidian option yet, but after a rewrite of the parser inspired by this website (https://supunsetunga.medium.com/writing-a-parser-getting-started-44ba70bb6cc9) it became possible to have the euclidian syntax patternable, for example: "bd(<3 5>/16, 8)/2" will work.

Usage:
```
x = JSMini("1 2 <3 6> 4")

x.next_cycle
x.next_cycle
```
Each call to "next_cycle" will return an array of steps, where each step is an array by itself, holding these values: trig, delta, dur, str, num;
- "trig" : if the step should play, or just occupy time silently
- "delta" : the amount of time that should pass before the next step is played
- "dur" : the duration of the step in cycles (how long does it "sound")
- "string" : the string portion of a note (from the "ss:n" format)
- "number" : the number portion of a note

The example above should result in cycles "1 2 3 4" and "1 2 6 4".

JSMini is used in Pmini and JSTidy to do their thing.

The ```log_nodes``` method will log the internal tree that has been parsed to the post window.  

The ```log_tokens``` method will log what tokens have been parsed from the pattern specification.

The ```log(n)``` method will log the first n cycles to the post window.  
```
These functions can be used to test if JSMini is working as expected, and if not, where it may go wrong.

### Inner working

The given pattern string is parsed to a tree of objects, and then the root of that tree is asked for the next cycle (a bunch of steps). The root uses it's children to get their steps and so on. Each child may have different parameters that will make it generate steps differently.  
This way, the nesting, alternating and such has been implemented.

Things can get pretty complicated if you can do things like "1 2 3/5.11 4".  
The third step should play a lot more slow than steps 1 2 and 4. What JSMini does is, it reserves a quarter of the cycle for each step, and if a step wants to play slower, then it may do so within the time that has been reserved for it.  

So if for example step 3 is like "3/2", then in the first quarter cycle, it will trigger at the beginning of that quarter. But it will not fit in that quarter, so the remaining time (not fitting in the quarter) is calculated, and remembered in the step object in the parse-tree. The next time this step get's a chance to play in a quarter cycle, it will first check if there still is remaining time to wait out. So first this waiting time is "consumed", and when that has happened, then the step will trigger once again. This may happen at any speed, so "1 2 3/8.4362 4" is perfectly possible.

Playing faster works the same: the quarter cycle is filled with triggered steps until it has been filled completely. It's like filling a bucket (quarter of a step) with water (time). When there is water left over after the bucket is full, it is kept for the next bucket.

The "_" results in a Space node object in the tree. The duration of this step is added to the duration of the previous step, which will then last longer. This works within one cycle, but also over to the next cycle.
Consider a pattern like ```"<_ 1> 2 _ 3"```: every other cycle, step "3" lasts 1/2 step.

Another thing i encountered with the alternating steps is, that you cannot use the cycle number to select the alternative.I did that at first using a modulo (%) operation: for 2 alternatives, use ```cycle % 2```.  
If you do that, then a pattern like ```"1 2 <3 <4 5>> 6"``` will result in "1 2 3 6", "1 2 5 6", "1 2 3 6" etc, but never "1 2 4 6". This because "<4 5>" will only be selected when the cycle number is odd, and within this step, "5" will thus always be selected.  
In JSMini each node in the tree counts cycles by itself: each call to the "make more steps" method will increment it. The step uses that for the modulo operation. And then you will get "1 2 3 6", "1 2 4 6", "1 2 3 6", "1 2 5 6", etc.

## JSTidy

File tidy.sc contains the code for the JSTidy class (and supporting classes), and some extensions (extra methods) on the NodeProxy and ProxySpace classes. Together, they make possible what is shown in the example code at the start of this README.

### ProxySpace and NodeProxy: create the mix

#### NodeProxy

A few extensions on NodeProxy and ProxySpace make it easier to create a "mixer" with built-in "effects".

When making a mixer with effects, somehow you have to address the problem of "order of execution". If an FX synth reads its input from a bus, and a SOURCE synth writes its audio to that bus, then it will only work if the SOURCE synth writes its audio BEFORE the FX synth reads it. This can be accomplished by putting the SOURCE synth ABOVE the FX synth in the Node-graph of the SC Server.  
The SCSynth does this for every block of audio: it clears the bus, it lets synths use the bus, activating them one after the other from the TOP of the nodetree to the BOTTOM.

NodeProxies solve this "problem" easily: each NodeProxy has a number of "slots" in which you can put "things" like a function, synth, filter. Check the documentation on "supported sources" for NodeProxy class.  
When a NodeProxy generates its audio output, it activates all processes that are attached to its slots, in slot order. So slot 0 gets a chance to use the bus of the NodeProxy first, and then slot 1, and then slot 2, ad infinitum.  
And then the output of the NodeProxy is "ready": other NodeProxies can read it, or a Monitor on the NodeProxy can play the audio to hardware buses (or any other buses).

Imagine a NodeProxy acting as a SOURCE, and another NodeProxy, acting as an FX. The SOURCE NodeProxy has some synth playing on its private bus.

On the FX NodeProxy, we could do:
```
~fx[n] = \filterIn -> { |in| ~source * 0.3 }
```
This will read the finished audio from the ~source proxy, and write it on the bus of the ~fx proxy with a "gain" of 0.3. It mixes with other audio that may exist on the ~fx proxies bus. The only question is: what slot (n) to use?

We must be able to add any other proxy signal to ~fx, and for each proxy, we must use a unique number as slot number. Especially if we want to change the gain of remove it entirely, we must still remember the slot number somehow.  
It took me a while, but here's my trick: the index of the private bus of the ~source proxy is a unique number that can be used as a slot number. Every NodeProxy has it's own audio bus, and so they will all have a different bus index number.

We can ask the server how much audio buses it maximally has, and so we know that the actual effect can be placed on a slot somewhere above that number of audio buses. If the server has 1024 audio buses max, then we get this:
```
~fx[~source.bus.index + 10] = \filterIn -> { |in| ~source * 0.3 }
~fx[1024 + 10] = \filterIn -> { |in| LPF.ar(in, 200) }
```
I add 10 to all the slotnumbers to steer clear from the hardware ins/outs.

I don't know how they do it, but this way JITLib takes care of the "order of execution" problem for me. I can change effects while everything is playing, i can change gains, remove them, it just works. Great.

I added the fx() function to NodeProxy, which will install a fx like above, but also it adds the name of the proxy to a list of effect proxies that is kept in the JSTidy class (classvar). If you lateron write something like "room 0.1", then JSTidy can recognize "room" as the name of the ~room proxy, and then knows what to do: set the gain to 0.1.

Another extension on NodeProxy is the ">" function, which will send the signal of the proxy to another proxy with a given gain.
```
s.boot;
p = ProxySpace.push(s);
~out.play;
~raw.fx { |in| in } > "out 0.8";
```
This "mixer" has proxy ~out playing to hardware bus, and proxy ~raw playing to ~out with a gain of 0.8. If you send audio to ~raw, it will be played to ~out, and then your speakers. You can extend this like ever you want!  
The fx() function supports supplying a Function, like above, but you can also give it a symbol, which will be taken for a SynthDef.
```
~comb.fx(\comb, [\delay, ~delay, \pan, ~lfo, \dec, 1]) > "raw 0.8"
```
Here, i created some ```\comb``` SynthDef and the extra arguments will be passed on to the synth instance of it. It will play to the ~raw bus.

It is developing a bit, but the way i make the "mix" will stabilize to something that is handy, and then it could be moved into some "setup.scd" to always just "be there". The ~raw could be used for the Kick and Bass sounds, and i could create a ~hpf NodeProxy with a high pass filter writing to ~raw. All instruments other than Kick or Bass must then write to ~hpf. This will prevent bass rumbling and interference in the resulting audio, maybe.

#### ProxySpace

On ProxySpace i added method config(bpm, bpb), which installs a TempoClock with a tempo according to "bpm" (beats per minute), and also sets quantisation according to "bpb" (beats per bar). I am not satisfied with this method, and if the call goes into some setup.scd file, it is no longer needed.

Method ```hush(<fadetime>)``` is also added to ProxySpace. A short way to end the performance.

I find it hard to start playing sounds again after using ```hush```. Usually i recompile the class library to totally reset supercollider and then i can start again. I still have to think about/work on that.

### Interpreter: trigger sounds

#### Interpreter limitations

The Interpreter is something i have to deal with. I messed with setting a prepocessor on the Interpreter, in which you can do things with the code-to-be-interpreted, but the result is remarkably unstable, crashing SCLang if anything goes wrong. Not recommended.

```d1 $ s "bd sn" ``` Will never work in the Interpreter.

I want to use NodeProxies anyway, so there will be differences.  
What i have so far is this:
```
~a < "s bd sn" - "raw 0.3"
```
You must send the sound somewhere, or you will not hear it. Hence "raw 0.3".

The ```#``` sign is not possible in SC Interpreter, i replaced it with ```-```, which also looks quite tidy :).

The "<" function has been added to NodeProxy to get things going. The Interpreter first encounters ```~a```, which results in a NodeProxy (we are in ProxySpace). The "<" method on the proxy is called, with a String argument.  
The String argument has this format: ```<function name> <pattern>```.  

The "<" method creates a JSTidy object around the proxy, and this JSTidy object will handle the function and the pattern. Yep, it starts building a tree again.  
The result so far is that JSTidy object, and the Interpreter continues to the ```-``` operator. Of course, the JSTidy object has this method, and it will accept, again, a String as argument. JSTidy processes the String (containing again a function name and a pattern), returns itself, and then this can be repeated as required, calling many functions with many patterns. Each function stores something inside the tree that is built up inside the JSTidy object.

Finally, the code to interpret is all done, and then the Interpreter will call ```printOn``` on the resulting JSTidy object, so that something is written to the post window. And in the implementation of the ```printOn``` method, i have the opportunity to start (or replace a running-) a Routine that will play the cycles.

Sneaky and hacky, but reliable.

#### Functions supported by the JSTidy class

#### Inner working: building a tree that produces cycles

TODO: explain, usage, Tidal Cycles functions supported

## Pmini

TODO: explain how it works internally

### Usage:
```
(
Pbindef(\x).clear;
Pbindef(\x,
	\instrument, \default,
	[\dur, \str, \num], Pmini("[[[3:4 3:5 3:6 3:7] ~] 5(3,8):4]/2"),
	\legato, 0.2,
	\degree, Pfunc({ |e|
		case { e.str == "~" } { \rest } { e.str.asInteger };
	}),
	\octave, Pfunc({ |e| (e.num ? 5).asInteger }),
).play;
)
```

## Roadmap

Current version v1.0.0, 2023-04-25

I don't think i will have time to implement ALL functionality of Tidal Cycles. I just want to implement enough to enjoy making music with it. But maybe if more people join in to add functions.. who knows. Have to figure out this github thing..





