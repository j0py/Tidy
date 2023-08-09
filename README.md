# Tidy
Tidal Cycles syntax for SuperCollider

With Tidy you can use the Tidal Cycles syntax (functions and mininotation) directly "inside" the Interpreter of SuperCollider. I am a great fan of the syntax and "tidyness" of Tidal Cycles and i want to use it together with NodeProxies in ProxySpace. The proxies create the "mix", and the cycles trigger the sounds.

First i wrote a mininotation parser, and this resulted in the Pmini quark, where you can use mininotation in SuperCollider patterns. The Tidy quark needs the mininotation parser too, and so you need to install the Pmini quark if you wish to install the Tidy quark.

In the usage example below, you see me using some JSSamples class. This is a small class that makes it easier to load and use audio samples. It is included in the Tidy quark, but i could also publish it as a separate quark. Not sure what is the best thing to do here.

## Usage example

```
// start SuperCollider and then:

s.boot

JSSamples.load("samples")

p = ProxySpace.push(s).config(70, 4)

// set up a "mixer" using proxies:

~out.play
~raw.fx { |in| in } > "~out 1"
~delay = { 3 / 4 / ~tempo }
~lfo = { SinOsc.ar(0.1) }
~room.fx(\eliverb) > "~raw 0.5"
~comb.fx(\comb, [\delay, ~delay, \pan, ~lfo, \dec, 4]) > "~raw 0.8"

// trigger sounds, and send them to the mixer:

~a < "off 0.125" |+ "note 3" | "note 0 -3 6 8" - "buf 1 2" - "snd sn" - "~raw 0.1" - "~comb 0.01"

~b < "buf 2" - "snd k2" - "~raw 0.2"

~c < "jux" - "rev" | "note [0 2 4 6 9]/10" - "def atone" - "~raw 0.02" - "~room 0.02" - "legato 2"

// end everything (8 seconds fadeout)

p.hush(8)
```

## Quark content:

- samples.sc
- tidy.sc

## samples.sc

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

## tidy.sc

This file the JSTidy class (and supporting classes), and some extensions (extra methods) on the NodeProxy and ProxySpace classes. Together, they make possible what is shown in the example code at the start of this README.

### ProxySpace and NodeProxy: create the mix

#### NodeProxy

I added a few operators on NodeProxy and ProxySpace make it easier to create a "mix" with "effects".

##### fx(function_or_symbol, extra_args)

The fx function installs a synth at the end of the slots of the NodeProxy. If you give it a Function object, then it will use the NodeProxy \filterIn role. If you use a Symbol, then it expects that to be the name of a SynthDef that uses ReplaceOut at the end, and puts that at the end of the slots chain.

##### to(str, gain=1.0)

This method will send signal from this NodeProxy to a NodeProxy with the given name in the "str" parameter.

##### > (str)

HIER

The following operators to NodeProxy do not have to do with creating the mix, but with working like Tidal Cycles while in SuperCollider's Interpreter.

##### < (str)

##### << (str)

##### hush(fadeTime)





When making a mix with effects, somehow you have to address the problem of "order of execution". If an FX synth reads audio input from a bus, and a SOURCE synth writes audio to that bus, then it will only work if the SOURCE synth writes audio BEFORE the FX synth reads it. This can be accomplished by putting the SOURCE synth ABOVE the FX synth in the Node-graph of the SC Server.  
The SCSynth does the following for every block of audio: it clears the bus, it lets synths use the bus, activating them one after the other from the TOP of the nodetree to the BOTTOM.

NodeProxies solve this using "slots" in which you can put "objects" like a function, synth or filter. Check the documentation on "supported sources" for NodeProxy class.  
When a NodeProxy generates its audio output, it activates all processes that are attached to its slots, in slot order. So slot 0 gets a chance to use the NodeProxy's private bus first, and then slot 1, and then slot 2, etc.  
Finally the output of the NodeProxy is "ready": other NodeProxies can read it, or a Monitor on the NodeProxy bus can play the audio to hardware buses (or any other buses).

Imagine a NodeProxy acting as a SOURCE, and another NodeProxy, acting as an FX. The SOURCE NodeProxy has some synth playing on its private bus.

On the FX NodeProxy, we could do:
```
~fx[n] = \filterIn -> { |in| ~source * 0.3 }
```
This will read the finished audio from the ~source proxy, and write it on the bus of the ~fx proxy with a "gain" of 0.3. It mixes with other audio that may exist already on the ~fx proxies bus. The only question is: what slot (n) to use?

We must be able to add any proxy signal to ~fx, and for each proxy, we must use a unique number as slot number. Especially if we want to change the gain of remove it entirely, we must still remember the slot number somehow.  
It took me a while, but here's my trick: the index of the private bus of the ~source proxy is a unique number that can be used as a slot number. Every NodeProxy has it's own audio bus, and so they will all have a different bus index number.

We can ask the server how much audio buses it maximally has, and so we know that the actual effect function or synth can be placed on a slot somewhere above that number of audio buses. If the server has 1024 audio buses max, and we place the fx synth 10 slots (to be safe) above that, then we get this:
```
~fx[~source.bus.index + 10] = \filterIn -> { |in| ~source * 0.3 }
~fx[1024 + 10] = \filterIn -> { |in| LPF.ar(in, 200) }
```
I add 10 to all the slotnumbers to steer clear from the hardware ins/outs, although that might not even be necessary.

I don't know how they do it, but this way the Nodeproxies take care of the "order of execution" problem for me. I can change effects while everything is playing, i can change gains, remove them, it just works.

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

## Roadmap

Current version v1.0.0, 2023-04-25

I don't think i will have time to implement ALL functionality of Tidal Cycles. I just want to implement enough to enjoy making music with it. But maybe if more people join in to add functions.. who knows. Have to figure out this github thing..





