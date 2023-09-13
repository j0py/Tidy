# Tidy
Tidal Cycles syntax for SuperCollider

Tidy can combine function/mini-notation-pattern pairs inside the SuperCollider Interpreter almost like you can in Tidal Cycles. I like the syntax and "tidyness" of Tidal Cycles a lot, but i want to use it in SuperCollider and be able to add/change things myself.

First i wrote a mini-notation parser, JSMiniParser, and this resulted in the Pmini quark, where you can use mini-notation in a SuperCollider pattern. The Tidy quark needs the mini-notation parser too, and so you need to install the Pmini quark if you wish to install the Tidy quark.

In the usage example below, you see me using some JSSamples class. This is a small class that makes it easier to load and use audio samples. It is included in the Tidy quark, but i could also publish it as a separate quark. Not sure what is the best thing to do here. All these quarks tend to get entangled with each other..

## Usage example

```
// start SuperCollider and then:

s.boot

JSSamples.load("samples")

JSTidy.tempo(70/60).quant(4)

// set up a "mixer":
\delay -- { 3 / 4 / \tempo.bus.asMap }
\lfo   -- { SinOsc.ar(0.1) }
\out   -- [0, 1, \copy]
\room  -- [\out, 0.5, \reverb]
\comb  -- [\out, 0.8, \comb, [\delay, \delay.bus.asMap, \pan, \lfo.bus.asMap, \dec, 4]

// trigger sounds, and send them to the mixer:
\a -- "off 0.125" |+ "note 3" | "note 0 -3 6 8" - "buf 1 2" - "snd sn" - "=out 0.1" - "=comb 0.01"
\b -- "buf 2" - "snd k2" - "=out 0.2"
\c -- "jux" - "rev" | "note [0 2 4 6 9]/10" - "def atone" - "=raw 0.02" - "=room 0.02" - "legato 2"

JSTidy.end(8)
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

The "mysamples" folder is expected to have subfolders, each containing WAV files.  
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

This file contains the JSTidy class (and supporting classes), and some extensions (extra methods) on the NodeProxy and ProxySpace classes. Together, they make possible what is shown in the example code at the start of this README.

### ProxySpace and NodeProxy: creating the mixer

#### NodeProxy

I added a few operators on NodeProxy and ProxySpace to create a "mixer" with "effects", and to enable chaining the function/pattern pairs inside the SuperCollider Interpreter, like Tidal Cycles.

```fx(function_or_symbol, extra_args)```

The fx function installs a synth at the end of the slots of the NodeProxy. If you give it a Function object, then it will use the NodeProxy \filterIn role. If you use a Symbol, then it expects that to be the name of a SynthDef that uses ReplaceOut at the end, and puts that at the end of the slots chain.

```to(str, gain=1.0)```

This method will send signal from this NodeProxy to the (other) NodeProxy with the given name in the "str" parameter.

```> (str)```

Sends the signal from this NodeProxy to other NodeProxies that have been specified in (str). The (str) is split into an array of substrings, separated by spaces. The substrings are paired (clumped), and each pair denotes the name of a NodeProxy to send to and a floating point number, which is the gain. ```"~room 0.2 ~raw 0.8"```

```< (str)```

Initializes the NodeProxy to audio rate and then adds the function + pattern in (str) to a new JSTidy object that is created around the NodeProxy.

```<< (str)```

Initializes the NodeProxy to control rate and then adds the function + pattern in (str) to a new JSTidy object that is created around the NodeProxy.

```hush(fadeTime)```

Fades out the signal of this NodeProxy and stops the Routine that is triggering sounds on it.

#### Order of execution

The correct Order of Execution is done by using the slot numbers of the NodeProxies. This let's you add new FX's and/or new playing proxies while everything else is playing, and you do not have to worry about what you do first. I haven't tried to find how NodeProxies do this internally, but as far as i can see (ahum, hear) it works.

The only problem is what slotnumbers to use when sending signal from ANY proxy to ANY other proxy in ANY order. The slotnumber must be a unique number. After some time i thought of this trick: the private bus of a proxy is unique in the server and so its index can be used as a unique slot number for that proxy on any other proxy.

```
~fx[~source.bus.index + 10] = \filterIn -> { |in| ~source * gain }
~fx[max_audiobuses + 10] = \filterIn -> { |in| LPF.ar(in, 200) }
```
I add 10 to all the slotnumbers to steer clear from the hardware ins/outs, although that might not even be necessary.

I don't know how they do it, but this way the Nodeproxies take care of the "order of execution" problem for me. I can change effects while everything is playing, i can change gains, remove them, it just works.

```
s.boot;
p = ProxySpace.push(s);
~out.play;
~raw.fx { |in| in } > "~out 0.8";
```
This "mixer" has proxy ~out playing to hardware bus, and proxy ~raw playing to ~out with a gain of 0.8. If you send audio to ~raw, it will be played to ~out, and then your speakers. You can extend this like ever you want.
Maybe a "~hpf" effect for all sounds except kick and bass to keep the mix a bit clear at the low end?

The fx() function supports supplying a Function, like above, but you can also give it a symbol (+ extra params array), which will be taken for a SynthDef name.
```
~comb.fx(\comb, [\delay, ~delay, \pan, ~lfo, \dec, 1]) > "~raw 0.8"
```

#### ProxySpace

```config(bpm, bpb)```
Installs a TempoClock with a tempo according to "bpm" (beats per minute), and also sets quantization according to "bpb" (beats per bar). I am not satisfied with this method, and if the creation of the ProxySpace ever moves into some setup.scd file, then i will remove the config function and just do things in the setup.scd file just after creating the ProxySpace.

```hush(<fadetime>)```
A short way to hush all NodeProxies.
After hush you should ```pop()``` the ProxySpace to return to the original SuperCollider environment.
And after popping, you can push a new ProxySpace and start over.

### SuperCollider's Interpreter: trigger sounds

#### Interpreter limitations

```d1 $ s "bd sn" ``` Will never work -as is- in the Interpreter.

I want to use NodeProxies anyway, so there will be differences with Tidal Cycles.  
What i have so far is this:
```
~a < "sound bd sn" - "~raw 0.3"
```
You must send the sound somewhere, or you will not hear it. Hence "~raw 0.3".

The ```#``` sign is not possible in the Interpreter, i replaced it with ```-```, which also looks quite tidy :).

#### Combining function/pattern pairs in the Interpreter

The "<" function has been added to NodeProxy to get things going. The Interpreter first encounters ```~a```, which results in a (new or existing) NodeProxy (we are in ProxySpace). The "<" method on the proxy is called, with a String argument.  
The String argument has this format: ```<function name> <pattern>```.  

The "<" method creates a JSTidy object around the proxy, and this JSTidy object will process the function and the pattern and start building a tree of objects inside itself.
The result so far is the JSTidy object, and the Interpreter continues to the ```-``` operator. I defined that operator on the JSTidy object, and it will accept, again, a function/pattern String as argument. JSTidy processes the String, returns itself, and then this can be repeated as required, calling many functions with many patterns. Each function stores something inside the tree that is built up inside the JSTidy object.

Finally, the code to interpret is all done, and then the Interpreter will call ```printOn``` on the resulting JSTidy object, so that something is written to the post window. And in the implementation of the ```printOn``` method, i have the opportunity to start (or replace) a Routine that will play the cycles.

Everywhere where i wrote "-" operator, you could also use "|>", "|+", "|*", "|<", "|/", "|%" operators or their related ones with different "where the structure comes from": "|>", ">|", "|>|".

#### But the Stack and Seq functions need to parse an array

```
(
~a < "seq 0 1 2 1" -- [
   \ -- "note 1 2 3" - "sound bd",
   \ -- "note 4 5 6" - "sound sn",
] - "~raw 0.5"
)
```

The Seq function takes a pattern of indices, and plays the fragments that the indices point to one after the other.

After interpreting ```~a < "seq 0 1 2 1"``` the Interpreter invokes the "--" operator on the JSTidy object, with an array as parameter. The "--" operator will make the elements of the array children of the preceding function/pattern in its internal tree (the Seq in this case). But first, the Interpreter will interpret the array elements one by one.

For this, the Symbol class has also received a new "--" operator: it will create a new JSTidyTree object, which will process the first function/pattern string and then return itself. The Interpreter will then invoke the "-" operator on that as before. Finally, the array will have become an array of JSTidyTree objects, each with its own tree built inside of it.

The "--" operator on JSTidy who got this array as parameter will return itself again (a JSTidy), and so after the array, you can supply more function/pattern pairs. Even another Seq should be no problem!

So yes, Seq has been implemented this way and it works. Not sure if Tidal Cycles has something similar.

Stack should be possible too, combining all triggered events of all JSTidyTree elements in the array into 1 busy cycle.
Only, if one step wants to send it's signal to the ~reverb for example, how then can you prevent all the other steps from sending to the ~reverb?

#### Sends

There is a difference between sending signal from one proxy to another in the mixer, and in one of the playing proxies.

In the mixer you can simply use the ```\filterIn -> { function }``` method as in function NodeProxy.fx().

But in a playing proxy you want a send to be patternable: some steps must send to the reverb, while others do not.

The solution i came up with involves adding more Out's to your SynthDefs.

```
SynthDef(\bla, {
    etc;
    Out.ar(\out.kr(0), sig);
    Out.ar(\send1.kr(0), sig * \gain1.kr(0));
    Out.ar(\send2.kr(0), sig * \gain2.kr(0));
    Out.ar(\send3.kr(0), sig * \gain3.kr(0));
    // as many as you want
}).add
```

If a proxy "~a" encounters a send (recognizeable by the function name starting with a "~") like "~reverb 0 1", then:
- the ~reverb proxy is created if it does not yet exist
- another proxy is created, specifically for sending signal from ~a to ~reverb
- the name of that proxy in ProxySpace will be "~a_reverb"
- the end signal of ~a_reverb is sent to the ~reverb proxy (~a_reverb has a bus, and so it has it's own slot number)
- when a step is played for proxy ~a, all the sends to other proxies are given to it's synthdef using \sendx, \gainx pairs

And that works.
If you do not add the extra Out's to your SynthDef, effects will not be sent, the \sendx/\gainx pairs are simply ignored.
If you remove the send and the re-evaluate, the extra proxy will remain in place. Proxy ~a will write nothing to its bus, because you removed the send. It will be silent.  
So it costs extra buses on the server, but we've got 1024 (or more if you want) of those..

#### Functions supported by the JSTidy

Today (2023-08-09) i have these functions implemented, but they need some thorough testing:

- sends (as described earlier)
- jux ```~a < "jux 0.6" |> "rev" | etc ```
- off ```~a < "off 0.25" |+ "n 7" | etc ```
- rev ```~a < "n 1 2 3 4" |> rev |> "s sn" etc ```
- seq (described earlier)
- a chord function with optional strumming "chord 123 024:3"
- slice ```~a < "slice 8 1 2 3 4" | etc```
- every ```~a < "every 4" |> "rev" | "n 1 2 3 4" - etc```

You might notice the "|" operator used now and then. That is the replacement for the "$" operator of Tidal Cycles. It means that all stuff after the "|" is generated first, and then the stuff before the "|" can do stings with it.
So, normally, everything is interpreted from left to right, but using the "|" operator can break through that.

```~a < "off 0.25" |+ "n 7" - "rev" | etc ```

The "etc" bit is first asked for the next cycle, and then that cycle is messed up by the "off" function and combined other functions.

The rules for function names are (up until now):
- if the name of a function starts with a "~", then it is treated as a send.
- if the name of a function exists as a function class (prefixed with "JSTidyFP_") then an object of that class is used.
- In all other cases, the function name is just added as a key in the dict for each step, with a value coming from the pattern.

So if you come up with a new SynthDef which has exotic parameter "xyz", then you can supply values for that parameter straightaway.

### Roadmap

I don't think i will have time to implement ALL functions of Tidal Cycles. I just want to implement enough to enjoy making music with. But maybe if more people join in to add functions.. who knows.

I will make some Youtube videos to show you how you can use Tidy.

The Stack function i will implement. At least the sends to other proxies have now been sorted out.

Many wild ideas come to my mind at night. A "rec x" function, that will allocate buffer "x", and record microphone input into that buffer during the next cycle (it knows the length of a cycle, so it knows how big the buffer should be). Only after re-evaluation will it record once more in the same buffer. Of course you also need a "play x" function to play buffer "x" (which will last a whole cycle). Maybe slice that up or so. Maybe even "rec x ~a" if you want to record the output of proxy "~a" and use immediately afterwards during a live coding performance.

Sleep well..
