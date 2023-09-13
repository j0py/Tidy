# Tidy
Tidal Cycles syntax for SuperCollider

Tidy can combine function/mini-notation-pattern pairs inside the SuperCollider Interpreter almost like you can in Tidal Cycles. I like the syntax and "tidyness" of Tidal Cycles a lot, but i want to use it in SuperCollider and be able to add/change things myself.

First i wrote a mini-notation parser, JSMini, and this resulted in the Pmini quark, where you can use mini-notation in a SuperCollider pattern. The Tidy quark needs the mini-notation parser too, and so you need to install the Pmini quark if you wish to install the Tidy quark.

## Usage example

```
s.boot

\tidy .load("samples".resolveRelative)
\tidy .bpm(70)
\tidy .quant(4)

// set up a mix
\delay -- { 3 / 4 / \tempo.bus.asMap }
\lfo   -- { SinOsc.kr(0.1) }
\out  .fx { |in, gain| In.ar(in, 2) * gain } .play(0, 1)
\room .fx(\eliverb) .play(\out, 0.5, [dec: 1])
\comb .fx { |in, gain| CombL.ar(In.ar(in, 2), 0.2, 0.2, 1.0) * gain }
      .play(\room, 0.5, [\delay, \delay.bus.asMap, \pan, \lfo.bus.asMap, \dec, 4])

// trigger sounds, and send them to the mix:
\a -- "off 0.125" |+ "note 3" | "note 0 -3 6 8" - "buf 1 2" - "snd sn" - "=out 0.1" - "=comb 0.01"
\b -- "buf 2" - "snd k2" - "=out 0.2"
\c -- "jux" - "rev" | "note [0 2 4 6 9]/10" - "def atone" - "=raw 0.02" - "=room 0.02" - "legato 2"

\tidy .end(8)
```

## Samples

```
\tidy .load("mysamples".resolveRelative)
```
This will load samples from folder "mysamples", which is resolved relative to the file where this load statement is in.
The result will be an absolute path, and that is what Tidy needs to find the samples.

The "mysamples" folder is expected to have subfolders, each containing WAV files.  
Subfolders usually have names like "kick", "snare", etc but can have any name.

The WAV files "mysamples/animals/*.wav" for example will be stored (in Buffer objects) in global Library storage:

```
Library.at(\samples, \animals, index)
```
The ```\samples``` symbol is hard-coded, the ```\animals``` symbol is called the name of the folder containing the samples, the ```index``` is just a numerical index of each sample.
```

## the Mixer

The Mixer is a collection of "effects", each gaving their own stereo input bus, and each writing to some other stereo bus or hardware output.

You need at least 1 "strip" in the mixer, where you can send audio signals, that will then go to the hardware output (bus 0).

```
\out  .fx { |in, gain| In.ar(in, 2) * gain } .play(0, 1)
```

The ".fx" function on the Symbol class receives a Function argument. The function (defined between the "{}" brackets) expects 2 arguments: the index of a bus to read signal from, and a gain. The function above does nothing special with the signal, just reads is from the given input bus, multiplies it with the gain and returns the resulting signal.

The result of the ".fx" function, called with a Function as argument, is an object that understands the ".play" method. This method needs 2 arguments: the index of a bus to write the signal to, or the name of an earlier defined fx. The second argument is again the gain. The ".play" method creates a synth from the function and it makes this synth write its output to the given output bus. The gain is passed into the synth as a parameter (the second parameter of the function).

The function that you supply can also be a bit more complicated:

```
\comb .fx { |in, gain| CombL.ar(In.ar(in, 2), 0.2, 0.2, 1.0) * gain } .play(\out, 0.5)
```

This function reads the input signal, processes it with the CombL Ugen and plays the resulting signal to the \out fx bus that was defined earlier. So it you send signal to the \comb bus, then the CombL effect is applied and then it is sent through to the \out "effect".

Instead of a Function object, you can also supply the name of a SynthDef that you have created.

```
\room .fx(\eliverb) .play(\out, 0.5, [dec: 1])
```

In this case, the SynthDef is named "eliverb", it is played to the \out bus with a gain of 0.5. After the gain, you can include an array of key/value pairs that will be arguments to the SynthDef. This also works if you defined a Function that uses arguments inside its function body.

### Order of execution

In SuperCollider, audio is calculated in chunks of 64 samples. This has to do with a tradeoff between efficiency and latency.

An audio bus is in fact an array that can hold 64 floating point numbers (a mono bus).

All Synth nodes "live" inside the SCSynth server in a certain order, and it is in that order that they get a chance to calculate some audio.

For each chunk of 64 samples, roughly this happens:
- all buses are cleared (all will have 64 samples with value 0.0)
- according to their order inside SCSynth, each Synth is triggered to calculate the next 64 samples.
- the results of each Synth is added to the content of one or more audio buses.
- finally the content of the hardware bus (audio bus with index 0) is sent to the hardware of the computer.

The computer needs far less than 64 / 48000 seconds to do these calculations, and so this is very well possible. If the computer would be too slow to calculate the audio in time, you will hear it, and in the post window there will be "late" messages.

So if there is a Reverberation synth in he server that reads from bus A, adds reverberation, and writes the output to bus B, then other Synths who's output need to be reverberated must write their signal to bus A BEFORE the Reverberation synth reads it. This is known as the "Order of execution" problem, and in the SuperCollider docs it is explained much better that i did just now.

When adding Synths (effects as well as triggered notes) Tidy always uses "addToHead", which is the default of SuperCollider. So you if start Synth A and then Synth B, then Synth B will calculate it's 64 samples BEFORE Synth A does. Synth B sits "on top" of the stack.

This is why when using Tidy, first the Mixer must be created, and then you can trigger notes. Each triggered note will result in a Synth on top of the stack of Synths, and so it's output will be processed by the synths below it. The triggered note Synths only live a short life anyway. The effect Synths on the bottom of the stack are a bit more "permanent" and so it is good that they end up at the bottom.

Still, you can replace an effect synth by re-evaluating it's code (possibly with changed parameters or gain). A new Synth will be added immediately before the old one in the stack of Synths and then the old one will be "released". The order of execution will remain valid.

To make this possible, your effect SynthDef must use a sustained envelope with gate argument: the old effect synth can then fade out after being released, while the new effect synth is started. If you use ```Env.asr(0.5, 1, 0.5).kr(2, \gate.kr(1))``` then the switch of synths will go smoothly.

## Sequencing

The "--" operator has been added to the Symbol class in SuperCollider, and it expects a String argument:

```
\a -- "buf 0" - "snd hi" - "=out 0.5"
```

The String argument contains the "buf" function and the mini-notation pattern "0". So buffer the index is 0 during the whole cycle.

The result of the "--" operator is a JSTidy object. The Interpreter then continues using that object. It encounters a "-" operator with, again, a String argument. Of course, the JSTidy object has a "-" method defined, and so this method is called. The result of that call will be the same JSTidy object. And so you can have as many "-" operators with string arguments as you want.

If the function name in the String arguments has a "=" as first character, then it means that the rest of the name of the function is in fact the name of an output bus where the triggered sound must be sent to. The pattern in that case is a mini-notation pattern for the volume of the triggered notes. In the example above, the triggered Synths will write to the bus of the \out effect, with a gain of 0.5.

When the SuperCollider Interpreter has finished processing all input, the "printOn" method is called on the resulting object (which is a JSTidy object). Normally, an object is expected to post something in the post window in the "printOn" method. JSTidy soes that too, but it also starts a Routine that will start sequencing notes at the next quantisation point.

Re-evaluating the input line will stop the old routine and start a new one at the next quantisation point.

To support the "stack" function of Tidal Cycles, the "--" operator has also been added to the JSTidy class. That is because the Stack function takes an array as argument. Using the Stack function, you can sequence multiple cycles in parallel. Quickly i got the idea to create a "seq" function too, that would then sequence multiple sub-sequences in any order you want:

```
(
\a -- "seq 0 1 2 1" -- [
   \ -- "note 1 2 3" - "sound bd",
   \ -- "note 4 5 6" - "sound sn",
   \ -- "note 6 5 4" - "sound sn",
] - "=out 0.5"
)
```

So after processing "--" with the "seq 0 1 2 1" argument, the Interpreter has a JSTidy object as result. Then comes the "--" operator with an array as argument. The Interpreter will then first parse each array element, which results in an array of JSTidy objects. And this array of JSTidy objects is then the parameter for the "--" operator. The JSTidy object stores the array, and returns itself. And then the Interpreter continues by parsing "-" and "=out 0.5".

The "seq" function will let the JSTidy objects in the array generate the triggers for the next cycle. So the first cycle will be filled by the JSTidy object on index 0 inside the array. The next cycle will be done by the JSTidy object on index 1 etc etc.

The "stack" function will soon be implemented, and it should just combine all triggers from all JSTidy objects in the array into 1 (busy) cycle.

Instead of the "-" operator, you can also use "|>", "|+", "|*", "|<", "|/", "|%" operators or their related ones with different "where the structure comes from": "|>", ">|", "|>|". The "-" is the shortcut for "|>" (structure from the left, values from the right).

## Sends

Triggered notes must be able to send their signal to one of the buses (or multiple buses) of the effects.

You can send some signal using ``` - "=out 0.2 0.6 0.1"``` for example. The "=out" means "effect bus named 'out'" and the pattern is a pattern of gains for the steps.

For that to work, all triggered Synths will receive outx/gainx arguments:


```
SynthDef(\bla, {
    var sig;

    sig = (some logic);
    
    Out.ar(\out1.kr(0), sig * \gain1.kr(0) * \gain.kr(1));
    Out.ar(\out2.kr(0), sig * \gain2.kr(0) * \gain.kr(1));
    Out.ar(\out3.kr(0), sig * \gain3.kr(0) * \gain.kr(1));
    Out.ar(\out4.kr(0), sig * \gain4.kr(0) * \gain.kr(1));
    // as many as you want
}).add
```

So you need to include these lines in your SynthDefs, although 4 outputs is probably too much; 2 should be ok.

If you defined only 2 outputs and the Synth receives out3/gain3 arguments, then these arguments will simply not be used.

## Functions supported by JSTidy

Today (2023-08-09) i have these functions implemented, but they need some thorough testing:

- jux ```\a -- "jux 0.6" |> "rev" | etc ```
- off ```\a -- "off 0.25" |+ "n 7" | etc ```
- rev ```\a -- "n 1 2 3 4" |> rev |> "s sn" etc ```
- seq (described earlier)
- a chord function with optional strumming "chord 123 024:3"
- slice ```\a -- "slice 8 1 2 3 4" | etc```
- every ```\a -- "every 4" |> "rev" | "n 1 2 3 4" - etc```

You might notice the "|" operator used now and then. That is the replacement for the "$" operator of Tidal Cycles. It means that all stuff after the "|" is generated first, and then the stuff before the "|" can do things with it.
So, normally, everything is interpreted from left to right, but using the "|" operator can break through that.

```\a -- "off 0.25" |+ "n 7" - "rev" | etc ```

The "etc" bit is first asked for the next cycle, and then that cycle is messed up by the "off" function and combined other functions.

The rules for function names are (up until now):
- if the name of a function starts with a "=", then it is treated as a send.
- if the name of a function exists as a function class (prefixed with "JSTidyFP_") then an object of that class is used.
- in all other cases, the function name is just added as an argument to the synth, with its value coming from the pattern.

So if you come up with a new SynthDef which has exotic parameter "xyz", then you can supply values for that parameter straightaway.

## Roadmap (2023-09-13)

I will make some Youtube videos to show you how you can use Tidy.

The Stack function i will implement.
