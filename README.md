# Tidy
Tidal Cycles syntax for SuperCollider

Tidy can combine function/mini-notation-pattern pairs inside the SuperCollider Interpreter almost like you can in Tidal Cycles. I like the syntax and "tidyness" of Tidal Cycles a lot, but i want to use it in SuperCollider and be able to add/change things myself, without having to learn Haskell from scratch.

First i wrote a mini-notation parser, JSMini, and this resulted in the "Pmini" quark, where you can use mini-notation in a SuperCollider pattern. The Tidy quark needs the mini-notation parser too, so you need to install the Pmini quark if you wish to install the Tidy quark.

## Usage example

```
s.boot // or a more robuust setup with a limiter in it.

// config
\tidy .load("~/samples") .cps(70) .quant(4)

// setup (stereo fx) buses to write signal to
\0 -- [0, \id]
\1 -- [\0, \eliverb, [decay: 1], 0.5]

// generate signal
\k -- "buf 2" - "snd kick" - "gain 0.2"
\s -- "off 0.125" |+ "note 3" | "note 0 -3 6 8" - "buf 1 2" - "snd sn" - "gain 0.2" - "mix f4"
\a -- "jux" - "rev" | "note [0 2 4 6 9]/10" - "def atone" - "gain 0.02" - "mix f2" - "leg 2"

// fade out
\tidy .end(8)
```

## Video demo's

On youtube, search for "supercollider tidy". Quality of the videos will improve as i make more of them. Trying to keep them lean and mean.

## Samples

```
\tidy .load("~/mysamples")
```
This will load samples from folder "~/mysamples". Tidy uses the "standardizePath" function to convert the given path to an absolute path, where the samples you want to use can be found.

The "mysamples" folder is expected to have subfolders, each containing WAV files.  
Subfolders usually have names like "bd", "sn", etc but can have any name.

The WAV files "~/mysamples/animals/*.wav" for example will be stored (in Buffer objects) in global Library storage:

```
Library.at(\samples, \animals, index)
```

The ```\samples``` symbol is hard-coded, the ```\animals``` symbol is the name of the folder containing the samples and the ```index``` is just a numerical index starting from 0.

## declaring effects

In Tidy, the symbols ```\0```, ```\1```, .. ```\9``` are reserved for stereo effects. Each effect has it's own stereo input bus where others may write audio signals to.

Declaring an effect goed like this:

```\0 -- [<output>, <synthdef>, <params>, <gain>, <target>]```

The ```<output>``` can be an integer number, interpreted as an audio bus index, or the symbol of an earlier declared effect, in which case the input bus of that effect will be the output bus for this effect.

The ```<synthdef>``` is the name of the synthdef to use (stereo in/out).

The ```<params>``` is an optional array holding parameters for the effect synth.

The ```<gain>``` is an optional gain (default 1.0) with which effect will use when writing to its output bus.

The ```<target>``` is an optional symbol of a declared effect. The synth for this effect will be added before the target synth in the node tree. Without a target, the synth for the effect will be added at the tail of the default group.

Usually, there is one effect always present:

```\0 -- [0, \id]```

The ```\id``` synthdef just copies its input to its output. This effect synth will write to hardware bus index 0 with a gain of 1.0.

And then you can do:

```\1 -- [\0, \tank, [decay: 4]]```

The ```\tank``` effect will write to the input bus of the ```\id``` effect, and it has a decay parameter supplied.

In total you can have 9 effects (because without the ```\0``` effect you will hear nothing). You can make a cascade of effects or let them all just write to hardware output. Or something in between.

The ```<synthdef>``` symbol may also be a function:


```\4 -- [\0, { |in, gain| CombL.ar(In.ar(in, 2), 0.2, 0.2, 1.0) * gain }]```

This function reads the input signal, processes it with the CombL Ugen and plays the resulting signal to the \0 effect bus that was defined earlier. So if you send signal to the \4 bus, then the CombL effect is applied and then it is sent through to the \0 bus.

## sending signal to the effects

Using the "mix" function, you can send the signal to the declared effects. The mix function needs a hexadecimal number, and each digit of that number corresponds to a declared effect. So in ```- "mix f004" -```, the full signal will be sent to effect ```\0```, and a quarter of the signal is sent to effect ```\3```.
 
The mix function is patternable like any other: ```- "mix <f004 0800>" -```.

### Order of execution

In SuperCollider, audio is calculated in chunks of 64 samples. This has to do with a tradeoff between efficiency and latency.

An audio bus is in fact an array that can hold 64 floating point numbers (for a mono bus).

All Synth nodes "live" inside the SCSynth server in a certain order, and it is in that order that they get a chance to calculate some audio, one after the other.

For each chunk of 64 samples, roughly this happens:
- all buses are cleared (all will have 64 samples with value 0.0)
- according to their order inside SCSynth, each Synth is triggered to calculate the next 64 samples
- the result of each Synth is added to the content of one or more audio buses
- finally the content of the hardware bus (audio bus with index 0) is sent to the hardware of the computer

The computer needs far less than 64 / 48000 seconds to do these calculations, and so this is very well possible. If the computer would be too slow to calculate the audio in time, you will hear it, and in the post window there will be "late" messages.

So if there is a Reverberation synth in the server that reads from bus A, adds reverberation, and writes the output to bus B, then other Synths who's output need to be reverberated must write their signal to bus A BEFORE the Reverberation synth reads it. This is known as the "Order of execution" problem, and in the SuperCollider docs it is explained much better than i did just now. The main thing is that the order of the synths inside the server matters.

When adding Synths (effects as well as triggered notes) Tidy always uses "addToHead", which is the default of SuperCollider. So you if start Synth A and then Synth B, then Synth B will calculate it's 64 samples BEFORE Synth A does. Synth B sits "on top" of Synth A inside the stack of nodes in the server.

AddToHead is a good default choice: all triggered notes will always be able to be processed by the fx Synths that exist for a longer time in the server, and thus always end up at the bottom of the stack of nodes.

If you want the \1 effect to write to the \0 effect, then you must define the \0 effect BEFORE the \1 effect. The other way around will not work: the effects will be in the wrong order inside the server. In case you make a mistake while live coding, i added a "target" argument. If you supply a target, then addBefore will be used with that target. So if \0 sits on top of \1, then re-add \1 with target \0 to correct the situation.

When re-adding an effect, it is assumed that the synthdef has a "gate" argument, and an envelope that fades in and out by some amount of seconds. This way, the old effect synth will fade out, while the new one fades in, and the overall sound will be affected as less as possible. This is convenient if you want to re-add an effect to change some parameters for it.

## Sequencing

The "--" operator has been added to the Symbol class in SuperCollider, and it expects a String argument:
```
\a -- "buf 0" - "snd hi" - "out 0.5"
```
The String argument contains the "buf" function and the mini-notation pattern "0". So the buffer index value will be 0 during the whole cycle.

The result of the "--" operator is a JSTidy object. The Interpreter then continues using that object. It encounters a "-" operator with, again, a String argument. Of course, the JSTidy object has a "-" method defined, and so this method is called. The result of that call will be the same JSTidy object. And so you can have as many "-" operators with string arguments as you want. They all add their little bit of information to the JSTidy object.

If the function name in the String arguments equals the name of one of the defined effect buses, then the pattern is a mini-notation pattern for the volume with which the triggered notes will be sent to the bus. In the example above, the triggered Synths will write to the bus of the \out effect, with a gain of 0.5.

When the SuperCollider Interpreter has finished processing all input, the "printOn" method is called on the resulting object (which is a JSTidy object). Normally, an object is expected to post something in the post window in the "printOn" method. JSTidy does that too, but it also starts a Routine that will start sequencing cycles (with steps that trigger synths) at the next quantisation point.

Re-evaluating the input line will stop the old routine and start a new one at the next quantisation point.

To support the "stack" function of Tidal Cycles, the "--" operator has also been added to the JSTidy class. That is because the Stack function takes an array as argument. Using the Stack function, you can sequence multiple cycles in parallel. Quickly i got the idea to create a "seq" function too, that would then sequence multiple sub-sequences in any order you want:

```
(
\a -- "seq 0 2 1" -- [
   \ -- "note 1 2 3" - "snd bd",
   \ -- "note 4 5 6" - "snd sn",
   \ -- "note 6 5 4" - "snd sn",
] - "out 0.5"
)
```
So after processing "--" with the "seq 0 2 1" argument, the Interpreter has a JSTidy object as result. Then comes the "--" operator with an array as argument. The Interpreter will then first parse each array element, which results in an array of JSTidy objects (because each array element starts with a Symbol and a "--" operator). And this array of JSTidy objects is then the parameter for the "--" operator. The JSTidy object stores the array, and returns itself. And then the Interpreter continues by parsing "-" and "out 0.5".

The "seq" function will let one of the JSTidy objects in the array generate the next cycle. In the example above, the first cycle will be generated by the JSTidy object on index 0 in the array. The next cycle will be done by the JSTidy object on index 2 and so on. After the seq pattern has been run through, it will start again from the beginning.

The "stack" function also takes an array of JSTidy objects, and then it will combine all the cycles from all these objects into one cycle.

Instead of the "-" operator, you can also use "|>", "|+", "|*", "|<", "|/", "|%" operators or their related ones with different "where the structure comes from": "|>", ">|", "|>|". The "-" is the shortcut for "|>" (structure from the left, values from the right). In Tidal Cycles they use the "#" character for that, but in SuperCollider that character has special meaning and cannot be used as an operator.

## Sends

Triggered notes must be able to send their signal to one of the effect buses (or more than one).

You can send some signal using ``` - "out 0.2 0.6 0.1"``` for example. "out" Should correspond with the name of one of the effect buses. The pattern is a pattern of gains for the steps.

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

## Mini-notation supported by JSTidy

All mini-notation, except the "." shorthand for pattern grouping is supported.

Below is a list, derived from the given webpage:

```
http://tidalcycles.org/docs/reference/mini_notation/

~    Create a rest                           "~ hh"
[ ]  Create a pattern grouping               "[bd sd] hh"
.    Shorthand for pattern grouping          "bd sd . hh hh hh"
,    Play multiple patterns at the same time "[bd sd, hh hh hh]"
*    Repeat a pattern                        "bd*2 sd"
/    Slow down a pattern                     "bd/2"
|    Create a random choice                  "[bd |cp |hh]"
< >  Alternate between patterns              "bd <sd hh cp>"
!    Replicate a pattern                     "bd!3 sd"
_    Elongate a pattern                      "bd _ _ ~ sd _"
@    Elongate a pattern                      "gong@3 gong"
?    Randomly remove events from pattern     "bd? sd"
:    Selecting samples                       "bd:3"
( )  Euclidean sequences                     "bd(3,8)"
{ }  Polymetric sequences                    "{bd bd bd bd, cp cp hh}"
{ }% Polymetric sequence subdivision         "{bd cp hh}%8"
```

The mini-notation is handled by classes in the Pmini quark (on which the Tidy quark depends). 


## Functions supported by JSTidy

It is cumbersome to keep this section up-to-date with the software, so i created a ".doc" command, which will post the functions supported in the post window:
```
\tidy .doc
```
The "|" operator is the replacement for the "$" operator of Tidal Cycles. All stuff after the "|" is generated first, and then the stuff before the "|" can do things with it.

Normally, everything is interpreted from left to right, but the "|" operator can break through that.

A good example is the "every" function:
```
\a -- "every 4 -1" |+ "n 4" | "n 0 2 4 7" - "d atone" - "out 0.4"
```
All cycles are generated by the bit after the "|" character. The "every" function will add 4 to the "n" value for every fourth cycle. "every 4" and "every 4 0" will do this for cycles 0, 4, 8, 12, etc. "every 4 -1" will do this for cycles 3, 7, 11, etc. The second number is subtracted from the cycle number and then the modulo operation is done using the first number. So if you want "every" to act on cycles 3, 7, 11, etc, then do "every 4 -1", because 3 - -1 = 4, modulo 4 = 0.

The ".doc" function will never be able to post all that info in the post window; it will be a summary.
Each function will be documented extensively in another markdown file.

The rules for function names are (up until now):
- if the name of the function equals the name of one of the effect buses, it is treated as a "send"
- if the name of a function exists as a function class (prefixed with "JSTidyFP_") then an object of that class is used
- in all other cases, the function name is just added as an argument to the synth, with its (float) value coming from the pattern

So if you come up with a new SynthDef which has exotic parameter "xyz", then you can supply values for that parameter straightaway.

## Roadmap (2023-09-13)

Make Youtube videos to show how to use Tidy. But for that i must first create my own sounds and/or samples.

Document all functions in a separate file, or in Tidy itself maybe (```\tidy .doc every``` ?).
