# Tidy
Tidal Cycles syntax for SuperCollider

Tidy can combine function/mini-notation-pattern pairs inside the SuperCollider Interpreter almost like you can in Tidal Cycles. I like the syntax and "tidyness" of Tidal Cycles a lot, but i want to use it in SuperCollider and be able to add/change things myself, without having to learn Haskell.

First i wrote a mini-notation parser which resulted in the "Pmini" quark, where you can use the Tidal Cycles mini-notation in a SuperCollider pattern. The Tidy quark needs the mini-notation parser too, so you need to install the Pmini quark together with the Tidy quark.

## Usage example

```
s.boot // or a more robuust setup with a limiter in it.

// config
Tidy.setup.load("~/samples").quant(1).bpm(110)

// setup (stereo fx) buses to write signal to
\1 -- [\gverb, roomsize: 6]

// generate signal
\k -- "map bd!4" - "buf 2" - "gain 0.2"

\s -- "off 0.125" | "degree 0 -3 6 8" - "buf 1 2" - "map sn" - "gain 0.2:6" - "mix f4"

\a -- "jux" - "rev" | "note [0 2 4 6 9]/10" - "def atone" - "gain 0.02" - "mix f2" - "legato 2"

// fade out
Tidy.end(8)
```

## Video demo's

On youtube, search for "supercollider tidy". Quality of the videos will improve as i make more of them. Trying to keep them lean and mean. Of course, as Tidy changes, some older videos will show different syntax than newer ones.

## Samples

```
Tidy.load("~/mysamples")
```
This will load samples from folder "~/mysamples". Tidy uses the "standardizePath" function to convert the given path to an absolute path, where the samples you want to use can be found.

The "mysamples" folder is expected to have subfolders, each containing WAV files.  
Subfolders usually have names like "bd", "sn", etc but can have any name.

## Effects

In Tidy, the symbols ```\1``` to ```\9``` are reserved for stereo effects. Each effect has it's own stereo input bus where others may write audio signals to. (You can get the bus using ```\2.bus``` for example)

Declaring an effect goes like this:

```\1 -- [<synthdef>, <param>, ...]```

The ```<synthdef>``` is the name of the synthdef to use (stereo in/out).

Each ```<param>``` is an optional parameter for the effect synth (example: ```decay:2```).

## Sending signal to the effects

Using the "mix" function, you can send the signal to the declared effects. The mix function needs a hexadecimal number, and each digit of that number corresponds to a declared effect. Except for the first hexadecimal digit, which corresponds to direct out. 

So in ```- "mix f004" -```, the full signal will be sent to direct out, and a quarter of the signal is sent to effect ```\3``` (the index counting starts at 0).
 
The mix function is patternable like any other: ```- "mix <f004 0800>" -```.

## Effects sending signal to each other

This can be done using the global "mix" command:

```
\mix -- "2 f3 4 f001"
```

The format is: ```"<fx> <mix> <fx> <mix> .."```.

In the example above, the output of fx ```\2``` is send 100% to direct out, and 25% to fx ```\1```.
The output of fx ```\4``` is sent 100% to direct out and 1/16th to fx ```\3```.

### Order of execution

In SuperCollider, audio is calculated in chunks of 64 samples. This has to do with a tradeoff between efficiency and latency.

An audio bus is in fact an array that can hold 64 floating point numbers (for a mono bus).

All Synth nodes "live" inside the SCSynth server in a certain order, and it is in that order that they get a chance to calculate some audio, one after the other.

For each chunk of 64 samples, roughly this happens:
- all buses are cleared (all will have 64 samples with value 0.0)
- according to their order inside SCSynth, each Synth is asked to calculate the next 64 samples
- the result of each Synth is added to the content of one or more audio buses
- finally the content of the hardware bus (audio bus with index 0) is sent to the hardware of the computer

The computer needs far less than 64 / 48000 seconds to do these calculations, and so this is very well possible. If the computer would be too slow to calculate the audio in time, you will hear it, and in the post window there will be "late" messages.

So if there is a Reverberation synth in the server that reads from bus A, adds reverberation, and writes the output to bus B, then other Synths who's output need to be reverberated must write their signal to bus A BEFORE the Reverberation synth reads it. This is known as the "Order of execution" problem, and in the SuperCollider docs it is explained much better than i did just now. The main thing is that the order of the synths inside the server matters.

When adding Synths (effects as well as triggered notes) Tidy always uses "addToHead", which is the default of SuperCollider. So you if start Synth A and then Synth B, then Synth B will calculate it's 64 samples BEFORE Synth A does. Synth B sits "on top" of Synth A inside the stack of nodes in the server.

AddToHead is a good default choice: all triggered notes will always be able to be processed by the fx Synths that exist for a longer time in the server, and thus always end up at the bottom of the stack of nodes.

If you want the ```\2``` effect to write to the ```\1``` effect, then you must define the ```\1``` effect BEFORE the ```\2``` effect. The other way around will not work: the effects will be in the wrong order inside the server. In case you make a mistake while live coding, you can redefine both the effects, while swapping their definitions. A redefined effect will not use addToHead but instead replace the existing version of the effect synth.

When re-adding an effect, it is assumed that the effect synthdef has a "gate" argument, and an envelope that fades in and out by some amount of seconds. This way, the old effect synth will fade out, while the new one fades in, and the overall sound will be affected as less as possible. This is convenient if you want to re-add an effect to change some parameters for it.

## Sequencing

The "--" operator has been added to the Symbol class in SuperCollider, and it can be given a String argument:
```
\a -- "buf 0 <1 2>" - "map hi" - "2 0.5"
```
The String argument contains the "buf" function and a mini-notation pattern. The buffer index value will be 0 during the first half of each cycle.

The result of the "--" operator is a JSTreeBuilder object. The Interpreter then continues using that object. It encounters a "-" operator with, again, a String argument. Of course, the JSTreeBuilder object has a "-" method defined, and so this method is called. The result of that call will be the same JSTreeBuilder object. And so you can have as many "-" operators with string arguments as you want. They all add their little bit of information to the JSTreeBuilder object.

If the function name in the String arguments equals the name of one of the defined effect buses, then the pattern is a mini-notation pattern for the volume with which the triggered notes will be sent to that bus. In the example above, the triggered Synths will write to the bus of the ```\2``` effect, with a gain of ```0.5```.

When the SuperCollider Interpreter has finished processing all input, the "printOn" method is called on the resulting object (which is a JSTreeBuilder object). Normally, an object is expected to post something in the post window in the "printOn" method. JSTreeBuilder does that too, but it also installs a new tree to start sequencing cycles with at the next quantisation point.

Re-evaluating the input line will install a new tree again, which will possibly generate different cycles.

To support the "stack" function of Tidal Cycles, the "--" operator has also been added to the JSTreeBuilder class. That is because the Stack function takes an array as argument. Using the Stack function, you can sequence multiple cycles in parallel. Quickly i got the idea to create a "seq" function too, that would then sequence multiple sub-sequences in any order you want:

```
(
\a -- "seq 0 2 1" -- [
   \ -- "note 1 2 3" - "map bd",
   \ -- "note 4 5 6" - "map sn",
   \ -- "note 6 5 4" - "map sn",
] - "gain 0.5"
)
```
So after processing "--" with the "seq 0 2 1" argument, the Interpreter has a JSTreeBuilder object as result. Then comes the "--" operator with an array as argument. The Interpreter will then first parse each array element, which results in an array of JSTreeBuilder objects (because each array element starts with a Symbol and a "--" operator). And this array of objects is then the parameter for the "--" operator. The JSTreeBuilder object stores the array, and returns itself. And then the Interpreter continues by parsing "-" and "gain 0.5".

The "seq" function will let one of the JSTreeBuilder objects in the array generate the next cycle. In the example above, the first cycle will be generated by the JSTreeBuilder on index 0 in the array. The next cycle will be done by the one on index 2 and so on. After the seq pattern has been run through, it will start again from the beginning.

The "stack" function also takes an array of JSTreeBuilders, and then it will combine all the cycles from all these objects into one cycle.

Instead of the "-" operator, you can also use "|>", "|+", "|*", "|<", "|/", "|%" operators or their related ones with different "where the structure comes from": "|>", ">|", "|>|". The "-" is the shortcut for "|>" (structure from the left, values from the right). In Tidal Cycles they use the "#" character for that, but in SuperCollider that character has special meaning and cannot be used as an operator.

## Sends

Triggered notes must be able to send their signal to one of the effect buses (or more than one).

You can send some signal using ``` - "2 0.2 0.6 0.1"``` for example. "2" Should correspond with the name of one of the effect buses. The pattern is a pattern of gains for the steps.

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

All mini-notation is supported.

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
Tidy.doc
```
The "|" operator is the replacement for the "$" operator of Tidal Cycles. All stuff after the "|" is generated first, and then the stuff before the "|" can do things with it.

Normally, everything is interpreted from left to right, but the "|" operator can break through that.

A good example is the "every" function:
```
\a -- "every 4" |+ "note 4" | "note 0 2 4 7" - "def atone" - "gain 0.4"
```
Each function will be documented in another markdown file that i still have to start with.

You can also have a peek in the code of course.. look for ```JSTidyFP_xxx``` functions. They usually have some comments on to including an example of how to use them.

The rules for function names are (up until now):
- if the name of the function equals the name of one of the effect buses, it is treated as a "send"
- if the name of a function exists as a function class (prefixed with "JSTidyFP_") then an object of that class is used
- in all other cases, the function name is just added as an argument to the synth, with its (float) value coming from the pattern

So if you come up with a new SynthDef which has exotic (floating point value) parameter "xyz", then you can supply values for that parameter straightaway.


