# Tidy
Tidal Cycles syntax for SuperCollider

Producing audio is done by launching Synth objects in the server, and giving them parameters (like frequency).

Each track is a line of text that you can activate to start sequencing audio (launching synths).

On this like of text you use tag/pattern pairs to specify the parameters and how they should change during the course of one _cycle_. In the pattern you use the so called "mini-notation" that is well known from TidalCycles to create timing structure and specify values.

Tidy uses the mini-notation parser that is in the Pmini quark, so you need to install that quark together with Tidy.

## Usage / syntax example

```
s.boot

Tidy.setup.load("~/samples").quant(1).bpm(110)

\1 -- \gverb - "roomsize 6"

\k -- "map bd:2!4" - "gain 0.2"

\a -- "degree [0 2 4 6 9]/10" - "def atone" - "gain 0.02" - ">1 0.2"

Tidy.end(8)
```

### Tracks

A Track is like a track in a DAW, and it can process audio data (sound) or control data (numbers that eventually control sound somewhere somehow). Each Track has an internal Bus. There are 3 _types_ of tracks: audio, fx and control.

#### Audio tracks

An audio track has a stereo audio bus, which can be _read_ by the _synth_ nodes that are played on the track.

Synths that are launched on an audio track usually are just _notes_ that play for some time, and then disappear.

#### Fx tracks

A Fx track also has a stereo audio bus, and there is only one synth in the server at a time for these kind of tracks. This synth reads the audio from the internal bus, changes it, and then sends it to other audio buses.

Synths that are launched on Fx tracks stay around much longer than synths played on Audio tracks.

#### Control tracks

A Control track has a mono control bus. At any point in time, the value on this bus is just a floating point number. A synth that is launched on a control track _writes_ a value to the control bus. Other audio of fx tracks can use these values as parameters for their synths.

### Starting a track

Each line or multi-line-block that is evaluated must start with the name of the track, in the form of a Symbol.

The name of the track will determine the _type_ of the track:   
- Fx track names are always numbers (1 ... n)
- Audio track names are always one character long (a ... z)
- Control track names are not numbers and longer than one character

If a track with the parsed name does not exist yet, then it is created (allocation of its internal bus).

After the name symbol, the special ```--``` operator follows, and this operator needs to be followed by one parameter which can be a Function, another Symbol or a String.

#### a Function

```
\a -- { |in| SinOsc.ar(\freq.kr(200), 0, 0.1) } - "freq 300"
```
The function above generates a sinewave. It could use the audio signal of its internal bus (the _in_ parameter), but in this case it does not. The frequency for the sinewave is a parameter for the synth that is created, with a default value of 200Hz. Following the function, the ```-``` operator plus a string is supplying a value of 300Hz for the frequency. This, of course, will override the default value of 200Hz. 

#### a Symbol

If we would first create a Synthdef like this (simplified version):
```
SynthDef(\asine, {
  Out.ar(0, SinOsc.ar(\freq.kr(200), 0, 0.1))
})
```
Then the server would 'know' ```\asine``` already and we could do it this way:
```
\a -- \asine - "freq 300"
```

A Function or Symbol is used to launch a synth that will generate a continuous signal for a relatively long period of time in the server. The String parameter is used to launch multiple shorter synths (a "sequence").

#### a String

```
\a -- "degree 0 2 3 4" - "def asine"
```
A string parameter holds 2 items together in 1 string: a _tag_ and a _pattern_.   

The tag can be a known tag, of which a list will follow later in this document, or a parameter for the synth that is launched. In the above example, the first string (holding "degree") determines the _structure_ for the cycles that will be sequenced here. In this case 4 notes in a cycle, so every cycle, 4 synths will be launched (and removed again), each receiving different parameters, and each generating some audio.

The "def asine" part tells us that the launched synths are of SynthDef ```\asine```.

The whole sequence eventally creates a _dictionary_ of tag/value pairs for each note (synth) that is played (launched).   
Some tags in this dictionary have a special meaning or trigger specific things. All other tags go straight into the synth parameter list when launched.

It really helps here if you already know a bit how TidalCycles is used to generate sound. A small comparison:
```
d1 $ n "0 2 3 4" # s "asine"   // a TidalCycles "orbit"

vs

\a -- "degree 0 2 3 4" - "def asine"    // a Tidy "track"
```

### You can always sequence tag/value pairs

You may have noticed above, that when using a Function or Symbol, there still can be a sequence following the Function or Symbol. The sequence will "play cycles" in that case, but instead of launching a separate synth for each note played, each step of the cycles sets the parameters on the already running synth (of the Function or Symbol).

```
consider:

\a -- { |in| SinOsc.ar(\freq.kr(200), 0, 0.1) } - "freq 300 400"

This launches 1 single synth, and every cycle, the "freq" parameter on that synth is set to 300, and then to 400 Hz.

versus:

\a -- "freq 300 400" - "def asine"

In this case, 2 separate synths are launched every cycle. One with a "freq" parameter of 300Hz, and one of 400Hz.

A subtle difference, but both scenarios have their uses.
```

#### Evaluations

There is a special case when using the sequencing-with-strings method, and that is about whether you specify a value (or values) for the "def" parameter, or not.

```
\a -- { |in| SinOsc.ar(\freq.kr(200), 0, 0.1) } - "freq 500 600"

\a -- "freq 300 400"

\a -- "freq 300 400" - "def asine"

```
Evaluating the first line will:
- launch a new synth in the server (and remove the prevous one, if any)
- start sequencing the "freq" parameter on that synth using _pattern_ "500 600".

Evaluating the second line will:
- leave the running synth as is
- start sequencing the "freq" parameter on that synth using _pattern_ "300 400".

Evaluating the third line will:
- remove the playing synth if there is one
- launch 2 synths per cycle, having "freq" parameters 300 and 400.

So adding the "def" parameter changes the behaviour from setting parameters on a running synth to launching a new synth for every step of the "freq" pattern: "300 400".

If you use ```\1``` as track name, then it would behave like an Fx, and you could then sequence parameters on the running Fx synth. Imagine a delay with a delay time that changes according to a sequence (so in the same rhythm as other tracks that are playing).

If you use, for example, ```\vel``` as track name, and you make the Function or SynthDef control-rate, then you can generate a control signal on the bus of that track, of which the value-changes will follow the rhythm of other tracks.   
How you can then use that control signal is what comes next.

### Which value prevails and who determines the cycle structure

The first tag/pattern pair of the sequence determines the structure of the cycle(s) by default.  You can alter that using different "combine" methods between the tag/pattern pairs of the sequence.

Also, there are 6 possible ways to combine tag/pattern pairs where _the same_ tag appears more than once in the sequence.

```
values                    right left add mul div mod
structure from the left    |>    |<  |+  |*  |/  |%
structure from the right    >|    <|  +|  *|  /|  %|
structure from both        |>|   |<| |+| |*| |/| |%|
structure from both        NP     <   +   *   /   % 

(NP means "not possible")
```

### Sends

You can send audio to other tracks by specifying a ```>``` character plus the name of the other track as tag. And then the pattern that follows gives a number controlling how much audio is sent to the other track.   
Bus number 0 (hardware outputs) are a track with no name: ``` - "> 0.5" - ``` would send half the audio directly to main out.

If you like hexadecimal numbers, then you can also use the "mix" tag. This is a special tag that wants to receive hexadecimal strings from the pattern. Each hexadecimal character gives us 16 possibilities for the send gain. Hexadecimal "0" to "f" are mapped to floating point numbers 0 to 1. The first character is the send value for hardware bus 0. The others are for track names ```\1```, ```\2``` etc. So you can only use the mix tag if you follow this naming convention.   
The send values from the "mix" tag are _overruled_ by the send values from the ```>``` plus track name tags.

### Controls

Using the values of control tracks in audio tracks.

```
\vel -- { SinOsc.kr(0.2).range(0.2, 0.8) }

\a -- "degree 3 5" - "def asound" - "velocity =vel 0.2"
```
Above, a control rate track named "vel" is created, and a synth is launched that writes a slow sine signal onto the bus of the track.

The audio track ```\a``` uses SynthDef ```\asound```, and this SynthDef defines a "velocity" parameter.   
On track ```\a``` two notes are played every cycle. The first note will use the value of the "vel" control bus, the second note will always have a velocity of 0.2.

A "=xxx" value in a pattern uses the control bus value of the control track with name ```\xxx```. If the control bus value is a fast changing value, then you can hear it change during each note on the audio track while that note is playing.

This way, a control track can be used by many other audio or control tracks (modular approach).

Inside a function, you can also use the control track value, because ```\xxx.bus``` will give you the control-rate bus that belongs to the track. Inside a function you could then do ```In.kr(\xxx.bus, 1)``` and use the control signal.

#### The ```cv``` tag

There is a special tag for control tracks that you can use to "just put this number value on the control bus".
```
\vel -- "cv 0.2 0.6"
```
With this tag you can change the control bus value in sync with the rhythm of all other tracks. That would be more difficult to do using a Function to generate the control signal.

### The special ```\tidy``` track

There is one track name that is special: ```\tidy```.

```
\tidy -- "scale minor" - "root 2"
```
You can use it to set a global _scale_ and/or _root_. These are used to calculate _degrees_ to frequencies.

In a sequence, you can, however override the global scale and root if you want.

If you do not set anything, then scale is ```\major``` and root note is 0 (midinote 60).

### Known tags

The following is a list of tags that have special meaning in Tidy. Any other tag will be passed on as parameter to a synth.

```
def     : what synthdef to use
map     : from which map on the disk to play samples
buf     : within the map, which sample to play
begin   : 0..1 where in the sample to start playing
end     : 0..1 where in the sample to stop playing
rate    : 0..1..n at what relative rate to play the sample
slice   : ```slice N 7 2 3 4``` cut the sample in N slices and play slice 7, 2, 3, 4 in that order
flip    : reverse the sample and align it to the right of the step

Tidy uses the frequency calculation from the SuperCollider Event object:

scale       : scale, default major [0 2 4 5 7 9 11]
degree      : index into the scale yielding the note number
note        : note number from the scale (semitiones)
octave      : octave, default 5
root        : root note (0 == C) of the scale
midinote    : note + root / octavesteps + octave - 5 * (12 * octaveRatio.log2) + 60
freq        : midinote.midicps

so either use \degree and \scale, or use \note
so either use \note, \root and \octave, or use \midinote
or directly specifiy freq

delta       : time before the next note is triggered
dur         : duration of the note (can exceed delta)
legato      : relative note length (default 0.8)
sustain     : note length (legato * dur), after which _gate_ is closed

gain        : gain for the track: ```gain 0.2:5``` go from current gain to 0.2 in 5 seconds
gain_n      : seconds during which to from one gain to another (fade in/out)
amp         : the amplitude of the audio is multiplied by this value. for creating accents.
pan         : pan audio from left (-1) to right (+1)

>, out, out1, out2, out3, out4    : bus numbers to send signal to, usually generated internally
gain0, gain1, gain2, gain3, gain4 : gains corresponding to the bus numbers above.
```

### Sub-sequence tags
This plays one of the sub-sequences from the given array:

```
\a -- "seq 0 2 1" -- [
   \-- "note 1 2 3" - "map bd",
   \-- "note 4 5 6" - "map sn",
   \-- "note 6 5 4" - "map sn",
] - "gain 0.5"
```

During the first cycle, the first subseq is played (with index 0). During the second cycle, the last subseq is played (index 2) and so on. "0 2 1" is just a pattern, so you can also do: "0!12 <2 1>"..

The following will play multilple sub-sequences in parallel on the track:

```
\a -- "stack" -- [
  \-- "hex <aaaa!3 aea6>" - "map hh",
  \-- "hex 8180" - "def kick" - "mix f0",
  \-- "hex 0808" - "map sn:3",
] - "vel [0.8 0.3 0.1 0.6 0.9]/2.1"
```

The value for \vel is given to all sub-sequences.

### Sequence modifying keys

Sequences of tag/pattern pairs can be combined / modified using the ```|``` character.

#### Off
```
\a -- "off 0.125" |+ "note -2" | "note 0 2 3 5" - "def atone"
```
Together with notes 0, 2, 3 and 5, the same notes will play, only 2 notes lower, and 0.125 beats later in time.

#### Every
```
\a -- "every 4" |+ "note -2" | "note 0 2 3 5" - "def atone"
```
Every 4th cycle, the notes will be lowered by 2.

#### Rev
```
\a -- "rev" | "note 0 2 3 5" - "def atone"
```
The cycle is played backwards.
#### Jux
```
// thank you, Alex!
\a -- "jux" - "rev" | "note 0 2 3 5" - "def atone"
```
The cycles is played in the right ear, and at the same time reversed in the left ear.

### Track functions

```
\a .mute    : mute it
\a .unmute  : unmute it
\a .hush    : stop sequencing and set gain to 0
\a .solo    : solo it (muting all others)
```

### Global functions
```
Tidy.load
Tidy.sample
Tidy.end
Tidy.bpm
Tidy.quant
Tidy.solo
Tidy.hush

```
### Order of execution

New synth nodes on _audio_ tracks are added to the _head_ of the stack of nodes in the sound server of SuperCollider, SCSynth.

New synth nodes for Fx tracks are added _right before_ the existing synth node in the server. The existing synth node is then removed, so that the new node in fact takes the place in the stack of nodes of the old node.
If there is no old synth node, then the new node is added to the _head_ of the stack of nodes in the server.

The _order_ in which Fx tracks (and their synths) are created matters. All the synths in the server get their turn to generate or process some audio or control data from _top to bottom_.   

Consider the following two lines of code:
```
\1 -- \reverb - "decay 2"

\a -- "note 2 4" - "def atone" - ">1 0.2"
```

The ```\reverb``` synth is added to the _head_ of the stack, because there is no earlier added synth in the stack for track ```\1```. Then, the ```\a``` track adds a synth (playing note 2) to the _head_ of the stack. This synth will be _above_ the ```\reverb``` synth in the stack. So if this synth sends its generated audio (note 2) to the audio bus of the ```\1``` Fx track, then the ```\reverb``` synth can then process that audio, and add reverb.

After a while, you want to add maybe a Delay effect too:
```
\2 -- \delay - "delay 0.2"
```

This synth is added to the _head_ too, and it will end up right above the ```\reverb``` synth. Remember that the synths of ```\a``` are short-lived, and quickly disappear from the server after their note has been played.   
So now, a synth on track ```\a``` can choose to send some signal to ```\2``` and/or ```\1```, and because Fx ```\2``` sits above Fx ```\1```, it can send the delayed audio through the reverb.

The point is, if you would add ```\2``` first, and then ```\1```, then the reverb would sit on top of the delay effect in the server, and it would not work.   
The way to fix that is then to _hush_ the ```\2``` track, and then evaluate it again. After that, the ```\delay``` effect synth will sit on top of the ```\reverb``` synth.

## Video demo's

On youtube, search for "supercollider tidy". Quality of the videos will improve as i make more of them. Trying to keep them lean and mean. Of course, as Tidy changes, some older videos will show different syntax than newer ones.

## Loading samples

```
Tidy.load("~/mysamples")
```
The "mysamples" folder is expected to have subfolders, each containing WAV files.  
Subfolders usually have names like "bd", "sn", etc but can have any name.
```
Tidy .sample(\bd)  : shows a list of samples in the \bd map
Tidy .sample(\bd, 2) : shows details of a specific loaded sample
Tidy .audit(\bd)     : plays all samples in the \bd map one time
```

## Mini-notation supported 

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

