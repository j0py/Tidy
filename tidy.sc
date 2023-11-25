/*
2023-11-18
practice (out loud)
load only good samples
CmdPeriod behaviour
euclids inside <> do not seem to work (but maybe illogical to want)
idea: sample - reverse - add delays - reverse back
idea: sample - reverse - reverb - resample - loop
looper: start at frame 0, to frame y, return to frame x, to frame y, etc etc, at release, continue to last frame
idea: "fadein 8" should fade in during 8 seconds, and also fadeout? hush?
      - fadein while multiple synths are triggered
      - fadein during the lifetime of 1 longer running synth
      - both must be supported --> gain must be a control bus with a function
      - fadeout could also be done using a control bus + function!
	  - solo / unsolo could use the bus too

idea: "dup 3" | ... would repeat all cycles 3 times
idea: "do 0 1 6 7 5 6" | ... would play the given cycle numbers
idea: "rot 2" : rotate steps of each cycle 2 steps
how to implement swing? "b [1 ~ 2]@2 3 4"
idea: "life 0.6" : brings in small random variations (wow:flutter) am/fm, like velocity
idea: "seed 1234" : control randomness
idea: "trig 1000100101" or "hex 7fa4" to generate/override structure
mini notation parser more robust: monitor the index through recursive looping
mini-notation "b [0|1|2|3|~]" could be "b [0 1 2 3 ~]??" ("pick one of them randomly")
startrek samples, cars you recorded in stereo
make vital classic bend VOsc wavetables: sine - tri - saw - pulse - pulsew
test slice, fit, grow, shrink, crop !!!

create glide option = gliding "freq" parameter
  - in every SynthDef:
	freq = Select.kr(glidetime > 0, [freq, XLine.kr(glidefrom, freq, glidetime)]);
  - step.play could give synth a freq_bus arg and set a value on that bus
    this could affect a running synth with legato > 1 ! that synth would glide..
  - i think control buses could be very flexible: it would work for one long
    running synth as well as a cloud of scattered little synthies

log: maybe add a parametername to log

create fade option = gliding "gain" parameter
make all sample / synthdef volumes RIGHT
need erconv reverb or "double-tank with er" (alik rustamoff)

growl synthdef wants to send intermediate signal result to a reverb:
- growl synthdef declares args "room" and "room_bus"
- the pattern supplies: "room 0.4"
- in step.play: if the synthdef has an arg called "room", then 
  - step.play adds argument to synth: room_bus = 45
  - growl can send signal to "room_bus"
- else
  - existing put_sends mechanism will supply \outx and \gainx args to growl
so the synthdef dictates the name for the reverb (in tidal it is always "room" too)

\tidy .setup could install scope/meter too

idea: visuals: a bash routine displaying animated gifs behind your transparent window
it must be cheap on cpu and hassle free; fill a folder with the gifs and run it.

\tidy .query : does s.queryAllNodes

study these:
{
	var sig = WhiteNoise.ar(0.1);
	BLowShelf.ar(sig, MouseX.kr(300, 3000) * 0.5, 1.0, -60) +
	BHiShelf.ar(sig, MouseX.kr(300, 3000) / 0.5, 1.0, -60)
}.play

2023-10-26 wat heeft een filmer nodig
intro - theme - build - climax - theme(emotional version) - intro(conclusive version)
length of these parts depends on film material, should be easily adaptable

create your chorus fx (with also a feedback option)
*/

JSTidy {
	classvar loglevel, <postprocessors;

	var <>name, <tree, cur;

	*new { |name| ^super.new.name_(name) }

	*add_internal_synthdefs {

		Library.at(\tidy, \internal_synthdefs_added) !? { ^this };

		SynthDef(\fader, {
			var bus, val, target, fadetime;
			target = \target.kr(0);
			bus = \bus.kr(0);
			val = In.kr(bus, 1);
			val = Env([val, val, target], [0, \fadetime.kr(0)]).kr(0, \trig.tr(0));
			ReplaceOut.kr(bus, val);
		}).add;

		SynthDef(\playbuf_s, {
			var freq = \freq.kr(60.midicps, \freqlag.kr(0));
			var gate = \gate.kr(1);
			var rate = \rate.kr(1); // * freq / 60.midicps;
			var bufnum = \bufnum.kr(0);
			var begin = \begin.kr(0) * BufFrames.kr(bufnum);
			var att = \att.kr(0.02);
			var rel = \rel.kr(0.02);
			var sig = PlayBuf.ar(2, bufnum, rate, startPos: begin);
			sig = sig * Env.asr(att, 1, rel).kr(2, gate);
			sig = LeakDC.ar(sig);
			// maybe use Balance2 ?
			sig = Splay.ar(sig, 0, \vel.kr(0.5), \pan.kr(0));

			sig = sig * \gain.kr(1) * \fader.kr(1);

			Out.ar(\out1.kr(0), sig * \gain1.kr(0));
			Out.ar(\out2.kr(0), sig * \gain2.kr(0));
			Out.ar(\out3.kr(0), sig * \gain3.kr(0));
			Out.ar(\out4.kr(0), sig * \gain4.kr(0));
		}).add;

		SynthDef(\playbuf_m, {
			var freq = \freq.kr(60.midicps, \freqlag.kr(0));
			var gate = \gate.kr(1);
			var rate = \rate.kr(1); // * freq / 60.midicps;
			var bufnum = \bufnum.kr(0);
			var begin = \begin.kr(0) * BufFrames.kr(bufnum);
			var att = \att.kr(0.02);
			var rel = \rel.kr(0.02);
			var sig = PlayBuf.ar(1, bufnum, rate, startPos: begin);
			sig = sig * Env.asr(att, 1, rel).kr(2, gate);
			sig = LeakDC.ar(sig);
			sig = Pan2.ar(sig, \pan.kr(0), \vel.kr(0.5));

			sig = sig * \gain.kr(1) * \fader.kr(1);

			Out.ar(\out1.kr(0), sig * \gain1.kr(0));
			Out.ar(\out2.kr(0), sig * \gain2.kr(0));
			Out.ar(\out3.kr(0), sig * \gain3.kr(0));
			Out.ar(\out4.kr(0), sig * \gain4.kr(0));
		}).add;

		Library.put(\tidy, \internal_synthdefs_added, true);
	}
	
	// func receives the step dictionary as parameter
	*addpostprocessor { |func|
		postprocessors = postprocessors ? List.new;
		postprocessors.add(func);
	}
	
	*should_log { |level|
		loglevel ?? { loglevel = [] };
		^(loglevel.indexOf(level).notNil);
	}

	*log { |level|
		loglevel ?? { loglevel = [] };
		level ?? { loglevel = [] } !? { loglevel = loglevel.add(level) };
	}

	*end { |seconds=1|
		Library.at(\tidyar) !? { |dict|
			dict.keys.do { |name|
				Library.at(\tidyar, name, \routine) !? {
					JSTidy.hush(name, seconds)
				}
			}
		};

		Library.at(\tidykr) !? { |dict|
			Routine({
				(seconds * TempoClock.tempo).wait;
				dict.keys.do { |name|
					Library.at(\tidykr, name, \synth) !? { |synth|
						synth.release
					};
					"released %".format(name).postln;
				}
			}).play;
		};
	}
	
	*hush { |name, seconds=1|
		Routine({
			// stop the routine
			var beats = max(0.2, seconds) * TempoClock.tempo;
			Library.put(\tidyar, name, \fade, beats);
			beats.wait;
			Library.at(\tidyar, name, \routine) !? { |routine|
				Library.put(\tidyar, name, \routine, nil);
				routine.stop
			};
			Library.put(\tidyar, name, \fade, nil);
			
			"hushed %".format(name).postln;
		}).play;
	}

	// cycles per second
	*cps { |cps|
		if(cps.isNil) {
			var cps = TempoClock.tempo;
			"tempo: % cps (% bpm)".format(cps, cps * 4 * 60).postln;
			^cps;
		} {
			TempoClock.tempo_(cps);
			("\\tempo -- { DC.kr(" ++ (cps) ++ ") }").interpret;
		}
	}
	
	// quant is in cycles (= 1 TempoClock beat)
	*quant { |quant|
		if(quant.isNil) { ^(Library.at(\tidy, \quant) ? 1) };
		Library.put(\tidy, \quant, quant.asInteger)
	}

	*quantize {
		var now, quant;
		now = thisThread.beats;
		quant = JSTidy.quant;
		((now + quant).div(quant) * quant - now).wait;
	}

	*load { |folder|

		var f = {
			var s = Server.default;
			folder = folder.standardizePath;
			(folder +/+ "*").pathMatch.do({ |bank|
				var index = 0;
				(bank +/+ "*.wav").pathMatch.do({ |file|
					Library.put(
						\samples,
						bank.basename.withoutTrailingSlash.asSymbol,
						index,
						Buffer.read(s, file)
					);
					index = index + 1;
				});
				s.sync;
			});
			s.sync;
			"Loaded %".format(folder.quote).postln;
			JSTidy.loaded;
		};
		
		if(thisProcess.mainThread == thisThread) { Routine(f).play } { f.value };
	}

	*loaded { |width=40|
		var len=width;
		"".padLeft(width, "-").postln;
		if(Library.at(\samples).isNil) {
			"** no banks / samples **".post
		} {
			Library.at(\samples).keys.asArray.sort.do({ |k|
				var val = Library.at(\samples, k.asSymbol);
				var str = "% % - ".format(k, val.size);
				if((len - (str.size)) < 0) { "".postln; len=width };
				len = len - (str.size);
				str.post;
			});
		};
		"".postln;
		"".padLeft(width, "-").postln;
		^"".postln;
	}
	
	printOn { |stream|
		// if tree is nil then something has gone wrong during the
		// creation of the new tree. in that case: stop here, so that the old
		// tree will keep going.
		tree ?? { "tree nil".postln; ^this };

		if(JSTidy.should_log(\tree)) { tree.log };

		name.printOn(stream);  // output to postwindow

		// make sure that only 1 routine will replace the running
		// routine at the next quantisation point. one could
		// evaluate some code twice within one quantisation period!
		Library.at(\tidyar, name, \evaluated) ?? { 

			// a logical cycle lasts 1 TempoClock beat, quant is in cycles
			Library.put(\tidyar, name, \evaluated, Routine({
				var quant, nudge=0.001, now, wait;

				//JSTidy.add_synthdefs;
				//Server.default.sync; // this might take some time
				
				now = thisThread.beats; // so do quantisation now
				quant = JSTidy.quant;

				// stop the old routine <nudge> before starting the new one.
				// nudge=0 : old routine triggers one more note, while the
				// new routine also triggers it -> gives double note!
				wait = (now + quant).div(quant) * quant - now;
				if(wait < nudge) { wait = wait + quant };
				
				(wait - nudge).wait;
				Library.at(\tidyar, name, \routine) !? { |r| r.stop };

				nudge.wait;
				Library.put(\tidyar, name, \routine, Routine({
					var cycle, fading, faded, fadebeats, gain=1, repeat;
					
					// enable re-evaluation
					Library.put(\tidyar, name, \evaluated, nil);

					cycle = tree.get(JSTidyCycle.new, name);

					repeat = inf;
					cycle.steps.do({ |step|	step.at(\once) !? { repeat = 1 } });

					fading = false;
					repeat.do({
						if(JSTidy.should_log(\cycle), { cycle.postln });

						// todo: name this one "fadeout" and add "fadein" too :-)
						Library.at(\tidyar, name, \fade) !? { |beats|
							Library.put(\tidyar, name, \fade, nil);
							fading = true;
							faded = 0;
							fadebeats = beats;
						};

						cycle.steps.do({ |step|
							var slow;
							if(fading) { gain = faded.linexp(0,fadebeats,1,0.001) };

							step.play(name, gain);
							step.log;

							slow = (step.at(\slow) ? "1").asFloat;
							(step.delta * slow).wait;

							if(fading) { faded = faded + (step.delta * slow) };
						});

						cycle = tree.get(JSTidyCycle.new, name);
					});

					// you get here when the "once" function is used
					// this Routine will stop now, so remove it from library
					Library.put(\tidyar, name, \routine, nil);
					
				}).play);
			}).play);
		};
	}

	// if you make a mistake, you might get here.
	// printOn will do nothing if tree is nil: current sound keeps playing
	doesNotUnderstand { |selector ... args|
		tree = nil;
		JSTidyException("% not understood".format(selector)).throw;
	}

	-- { |array|
		var cur_is_branch;
		
		array.do { |jstidy| cur.add(jstidy.tree) };

		// cur is a Seq or a Stack, cur must be closest parent branch
		// hey j0py, better documentation please..
		while { cur.parent.notNil.and(cur.is_branch.not) } {
			//"cur2 %".format(cur).postln;
			cur = cur.parent;
		};
	}

	// returns a JSTidyXX function. str format: "<function name> <pattern>"
	// a JSTidyXX function takes a cycle, maybe alters it, and returns it.
	func { |str|
		var func, pat, class;

		str = str.split($ );
		func = str.removeAt(0);
		pat = str.join($ ).stripWhiteSpace;

		class = "%%".format(func[0].toUpper, func.drop(1).toLower);
		class = "JSTidyFP_%".format(class).asSymbol.asClass;
		class !? { ^class.new(pat) };
		^JSTidyFP(func, pat);
	}

	// add an object to the lazily created tree
	add { |obj|
		tree ?? { tree = cur = JSTidyBranch("tree") };
		cur.add(obj);
		obj.parent = cur;
		if(obj.become_cur_after_add) {
			cur = obj
		} {
			obj.children.do { |child|
				if(child.become_cur_after_add) { cur = child }
			}
		}
	}

	// create a JSTidy function and add it to the tree
	add_func { |str| this.add(this.func(str)) }

	// create and add a JSTidy branch to the tree
	// a JSTidy branch let's all its children consequtively alter a cycle.
	add_branch { |str| this.add(JSTidyBranch(str)) }

	// the | operator adds a branch (like the $ in Tidal Cycles)
	// in SuperCollider's Interpreter, the "$" operator is not possible
	// because the $ is in use for writing character entities. And so
	// the Interpreter will get confused if the $ is used.
	| { |str| this.add_branch("|").add_func(str) }

	// following operators add JSTidy functions to the tree,
	// thereby specifying who should define the timing structure
	// and what should be done with the value(s) coming from the
	// JSTidy functions (who will be overriding the other if both
	// specify a value for the same key).
	// In the end, all values for keys end up in a Dictionary inside
	// each generated JSTidyStep.
	//
	|<| {  |str| this.add(JSTidyCombBoth("<").add(this.func(str))) }
	< {  |str| this.add(JSTidyCombBoth("<").add(this.func(str))) }
	|< { |str| this.add(JSTidyCombLeft("<").add(this.func(str))) }
	<| { |str| this.add(JSTidyCombRight("<").add(this.func(str))) }

	|>| {  |str| this.add(JSTidyCombBoth(">").add(this.func(str))) }
	// this one is also problematic for the Interpreter
	//> {  |str| this.add(JSTidyCombBoth(">").add(this.func(str))) }
	|> { |str| this.add(JSTidyCombLeft(">").add(this.func(str))) }
	>| { |str| this.add(JSTidyCombRight(">").add(this.func(str))) }

	// the "-" is a tidy shortcut (like the # in Tidal Cycles).
	// the "#" is not possible in the Interpreter sadly.
	- { |str| this.add(JSTidyCombLeft(">").add(this.func(str))) }

	|+| {  |str| this.add(JSTidyCombBoth("+").add(this.func(str))) }
	+ {  |str| this.add(JSTidyCombBoth("+").add(this.func(str))) }
	|+ { |str| this.add(JSTidyCombLeft("+").add(this.func(str))) }
	+| { |str| this.add(JSTidyCombRight("+").add(this.func(str))) }

	|*| {  |str| this.add(JSTidyCombBoth("*").add(this.func(str))) }
	* {  |str| this.add(JSTidyCombBoth("*").add(this.func(str))) }
	|* { |str| this.add(JSTidyCombLeft("*").add(this.func(str))) }
	*| { |str| this.add(JSTidyCombRight("*").add(this.func(str))) }

	|/| {  |str| this.add(JSTidyCombBoth("/").add(this.func(str))) }
	/ {  |str| this.add(JSTidyCombBoth("/").add(this.func(str))) }
	|/ { |str| this.add(JSTidyCombLeft("/").add(this.func(str))) }
	/| { |str| this.add(JSTidyCombRight("/").add(this.func(str))) }

	% {  |str| this.add(JSTidyCombBoth("%").add(this.func(str))) }
	|% { |str| this.add(JSTidyCombLeft("%").add(this.func(str))) }
	%| { |str| this.add(JSTidyCombRight("%").add(this.func(str))) }
}

// a JSTidyCycle holds JSTidySteps.
JSTidyCycle {
	var <>steps, <>env;

	*new { |steps| ^super.new.make_steps(steps).make_index }

	make_steps { |steps_array|
		// step_array: [ [<trig>, <delta>, <dur>, <str>, <num>], .. ]
		steps_array !? {
			steps = steps_array.collect { |el|
				if(el.isArray) {
					el = JSTidyStep(el[0], el[1], el[2], el[3], el[4]);
				};
				el;
			}
		}
	}

	make_index {
		var indexes=[], times=[];
		steps !? {
			steps.do { |step, index|
				indexes = indexes.add(index);
				times = times.add(0);
				indexes = indexes.add(index);
				times = times.add(step.delta);
			};
			times.removeAt(0);
			env = Env(indexes, times);
		}
	}

	// use an Env to get a step index, given a time
	at { |time|
		steps ?? { ^nil };
		env ?? { ^nil };
		^steps.at(env.at(time));
	}

	printOn { |stream|
		stream << "cycle\n";
		steps !? { steps.do { |step| step.printOn2(stream) } };
	}
}

JSTidyStep {
	var <>trig, <>delta, <>dur, <>dict;

	*new { |trig, delta, dur, str, num|
		^super.newCopyArgs(trig ? 0, delta ? 1, dur ? 1)
		.dict_(Dictionary.new)
		.put(\str, str)
		.put(\num, num);
	}

	putAll { |argdict| dict.putAll(argdict) }
	put { |key, value| dict.put(key.asSymbol, value) }
	at { |key| ^dict.at(key.asSymbol) }

	/*
		about timing and tempo:
		-----------------------
		1 cycle == 1 TempoClock beat == 1 bar
		the total "delta" of all steps in a cycle = 1 TempoClock beat
		the "dur" of a step is in TempoClock beats
		sustainbeats = dur * legato
		sustainseconds = sustainbeats / TempoClock.tempo
		all synthdefs will have their gate shut after sustainbeats
		all synthdefs will receive sustainseconds as argument

		"slow"/"fast" do not alter TempoClock.tempo (this would affect other tracks)
		they make delta, duration, sustain bigger or smaller

		.wait is in TempoClock BEATS

		when playing a sample buffer:
        -----------------------------
		"fit", "grow", "shrink" alter buffer playback "rate".
		"grow" lowers the rate if buffer playback time < sustainseconds
		"shrink" increases the rate if buffer playback time > sustainseconds
        "fit" applies "shrink" or "grow" to fit buffer in sustainseconds exactly
		"stretch" stretch delta, duration and sustain to contain entire buffer duration
		default behaviour is to only stretch the sustain to match buffer duration

		TODO
		create control bus for frequency, along with a synth at kr rate getting
		a new setpoint for every step (freq + glide percentage)
		if glide is applicable for a step, then give the bus .asmap as freq arg
		if not, then just give a number as arg

		TODO
		create control bus for fader (all sends must be POST fader: if you
		fadeout some track, all the sends must fadeout too. I already have the
		synthdef for it (uses an Env).

		solo/unsolo, mute/unmute work with flags in Library.at(\tidy, \solo) and
		Library.at(\tidy, \mute). All steps check if any tracks are set in the
		solo or mute maps. If there are, and the track is among them, then the track
		can play (solo) or not (mute). if the track is not among them, it can play
		(mute) or not (solo). If the maps are empty, then all tracks can play;
		no one has been soloed or muted in that case. Inside step.play, just
		set the \gain parameter of the step to 0. This leaves the fader as is,
		and all the sends will shutup too.

		somehow i want to have overview and easy changing of faders/solo/mute/send

		\tidy .faders(0, 0, 0.2, 0, 0.4, 0, 1.0, 0, 0, 0) // 16 of them?
		\tidy   .solo(0, 0, 0, 0, 0, 1, 1, 0, 1, 0)
		\tidy   .mute(0, 0, 0, 0, 0, 1, 1, 0, 1, 0)
		\tidy   .send(\room, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0)

		\tidy .solo(\a, \b)
		\tidy .mute(\d)
		\tidy .unsolo(\b)
        \tidy .unmute_all
		etc

		would be quicker to have some curses window where you put the cursor
		and then type a number 0-9 (which would be enough precision)
		a bit like orca; the interpreter should re-interpret it from time 2 time
		
		(
		// all fader and send volumes written as 0-9, internally divided by 9
        //      f s m sends
		\a --- "1 - - out 4 room 2 comb 1"
		\b --- "3 - - out 6 room 0 comb 1"
		)
		
		mute/solo should simply set \trig to 0, just like degrade

		// would this work?
		\mute \a
		\solo \b
		\unsolo \all
		\faders \a 4 \b 6 \c 8

		// the sends can now be removed from the patterns, you can change them
		// without re-starting the pattern loop. But you cannot pattern them..
    */

	// \mute -- "a x b" could work because you can behave differently if
	// the symbol is the \mute symbol! \solo idem.

	play { |name, gain=1|
		var def, sustainbeats, buf;
		var rate = (dict.at(\rate) ? 1);

		//this.set_fader(name);
		//this.set_solo(name);
		
		// give degrade function a chance to clear the trigger
		if(trig > 0) { dict.at(\degrade) !? { |v| trig = v.coin.asInteger }	};
		if(trig <= 0) { ^this };

		dict.at(\legato) ?? {
			dict.put(\legato, 0.8); 
			dict.at(\stretch) !? { dict.put(\legato, 1) };
		};

		this.set_freq(name);
		sustainbeats = this.set_sustain(name);

		if((buf = this.set_buf(name)).isString) { buf.postln; ^this };

		buf !? {
			var bufseconds, bufbeats;

			this.put(\bufnum, buf.bufnum);

			// played notes change playbuf rate
			rate = rate * (dict.at(\freq) ? 60.midicps) / (60.midicps);

			// buffer samplerate vs system samplerate
			rate = rate * buf.sampleRate / Server.default.sampleRate;

			// calculate buffer duration in beats
			bufseconds = buf.duration / (dict.at(\slices) ? 1);
			bufbeats = bufseconds * TempoClock.tempo;

			// adjust rate / sustainbeats according to functions applied
			case
			// fit: adjust rate so that playbuf will last sustainbeats
			{ (dict.at(\fit) ? 0) > 0 } { rate = rate * bufbeats / sustainbeats }

			// grow: if buffer is small, grow it until sustainbeats
			{ (dict.at(\grow) ? 0) > 0 } {
				if(bufbeats < sustainbeats) { rate = rate * bufbeats / sustainbeats }
			}

			// shrink: if buffer is long, shrink it into sustainbeats
			{ (dict.at(\shrink) ? 0) > 0 } {
				if(bufbeats > sustainbeats) { rate = rate * bufbeats / sustainbeats }
			}

			// crop: let sustainbeats as is, which may cutoff a long buffer
			{ (dict.at(\crop) ? 0) > 0 } { }

			// stretch: adjust the timing into the next step so that the buffer
			// can be played in full; take release time into account though
			{ (dict.at(\stretch) ? 0) > 0 } {
				var relbeats;
				
				// stretch the cycle to contain buf

				// keep simulaneous notes simultaneous -- "n [0,2,4]" -
				if(delta > 0) { delta = bufbeats / dict.at(\legato); };

				// start the release phase before the gate shuts for sample fadeout
				relbeats = (dict.at(\rel) ? 0) * TempoClock.tempo;
				sustainbeats = max(0, bufbeats - relbeats);
			}
			// default: play whole sample (can overflow next cycle(s))
			{ sustainbeats = bufbeats };
		};

		dict.at(\speed) !? { |speed| rate = rate * speed };
		dict.put(\rate, rate);
		dict.put(\sustain, sustainbeats / TempoClock.tempo); // synthdef arg is in sec

		// determine instrument (synthdef) to use
		buf !? { if(buf.numChannels > 1) { def = \playbuf_s } { def = \playbuf_m } };
		dict.at(\def) !? { def = dict.at(\def).asSymbol };
		def ?? { "no def".postln; ^this };
		dict.put(\def, def); // also for logging
		def = SynthDescLib.at(def.asSymbol);
		def ?? { "def % unknown".format(dict.at(\def)).postln; ^this };

		// gain and sends
		dict.put(\gain, gain * (dict.at(\gain) ? 1));
		this.put_sends;

		Routine({
			var synth;
			
			// micro-timing: laid-back snaredrum
			dict.at(\late) !? { |ms| (ms.clip(0, 20) / 1000 * TempoClock.tempo).wait };
			
			// play note
			JSTidy.postprocessors.do { |p| p.(dict) };

			Server.default.bind { synth = Synth(def.name, dict.asPairs) };

			// end note
			if(def.hasGate) {
				sustainbeats.wait;
				Server.default.bind { synth.set(\gate, 0) };
			};
		}).play;
	}

	set_buf { |name|
		var buf;
		
		// playing a sample from the disk
		this.at(\snd) !? { |bank|
			var index = (dict.at(\buf) ? 0).asInteger;

			(buf = Library.at(\samples, bank.asSymbol, index)) ?? {
				^"buf % % unknown".format(bank, index);
			};

			^buf;
		};

		// playing a recorded sample
		this.at(\play) !? { |rec|
			(buf = Library.at(\tidyrec, rec.asSymbol)) ?? {
				^"rec buf % unknown".format(rec);
			};

			^buf;
		};

		^nil;
	}
		
	set_sustain { |name|
		var beats;
		
		beats = dict.at(\legato) * dur;
		^(beats * (dict.at(\slow) ? 1) / (dict.at(\fast) ? 1));
	}
	
	set_freq { |name|
		var scale = Scale.at((dict.at(\scale) ? \major).asSymbol);

		dict.at(\freq) ?? {
			var steps = 12;
			var mtranspose = dict.at(\mtranspose) ? 0;
			var degree = (dict.at(\note) ? 0).asFloat;
			var gtranspose = 0;
			var root = dict.at(\root) ? 0;
			var oct = dict.at(\octave) ? 5;

			var note = (degree + mtranspose).degreeToKey(scale, steps);
			// midi is the midinote (continuous intermediate values)
			var midi = ((note + gtranspose + root) / steps+oct) * 12.0;
			var ctranspose = 0;
			var harmonic = 1.0;
			var detune = 0;

			dict.at(\midinote) !? { midi = dict.at(\midinote) };
			dict.put(
				\freq,
				(midi + ctranspose).midicps * harmonic + detune
			);
		};
	}
	
	set_solo { |name|
		// we need a solo/unsolo bus just like with move_fader because:
		// - a running synth must hush if another orbit goes solo
		// - after the solo, the running synth must return to its fader setting
		// solo must not move the fader
		// all synths will have arg \solo.kr(1), so they assume that they are
		// 
	}
	
	set_fader { |name|
		var bus;
		
		(bus = Library.at(\tidyar, name, \faderbus)) ?? {
			Library.put(\tidyar, name, \faderbus, (bus = Bus.control));
			dict.at(\fadein) ?? { bus.setSynchronous(1) };
		};

		/*
		dict.at(\fadein) !? { |seconds|
			Routine({
				var val, local;

				Library.global.removeAt(\tidyar, name, \fadersynth) !? { _.free };
				val = bus.getSynchronous;
				
				if((seconds = val.linlin(0, 1, seconds, 0)) > 0) {
					Library.put(\tidyar, name, \fadersynth, (local = {
						Line.kr(val, 1, seconds)
					}.play(Server.default, bus)));
					Server.default.sync;
					(thisThread.clock.tempo * seconds).wait;
					if(local == Library.at(\tidyar, name, \fadersynth)) {
						Library.global.removeAt(\tidyar, name, \fadersynth);
						local.free;
						bus.setSynchronous(1);
					};
				};
			}).play;
		};
		
		dict.at(\fadeout) !? { |seconds|
			Routine({
				var val, local;

				Library.global.removeAt(\tidyar, name, \fadersynth) !? { _.free };
				val = bus.getSynchronous;

				if((seconds = val.linlin(1, 0, seconds, 0)) > 0) {
					Library.put(\tidyar, name, \fadersynth, (local = {
						Line.kr(val, 0, seconds)
					}.play(Server.default, bus)));
					Server.default.sync;
					(thisThread.clock.tempo * seconds).wait;
					if(local == Library.at(\tidyar, name, \fadersynth)) {
						Library.global.removeAt(\tidyar, name, \fadersynth);
						local.free;
						bus.setSynchronous(0);
					};
				};
			}).play;
		};
		*/
		
		dict.put(\fader, bus.asMap);
	}
	
	put_sends {
		var i = 1;
		Library.at(\tidyar).keys.do { |key|
			Library.at(\tidyar, key.asSymbol, \bus) !? { |bus|
				this.at(key.asSymbol) !? { |gain|
					gain = gain.asFloat;
					this.put(("out"++(i.asString)).asSymbol, bus.index);
					this.put(("gain"++(i.asString)).asSymbol, gain);
					i = i + 1;
				}
			}
		}
	}
	
	log {
		if(JSTidy.should_log(\step)) {
			this.postln;
		} {
			if((this.at(\log) ? 0) > 0) {
				this.postln;
			}
		}
	}
	
	printOn { |stream|
		var width=40;
		var len=width;
		
		stream << "step\ntrig:% delta:% dur:%\n".format(
			trig,
			delta.round(0.01),
			dur.round(0.01)
		);

		dict.keys.asArray.sort.do { |k|
			var str, val = dict.at(k.asSymbol);
			if(k != \log) {
				if(val.isFloat) { val = val.round(0.01) };
				str = "%:% ".format(k, val);
				if((len - (str.size)) < 0) { stream << "\n"; len=width; };
				stream << str;
				len = len - str.size;
			};
		};
		
		//dict.keysValuesDo { |k,v| stream << "%:%,".format(k,v) };
		//outs.keysValuesDo { |k,v| stream << "~%:%,".format(k,v) };
		stream << "\n";
	}

	// called by JSTidyCycle.printOn
	printOn2 { |stream|
		stream << "step % % %".format(
			trig,
			delta.round(0.01).asString.padLeft(6),
			dur.round(0.01).asString.padLeft(6)
		);

		dict.keysValuesDo { |k,v| stream << "%:%,".format(k,v) };
		stream << "\n";
	}
}

JSTidyException : Exception {
	reportError { this.errorString.postln }
}

/////////////////////////////////////////////////////
// OBJECTS IN THE TIDY TREE
/////////////////////////////////////////////////////

JSTidyNode {
	var <>children, <val, <>parent;

	*new { |val| ^super.newCopyArgs([], val.asString) }

	add { |child|
		children = children.add(child);
		child.parent = this; // @see JSTidy -- operator
	}

	log { |indent=""|
		"%% %".format(indent, this.class, (val ? "").quote).postln;
		children.do { |child| child.log(indent ++ "--") };
	}

	get { |cycle, name| ^cycle }

	become_cur_after_add { ^false }

	is_branch { ^false }

	steps_from_priority_queue { |pq|
		// calculate delta times for all the steps
		var steps = List.new;
		var time = 0;
		while { pq.notEmpty } {
			var next_time, step = pq.pop;
			next_time = pq.topPriority ? 1;
			step.delta = next_time - time;
			time = next_time;
			steps.add(step);
		};

		^steps.asArray;
	}
}

JSTidyBranch : JSTidyNode {
    get { |cycle, name|
		var last = children.last;

		if(last.notNil.and(last.is_branch)) {
			cycle = last.get(cycle, name);
			children.drop(-1).do { |child|
				cycle = child.get(cycle, name)
			};
		} {
			children.do { |child| cycle = child.get(cycle, name) };
		};

		^cycle;
	}

	// anything added to the tree after this branch should become a child
	// of this branch.
	become_cur_after_add { ^true }

	is_branch { ^true }
}

// (cycle) |> (child), (/+*%<>)
//
JSTidyCombLeft : JSTidyNode {
	get { |cycle, name|
		var time, child = children.first.get(cycle, name);

		time = 0;
		cycle.steps.do { |step|
			var other, keys;

			other = child.at(time);

			// collect all keys from both sides
			keys = step.dict.keys;
			other.dict.keys.do { |key| keys.add(key) };
			
			// combine the values for the keys
			keys.do { |key|
				case
				{ val == ">" }
				{ step.put(key, other.at(key) ? step.at(key)) }
				{ val == "<" }
				{ step.put(key, step.at(key) ? other.at(key)) }
				{ val == "+" }
				{ step.put(key, (step.at(key) ? 0) + (other.at(key) ? 0)) }
				{ val == "*" }
				{ step.put(key, (step.at(key) ? 1) * (other.at(key) ? 1)) }
				{ val == "/" }
				{
					var value = (other.at(key) ? 0);
					if(value == 0) {
						step.put(key, 0) // division by zero
					} {
						step.put(key, (step.at(key) ? 0) / value)
					};
				}
				{ val == "%" }
				{ step.put(key, (step.at(key) ? 0) % (other.at(key) ? 0)) }
				{ }
			};
			
			time = time + step.delta;
		};

		^cycle;
	}
}

// (cycle) >| (child), (/+*%<>)
//
JSTidyCombRight : JSTidyNode {
	get { |cycle, name|
		var time, child = children.first.get(cycle, name);

		time = 0;
		child.steps.do { |step|
			var other, keys;

			other = cycle.at(time);

			// collect all keys from both sides
			keys = step.dict.keys;
			other.dict.keys.do { |key| keys.add(key) };
			
			keys.do { |key|
				case
				{ val == ">" }
				{ step.put(key, step.at(key) ? other.at(key)) }
				{ val == "<" }
				{ step.put(key, other.at(key) ? step.at(key)) }
				{ val == "+" }
				{ step.put(key, (step.at(key) ? 0) + (other.at(key) ? 0)) }
				{ val == "*" }
				{ step.put(key, (step.at(key) ? 1) * (other.at(key) ? 1)) }
				{ val == "/" }
				{
					var value = (step.at(key) ? 0);
					if(value == 0) {
						step.put(key, 0) // division by zero
					} {
						step.put(key, (other.at(key) ? 0) / value)
					};
				}
				{ val == "%" }
				{ step.put(key, (other.at(key) ? 0) % (step.at(key) ? 0)) }
				{ }
			};

			time = time + step.delta;
		};

		^child;
	}
}

JSTidyCombBoth : JSTidyNode {
	get { |cycle, name|
		^cycle.steps_(
			this.make_steps(
				cycle.steps.asList,
				children.first.get(cycle, name).steps.asList
			)
		);
	}

	make_steps { |steps1, steps2|
		var step1, step2, steps=List.new;

		steps1 = steps1.asList;
		steps2 = steps2.asList;

        while { (steps1.size > 0) or: (steps2.size > 0) } {

			step1 = step2 = nil;

			if(steps1.size > 0) { step1 = steps1.removeAt(0) };
			if(steps2.size > 0) { step2 = steps2.removeAt(0) };

			case
			{ step1.isNil } { steps.add(step2); step2 = nil }
			{ step2.isNil } { steps.add(step1); step1 = nil }
			{
				case
				{ abs(step1.delta - step2.delta) < 0.001 }
				{
					this.combine(step1, step2);
					steps.add(step1);
				}
				{ step1.delta < step2.delta }
				{
					var d = step2.delta - step1.delta;
					var step2a = step2.deepCopy.delta_(d).dur_(d);
					steps2.insert(0, step2a);
					step2.delta = step1.delta;
					step2.dur = step1.dur;
					this.combine(step1, step2);
					steps.add(step1);
				}
				{
					var d = step1.delta - step2.delta;
					var step1a = step1.deepCopy.delta_(d).dur_(d);
					steps1.insert(0, step1a);
					step1.delta = step2.delta;
					step1.dur = step2.dur;
					this.combine(step2, step1);
					steps.add(step2);
				}
			}
		};

		^steps;
	}

	combine { |step, stepAt, right|
		stepAt.dict.keysValuesDo { |key, value|
			case
			{ val == ">" }
			{
				if(right.isNil) {
					step.put(key, value)
				}
			}
			{ val == "<" }
			{
				if(right.notNil) {
					step.put(key, value)
				}
			}
			{ val == "+" }
			{ step.put(key, (step.at(key) ? 0) + value) }
			{ val == "*" }
			{ step.put(key, (step.at(key) ? 1) * value) }
			{ val == "/" }
			{
				if(right.isNil, {
					if(value == 0, {
						step.put(key, 0) // division by zero
					}, {
						step.put(key, (step.at(key) ? 0) / value)
					});
				},{
					var divider = (step.at(key) ? 0);
					if(divider == 0, {
						step.put(key, 0) // division by zero
					}, {
						step.put(key, value / divider)
					});
				})
			}
			{ val == "%" }
			{
				if(right.isNil, {
					step.put(key, (step.at(key) ? 0) % value)
				}, {
					step.put(key, value % (step.at(key) ? 0))
				})
			}
			{ }
		};
	}
}

JSTidyPattern : JSTidyNode {
	var seq;

	get { |cycle, name|
		seq = seq ? JSMiniParser(val).parse; // lazy instantiate
		^JSTidyCycle(seq.next_cycle);
	}
}

// play sub-sequences
JSTidyFP_Seq : JSTidyNode {
	var <>pattern;
	
	*new { |pattern|
		var instance = super.new("seq");
		instance.pattern_(pattern);
		instance.add(JSTidyPattern(pattern));
		^instance;
	}

	become_cur_after_add { ^true }

	get { |cycle, name|
		var index;
		var seq; // a queue of steps

		seq = Library.at(\tidyseq, name.asSymbol, \seq);
		
		Library.at(\tidyseq, name.asSymbol, \pattern) !? { |pat|
			if(pat != pattern) { seq = nil };
		};
		Library.put(\tidyseq, name.asSymbol, \pattern, pattern);

		// keep a queue of steps of the sequence
		seq ?? { seq = List.new };
		if(seq.size <= 0) {
			seq.addAll(children.first.get(cycle, name).steps)
		};

		// which child branch will deliver the next cycle?
		// remember: your first child is the seq pattern
		// dur/delta of the steps is ignored. we use 1 step for each cycle.
		index = seq.removeAt(0).at(\str).asInteger;
		index = index % (children.size - 1) + 1;

		Library.put(\tidyseq, name.asSymbol, \seq, seq);
		
		^children.at(index).get(cycle, name);
	}
}

// mix-play sub-sequences
JSTidyFP_Stack : JSTidyNode {
	*new { |pattern| ^super.new("stack") }

	get { |cycle, name|
		var pq=PriorityQueue.new;

		children.do { |child|
			var time = 0;
			child.get(JSTidyCycle.new, name).steps.do { |step|
				pq.put(time, step);
				time = time + step.delta;
			}
		};
		
		^JSTidyCycle(this.steps_from_priority_queue(pq));
	}

	become_cur_after_add { ^true }
}

// "note 0 2 4" - "chord 123:1 135" // str = the chord, num = strum 0 - 9
// 2023-01-11: chord is out of order at the moment, use "note [0,2,4]"
// 
JSTidyFP_Chord : JSTidyNode {
	*new { |pattern| ^super.new("chord").add(JSTidyPattern(pattern)) }

	get { |cycle, name|
		var time, chords = children.first.get(cycle, name);

		time = 0;
		cycle.steps.do { |step|
			var chord = chords.at(time);
			step.put(\chord, chord.at(\str) ? "0");
			step.put(\strum, (chord.at(\num) ? 0).asInteger);
			time = time + step.delta;
		};
		^cycle;
	}
}

// ~a < "jux 0.6" |> "rev" | ...
// pan original left, altered right
JSTidyFP_Jux : JSTidyNode {
	var <>by;

	*new { |pat|
		var by=0.5;
		if(pat.size > 0, { by = pat.split($ ).at(0).asFloat });
		^super.new("jux").by_(max(0, min(1.0, by)));
	}

	become_cur_after_add { ^true }

	get { |cycle, name|
		var time, org, alt, steps, pq=PriorityQueue.new;

		org = children.last.get(cycle, name); // from the JSTidyBranch

		// put steps of the org cycle in the PriorityQueue
		time = 0;
		org.steps.do { |step|
			pq.put(time, step.put(\pan, -1 * by));
			time = time + step.delta;
		};

		// calculate steps for the alt cycle
		alt = org.deepCopy;
		children.drop(-1).do { |child| alt = child.get(alt) };

		// put steps of the alt cycle in the PriorityQueue
		time = 0;
		alt.steps.do { |step|
			pq.put(time, step.put(\pan, by));
			time = time + step.delta;
		};

		^JSTidyCycle(this.steps_from_priority_queue(pq));
	}
}

// ~a < "off 0.25" |+ "n 7" | ..
// add timeshifted, altered layer
// todo: the shift time should be patternable (e.g. "off <0.25 0.375>" - bla bla)
JSTidyFP_Off : JSTidyNode {
	var <>shift, <>stack;

	*new { |pat|
		^super.new("off")
		.shift_(max(0, min(1.0, pat.split($ ).at(0).asFloat)));
	}

	become_cur_after_add { ^true }

	get { |cycle, name|
		var time, org, alt, steps, pq=PriorityQueue.new;

		org = children.last.get(cycle, name); // from the JSTidyBranch

		// add steps of org cycle to PriorityQueue
		time = 0;
		org.steps.do { |step|
			pq.put(time, step);
			time = time + step.delta;
		};

		// add steps of the stack + alt cycle to PriorityQueue
		stack ?? { stack = [ JSTidyStep(0, shift, shift, "~", 0) ] };
		time = 0;
		stack.do { |step|
			pq.put(time, step);
			time = time + step.delta;
		};
		stack = [];

		// calculate shifted steps for the alt cycle
		alt = org.deepCopy;
		children.drop(-1).do { |child| alt = child.get(alt, name) };

		// continue with the time from adding stack items to PriorityQueue
		alt.steps.do { |step|
			case
			{ time > 0.999 } { stack = stack.add(step) }
			{ (time + step.delta) > 0.999 } {
				// split the step and add some silence to the stack
				var d = time + step.delta - 1;
				stack = stack.add(step.deepCopy.delta_(d).trig_(0));
				step.delta_(1 - time); // the duration remains longer
				pq.put(time, step);
			} {
				pq.put(time, step);
			};
			time = time + step.delta;
		};

		^JSTidyCycle(this.steps_from_priority_queue(pq));
	}
}

// reverse the dictionaries
// TODO: i think you can also reverse the steps now, cos all cycles are completely
// filled with steps (notes, elongaters, rests)
JSTidyFP_Rev : JSTidyNode {
	*new { |pattern| ^super.new("rev") }

	get { |cycle, name|
		var dicts = cycle.steps.collect { |step| step.dict };
		dicts = (dicts ? []).reverse;
		cycle.steps.do { |step| step.dict_(dicts.removeAt(0)) };
		^cycle;
	}
}

JSTidyFP_Slice : JSTidyNode {
	var <>count;

	*new { |pattern|
		var instance = super.new("slice");
		var str = pattern.split($ );
		instance.count = max(1, str.removeAt(0).asInteger);
		pattern = str.join($ ).stripWhiteSpace;
		if(pattern.size > 0, { instance.add(JSTidyPattern(pattern)) });
		^instance;
	}

	// todo: could use GrainIn to fit sample without pitch change
	get { |cycle, name|
		cycle = children.first.get(JSTidyCycle.new, name);
		
		cycle.steps.do { |step|
			var slice = (step.at(\str) ? 0).asInteger;

			if(slice < 0) {
				slice = abs(slice);
				step.put(\rate, -1);
				step.put(\begin, max(1, min(slice + 1, count)) / count);
			} {
				step.put(\begin, max(0, min(slice, count - 1)) / count);
			};

			step.put(\legato, (step.at(\legato) ? 1));
			step.put(\slices, count);
			step.put(\str, nil);
			
			// use "fit", "crop", "grow" or "shrink" to get what u want
		};

		^cycle;
	}
}

// \a -- "every 8 -1" >| "b 1 2 3 4" | etc; takes action on turns 7, 15, 23, etc
JSTidyFP_Every : JSTidyNode {
	var <>when, <>offset, turn;

	*new { |pattern|
		var split = pattern.split($ );
		^super.new("every")
		.when_(split.at(0).asInteger)
		.offset_((split.at(1) ? "0").asInteger);
	}

	become_cur_after_add { ^true }

	get { |cycle, name|
		cycle = children.last.get(cycle, name); // should be a JSTidyBranch

		// let your children alter the cycle when it is your turn
		turn = (turn ? -1) + 1;
		if(turn > 0) {
			if(((turn + (0 - offset)) % when) == 0) {
				children.drop(-1).do { |child|
					cycle = child.get(cycle, name)
				};
			};
		};

		^cycle;
	}
}

// OK
JSTidyFP : JSTidyNode {
	*new { |val, pattern|
		var abbr = [
			\d, "def",
			\b, "buf",
			\s, "snd",
			\n, "note",
			\leg, "legato",
			\oct, "octave",
		];
		var instance = super.new(abbr.asDict.at(val.asSymbol) ? val);
		if(val == "log") { pattern = "1" };
		if(val == "once") { pattern = "1" };
		if(val == "degrade" and: (pattern.size <= 0)) { pattern = "0.5" };
		if(val == "fit" and: (pattern.size <= 0)) { pattern = "1" };
		if(val == "crop" and: (pattern.size <= 0)) { pattern = "1" };
		if(val == "grow" and: (pattern.size <= 0)) { pattern = "1" };
		if(val == "shrink" and: (pattern.size <= 0)) { pattern = "1" };
		if(val == "stretch") { pattern = "1" };

		if(val == "fadein" or: (val == "fadeout")) {
			if(pattern.size <= 0) { pattern = "1" };
			pattern = pattern.split($ ).first; // interpreted as float later
		};

		if(pattern.size > 0) { instance.add(JSTidyPattern(pattern)) };
		^instance;
	}

	// abcdefg a# ab a1 a7, c5 = 60
	notemidi { |str|
		var octave = 5;
		var sharp = 0;
		var flat = 0;
		var note = $c;
		
		if("abcdefg".contains(str[0])) { note = str[0] };
		if(str.size == 2) {
			if(str[1] == $#) { sharp = true };
			if(str[1] == $b) { flat = true };
			if("12345678".contains(str[1])) { octave = str[1].asInteger };
		};
		if(str.size == 3) {
			if(str[1] == $#) { sharp = true };
			if(str[1] == $b) { flat = true };
			if("12345678".contains(str[2])) { octave = str[2].asInteger };
		};

		note = note.ascii - $a.ascii;
		if(note >= 2) {
			note = 12 * octave + note
		} {
			note = 12 * (octave - 1) + note
		};

		if(flat) { note = note - 1 };
		if(sharp) { note = note + 1 };

		^note;
	}

	get { |cycle, name|
		// return a cycle with value from your pattern filled in for val
		cycle = children.first.get(cycle, name);

		cycle.steps.do { |step|
			step.at(\str) !? { |str|
				if(str[0] == $=) {
					// "=xxx" means "map controlbus xxx for this param"
					var bus;
					bus = Library.at(\tidykr, str.drop(1).asSymbol, \bus);
					bus !? { step.put(val.asSymbol, bus.asMap) }
				} {
					// interpret str depending on the function name (val)
					case
					{ str == "~" } { step.trig = 0 }
					{ val == "def" } { step.put(\def, str.asSymbol) }
					{ val == "buf" } { step.put(\buf, str.asInteger) }
					{ val == "vel" } { step.put(\vel, str.asFloat) }
					{ val == "snd" } { step.put(\snd, str.asSymbol) }
					{ val == "play" } { step.put(\play, str.asSymbol) }
					{ val == "note" } {
						if("abcdefg".contains(str[0]), {
							step.put(\midinote, this.notemidi(str));
						}, {
							step.put(\note, str.asFloat);
						});
					}
					{ val == "scale" } { step.put(\scale, str.asSymbol) }
					{ val == "speed" } {
						step.put(\speed, str.asFloat.midiratio)
					}
					{ step.put(val.asSymbol, str.asFloat) };
				};
			};

			step.at(\num) !? { |num|
				case
				{ val == "snd"   } { step.put(\buf,  num.asInteger) }
				{ val == "note"  } { step.put(\oct,  num.asInteger) }
				{ val == "def"   } { step.put(\note, num.asInteger) }
				{ val == "scale" } { step.put(\oct,  num.asInteger) }
				{ }
			};

			step.put(\str, nil);
			step.put(\num, nil);
		};

		^cycle;
	}
}

/////////////////////////////////////////////////////
// HOOKS
/////////////////////////////////////////////////////

JSTidyFX {
	var <>name, <>func_or_symbol;

	*new { |name, func_or_symbol|
		^super.newCopyArgs(name, func_or_symbol)
	}

	play { |out=0, gain=0, args, target|
		var in, node, old, addAction, server;

		server = Server.default;
		gain = gain.asFloat.clip(0, 1);
		args = args ? [];
		
		Routine({
			// create audio input bus for the effect
			(in = Library.at(\tidyar, name.asSymbol, \bus)) ?? {
				in = Bus.audio(server, 2);
				Library.put(\tidyar, name.asSymbol, \bus, in);
			};

			server.sync;

			case
			{ out.isInteger } { out = out.asInteger }
			{ (out = Library.at(\tidyar, out.asSymbol, \bus)).notNil }
			{ out = out.index }
			{ out = 0 };

			args = args ++ [\in, in.index, \gain, gain];

			// make controlbus mapping possible with "=xxx" syntax
			args = args.collect({ |el, i|
				var result = el;

				if(((i % 2) == 1).and(el.isString)) {
					if(el[0] == $=) {
						var key = el.drop(1).asSymbol;
						Library.at(\tidykr, key, \bus) !? { |bus|
							result = bus.asMap;
						}
					}
				};
				
				result;
			});

			old = Library.at(\tidyar, name.asSymbol, \synth);
			old !? { if(old.isRunning.not) { old = nil } };
			
			target !? { target = Library.at(\tidyar, target.asSymbol, \synth) };
			target ?? { target = old };
			addAction = \addToHead;
			target !? { addAction = \addBefore };

			case
			{ func_or_symbol.isFunction }
			{
				// the function must expect an \in argument
				// the function may use the \gain argument
				node = func_or_symbol.play(
					target: target,
					addAction: addAction,
					outbus: out,
					fadeTime: 0.5,
					args: args
				);
			}
			{
				// the synthdef should have a sustaining envelope
				// with \gate arg (and fadein/fadeout somewhat)
				// the synthdef must use an \in and \outx bus argument
				// the synthdef may use the \gain argument
				node = Synth(
					defName: func_or_symbol.asSymbol,
					args: args ++ [\out, out],
					target: target,
					addAction: addAction
				);
			};

			NodeWatcher.register(node, true); // for "isRunning" above
			
			old !? { old.release };
			
			server.sync;

			Library.put(\tidyar, name.asSymbol, \synth, node);
		}).play;
	}

	printOn { |stream| "fx: %% ".format("\\", name).printOn(stream) }
}

+ Symbol {
	doc {
		if(this != \tidy) { ^super.help };

		"".postln;
		"commands".postln;
		"-------------------------------------------".postln;
		"\\tidy .load(folder) : load samples".postln;
		"\\tidy .loaded : post samples loaded".postln;
		"\\tidy .cps(x) : set cycles per second".postln;
		"\\tidy .quant(x) : set quantisation".postln;
		"\\tidy .rec(name, cycles, bus, nudge)".postln;
		"\\tidy .save(name, folder) : saves it".postln;
		"\\tidy .end(x) : fadeout + end in x seconds".postln;
		"\\tidy .setup(cps, samplespath, scdfile)".postln;
		"\\tidy .show(\\tree | \\cycle | \\step)".postln;
		"".postln;
		"\\bus .fx { funct } .play(dest, gain, [])".postln;
		"\\bus .fx(\synthdef) .play(dest, gain, [])".postln;
		"\\bus -- { function } : create control bus".postln;
		"\\bus .bus : return control bus object".postln;
		"".postln;
		"\\name -- \"func pattern\" .. : play notes".postln;
		"\\name .hush(seconds) : fade out and stop".postln;
		"".postln;
		"seq,stack,chord,jux,rev,off,splice,every,play".postln;
		"".postln;
		"left: -, |>, |<, |*, |+, |/, |%".postln;
		"right: >|, <|, *|, +|, /|, %|".postln;
		"both: |>|, |<|, |*|, |+|, |/|, |%|".postln;
		
		^"".postln;
	}

	setup { |cps, samples, scd|
		if(this == \tidy) {
			var s = Server.default;

			s.options.memSize = 2 ** 20;
			s.options.numBuffers = 2048;
			s.newBufferAllocators;
			s.newBusAllocators;

			s.waitForBoot({
				var channels = s.options.numOutputBusChannels;

				/* Guard (you may re-execute this part anytime) */
				SynthDef(\guard, {
					var in = In.ar(0, channels), zero = DC.ar(0);
					in = LeakDC.ar(in);
					in = Select.ar(CheckBadValues.ar(in,0,0), [in,zero,zero,in]);
					in = Limiter.ar(in);
					ReplaceOut.ar(0, in);
				}).add;

				s.sync;

				// SynthDefs and Library survive CmdPeriod, synths do not
				if(Library.at(\guard).isNil, {
					Library.put(\guard, {
						Synth(\guard, [], RootNode(s), \addToTail);
						s.sync;
						s.queryAllNodes;
					});
					CmdPeriod.add({ Routine { Library.at(\guard).value }.play; });
					Library.at(\guard).value;
				});

				s.sync;

				JSTidy.add_internal_synthdefs;
				JSTidy.cps(cps ? (80 / 60 / 4)); // default 80 bpm 4/4

				samples !? { JSTidy.load(samples) };

				s.sync;

				"*** Tidy setup complete ***\n".postln;
				
				scd !? { |file|
					file = file.standardizePath;
					"Loading %".format(file.quote).postln;
					file.load;
				};

				s.sync;
			});
		}
	}
	
	end { |fadeTime=1| if(this == \tidy) { JSTidy.end(fadeTime) } }

	cps { |cps| if(this == \tidy) { JSTidy.cps(cps) } }
		
	quant { |quant| if(this == \tidy) { JSTidy.quant(quant) } }

	load { |folder| if(this == \tidy) { JSTidy.load(folder) } }
	
	loaded { if(this == \tidy) { JSTidy.loaded } }

	show { |what| if(this == \tidy) { JSTidy.log(what) } }

	fx { |in| ^JSTidyFX(this, in) }	// return object to the Interpreter
	
	-- { |in|
		case
		{ in.isFunction }
		{
			var bus, node, server = Server.default;

			Routine({
				// create control bus
				(bus = Library.at(\tidykr, this, \bus)) ?? {
					bus = Bus.control(server, 1);
					Library.put(\tidykr, this, \bus, bus);
				};

				node = Library.at(\tidykr, this, \synth);

				server.bind {
					node !? { node.release };
					node = in.play(nil, bus.index);
				};

				server.sync;

				Library.put(\tidykr, this, \synth, node);
			}).play;
		}
		{ in.isString }
		{
			^JSTidy(this).add_branch("--").add_func(in)
		}
		{
			"strange input %".format(in.class).postln
		}
	}

	hush { |seconds=1| JSTidy.hush(this, seconds) }

	bus { ^Library.at(\tidykr, this, \bus) }

	rec { |name, cycles, bus, nudge=(-0.25)|
		if(this != \tidy) { ^super.rec };
		
		Routine({
			var seconds, buf, old, now, quant;
			var server = Server.default;

			SynthDef(\rec, {
				var bufnum = \buf.kr(0);
				var in = In.ar(\in.kr(0), 2);
				RecordBuf.ar(in, bufnum, loop: 0, doneAction: 2);
			}).add;
			server.sync;
			
			seconds = cycles / TempoClock.tempo; // 1 cycle = 1 TempoClock beat
			"rec % cycles (% seconds)".format(cycles, seconds).postln;
			buf = Buffer.alloc(server, seconds * server.sampleRate, 2);
			server.sync;
			"rec bufnum %".format(buf.bufnum).postln;

			JSTidy.quantize;
			
			// start \rec synth 1 cycle from now + start countdown now
			server.makeBundle(1 / TempoClock.tempo - nudge, {
				Synth(\rec, [buf: buf, in: bus], nil, \addToTail);
			});

			4.do { |i| "..%".format(4 - i).postln; (1/4).wait };
			"..go!".postln;
			
			// countup in post window during \rec synth lifetime
			(cycles * 4).do { |i| "--%".format(i+1).postln; (1/4).wait; };

			// store the new recording; free old one if any
			old = Library.at(\tidyrec, name.asSymbol);
			Library.put(\tidyrec, name.asSymbol, buf);
			old !? { |b| b.free };
			
			"record finished".postln;
		}).play;
	}

	save { |name, folder|
		Library.at(\tidyrec, name.asSymbol) !? { |buffer|
			var path;
			// TODO: if folder contains a filename at the end:
			// - write to that file
			
			// else, do this:
			folder = folder.standardizePath;
			path = folder ++ "/" ++ name ++ ".wav";
			"Writing %% to %".format("\\", name, path.quote).postln;
			buffer.write(path, "wav");
		};
	}
}
