/*
Midiin + rec
Kbdin + rec

	\help -- "" // lists all functions
	\help -- "<function>"

practice (out loud)
load only good samples
CmdPeriod behaviour
euclids inside <> do not seem to work (but maybe illogical to want)

idea: sample - reverse - add delays - reverse back
idea: sample - reverse - reverb - resample - loop
loop: start, then loop middle of sample, at release play to the end
idea: "dup 3" | ... would repeat all cycles 3 times
idea: "do 0 1 6 7 5 6" | ... would play the given cycle numbers
idea: "rot 2" : rotate steps of each cycle 2 steps
how to implement swing? "b [1 ~ 2]@2 3 4"
idea: "life 0.6" : small variations (wow:flutter) am/fm, like velocity
idea: "seed 1234" : control randomness;
key tracking: lpf = (lpf.midicps + (kt * freq))
idea: "trig 1000100101" or "hex 7fa4" to generate/override structure
mini notation parser more robust: monitor index through recursive looping
idea: "b [0 1 2 3 ~]??" means "pick one of them randomly"
startrek samples, cars you recorded in stereo
make vital classic bend VOsc wavetables: sine - tri - saw - pulse - pulsew
log: maybe add a parametername to log to only log that
make all sample / synthdef volumes RIGHT

growl synthdef wants to send intermediate signal result to a reverb:
- growl synthdef declares args "room" and "room_bus"
- the pattern supplies: "room 0.4"
- in step.play: if the synthdef has an arg called "room", then 
  - step.play adds argument to synth: room_bus = 45
  - growl can send signal to "room_bus"
- else
  - existing put_sends mechanism will supply \outx and \gainx args to growl
so the synthdef dictates the name for the reverb (in tidal it is always "room" too)

\tidy .query : does s.queryAllNodes

study these:
{
	var sig = WhiteNoise.ar(0.1);
	BLowShelf.ar(sig, MouseX.kr(300, 3000) * 0.5, 1.0, -60) +
	BHiShelf.ar(sig, MouseX.kr(300, 3000) / 0.5, 1.0, -60)
}.play

movie score: intro,theme,build,climax,theme(emotional),intro(conclusive)

connect to animatron?

meltdown function: lower volume + global freq multiplier + global tempo

implement superfm like roger, but keep it tidy..
*/

JSTidy {
	classvar loglevel, <postprocessors;

	var <>name, <tree, cur;

	*new { |name| ^super.new.name_(name) }

	*add_internal_synthdefs {

		Library.at(\tidy, \internal_synthdefs_added) !? { ^this };

		SynthDef(\tidy_fader, { |target=0, bus, fadetime=0, t_trig=1|
			var val = In.kr(bus, 1);
			val = Env([val, val, target], [0, fadetime]).kr(2, t_trig);
			ReplaceOut.kr(bus, val);
		}).add;
		
		SynthDef(\tidy_playbuf_2, {
			var freq, pfreq, glide, sustain, gate, rate, bufnum, begin;
			var att, rel, sig;
			
			gate = \gate.kr(1);
			rate = \rate.kr(1);

			// glide v6
			freq = \freq.kr(60.midicps);
			pfreq = \prevfreq.kr(440);
			sustain = \sustain.kr(0);
			glide = \glide.kr(0);
			freq = Env([pfreq, freq], glide * sustain, \exp).kr(0, gate);
			rate = \rate.kr(1) * freq / 60.midicps;
			
			bufnum = \bufnum.kr(0);
			begin = \begin.kr(0) * BufFrames.kr(bufnum);
			att = \att.kr(0.02);
			rel = \rel.kr(0.02);
			sig = PlayBuf.ar(2, bufnum, rate, startPos: begin);
			sig = sig * Env.asr(att, 1, rel).kr(2, gate);
			sig = LeakDC.ar(sig);
			// maybe use Balance2 ?
			//sig = Splay.ar(sig, 0, \vel.kr(0.5), \pan.kr(0));
			sig = Balance2.ar(sig[0], sig[1], \pan.kr(0), \vel.kr(0.5));
			sig = sig * \am.kr(1);
			sig = sig * \gain_bus.kr(1);
			sig = sig * abs(\mute_bus.kr(0).asInteger.clip(0,1) - 1);

			Out.ar(\out1.kr(0), sig * \gain1.kr(0));
			Out.ar(\out2.kr(0), sig * \gain2.kr(0));
			Out.ar(\out3.kr(0), sig * \gain3.kr(0));
			Out.ar(\out4.kr(0), sig * \gain4.kr(0));
		}).add;

		SynthDef(\tidy_playbuf_1, {
			var freq, pfreq, glide, sustain, gate, rate, bufnum, begin;
			var att, rel, sig;
			
			gate = \gate.kr(1);
			rate = \rate.kr(1);

			// glide v6
			freq = \freq.kr(60.midicps);
			pfreq = \prevfreq.kr(440);
			sustain = \sustain.kr(0);
			glide = \glide.kr(0);
			freq = Env([pfreq, freq], glide * sustain, \exp).kr(0, gate);
			rate = \rate.kr(1) * freq / 60.midicps;
			
			bufnum = \bufnum.kr(0);
			begin = \begin.kr(0) * BufFrames.kr(bufnum);
			att = \att.kr(0.02);
			rel = \rel.kr(0.02);
			sig = PlayBuf.ar(1, bufnum, rate, startPos: begin);
			sig = sig * Env.asr(att, 1, rel).kr(2, gate);
			sig = LeakDC.ar(sig);
			sig = Pan2.ar(sig, \pan.kr(0), \vel.kr(0.5));

			sig = sig * \am.kr(1);
			sig = sig * \gain_bus.kr(1);
			sig = sig * abs(\mute_bus.kr(0).asInteger.clip(0,1) - 1);

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

	*end { |seconds=0|
		seconds = max(0.2, seconds); // no plopping please

		Library.at(\tidyar) !? { |dict|
			dict.keys.do { |name| JSTidy.hush(name, seconds) }
		};

		Library.at(\tidykr) !? { |dict|
			Routine({
				(seconds * TempoClock.tempo).wait; // wait for hushes first
				dict.keys.do { |name|
					Library.at(\tidykr, name, \synth) !? { |synth|
						synth.release
					};
					"released %".format(name).postln;
				}
			}).play;
		};
	}
	
	*hush { |name, seconds=0|
		Routine({
			Library.at(\tidyar, name, \orbit) !? { |orbit|
				Library.put(\tidyar, name, \orbit, nil);
				orbit.stop(seconds);
			};
		}).play;
	}

	*mute { |str|
		var muted = Set.new;
		str.asString.split($ ).do { |key|
			case
			{ key.isNil } { }
			{ key.asString.size <= 0 } { }
			{ muted.add(key.asSymbol) }
		};
		Library.put(\tidy, \muted, muted);
		^this.pr_set_mute_buses;
	}

	*solo { |str|
		var soloed = Set.new;
		str.asString.split($ ).do { |key|
			case
			{ key.isNil } { }
			{ key.asString.size <= 0 } { }
			{ soloed.add(key.asSymbol) }
		};
		Library.put(\tidy, \soloed, soloed);
		^this.pr_set_mute_buses;
	}

	// something has changed in the soloed or muted list.
	// this might affect one or more orbits.
	// let all orbits re-think the value for their mute_bus
	//
	// why is there a mute_bus for each orbit?
	// if orbit \a starts playing a loooong note
	// and orbit \b is put in solo
	// then the loooong note on \a should be muted while still playing
	// this is done by using the value of the mute_bus in all synthdefs
	// as an inverted multiplier for the generated sound
	//
	*pr_set_mute_buses {
		var soloed = Library.at(\tidy, \soloed) ? Set.new;
		var muted = Library.at(\tidy, \muted) ? Set.new;

		(Library.at(\tidyar) ? Dictionary.new).keys.do { |key|
			Library.at(\tidyar, key.asSymbol, \orbit) !? { |orbit|
				orbit.set_mute_bus;
			};
		};

		^"soloed %, muted %".format(soloed.as(Array), muted.as(Array));
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

		var func = {
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
		
		if(thisProcess.mainThread == thisThread)
		{ Routine(func).play } { func.value };
	}

	*loaded { |bank|
		var width=40;
		"".padLeft(width, "-").postln;
		case
		{ Library.at(\samples).isNil } { "** no banks / samples **".post }
		{ bank.isNil }
		{
			var line="";
			Library.at(\samples).keys.asArray.sort.do({ |k|
				var val = Library.at(\samples, k.asSymbol);
				var str = "% % - ".format(k, val.size);
				if((line + str).size > width) { line.postln; line="" };
				line = line + str;
			});
			if(line.size > 0) { line.postln };
		}
		{ Library.at(\samples, bank.asSymbol).isNil } {
			"** unknown bank : % **".format(bank.asString).postln
		}
		{
			"bank % samples:".format(bank.asString.quote).postln;
			Library.at(\samples, bank.asSymbol).keys.asArray.sort.do({ |i|
				var buf = Library.at(\samples, bank.asSymbol, i);
				var filename = PathName(buf.path).fileName;
				var index = i.asString.padLeft(3);
				var sec = buf.duration.round(0.01).asString.padLeft(3);
				"% (% sec) %".format(index, sec, filename).postln;
			});
		};
		
		"".padLeft(width, "-").postln;
		^"".postln;
	}

	*audit { |bank|
		Routine({
			Library.at(\samples, bank.asSymbol).keys.asArray.sort.do({ |i|
				var buf = Library.at(\samples, bank.asSymbol, i);
				var filename = PathName(buf.path).fileName;
				"% %".format(i.asString.padLeft(3), filename).postln;
				case
				{ buf.numChannels == 2 } {
					{
						PlayBuf.ar(2, buf, doneAction:2);
					} .play
				} {
					{
						Pan2.ar(PlayBuf.ar(1, buf, doneAction:2), 0)
					} .play
				};
				(max(2, buf.duration) * TempoClock.tempo).wait;
			});
		}).play;
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

		// the next node must be added AFTER the array of JSTidy trees
		while { cur.parent.notNil.and(cur.is_branch.not) } {
			cur = cur.parent;
		};
	}

	// return JSTidyXX function. str: "<function name> <pattern>"
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

	add { |node|
		tree ?? { tree = cur = JSTidyBranch("tree") };
		cur.add(node);
		node.parent = cur;
		if(node.become_cur_after_add) {
			cur = node
		} {
			node.children.do { |child|
				if(child.become_cur_after_add) { cur = child }
			}
		}
	}

	add_func { |str| this.add(this.func(str)) }
	add_branch { |str| this.add(JSTidyBranch(str)) }

	| { |str| this.add_branch("|").add_func(str) }

	|<| {  |str| this.add(JSTidyCombBoth("<").add(this.func(str))) }
	< {  |str| this.add(JSTidyCombBoth("<").add(this.func(str))) }
	|< { |str| this.add(JSTidyCombLeft("<").add(this.func(str))) }
	<| { |str| this.add(JSTidyCombRight("<").add(this.func(str))) }

	|>| {  |str| this.add(JSTidyCombBoth(">").add(this.func(str))) }
	// ">" not possible in SuperCollider Interpreter
	|> { |str| this.add(JSTidyCombLeft(">").add(this.func(str))) }
	>| { |str| this.add(JSTidyCombRight(">").add(this.func(str))) }

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

	printOn { |stream|
		// if tree is nil then something has gone wrong while creating it
		// in that case: stop here, so that the old tree will keep going.
		tree ?? { "tree nil".postln; ^this };

		if(JSTidy.should_log(\tree)) { tree.log };

		name.printOn(stream);  // output to postwindow

		Routine({
			var orbit = Library.at(\tidyar, name, \orbit) ? JSOrbit(name);
			Library.put(\tidyar, name, \orbit, orbit.start(tree));
		}).play;
	}
}

// TODO: a disadvantage of Orbit with a routine that keeps running is that
// after re-evaluation, you are forced to wait till the cycle is
// done, and if you use slow=16, this takes a long time..
JSOrbit {
	var name, tree, routine;
	var <gain_bus, gain_routine, last_gain, gain_synth;
	var <mute_bus, last_mute;
	
	*new { |name| ^this.newCopyArgs(name) }

	alter { |step|
		this.pr_alter_gain(step);
		gain_bus ?? { gain_bus = Bus.control(Server.default, 1)	};
		step.put(\gain_bus, gain_bus.asMap);
		mute_bus ?? { mute_bus = Bus.control(Server.default, 1)	};
		step.put(\mute_bus, mute_bus.asMap);
	}

	/*
		possible commands:
		\solo -- "a b x"  // sets the list of soloed orbits
		\mute -- "a b x"  // sets the list of muted orbits
	*/
	set_mute_bus {
		var should_mute = this.should_mute.asInteger;
		if(should_mute == last_mute) { ^this };
		mute_bus ?? { mute_bus = Bus.control(Server.default, 1)	};
		mute_bus.setSynchronous(should_mute); // 0 or 1
		last_mute = should_mute;
	}
	
	should_mute {
		var soloed, muted;

		soloed = Library.at(\tidy, \soloed) ? Set.new;
		muted = Library.at(\tidy, \muted) ? Set.new;

		// solo wins from mute
		if(soloed.includes(name.asSymbol)) { ^false };
		if(muted.includes(name.asSymbol)) { ^true };
		if(soloed.size > 0) { ^true };
		^false;
	}
	
	pr_alter_gain { |step|
		var gain = (step.at(\gain) ? 0.1);
		if(gain == last_gain) {	^this };
		this.pr_set_gain(gain, max(0.02, step.at(\gainsec) ? 0));
	}
	
	pr_set_gain { |gain, sec=0|
		gain_bus ?? { gain_bus = Bus.control(Server.default, 1)	};
		gain_routine ?? {
			"% gain % -> % (%)".format(name, last_gain, gain, sec).postln;
			last_gain = gain;
			gain_routine = Routine({
				gain_synth !? { gain_synth.free };
				Server.default.bind {
					gain_synth = Synth(\tidy_fader, [
						\target, gain,
						\bus, gain_bus,
						\fadetime, sec
					]);
				};
				(sec * TempoClock.tempo).wait; // wait for synth
				gain_routine = nil; // allow next gain change
				gain_synth = nil;
			}).play;
		}
	}

	start { |newtree|
		tree = newtree;
		
		"% started".format(name).postln;
				
		routine ?? {
			this.pr_quantize_wait;
			
			routine = Routine({
				var cycle, repeat=inf, prevfreq;
				
				cycle = tree.get(JSTidyCycle.new, name);
				cycle.steps.do({ |x| x.at(\once) !? { repeat = 1 } });

				repeat.do({
					if(JSTidy.should_log(\cycle), { cycle.postln });

					cycle.steps.do({ |step|
						step.put(\prevfreq, prevfreq);
						step.play(name, this);
						step.log;
						prevfreq = step.at(\freq);
						step.delta.wait;
					});

					// tree could have been switched for a new tree while
					// the previous cycle was playing out (so if the cycle
					// lasts long then you have to wait for the new tree
					// to come alive long too).
					cycle = tree.get(JSTidyCycle.new, name);
				});

				routine = nil; // if you get here then erase yourself
			}).play;
		};
	}

	stop { |seconds=0|
		Routine({
			gain_routine !? { gain_routine.stop; gain_routine=nil };
			this.pr_set_gain(0, seconds);
			(seconds * TempoClock.tempo).wait;

			routine !? { routine.stop };
			routine = nil;

			gain_bus !? { gain_bus.free; gain_bus = nil };
			"% stopped".format(name).postln;
		}).play;
	}

	pr_quantize_wait {
		var now, quant, wait;
		
		now = thisThread.beats;
		quant = JSTidy.quant;
		wait = (now + quant).div(quant) * quant - now;
		wait.wait;
	}
}

JSTidyCycle {
	var <>steps, <>env;

	*new { |steps| ^super.new.make_steps(steps).make_index }

	// step_array: [ [<trig>, <delta>, <dur>, <str>, <num>], .. ]
	make_steps { |steps_array|
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

	at { |key|
		key = key.asSymbol;
		// tesing with key = \legato
		// TODO:
		// mind you: if the key is a direct synthdef param, then
		// you should NOT get the value from the bus by yourself
		// at all. how can we know this??
		if(key == \legato) {
			var val = dict.at(key);
			if(val.class == Symbol) {
				var bus, index = val.asString.drop(1).asInteger;
				bus = Bus(\control, index, 1, Server.default);
				^bus.getSynchronous;
			};
			
			^val;
		};
		
		^dict.at(key.asSymbol)
	}

	play { |name, orbit|
		var def, buf, slow, legato, sustain, degrade, rate, tempo;
		var bufbeats, bufseconds;
		
		// 1 cycle == 1 TempoClock beat == 1 bar
		// all synthdefs will have their gate shut after sustain beats
		// all synthdefs will receive sustain arg in seconds
		tempo = TempoClock.tempo;
		slow = (dict.removeAt(\slow) ? 1) / (dict.removeAt(\fast) ? 1);
		delta = delta * slow;
		dur = dur * slow;
		rate = (dict.at(\rate) ? 1);

		// mute & solo & degrade
		if(orbit.should_mute) { ^this };
		degrade = (dict.at(\degrade) ? 1).coin.asInteger;
		if((trig * degrade) <= 0) { ^this };

		orbit.alter(this);
		
		this.at(\snd) !? { |bank|
			var index = (dict.at(\buf) ? 0).asInteger;
			buf = Library.at(\samples, bank.asSymbol, index);
			buf ?? { "buf % % unknown".format(bank, index).postln; ^this };
		};
		
		this.at(\play) !? { |rec|
			buf = Library.at(\tidyrec, rec.asSymbol);
			buf ?? { "rec buf % unknown".format(rec).postln; ^this };
		};

		legato = (this.at(\legato) ? 0.8);
		buf !? { legato = (this.at(\legato) ? 1) };
		sustain = dur * legato;
		
		buf !? {
			var fit, stretch;
			
			def = "tidy_playbuf_%".format(buf.numChannels).asSymbol;
			bufseconds = buf.duration * ((dict.at(\end) ? 1) - (dict.at(\begin) ? 0));
			//bufseconds = buf.duration / (dict.at(\slices) ? 1);
			bufbeats = bufseconds * tempo;

			this.put(\bufnum, buf.bufnum);

			rate = rate * (dict.at(\freq) ? 60.midicps) / (60.midicps);
			rate = rate * buf.sampleRate / Server.default.sampleRate;

			case
			{ (fit = (dict.at(\fit) ? 0)) > 0 }
			{
				// stretch or shrink the step
				if(delta > 0) { delta = delta * fit }; // "n [0,2,4]"
				sustain = sustain * fit;
				// stretch or shrink the sample to fit inside it
				rate = rate * bufbeats / sustain;
			}
			{ (stretch = (dict.at(\stretch) ? 0)) > 0 }
			{
				// stretch or shrink the step to match the sample
				if(delta > 0) { delta = bufbeats * stretch }; //"n [0,2,4]"
				sustain = bufbeats * stretch;
			}
			// if bufbeats > sustain, then the sample will be cut short
			{ (dict.at(\crop) ? 0) > 0 } { }

			// default: delta remains what it is, sustain is adapted
			// so that the whole sample can play out during the step
			{ sustain = bufbeats };

			dict.put(\sustainbeats, sustain);
		};
		
		this.set_freq(name);
		dict.put(\prevfreq, dict.at(\prevfreq) ? dict.at(\freq));

		if(this.play_midinote) { ^this };

		// TODO: move this to an earlier stage, so that you can then
		// know what parameters are direct synth control inputs, and
		// what parameters need to be read from the control bus here.
		dict.at(\def) !? { def = dict.at(\def).asSymbol };
		def ?? { "no def".postln; ^this };
		dict.put(\def, def); // also for logging
		def = SynthDescLib.at(def.asSymbol);
		def ?? { "def % unknown".format(dict.at(\def)).postln; ^this };

		dict.at(\speed) !? { |speed| rate = rate * speed };
		dict.put(\rate, rate);
		dict.put(\sustain, sustain / tempo); // seconds!
		
		this.put_sends;

		Routine({
			var synth, ms;

			// why on earth does this stop the sound????
			ms = (dict.at(\late) ? 0).asFloat;

			if(ms > 0) {
				var beats = (ms.clip(0, 20) / 1000 * tempo);
				beats.wait;
			};
			
			JSTidy.postprocessors.do { |p| p.(dict) };
			Server.default.bind { synth = Synth(def.name, dict.asPairs) };
			sustain.wait;
			Server.default.bind { synth.set(\gate, 0) };
		}).play;
	}

	set_freq { |name|
		var bus, synth, scale;

		scale = Scale.at((dict.at(\scale) ? \major).asSymbol);

		dict.at(\freq) ?? {
			var steps = 12;
			var mtranspose = dict.at(\mtranspose) ? 0;
			var degree = (dict.at(\note) ? 0).asFloat;
			var gtranspose = 0;
			var root = dict.at(\root) ? 0;
			var oct = dict.at(\octave) ? 5;

			var note = (degree + mtranspose).degreeToKey(scale, steps);
			// midi is the midinote (continuous intermediate values)
			var midi = ((note + gtranspose + root) / steps + oct) * 12.0;
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

	play_midinote {
		dict.at(\midiout) !? { |chan|
			Library.at(\tidy, \midiout).noteOn(
				chan,
				dict.at(\freq).cpsmidi.asInteger,
				(dict.at(\vel) ? 0.5).linlin(0, 1, 0, 127)
			);
			^true;
		};
		^false;
	}
	
	// - "mix f4" -, with a sensible default
	put_sends {
		var mix, send=1;

		mix = this.at(\mix) ? "f";
		mix.do { |gain, i|
			Library.at(\tidyar, i.asSymbol, \bus) !? { |bus|
				gain = gain.digit.linlin(0, 15, 0, 1).asFloat;
				this.put(("out"++(send.asString)).asSymbol, bus.index);
				this.put(("gain"++(send.asString)).asSymbol, gain);
				send = send + 1;
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
		var steps = List.new;
		var time = 0;

		// calculate delta times for all the steps
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

	// the node added after this node should become a child of this node
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
				var stepval = step.at(key);
				var otherval = other.at(key);
				
				case
				{ stepval.isNil } { step.put(key, otherval) }
				{ otherval.isNil } { }
				// now we know that both are not nil..
				{ val == ">" } { step.put(key, otherval) }
				{ val == "<" } { }
				{ val == "+" } { step.put(key, stepval + otherval) }
				{ val == "*" } { step.put(key, stepval * otherval) }
				{ val == "/" } {
					if(otherval == 0) {
						step.put(key, 0)
					} {
						step.put(key, stepval / otherval)
					}
				}
				{ val == "%" } { step.put(key, stepval % otherval) }
				{ }
			};
			
			time = time + step.delta;
		};

		^cycle;
	}
}


// (cycle) |> (child), (/+*%<>)
//
JSTidyCombLeftDepr : JSTidyNode {
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
				{
					var stepval = step.at(key);
					var otherval = other.at(key);
					
					// string or number type?
					if((stepval.isString) or: (otherval.isString)) {
						step.put(key, (stepval ? "") ++ (otherval ? ""));
					} {
						step.put(key, (stepval ? 0) + (otherval ? 0));
					}
				}
				//{ step.put(key, (step.at(key) ? 0) + (other.at(key) ? 0)) }
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
				var stepval = step.at(key);
				var otherval = other.at(key);
				
				case
				{ stepval.isNil } { step.put(key, otherval) }
				{ otherval.isNil } { }
				// now we know that both are not nil..
				{ val == ">" } { }
				{ val == "<" } { step.put(key, otherval) }
				{ val == "+" } { step.put(key, stepval + otherval) }
				{ val == "*" } { step.put(key, stepval * otherval) }
				{ val == "/" }
				{
					if(stepval == 0) {
						step.put(key, 0) // division by zero
					} {
						step.put(key, otherval / stepval)
					}
				}
				{ val == "%" } { step.put(key, otherval % stepval) }
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
			var stepval = step.at(key);
			case
			{ val == ">" }
			{ if(right.isNil) { step.put(key, value) } }
			{ val == "<" }
			{ if(right.notNil) { step.put(key, value) } }
			{ val == "+" }
			{
				if((stepval.isString) or: (value.isString)) {
					step.put(key, (stepval ? "") ++ (value ? ""));
				} {
					step.put(key, (stepval ? 0) + value);
				}
			}
			{ val == "*" } { step.put(key, (stepval ? 1) * value) }
			{ val == "/" }
			{
				if(right.isNil, {
					if(value == 0, {
						step.put(key, 0) // division by zero
					}, {
						step.put(key, (stepval ? 0) / value)
					});
				},{
					var divider = (stepval ? 0);
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
					step.put(key, (stepval ? 0) % value)
				}, {
					step.put(key, value % (stepval ? 0))
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

JSTidyHexPattern : JSTidyNode {
	var seq, posted;

	get { |cycle, name|
		var steps;
		seq = seq ? JSMiniParser(val).parse; // lazy instantiate
		steps = [];
		seq.next_cycle.do { |step|
			var bits, delta, dur;

			// step: [<trig>, <delta>, <dur>, <str>, <num>]
			// str should be a hex string
			bits = step[3].collectAs({|c|
				c.digit.min(15).asBinaryDigits(4)
			}, Array).flatten;

			posted ?? { posted = 1; bits.join.postln }; // post once
			
			delta = step[1] / bits.size;
			dur = step[2] / bits.size;

			bits.do { |bit|
				steps = steps.add([bit, delta, dur, "~1"[bit], step[4]]);
			};
		};

		^JSTidyCycle(steps);
	}
}

// \a -- "s bd" - "bin <----722280080101 fcc0a123>" - "log"
// "0" velocity 0 = a rest, "-" is a prolonged note
// create a JSTidyBinPattern class that will create the steps
// with the str value as input. each step will have delta,dur,trig,vel
// \a -- "s bd" >| "bin <[----7222 80080101]!2 fcc0a123>" will work!

JSTidyFP_Bin : JSTidyNode {
	var <>pattern;
	
	*new { |pattern|
		var instance = super.new("bin");
		instance.pattern_(pattern);
		instance.add(JSTidyPattern(pattern));
		^instance;
	}

	get { |cycle, name|
		cycle = children.first.get(cycle, name);

		cycle.steps.do { |step|
			step.at(\str) !? { |str| step.put(\bin, str) };
			step.put(\str, nil);
			step.put(\num, nil);
		};

		^cycle;
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
/*
OLD_JSTidyFP_Rev : JSTidyNode {
	*new { |pattern| ^super.new("rev") }

	get { |cycle, name|
		var dicts = cycle.steps.collect { |step| step.dict };
		dicts = (dicts ? []).reverse;
		cycle.steps.do { |step| step.dict_(dicts.removeAt(0)) };
		^cycle;
	}
}
*/

JSTidyFP_Rev : JSTidyNode {
	*new { |pattern| ^super.new("rev") }

	get { |cycle, name|
		cycle.steps = cycle.steps.reverse;
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
				step.put(\end, step.at(\begin) + (1/count));
			} {
				step.put(\begin, max(0, min(slice, count - 1)) / count);
				step.put(\end, step.at(\begin) + (1/count));
			};

			step.put(\legato, (step.at(\legato) ? 1));
			//step.put(\slices, count);
			step.put(\str, nil);
			
			// use "fit", "crop" or "stretch"
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
					cycle = child.get(cycle, name);
				};
			};
		};

		^cycle;
	}
}

// use hex number(s) to create structure: - "hex 4026" - "b 0" - "s bd" -
// todo: - "hex 4026:2" - could result in \hex = 2 in the step dict
JSTidyFP_Hex : JSTidyNode {

	*new { |pattern|
		var instance = super.new("hex");
		if(pattern.size > 0) { instance.add(JSTidyHexPattern(pattern)) };
		^instance;
	}

	get { |cycle, name|
		cycle = children.first.get(cycle, name);

		cycle.steps.do { |step|
			step.put(\hex, (step.at(\str) ? step.trig).asString);
			step.put(\str, nil);
			step.put(\num, nil);
		};

		^cycle;
	}
}

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
		if(val == "stretch" and: (pattern.size <= 0)) { pattern = "1" };
		if(val == "mute" and: (pattern.size <= 0)) { pattern = "1" };
		if(val == "solo" and: (pattern.size <= 0)) { pattern = "1" };

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
					// a synthdef understands \c2 as arg, but inside
					// tidy logic, we must detect \c2 and get the
					// value from the controlbus ourselves.
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
					{ val == "mix" } { step.put(\mix, str) }
					{ step.put(val.asSymbol, str.asFloat) };
				};
			};

			step.at(\num) !? { |num|
				case
				{ val == "snd"   } { step.put(\buf,  num.asInteger) }
				{ val == "note"  } { step.put(\oct,  num.asInteger) }
				{ val == "def"   } { step.put(\note, num.asInteger) }
				{ val == "scale" } { step.put(\oct,  num.asInteger) }
				{ val == "gain"  } { step.put(\gainsec, num.asInteger) }
				{ }
			};

			step.put(\str, nil);
			step.put(\num, nil);
		};

		^cycle;
	}
}

JSVisuals {
	classvar window;
	var images;
	
	*new { |path| ^super.new.init(path ? "~/visuals") }

	init { |path|
		path = PathName(path.asString.standardizePath);
		if(path.isFile) {
			images = [ path.asString ];
		} {
			images = path.files.collect({ |p| p.fullPath });
		};

		"Visuals :".postln;
		images.do { |image|	image.postln };
		"".postln;
	}

	start {
		var size, dur, zoomout, maxsize, index=0, image, previmage,
		point, prevpoint, view, frames, framerate, sizeenv;

		window !? { window.close };
		window = Window.new(border: false, bounds: Window.screenBounds);

		size = 100; // width/height taken from image
		dur = 15; // seconds per image
		zoomout = 2;
		maxsize = size * zoomout;

		image = Image.open(images[index]);
		point = Point(
			rrand(maxsize, image.bounds.width - maxsize),
			rrand(maxsize, image.bounds.height - maxsize),
		);
		
		previmage = nil;
		prevpoint = nil;
		
		window.onClose_({
			image !? { image.free };
			previmage !? { previmage.free };
		});

		view = UserView.new(window, window.bounds);
		frames = 0;
		framerate = 20;
		
		sizeenv = Env([1,1,zoomout], dur * framerate / 2, \sin);
		
		view.drawFunc_({
			var w, h;

			if(frames <= 0) {
				previmage !? { previmage.free };
				previmage = image;
				prevpoint = point;
				index = index + 1 % images.size;
				image = Image.open(images[index]);
				point = Point(
					rrand(maxsize, image.bounds.width - maxsize),
					rrand(maxsize, image.bounds.height - maxsize),
				);
			};

			w = maxsize;
			h = w / window.bounds.width * window.bounds.height;

			previmage.drawInRect(
				rect: window.bounds,
				fromRect: Rect(
					prevpoint.x - (w/2),
					prevpoint.y - (h/2),
					w,
					h
				),
				fraction: 1.0
			);

			w = size * sizeenv.at(frames % (dur * framerate));
			h = w / window.bounds.width * window.bounds.height;

			image.drawInRect(
				rect: window.bounds,
				fromRect: Rect(point.x - (w/2), point.y - (h/2), w, h),
				fraction: (frames % (dur * framerate)).linlin(
					0, dur * framerate / 2, 0.0, 1.0
				)
			);

			frames = frames + 1 % (dur * framerate);
		});

		view.frameRate_(framerate);
		view.animate_(true);
		window.front;
	}

	*stop {
		{ window !? { window.close } }.defer;
	}
}

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
				// with \gate arg (and fade in/fade out somewhat)
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

/////////////////////////////////////////////////////
// HOOKS
/////////////////////////////////////////////////////

+ Symbol {
	doc {
		if(this != \tidy) { ^super.help };

		"".postln;
		"commands".postln;
		"-------------------------------------------".postln;
		"\\tidy .load(folder) : load samples".postln;
		"\\tidy .samples : post samples loaded".postln;
		"\\tidy .synthdefs : post synthdefs loaded".postln;
		"\\tidy .cps(x) : set cycles per second".postln;
		"\\tidy .quant(x) : set quantisation".postln;
		"\\tidy .rec(name, cycles, bus, nudge)".postln;
		"\\tidy .save(name, folder) : saves it".postln;
		"\\tidy .end(x) : fade out + end in x seconds".postln;
		"\\tidy .setup(cps, scd) default \\\"~/setup.scd\"".postln;
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

	defs {
		if(this == \tidy) {
			var width=40, defs;
			"".padLeft(width, "-").postln;
			defs = Dictionary.newFrom([\fx, List.new, \src, List.new]);
			SynthDescLib.global.synthDescs.keys.do { |k|
				case
				{ k.asString.keep(7) == "system_" } { }
				{ k.asString.keep(4) == "old_" } { }
				{
					var added = false;
					SynthDescLib.at(k).inputs.do { |iodesc|
						if(iodesc.asString == "audio In in 2") {
							defs[\fx].add(k);
							added = true;
						};
					};
					if(added.not) { defs[\src].add(k) };
				}
			};

			defs.keysValuesDo({ |key, v|
				var str="";
				key.asString.toUpper.postln;
				v.sort.do { |k|
					if((str.size + k.asString.size) > 40) {
						str.postln;
						str = "";
					};
					str = str + k.asString + "-"
				};
				if(str.size > 0) { str.postln };
			});
			"".padLeft(width, "-").postln;
		}
	}

	setup { |cps, scd|
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
					in = Select.ar(
						CheckBadValues.ar(in,0,0),
						[in,zero,zero,in]
					);
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
					CmdPeriod.add({
						Routine({ Library.at(\guard).value }).play;
					});
					Library.at(\guard).value;
				});

				s.sync;

				JSTidy.add_internal_synthdefs;
				JSTidy.cps(cps ? (80 / 60 / 4)); // default 80 bpm 4/4

				scd = (scd ? "~/setup.scd").standardizePath;
				if(File.exists(scd)) { scd.load };

				s.sync;

				"*** Tidy setup complete ***\n".postln;
			});
		}
	}

	midi {
		if(this == \tidy) {
			Routine({
				// connect with QJackCtl to CH345 device ("OUT" means out)
				MIDIClient.init;
				Server.default.sync;
				Library.put(\tidy, \midiout, MIDIOut(0));
				"*** midi ready ***".postln;
			}).play;
		};
	}
	
	end { |fadeTime=1|
		if(this == \tidy) {
			Routine({
				fadeTime.wait;
				JSVisuals.stop;
			}).play;
			JSTidy.end(fadeTime);
		}
	}

	cps { |cps| if(this == \tidy) { JSTidy.cps(cps) } }
		
	quant { |quant| if(this == \tidy) { JSTidy.quant(quant) } }

	load { |folder| if(this == \tidy) { JSTidy.load(folder) } }

	visuals { |folder| if(this == \tidy) { JSVisuals(folder).start } }
	
	samples { |bank| if(this == \tidy) { JSTidy.loaded(bank) } }

	audit { |bank| if(this == \tidy) { JSTidy.audit(bank) } }

	sample { |bank, index|
		if(this == \tidy) {
			^Library.at(\samples, bank.asSymbol, index)
		}
	}
	
	show { |what| if(this == \tidy) { JSTidy.log(what) } }

	fx { |in| ^JSTidyFX(this, in) }	// return object to the Interpreter

	get { if(this == \tempo) { ^(\tempo.bus.getSynchronous) } }
	
	scope {
		defer {
			var scope = Server.default.scope;
			scope.window.alwaysOnTop_(true)
		};
	}

	-- { |in|
		case
		{ this == \mute } { ^JSTidy.mute(in.asString) }
		{ this == \solo } { ^JSTidy.solo(in.asString) }
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
			"strange input %% -- %".format("\\", this, in.class).postln
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
			if(folder.endsWith(".wav")) {
				path = folder;
			} {
				folder = folder.standardizePath;
				path = folder ++ "/" ++ name ++ ".wav";
			};
			"Writing %% to %".format("\\", name, path.quote).postln;
			buffer.write(path, "wav");
		};
	}
}
