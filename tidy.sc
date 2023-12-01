/*
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
idea: "seed 1234" : control randomness; key tracking
idea: "trig 1000100101" or "hex 7fa4" to generate/override structure
mini notation parser more robust: monitor index through recursive looping
idea: "b [0 1 2 3 ~]??" means "pick one of them randomly"
startrek samples, cars you recorded in stereo
make vital classic bend VOsc wavetables: sine - tri - saw - pulse - pulsew
log: maybe add a parametername to log to only log that
make all sample / synthdef volumes RIGHT

store these in Library; inside play method a step can check it's name
\solo -- "a b one beat" // soloes 4 tracks by name
\mute -- "a b c" // same way
	
pattern "mute 1 0 1 1" should work too, or "solo 1 0 0 1"

growl synthdef wants to send intermediate signal result to a reverb:
- growl synthdef declares args "room" and "room_bus"
- the pattern supplies: "room 0.4"
- in step.play: if the synthdef has an arg called "room", then 
  - step.play adds argument to synth: room_bus = 45
  - growl can send signal to "room_bus"
- else
  - existing put_sends mechanism will supply \outx and \gainx args to growl
so the synthdef dictates the name for the reverb (in tidal it is always "room" too)

idea: visuals: a bash routine displaying animated gifs behind your transparent window it must be cheap on cpu and hassle free; fill a folder with the gifs and run it. maybe open a window using sc?

\tidy .query : does s.queryAllNodes

study these:
{
	var sig = WhiteNoise.ar(0.1);
	BLowShelf.ar(sig, MouseX.kr(300, 3000) * 0.5, 1.0, -60) +
	BHiShelf.ar(sig, MouseX.kr(300, 3000) / 0.5, 1.0, -60)
}.play

movie score: intro,theme,build,climax,theme(emotional),intro(conclusive)
*/

JSTidy {
	classvar loglevel, <postprocessors;

	var <>name, <tree, cur;

	*new { |name| ^super.new.name_(name) }

	*add_internal_synthdefs {

		Library.at(\tidy, \internal_synthdefs_added) !? { ^this };

		SynthDef(\tidy_fader, {
			var bus, val, target, time, trig;
			target = \target.kr(0);
			bus = \bus.kr(0);
			time = \fadetime.kr(0);
			val = In.kr(bus, 1);
			trig = \trig.tr(0);
			val = Env([val, val, target], [0, time]).kr(0, trig);
			ReplaceOut.kr(bus, val);
		}).add;

		SynthDef(\playbuf_2, {
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
			sig = Splay.ar(sig, 0, \vel.kr(0.5), \pan.kr(0));

			sig = sig * \gain.kr(1);

			Out.ar(\out1.kr(0), sig * \gain1.kr(0));
			Out.ar(\out2.kr(0), sig * \gain2.kr(0));
			Out.ar(\out3.kr(0), sig * \gain3.kr(0));
			Out.ar(\out4.kr(0), sig * \gain4.kr(0));
		}).add;

		SynthDef(\playbuf_1, {
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

			sig = sig * \gain.kr(1);

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
	
	*hush { |name, seconds=0|
		Routine({
			// stop the routine
			var beats;
			seconds = max(0.2, seconds);
			beats = seconds * TempoClock.tempo;
			Library.at(\tidyar, name, \fader_synth) !? { |synth|
				Server.default.bind {
					synth.set(\target, 0, \trig, 1, \fadetime, seconds)
				}
			};
			beats.wait;
			Library.at(\tidyar, name, \routine) !? { |routine|
				Library.put(\tidyar, name, \routine, nil);
				routine.stop
			};
			
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
		// if tree is nil then something has gone wrong while creating tree
		// in that case: stop here, so that the old tree will keep going.
		tree ?? { "tree nil".postln; ^this };

		if(JSTidy.should_log(\tree)) { tree.log };

		name.printOn(stream);  // output to postwindow

		// make sure that only 1 routine will replace the running
		// routine at the next quantisation point. one could
		// evaluate some code twice within one quantisation period!
		Library.at(\tidyar, name, \evaluated) ?? { 

			// a logical cycle lasts 1 TempoClock beat, quant is in cycles
			Library.put(\tidyar, name, \evaluated, Routine({
				var quant, nudge=0.001, now, wait, bus, synth;

				// make sure there is a fader bus
				(bus = Library.at(\tidyar, name, \fader_bus)) ?? {
					bus = Bus.control(Server.default, 1);
					Library.put(\tidyar, name, \fader_bus, bus);
				};

				// put fader synth on it, for setting the bus value
				(synth = Library.at(\tidyar, name, \fader_synth)) ?? {
					synth = Synth(\tidy_fader, [
						\target, 0,
						\bus, bus,
						\fadetime, 0,
						\trig, 0,
					]);
					Server.default.sync;
					Library.put(\tidyar, name, \fader_synth, synth);
				};
				
				// quantisation
				now = thisThread.beats;
				quant = JSTidy.quant;

				// stop old routine <nudge> before starting the new one.
				// nudge=0 : old routine triggers one more note, while the
				// new routine also triggers it -> gives double note!
				wait = (now + quant).div(quant) * quant - now;
				if(wait < nudge) { wait = wait + quant };
				
				(wait - nudge).wait;
				Library.at(\tidyar, name, \routine) !? { |r| r.stop };

				nudge.wait;
				Library.put(\tidyar, name, \routine, Routine({
					var cycle, repeat, fading=0, prevfreq;
					
					// enable re-evaluation
					Library.put(\tidyar, name, \evaluated, nil);

					cycle = tree.get(JSTidyCycle.new, name);

					repeat = inf;
					cycle.steps.do({ |x| x.at(\once) !? { repeat = 1 } });

					repeat.do({
						if(JSTidy.should_log(\cycle), { cycle.postln });

						cycle.steps.do({ |step|
							step.put(\prevfreq, prevfreq);
							step.put(\fading, fading);
							step.play(name);
							step.log;
							fading = step.at(\fading);
							prevfreq = step.at(\freq);
							step.delta.wait;
						});

						cycle = tree.get(JSTidyCycle.new, name);
					});

					// you get here if the "once" function is used
					// this Routine will stop, so remove it from library
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
	at { |key| ^dict.at(key.asSymbol) }

	play { |name|
		var def, buf, slow, legato, sustain, degrade, rate, tempo;

		// 1 cycle == 1 TempoClock beat == 1 bar
		// all synthdefs will have their gate shut after sustain beats
		// all synthdefs will receive sustain arg in seconds
		tempo = TempoClock.tempo;
		slow = (dict.removeAt(\slow) ? 1) / (dict.removeAt(\fast) ? 1);
		delta = delta * slow;
		dur = dur * slow;
		legato = (dict.at(\legato) ? 0.8);
		sustain = dur * legato;
		rate = (dict.at(\rate) ? 1);
		degrade = (dict.at(\degrade) ? 1).coin.asInteger;
		if((trig * degrade) <= 0) { ^this };
		this.set_freq(name, sustain);
		dict.put(\prevfreq, dict.at(\prevfreq) ? dict.at(\freq));
		
		// at evaluation, fading is 0. it can be set to 1 only one time.
		if((dict.at(\fading) ? 0) <= 0) {
			dict.at(\gain) !? { |val|
				var sec = dict.at(\gainsec) ? 0;
				Library.at(\tidyar, name, \fader_synth) !? { |synth|
					Server.default.bind {
						synth.set(\target, val, \fadetime, sec, \trig, 1);
					};
					dict.put(\fading, 1); // @see JSTidy.printOn
				};
			}
		};
		
		dict.put(\gain, Library.at(\tidyar, name, \fader_bus).asMap);
		if(this.play_midinote) { ^this };
		this.at(\snd) !? { |bank|
			var index = (dict.at(\buf) ? 0).asInteger;
			buf = Library.at(\samples, bank.asSymbol, index);
			buf ?? { "buf % % unknown".format(bank, index).postln; ^this };
		};
		this.at(\play) !? { |rec|
			buf = Library.at(\tidyrec, rec.asSymbol);
			buf ?? { "rec buf % unknown".format(rec).postln; ^this };
		};
		buf !? {
			var bufseconds = buf.duration / (dict.at(\slices) ? 1);
			var bufbeats = bufseconds * tempo;

			this.put(\bufnum, buf.bufnum);
			rate = rate * (dict.at(\freq) ? 60.midicps) / (60.midicps);
			rate = rate * buf.sampleRate / Server.default.sampleRate;

			case
			{ (dict.at(\fit) ? 0) > 0 } { rate = rate * bufbeats/sustain }
			{ (dict.at(\stretch) ? 0) > 0 } {
				// stretch the cycle: will not sync to other tracks!!
				if(delta > 0) { delta = bufbeats }; // "n [0,2,4]"
				dur = sustain = bufbeats;
			}
			{ (dict.at(\crop) ? 0) > 0 } { }
			{ sustain = bufbeats }; // default: not stretching the cycle
		};

		dict.at(\speed) !? { |speed| rate = rate * speed };
		dict.put(\rate, rate);
		dict.put(\sustain, sustain / tempo); // seconds!
		
		// determine instrument (synthdef) to use
		buf !? { def = "playbuf_%".format(buf.numChannels).asSymbol };
		dict.at(\def) !? { def = dict.at(\def).asSymbol };
		def ?? { "no def".postln; ^this };
		dict.put(\def, def); // also for logging
		def = SynthDescLib.at(def.asSymbol);
		def ?? { "def % unknown".format(dict.at(\def)).postln; ^this };

		this.put_sends;

		Routine({
			var synth;
			dict.at(\late) !? { |ms| (ms.clip(0,20)/1000 * tempo).wait };
			JSTidy.postprocessors.do { |p| p.(dict) };
			Server.default.bind { synth = Synth(def.name, dict.asPairs) };
			if(def.hasGate) {
				sustain.wait;
				Server.default.bind { synth.set(\gate, 0) };
			};
		}).play;
	}

	set_freq { |name, sustain|
		var bus, synth, scale, root2, sustainseconds;

		sustainseconds = sustain / TempoClock.tempo;
		scale = Scale.at((dict.at(\scale) ? \major).asSymbol);

		// if you do "root =root", then dict.at(\root) will be a symbol
		// that symbol points to a controlbus. get the value from that
		// controlbus at the moment you need it and use that
		// TODO: make something more universal for this..
		(dict.at(\root) ? 0) !? { |root|
			if(root.asString.at(0) == $c) {
				var bus;
				bus = root.asString.drop(1).asInteger.asBus;
				root2 = bus.getSynchronous;
			} {
				root2 = root.asInteger;
			};
		};

		dict.at(\freq) ?? {
			var steps = 12;
			var mtranspose = dict.at(\mtranspose) ? 0;
			var degree = (dict.at(\note) ? 0).asFloat;
			var gtranspose = 0;
			var root = dict.at(\root) ? 0;
			var oct = dict.at(\octave) ? 5;

			var note = (degree + mtranspose).degreeToKey(scale, steps);
			// midi is the midinote (continuous intermediate values)
			var midi = ((note + gtranspose + root2) / steps + oct) * 12.0;
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
	
	put_sends {
		var i = 1;
		Library.at(\tidyar).keys.do { |key|
			Library.at(\tidyar, key.asSymbol, \bus) !? { |bus|
				this.at(key.asSymbol) !? { |gain|
					gain = (gain ? 1).asFloat;
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
		if(val == "stretch") { pattern = "1" };

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

				//samples !? { JSTidy.load(samples) };

				//s.sync;

				scd = (scd ? "~/setup.scd").standardizePath;
				if(File.exists(scd)) { scd.load };

				//scd !? { |file|
				//	file = file.standardizePath;
				//	"Loading %".format(file.quote).postln;
				//	file.load;
				//};

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
	
	end { |fadeTime=1| if(this == \tidy) { JSTidy.end(fadeTime) } }

	cps { |cps| if(this == \tidy) { JSTidy.cps(cps) } }
		
	quant { |quant| if(this == \tidy) { JSTidy.quant(quant) } }

	load { |folder| if(this == \tidy) { JSTidy.load(folder) } }
	
	loaded { if(this == \tidy) { JSTidy.loaded } }

	show { |what| if(this == \tidy) { JSTidy.log(what) } }

	fx { |in| ^JSTidyFX(this, in) }	// return object to the Interpreter

	% { |ha| "%: %".format("%", ha).postln } // experimental

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
