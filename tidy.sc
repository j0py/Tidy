// JSTidy using Track objects: to spread code over classes instead of 1 big

JSTidy {
	classvar <>freqmul=1, <>scalename, <>root, <>log=0;
	
	var <>name, <tree, cur;

	*new { |name| ^super.new.name_(name) }

	*scale { |name| scalename = name !? { name.asSymbol } ?? nil }

	*scales { JSUtil.postlist(Scale.names.sort) }

	*end { |seconds=0|
		seconds = max(0.2, seconds); // no plopping please

		JSLoop.instance.end(seconds); // stop sequencers

		Routine({
			(seconds * TempoClock.tempo).wait; // wait for sequencers

			// stop kr synths (control values)
			Library.at(\tidykr) !? { |dict|
				dict.keys.do { |name|
					Library.at(\tidykr, name, \synth) !? { |synth|
						synth.release;
						"released %".format(name).postln;
					};
				}
			};

			// stop effect synths
			Library.at(\tidyar) !? { |dict|
				dict.keys.do { |name|
					Library.at(\tidyar, name, \synth) !? { |synth|
						synth.release;
						"released %".format(name).postln;
					};
				};
			};

			// after 1 second, the \id synth (which has a release time of
			// 0.5 seconds) will have released, so no audio is going to
			// the hardware outputs anymore.
			TempoClock.tempo.wait;
			
			// free buses
			Library.at(\tidyar) !? { |dict|
				dict.keys.do { |name|
					Library.at(\tidyar, name, \bus) !? { |bus|
						bus.free;
						"free fx bus %".format(name).postln;
					};
				};
			};

			Library.at(\tidykr) !? { |dict|
				dict.keys.do { |name|
					Library.at(\tidykr, name, \bus) !? { |bus|
						bus.free;
						"free cv bus %".format(name).postln;
					};
				};
			};
		}).play;
	}
	
	*mute { |str_or_symbol|
		var muted = Library.at(\tidy, \muted) ? Set.new;
		if(str_or_symbol.isString) {
			muted = Set.new; // removeAll does not work on Set..
			str_or_symbol.split($ ).do { |key|
				case
				{ key.isNil } { }
				{ key.asString.size <= 0 } { }
				{ muted.add(key.asSymbol) }
			}
		} { // str_or_symbol is a Symbol
			muted.add(str_or_symbol.asSymbol);
		};

		Library.put(\tidy, \muted, muted);
		^this.pr_set_mute_buses;
	}

	*unmute { |str_or_symbol|
		var muted = Library.at(\tidy, \muted) ? Set.new;
		if(str_or_symbol.isString) {
			str_or_symbol.split($ ).do { |key|
				case
				{ key.isNil } { }
				{ key.asString.size <= 0 } { }
				{ muted.remove(key.asSymbol) }
			}
		} { // str_or_symbol is a Symbol
			muted.remove(str_or_symbol.asSymbol);
		};

		Library.put(\tidy, \muted, muted);
		^this.pr_set_mute_buses;
	}

	*solo { |str_or_symbol|
		var soloed = Set.new;
		if(str_or_symbol.isString) {
			str_or_symbol.split($ ).do { |key|
				case
				{ key.isNil } { }
				{ key.asString.size <= 0 } { }
				{ soloed.add(key.asSymbol) }
			}
		} { // str_or_symbol is a Symbol
			soloed.add(str_or_symbol.asSymbol)
		};
		
		Library.put(\tidy, \soloed, soloed);
		^this.pr_set_mute_buses;
	}

	*unsolo { |str_or_symbol|
		var soloed = Library.at(\tidy, \soloed) ? Set.new;
		if(str_or_symbol.isString) {
			str_or_symbol.split($ ).do { |key|
				case
				{ key.isNil } { }
				{ key.asString.size <= 0 } { }
				{ soloed.remove(key.asSymbol) }
			}
		} { // str_or_symbol is a Symbol
			soloed.remove(str_or_symbol.asSymbol)
		};
		
		Library.put(\tidy, \soloed, soloed);
		^this.pr_set_mute_buses;
	}

	// something has changed in the soloed or muted list.
	// this might affect one or more tracks.
	// let all tracks re-think the value for their mute_bus
	//
	// why is there a mute_bus for each track?
	// if track \a starts playing a loooong note
	// and track \b is put in solo
	// then the loooong note on \a should be muted while still playing
	// this is done by using the value of the mute_bus in all synthdefs
	// as an inverted multiplier for the generated sound
	//
	*pr_set_mute_buses {
		var soloed = Library.at(\tidy, \soloed) ? Set.new;
		var muted = Library.at(\tidy, \muted) ? Set.new;

		JSLoop.instance.set_mute_bus;

		^"soloed %, muted %".format(soloed.as(Array), muted.as(Array));
	}

	// cycles per second
	*cps { |cps|
		if(cps.isNil) {
			cps = TempoClock.tempo;
			^"cps: % (% bpm)".format(cps, cps * 4 * 60);
		};

		Routine({
			JSTidy.quantize;
			TempoClock.tempo_(cps);
		}).play;
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

		if(JSTidy.log == \tree) { tree.log };

		name.printOn(stream);  // output to postwindow

		JSLoop.instance.put(name, tree);
	}
}

JSLoop {
	classvar singleton;

	var routine, <tracks;
	
	*instance { ^(singleton = singleton ? JSLoop.new) }

	*new { ^super.new.init }

	init {
		tracks = Dictionary.new;
		routine = Routine({
			loop {
				tracks.do({ |track|
					if(track.hushed.not) {
						Routine({ track.play_next_cycle }).play
					};
				});
				1.wait
			}
		}).play;
	}

	// you get a new tree (asynchronous!); track may not exist yet
	put { |name, tree|
		var track = tracks.at(name) ? JSTrack.new(name);
		tracks.put(name, track);
		track.put(tree);
	}
	
	end { |seconds = 0|
		tracks.do { |track| track.hush(seconds) };
		Routine({
			(seconds * TempoClock.tempo).wait;
			routine !? { routine.stop; routine = nil };
			singleton = nil;
			"*** ended ***".postln;
		}).play;
	}

	hush { |name, sec=0| tracks[name.asSymbol] !? { |t| t.hush(sec) } }

	set_mute_bus { tracks.do { |track| track.set_mute_bus } }
	
	pr_quantize_wait { |shorter=0|
		var now, quant, wait;
		
		now = thisThread.beats;
		quant = JSTidy.quant;
		wait = (now + quant).div(quant) * quant - now;
		wait = wait - shorter;
		wait.wait;
	}
}

JSTrack {
	var <>name, <>prevfreq;
	var <gain_bus, gain_routine, last_gain, gain_synth;
	var <mute_bus, last_mute, queue, tree, newtree;
	var <hushed=false, hushing=false;

	*new { |name| ^super.new.name_(name).init }

	init {
		gain_bus = Bus.control(Server.default, 1);
		mute_bus = Bus.control(Server.default, 1);
	}

	put { |atree|
		newtree = atree;
		tree = tree ? newtree;
		hushed = false;
		hushing = false;
	}
	
	play_next_cycle {
		var steps = [], delta = 1;

		tree = newtree;
		queue = queue ? List.new;
		
		while { delta > 0.0001 } {
			var step, clone, slow;

			if(queue.size <= 0) { this.add_to_queue(tree) };

			step = queue.removeAt(0);
			slow = step.dict.removeAt(\slow) ? 1;
			slow = slow / (step.dict.removeAt(\fast) ? 1);
			step.delta = step.delta * slow;
			step.dur = step.dur * slow;
			
			if(delta >= step.delta) {
				delta = delta - step.delta;
			} {
				// insert a rest step at head of the queue
				queue.insert(0, JSTidyStep.rest(step.delta - delta));
				step.delta = delta;
				delta = 0;
			};

			steps = steps.add(step);
		};

		steps.do({ |step|
			// trigger the step
			step.put(\prevfreq, prevfreq);
			step.play(name, this);
			step.log;
			prevfreq = step.at(\freq);
			step.delta.wait;
		});
	}

	add_to_queue { |tree|
		var rot, cycle = tree.get(JSTidyCycle.new, name);
		cycle.steps.do({ |x| rot ?? rot = x.at(\rot) });
		cycle.rotate(rot ? 0);

		if(JSTidy.log == \cycle) { cycle.postln };

		queue.addAll(cycle.steps);
	}

	hush { |seconds|
		Routine({
			this.pr_set_gain(0, seconds);
			hushing = true;
			(seconds * TempoClock.tempo).wait;
			hushed = true;
			queue = List.new;
			last_gain = nil;
		}).play;
	}

	set_gain { |step|
		this.pr_alter_gain(step);
		step.put(\gain_bus, gain_bus.asMap);
		step.put(\mute_bus, mute_bus.asMap);
	}

	set_mute_bus {
		var should_mute = this.should_mute.asInteger;
		//"% %".format(name, should_mute).postln;
		if(should_mute == last_mute) { ^this };
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
		var gain = (step.at(\gain) ? 0.5);
		if(gain == last_gain) {	^this };
		this.pr_set_gain(gain, max(0.02, step.at(\gainsec) ? 0));
	}
	
	pr_set_gain { |gain, sec=0|
		if(hushing) { ^this };
		gain_routine !? { gain_routine.stop; gain_routine=nil };

		"% gain % -> % (%)".format(name, last_gain, gain, sec).postln;
		if(last_gain.isNil) { sec = 0 }; // do not miss 1st kickdrum..
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
			gain_routine = nil;
			gain_synth = nil;
		}).play;
	}
}

JSTidyCycle {
	var <>steps, <>env;

	*new { |steps| ^super.new.make_steps(steps).make_index }

	rotate { |rot|
		if(rot != 0) {
			steps = steps.rotate(rot);
			this.make_index;
		}
	}
	
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

	*rest { |delta|	^JSTidyStep(0, delta, delta, "~", 0) }
	
	putAll { |argdict| dict.putAll(argdict) }
	put { |key, value| dict.put(key.asSymbol, value) }

	at { |key|
		var val = dict.at(key.asSymbol);

		// check if we have a bus, and if so, read value from it
		// if you need to have the Bus yourself, then use dict.at()
		if(val.class == Bus) { ^val.getSynchronous };
		^val;
	}

	removeAt { |key|
		var val = this.at(key.asSymbol);
		dict.removeAt(key.asSymbol);
		^val;
	}

	play { |name, track|
		var def, buf, legato, sustain, degrade, rate, tempo;
		var bufbeats, bufseconds, do_not_play=false;
		
		// 1 cycle == 1 TempoClock beat == 1 bar
		// all synthdefs will have their gate shut after sustain beats
		// all synthdefs will receive sustain arg in seconds
		tempo = TempoClock.tempo;
		rate = (this.at(\rate) ? 1);

		// mute & solo & degrade
		if(track.should_mute) { do_not_play = true };
		degrade = (this.at(\degrade) ? 1).coin.asInteger;
		if((trig * degrade) <= 0) { do_not_play = true };
		if(trig <= 0) { do_not_play = true };

        // this might be a control signal track
		if(do_not_play == false) {
			dict.at(\cv) ? dict.at(\control) !? { |cv|
				Routine({
					var bus, server;
					server = Server.default;
					(bus = Library.at(\tidykr, name.asSymbol, \bus)) ?? {
						bus = Bus.control(server, 1);
						Library.put(\tidykr, name.asSymbol, \bus, bus);
					};
					bus.setSynchronous(cv.asFloat);
				}).play;

				do_not_play = true;
			}
		};
		
		if(do_not_play == false) {

			track.set_gain(this);
			
			dict.at(\snd) !? { |bank|
				var index = (this.at(\buf) ? 0).asInteger;
				buf = Library.at(\samples, bank.asSymbol).wrapAt(index);
				buf ?? {
					"buf % % unknown".format(bank, index).postln;
					do_not_play = true;
				};
			};
			
			dict.at(\play) !? { |rec|
				buf = Library.at(\tidyrec, rec.asSymbol);
				buf ?? {
					"rec buf % unknown".format(rec).postln;
					do_not_play = true;
				};
			};
		};
		
		if(do_not_play) {
			dict.keysValuesDo({ |k,v|
				if(v.class == Bus) { dict.put(k, v.asMap) }
			});
			^this;
		};
		
		legato = (this.at(\legato) ? 0.8);
		buf !? { legato = (this.at(\legato) ? 1) };
		sustain = dur * legato;
		
		buf !? {
			var fit, stretch, end, begin;
			
			def = "tidy_pb_%".format(buf.numChannels).asSymbol;
			end = (this.at(\end) ? 1);
			begin = (this.at(\begin) ? 0);
			bufseconds = buf.duration * (end - begin);
			bufbeats = bufseconds * tempo;

			this.put(\bufnum, buf.bufnum);
			rate = rate * buf.sampleRate / Server.default.sampleRate;

			case
			{ (fit = (this.at(\fit) ? 0)) > 0 }
			{
				// stretch or shrink the step
				if(delta > 0) { delta = delta * fit }; // "n [0,2,4]"
				sustain = sustain * fit;
				// stretch or shrink the sample to fit inside it
				rate = rate * bufbeats / sustain;
			}
			{ (stretch = (this.at(\stretch) ? 0)) > 0 }
			{
				// stretch or shrink the step to match the sample
				if(delta > 0) { delta = bufbeats * stretch }; //"n [0,2,4]"
				sustain = bufbeats * stretch;
			}
			// if bufbeats > sustain, then the sample will be cut short
			{ (this.at(\crop) ? 0) > 0 } { }

			// default: delta remains what it is, sustain is adapted
			// so that the whole sample can play out during the step
			{ sustain = bufbeats };

			dict.put(\sustainbeats, sustain);
		};

		this.set_freq(name);
		dict.put(\prevfreq, dict.at(\prevfreq) ? dict.at(\freq));

		if(this.play_midinote) { ^this };

		dict.at(\def) !? { def = dict.at(\def) };
		def ?? { "no def".postln; ^this };
		def = def.asSymbol;
		dict.put(\def, def); // also for logging

		this.at(\speed) !? { |speed| rate = rate * speed };
		dict.put(\rate, rate);

		// sustain is overrideable in seconds (for percussive synths)
		this.at(\sustain) ?? { dict.put(\sustain, sustain/tempo) }; //secs
		sustain = dict.at(\sustain) * tempo; // beats
		
		this.put_sends;

		dict.keysValuesDo({ |k, v|
			if(v.class == Bus) { dict.put(k, v.asMap) }
		});

		Routine({
			var synth, ms;

			// why on earth does this stop the sound????
			ms = (dict.at(\late) ? 0).asFloat;

			if(ms > 0) {
				var beats = (ms.clip(0, 20) / 1000 * tempo);
				beats.wait;
			};

			Server.default.bind { synth = Synth(def, dict.asPairs) };
			sustain.wait;
			Server.default.bind { synth.set(\gate, 0) };
		}).play;
	}

	set_freq { |name|
		var bus, synth, scale;

		scale = dict.at(\scale) ? JSTidy.scalename ? \major;
		scale = Scale.at(scale.asSymbol) ? Scale.major;

		this.at(\freq) ?? {
			var steps = 12;
			var mtranspose = this.at(\mtranspose) ? 0;
			var degree = (this.at(\note) ? 0).asFloat;
			var gtranspose = 0;
			var root = this.at(\root) ? 0;
			var oct = this.at(\octave) ? 5;

			var note = (degree + mtranspose).degreeToKey(scale, steps);
			// midi is the midinote (continuous intermediate values)
			var midi = ((note + gtranspose + root) / steps + oct) * 12.0;
			var ctranspose = 0;
			var harmonic = 1.0;
			var detune = 0;

			this.at(\midinote) !? { midi = this.at(\midinote) };
			dict.put(
				\freq,
				(midi + ctranspose).midicps * harmonic + detune
			);
		};

		dict.put(\freqmul, JSTidy.freqmul); // 1 or a \cxxx symbol
	}

	play_midinote {
		dict.at(\midiout) !? { |chan|
			Library.at(\tidy, \midiout).noteOn(
				chan,
				dict.at(\freq).cpsmidi.asInteger,
				(this.at(\vel) ? 0.5).linlin(0, 1, 0, 127)
			);
			^true;
		};
		^false;
	}
	
	put_sends {
		var send, mix, fx;

		// - "mix f4" - : gains for fx 0 and 1 (not patternable)
		mix = 0!10;
		(dict.at(\mix) ? "f").do { |gain, i|
			mix[i] = gain.digit.linlin(0, 15, 0, 1).asFloat;
		};

		// - "4 0.4 0.2" -, override gain for fx 4 (patternable)
		// - "4 =cvx" -, override gain for fx 4 with controlbus value
		fx = nil!10;
		9.do { |i| dict.at(i.asSymbol) !? { |gain| fx[i] = gain } };

		// distribute the values over the available outputs of the synth
		send = 1;
		9.do { |i|
			Library.at(\tidyar, i.asSymbol, \bus) !? { |bus|
				case
				{ fx[i].notNil } {
					this.put(("out"++(send.asString)).asSymbol, bus.index);
					this.put(("gain"++(send.asString)).asSymbol, fx[i]);
					send = send + 1;
				}
				{ mix[i] > 0 } {
					this.put(("out"++(send.asString)).asSymbol, bus.index);
					this.put(("gain"++(send.asString)).asSymbol, mix[i]);
					send = send + 1;
				}
				{}
			}
		}
	}

	/*
	put_sends_depr {
		var mix, send=1;

		mix = dict.at(\mix) ? "f";
		mix.do { |gain, i|
			Library.at(\tidyar, i.asSymbol, \bus) !? { |bus|
				gain = gain.digit.linlin(0, 15, 0, 1).asFloat;
				this.put(("out"++(send.asString)).asSymbol, bus.index);
				this.put(("gain"++(send.asString)).asSymbol, gain);
				send = send + 1;
			}
		}
	}
	*/
	
	log {
		if(JSTidy.log == \step) {
			this.postln;
		} {
			if((dict.at(\log) ? 0) > 0) {
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

JSDefs {
	classvar defs_added = 0;
	
	*def { |name, func, variants|
		SynthDef(name, {
			var sig, env, vel, freq, glide, sus, pfreq, gate,
			att, rel, crv, amp, lpf;

			vel = \vel.kr(0.5); // timbre (synthdef can use this)
			amp = \amp.kr(0.5); // volume accents in patterns or modulation
			gate = \gate.kr(1);
			freq = \freq.kr(440);
			pfreq = \prevfreq.kr(440);
			sus = \sustain.kr(0); // sustain in seconds
			glide = \glide.kr(0);
			freq = Env([pfreq, freq], glide * sus, \exp).kr(0, gate);
			att = \att.kr(0);
			rel = \rel.kr(0.1);
			crv = \crv.kr(-4);

			freq = freq * \freqmul.kr(1); // multiply freq everywhere
			
			sig = SynthDef.wrap(
				func,
				[],
				[freq, vel, gate, sus, att, rel, crv]
			);
			sig = sig * amp;
			sig = sig * \gain_bus.kr(1); // fade in/out
			sig = sig * abs(\mute_bus.kr(0).asInteger.clip(0,1) - 1);

			// lpf with key tracking
			lpf = \lpf.kr(20000);
			lpf = \kt.kr(0).linlin(0, 1, lpf, max(freq * (0.5+vel), lpf));
			sig = RLPF.ar(sig, lpf, \rq.kr(1).linlin(0, 1, 0.05, 1));
			
			8.do({ |i|
				Out.ar(
					("out"++(i+1)).asSymbol.kr(0),
					sig * ("gain"++(i+1)).asSymbol.kr(0)
				);
			});
		}, variants: variants).add;
	}

	*add_internal_defs {
		if(defs_added > 0) { ^this };

		SynthDef(\tidy_fader, { |target=0, bus, fadetime=0, t_trig=1|
			var val = In.kr(bus, 1);
			val = Env([val, val, target], [0, fadetime]).kr(2, t_trig);
			ReplaceOut.kr(bus, val);
		}).add;

		JSDefs.def(\tidy_pb_2, {
			arg freq, vel, gate, sustain, att, rel, crv;
			var sig, rate, bufnum, begin;

			rate = \rate.kr(1) * freq / 60.midicps;
			rate = rate * \rate2.kr(1);
			bufnum = \bufnum.kr(0);
			begin = \begin.kr(0) * BufFrames.kr(bufnum);
			sig = PlayBuf.ar(2, bufnum, rate, startPos: begin);
			sig = LeakDC.ar(sig);
			sig = Balance2.ar(sig[0], sig[1], \pan.kr(0));
			sig = sig * Env.asr(att, 1, rel, crv).kr(2, gate);
		});
		
		JSDefs.def(\tidy_pb_1, {
			arg freq, vel, gate, sustain, att, rel, crv;
			var sig, rate, bufnum, begin;
			
			rate = \rate.kr(1) * freq / 60.midicps;
			rate = rate * \rate2.kr(1);
			bufnum = \bufnum.kr(0);
			begin = \begin.kr(0) * BufFrames.kr(bufnum);
			sig = PlayBuf.ar(1, bufnum, rate, startPos: begin);
			sig = LeakDC.ar(sig);
			sig = Pan2.ar(sig, \pan.kr(0));
			sig = sig * Env.asr(att, 1, rel, crv).kr(2, gate);
		});

		defs_added = 1;
	}

	*defs {
		var defs = Dictionary.newFrom([
			\fx, SortedList.new, \src, SortedList.new
		]);
		SynthDescLib.global.synthDescs.keys.do { |k|
			case
			{ k.asString.keep(7) == "system_" } { }
			{ k.asString.keep(4) == "old_" } { }
			{ k.asString.keep(5) == "tidy_" } { }
			{
				var added = false;
				SynthDescLib.at(k).inputs.do { |iodesc|
					if(iodesc.asString == "audio In in 2") {
						defs[\fx].add(k.asString);
						added = true;
					};
				};
				if(added.not) {
					defs[\src].add(k.asString);
					SynthDescLib.at(k).def.variants !? { |variants|
						variants.keys.do { |v|
							defs[\src].add((k ++ "." ++ v).asString);
						}
					}
				};
			}
		};

		defs.keysValuesDo({ |key, v|
			key.asString.toUpper.postln;
			JSUtil.postlist(v);
		});
	}
}

JSUtil {
	*postline { |w=38| "".padLeft(w+6, "-").postln }
	
	*postlist { |list, sep|
		var w = 38, str = ",".ccatList(list).replace(",, ", "");
		sep = sep ? " - ";
		
		JSUtil.postline;
		while { str.size > w } {
			var i = w;
			while { str[i] != $  and: (i > 0) } { i = i - 1 };
			if(i > 0) {
				str.keep(i+1).replace(", ", sep).postln;
				str = str.drop(i+1);
			} {
				str.keep(w).replace(", ", sep).postln;
				str = str.drop(w);
			}
		};
		str.replace(", ", sep).postln;
		JSUtil.postline;
	}

	*load { |folder|
		var func = {
			var s = Server.default;
			folder = folder.standardizePath;
			(folder +/+ "*").pathMatch.do({ |bank|
				var samples = List.new;
				(bank +/+ "*.wav").pathMatch.do({ |file|
					samples.add(Buffer.read(s, file));
				});
				
				s.sync;

				Library.put(
					\samples,
					bank.basename.withoutTrailingSlash.asSymbol,
					samples
				);
			});
			s.sync;
			"Loaded %".format(folder.quote).postln;
			JSUtil.samples;
		};
		
		if(thisProcess.mainThread == thisThread)
		{ Routine(func).play } { func.value };
	}

	*samples { |bank|
		case
		{ Library.at(\samples).isNil } {
			JSUtil.postlist(["** no banks / samples **"])
		}
		{ bank.isNil }
		{
			JSUtil.postlist(
				Library.at(\samples).keys.asArray.sort.collect({ |key|
					var val = Library.at(\samples, key.asSymbol);
					"% %".format(key, val.size);
				})
			)
		}
		{ Library.at(\samples, bank.asSymbol).isNil } {
			JSUtil.postlist([
				"** unknown bank : % **".format(bank.asString)
			])
		}
		{
			JSUtil.postline;
			"bank % samples:".format(bank.asString.quote).postln;

			Library.at(\samples, bank.asSymbol).do({ |buf, i|
				var filename = PathName(buf.path).fileName;
				var index = i.asString.padLeft(3);
				var sec = buf.duration.round(0.01).asString.padLeft(3);
				"% (% sec) %".format(index, sec, filename).postln;
			});
			JSUtil.postline;
		};
	}

	*sample { |bank, index|	^Library.at(\samples, bank.asSymbol, index) }

	*audit { |bank|
		Routine({
			Library.at(\samples, bank.asSymbol).do({ |buf, i|
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
				var stepval = step.dict.at(key);
				var otherval = other.dict.at(key);
				
				case
				{ stepval.isNil } { step.put(key, otherval) }
				{ otherval.isNil } { }
				// now we know that both are not nil..
				{ val == ">" } { step.put(key, otherval) }
				{ val == "<" } { }
				{ val == "+" } {
					if(stepval.class == Bus) {
						stepval = stepval.getSynchronous;
					};
					if(otherval.class == Bus) {
						otherval = otherval.getSynchronous;
					};
					step.put(key, stepval + otherval)
				}
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
				var stepval = step.dict.at(key);
				var otherval = other.dict.at(key);
				
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
			time = time + step.delta; // slow/fast?
		};

		// add steps of the stack + alt cycle to PriorityQueue
		stack ?? { stack = [ JSTidyStep(0, shift, shift, "~", 0) ] };
		time = 0;
		stack.do { |step|
			pq.put(time, step);
			time = time + step.delta; // slow/fast?
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
		if(val == "rot" and: (pattern.size <= 0)) { pattern = "1" };

		if(pattern.size > 0) { instance.add(JSTidyPattern(pattern)) };
		^instance;
	}

	// create JSUtil class with this stuff
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
					// "=xxx" : value comes from the controlbus of xxx
					var bus;
					bus = Library.at(\tidykr, str.drop(1).asSymbol, \bus);
					bus !? { step.put(val.asSymbol, bus) }
				} {
					// interpret str depending on the function name (val)
					case
					{ str == "~" } { step.trig = 0 }
					{ val == "def" } { step.put(\def, str.asSymbol) }
					{ val == "buf" } { step.put(\buf, str.asInteger) }
					{ val == "rot" } { step.put(\rot, str.asInteger) }
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
				
				if(((i % 2) == 1).and(el.class == Symbol)) {
					Library.at(\tidykr, el, \bus) !? { |bus|
						result = bus.asMap;
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

	// if JSTidy implements it, then call it
	doesNotUnderstand { |selector ... args|
		if(this == \tidy) {
			JSTidy.class.findMethod(selector) !? { |method|
				JSTidy.performList(method.name, args);
				^this;
			};
		};

		super.doesNotUnderstand(selector, args);
	}

	doc {
		if(this != \tidy) { ^super.doc };

		"".postln;
		"commands".postln;
		"-------------------------------------------".postln;
		"\\tidy .load(folder) : load samples".postln;
		"\\tidy .samples : post samples loaded".postln;
		"\\tidy .defs : post synthdefs loaded".postln;
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

	defs { if(this == \tidy) { JSDefs.defs } }
	samples { |bank| if(this == \tidy) { JSUtil.samples(bank) } }

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

				JSDefs.add_internal_defs;
				JSTidy.cps(cps ? (80 / 60 / 4)); // default 80 bpm 4/4

				scd = (scd ? "~/setup.scd").standardizePath;
				if(File.exists(scd)) { scd.load };

				s.sync;

				"*** Tidy setup complete ***\n".postln;
			});
		}
	}

	midiout {
		if(this == \tidy) {
			Routine({
				// connect with QJackCtl to CH345 device ("OUT" means out)
				MIDIClient.init;
				Server.default.sync;
				Library.put(\tidy, \midiout, MIDIOut(0));
				"*** midiout ready ***".postln;
			}).play;
		};
	}

	// (temporarily) shutup the track which has my value as name
	hush { |seconds=1| JSLoop.instance.hush(this, seconds) }

	mute { ^JSTidy.mute(this) }
	solo { ^JSTidy.solo(this) }
	unmute { ^JSTidy.unmute(this) }
	unsolo { ^JSTidy.unsolo(this) }
	
	-- { |in|
		case
		//
		// effects: \0 .. \9 -- [out, func/symbol, params, gain, target]
		//
		{ (0..9).collect(_.asSymbol).includes(this) }
		{
			if((in.class == Array) and: (in.size >= 2)) {
				^JSTidyFX(this,in[1]).play(in[0], in[3]?1, in[2]?[], in[4])
			};
		
			^"%% -- [out, func/def, [], gain, target]".format("\\", this);
		}

		//
		// audio tracks: \a .. \z -- "func pattern" - ..
		//
		{ (97..122).collect(_.asAscii).collect(_.asSymbol).includes(this) }
		{
			if(in.isString) {
				^JSTidy(this).add_branch("--").add_func(in)
			};

			^"%% -- \"func pattern\" - ..".format("\\", this);
		}

		// specifics
		{ this == \mute } { ^JSTidy.mute(in.asString) }
		{ this == \solo } { ^JSTidy.solo(in.asString) }
		{ this == \unmute } { ^JSTidy.unmute(in.asString) }
		{ this == \unsolo } { ^JSTidy.unsolo(in.asString) }

		//
		// control-rate function
		//
		{ in.isFunction }
		{
			var bus, node, server = Server.default;

			Routine({
				// create control bus
				(bus = Library.at(\tidykr, this, \bus)) ?? {
					bus = Bus.control(server, 1);
					Library.put(\tidykr, this, \bus, bus);
				};

				// keep em in a classvar?
				node = Library.at(\tidykr, this, \synth);

				server.bind {
					node !? { node.free };
					node = in.play(nil, bus.index);
				};

				server.sync;

				Library.put(\tidykr, this, \synth, node);
			}).play;
		}

		//
		// control patterns (must have "cv" function)
		//
		{ in.isString }
		{
			^JSTidy(this).add_branch("--").add_func(in)
		}
		
		{
			^"strange input: %% -- %".format("\\", this, in.class)
		}
	}

	bus { ^Library.at(\tidykr, this, \bus) }

	rec { |name, cycles, bus=2, nudge=(-0.25)|
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
			
			seconds = cycles / TempoClock.tempo; // 1 cycle = 1 beat
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

/*
Midiin + rec
Kbdin + rec
CmdPeriod behaviour
euclids inside <> do not seem to work (but maybe illogical to want)
@2 inside <> is tricky too

idea: sample - reverse - add delays - reverse back
idea: sample - reverse - reverb - resample - loop
looper: start, loop middle of sample (crossfade), at release play till end

idea: "dup 3" | ... would repeat all cycles 3 times
idea: "do 0 1 6 7 5 6" | ... would play the given cycle numbers
how to implement swing? "b [1 ~ 2]@2 3 4"
idea: "seed 1234" : control randomness;
key tracking: lpf = (lpf + (kt * freq))
mini notation parser more robust: monitor index through recursive looping
idea: "b [0 1 2 3 ~]??" means "pick one of them randomly"
make vital classic bend VOsc wavetables: sine - tri - saw - pulse - pulsew
log: maybe add a parametername to log to only log that
make all sample / synthdef volumes RIGHT

study these:
{
	var sig = WhiteNoise.ar(0.1);
	BLowShelf.ar(sig, MouseX.kr(300, 3000) * 0.5, 1.0, -60) +
	BHiShelf.ar(sig, MouseX.kr(300, 3000) / 0.5, 1.0, -60)
}.play

movie score: intro,theme,build,climax,theme(emotional),intro(conclusive)
connect to animatron? with osc?
meltdown function: lower volume + global freq multiplier + global tempo
*/

