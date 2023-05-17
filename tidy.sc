/*
	2023-01-19 STACK,
	~a << "note 1 2 3, 5 6" |> "def atone" : 3 against 2
	~a << "note [1 2 3, 5 6]" |> "def atone" : 3 against 2
	~a << "note [1 <2 _> 3, 5 6]" |> "def atone" : 3 against 2 also
	~a << "note {1 2 3, 5 6}" |> "def atone" : triplets 15 26 35 16 25 36

	because of parallellism, you must resolve \space steps before
	joining two	(or more) parallel streams of steps into one stream
	of steps. a step only has \on, \dur, \string, \number

	2023-01-06, some ideas:
	-----------------------
	- "hover" function: granulates a buffer, but hovers on certain spots,
	given by a pattern, before moving on at normal speed
	- "shift" function: shift the cycle left or right for certain number
	of steps, given by a pattern (0 = no shift)
	- "slice" : the number part could be the slice number to play, while
	the string part could be the midiratio to play it with
	- "life" : bring in small random variations
*/

JSTidy {
	classvar loglevel, routines, <>fxnames;

	var <proxy, count, tree, cur;

	*new { |proxy| ^super.newCopyArgs(proxy) }

	*show { |count| ^super.newCopyArgs(nil, count) }

	*should_log { |level|
		loglevel ?? { loglevel = [] };
		^(loglevel.indexOf(level).notNil);
	}

	*log { |level|
		loglevel ?? { loglevel = [] };
		level ?? { loglevel = [] } !? { loglevel = loglevel.add(level) };
	}

	*hush { |index|
		routines ?? { routines = Order.new };
		routines.at(index) !? { |r| routines.put(index, nil); r.stop };
	}

	*stop {
		routines ?? { routines = Order.new };
		routines.do { |r| r.stop };
	}
	
	// Send signal from your proxy's private bus to other NodeProxies
	// using the \filterIn -> { } method. The slot number to use is
	// the bus index of your proxy + 10. This way, each NodeProxy
	// will use its own unique slot number.

    // do this:
	*send { |from, str|
		from.ar(2); // make sure a bus is allocated (we need the index)

		str.split($ ).clump(2).do { |pair|
			var to, gain, slot;
			
			to = currentEnvironment.at(pair[0].asSymbol);
			gain = pair[1].asFloat;
			slot = from.bus.index + 10;

			if(to.objects.at(slot).isNil, {
				to.put(slot, \mix -> { from.ar });
			});

			//"xset % % %".format(pair[0], "mix"++(slot), gain).postln;
			to.xset(("mix"++(slot)).asSymbol, gain);
		};
	}
	
	printOn { |stream|
		routines ?? { routines = Order.new };

		tree ?? { ^this }; // something may have gone wrong
		if(JSTidy.should_log(\tree), { tree.log });

		count !? {
			if(JSTidy.should_log(\cycle), {
				count.do { tree.get(JSTidyCycle.new) };
				tree.get(JSTidyCycle.new).postln;
			});
		};

		proxy !? {
			var slot;
			proxy.ar(2);
			slot = proxy.bus.index;
			proxy.printOn(stream);
			Routine({
				// on re-evaluation i want a new routine
				routines.at(slot) !? { |r| r.stop };
				routines.put(slot, Routine({
					var time, cycle, slow;
					loop({
						cycle = tree.get(JSTidyCycle.new);
						if(JSTidy.should_log(\cycle), { cycle.postln });
						slow ?? {
							var step = cycle.steps.last;
							
							slow = (step.at(\slow) ? 1);
							slow = slow / (step.at(\fast) ? 1);
						};
						time = 0;
						cycle.steps.do { |step, i|
							var on = step.on * slow; // from prev step!
							(on - time).wait; // time catches up
							time = on;
							step.play(proxy);
							slow = (step.at(\slow) ? 1);
							slow = slow / (step.at(\fast) ? 1);
						};
						max(0, slow - time).wait;
					});
				}).play(proxy.clock));
			}).play(proxy.clock, proxy.quant);
		}
	}

	// If you make a mistake, you might get here.
	// PrintOn does nothing if tree is nil.
	doesNotUnderstand { |selector ... args|
		tree = nil;
		JSTidyException("% not understood".format(selector)).throw;
	}

	<> { |input| }
	
	> { |str| JSTidy.send(proxy, str) }

	// A "branch" is a sub-tree.
	//<< { |str| this.add_branch("<<").add_func(str) }

	// "<function name> <pattern>"
	func { |str|
		var func, pat, class, inst;

		str = str.split($ );
		func = str.removeAt(0);
		pat = str.join($ ).stripWhiteSpace;

		class = "%%".format(func[0].toUpper, func.drop(1).toLower);
		class = "JSTidyFP_%".format(class).asSymbol.asClass;
		class !? { ^class.new(pat) };
		^JSTidyFP(func, pat);
	}

	// Add an object to the tree, which starts off with a branch.
	add { |obj|
		tree ?? { tree = cur = JSTidyBranch("tree") };
		cur.add(obj);
		if(obj.become_cur_after_add, { cur = obj });
	}

	add_func { |str| this.add(this.func(str)) }

	add_branch { |str| this.add(JSTidyBranch(str)) }

	| { |str| this.add_branch("|").add_func(str) }

	// These operators add another "<func> <pattern>" to the tree,
	// thereby specifying who should define the timing structure
	// and what should be done with the value(s) from the
	// "<func> <pattern>".
	//
	|<| {  |str| this.add(JSTidyCombBoth("<").add(this.func(str))) }
	< {  |str| this.add(JSTidyCombBoth("<").add(this.func(str))) }
	|< { |str| this.add(JSTidyCombLeft("<").add(this.func(str))) }
	<| { |str| this.add(JSTidyCombRight("<").add(this.func(str))) }

	|>| {  |str| this.add(JSTidyCombBoth(">").add(this.func(str))) }
	//> {  |str| this.add(JSTidyCombBoth(">").add(this.func(str))) }
	|> { |str| this.add(JSTidyCombLeft(">").add(this.func(str))) }
	>| { |str| this.add(JSTidyCombRight(">").add(this.func(str)))	}
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
	var <>steps;

	*new { |steps| ^super.newCopyArgs(steps ? []) }

	add { |step| steps = steps.add(step) }

	stepAt { |t|
		steps.do { |s| if((t >= s.on).and(t < (s.on + s.dur)), { ^s }) };
		^nil;
	}

	printOn { |stream|
		stream << "cycle\n";
		steps.do { |step| stream << " %".format(step) };
	}
}

JSTidyStep {
	var <>on, <>dur, <>dict;

	*new { |on, dur|
		^super.newCopyArgs(on, dur, Dictionary.new)
	}

	putAll { |argdict| dict.putAll(argdict) }
	put { |key, value| dict.put(key.asSymbol, value) }
	at { |key| ^dict.at(key.asSymbol) }

	play { |proxy|
		var instr, def, sustain, dx7, rootfreq;
		var scale = Scale.at((dict.at(\scale) ? \major).asSymbol);

		this.at(\snd) !? { |bank|
			var index = (this.at(\buf) ? 0).asInteger;
			var buf = Library.at(\samples, bank.asSymbol, index);

			buf !? {
				if(buf.numChannels > 1) {
					this.put(\instrument, \playbuf_stereo);
				} {
					this.put(\instrument, \playbuf_mono);
				};
				this.put(\bufnum, buf.bufnum);
			};

			this.put(\snd, nil);
			this.put(\buf, nil);
			this.put(\chord, nil);
			this.put(\strum, nil);
		};

		this.at(\def) !? { |def|
			this.put(\instrument, def.asSymbol);
			this.put(\def, nil);
		};

		this.at(\note) !? { |note|
			this.put(\degree, note.asFloat);
			this.put(\note, nil);
		};

		this.at(\dx7) !? { |preset|
			dx7 = DX7.new.preset(preset);
			this.put(\instrument, \dx7);
		};

		instr = dict.at(\instrument);
		def = SynthDescLib.at(instr.asSymbol);

		if(def.isNil, { "% unknown".format(instr).postln; ^this });

		dict.at(\freq) ?? {
			var stepsPerOctave = 12;
			var mtranspose = dict.at(\mtranspose) ? 0;
			var degree = dict.at(\degree) ? 0;
			var gtranspose = 0;
			var root = dict.at(\root) ? 0;
			var oct = dict.at(\octave) ? 5;

			var note = (degree + mtranspose).degreeToKey(scale, stepsPerOctave);
			// midi is the midinote (continuous intermediate values)
			var midi = ((note + gtranspose + root) / stepsPerOctave + oct) * 12.0;
			var ctranspose = 0;
			var harmonic = 1.0;
			var detune = 0;

			dict.at(\midinote) !? { midi = dict.at(\midinote) };

			dict.put(\freq, (midi + ctranspose).midicps * harmonic + detune);
		};

		dict.at(\legato) ?? { dict.put(\legato, 0.8) };
		sustain = (dict.at(\legato) ? 0.8) * dur
		* (dict.at(\slow) ? 1)
		/ (dict.at(\fast) ? 1);
		dict.put(\secs, sustain / proxy.clock.tempo);   // in seconds
		dict.put(\out, proxy.bus.index);

		if(JSTidy.should_log(\step), { this.postln });
		
		JSTidy.fxnames.do { |name|
			var gain = dict.at(name.asSymbol);
			var to = currentEnvironment.at(name.asSymbol);
			var slot = proxy.bus.index + 10;
			
			if(to.objects.at(slot).isNil, {
				to.put(slot, \mix -> { proxy.ar });
			});

			// hmmm..
			to.set(("mix"++(slot)).asSymbol, (gain ? 0).asFloat);
			//"% % gain %".format(name, "mix"++(slot), gain).postln;
		};

		//                         degree, root, octave
		//Scale.major.degreeToFreq(2, 60.midicps, 1);
		rootfreq = dict.at(\freq);
		
		(this.at(\chord) ? "0").do { |ch|
			ch = ch.asInteger - $0.asInteger;
			dict.put(\freq, scale.degreeToFreq(ch, rootfreq, 0));

			dx7 !? {
				dict.putAll(dx7.params(dict.at(\freq), dict.atFail(\vel, 64)))
			};

			Routine({
				var synth;
				Server.default.bind {
					synth = Synth(def.name, dict.asPairs);
				};
				
				if(def.hasGate, {
					sustain.wait;
					Server.default.bind { synth.set(\gate, 0) };
				});
			}).play;
			
			//Server.default.bind {
			//	var synth = Synth(def.name, dict.asPairs);
			//	if(def.hasGate, { sustain.wait; synth.set(\gate, 0) });
			//};
		};
	}

	printOn { |stream|
		stream << "step % % ".format(
			on.round(0.001).asString.padLeft(6),
			dur.round(0.001).asString.padLeft(6)
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
	var <>children, <val;

	*new { |val| ^super.newCopyArgs([], val.asString) }

	add { |child| children = children.add(child) }

	log { |indent=""|
		"%% %".format(indent, this.class, val.quote).postln;
		children.do { |child| child.log(indent ++ "--") };
	}

	get { |cycle| ^cycle }

	become_cur_after_add { ^false }
}

JSTidyBranch : JSTidyNode {
	get { |cycle|
		children.do { |child|	cycle = child.get(cycle) };
		^cycle;
	}

	become_cur_after_add { ^true }
}

JSTidyComb : JSTidyNode {
	combine { |step, stepAt, right|
		//" combine\n  %\n  %".format(step, stepAt).postln;
		stepAt.dict.keysValuesDo { |key, value|
			case
			{ val == ">" }
			{ step.put(key, value) }
			{ val == "<" }
			{ step.at(key) ?? { step.put(key, value) }}
			{ val == "+" }
			{ step.put(key, (step.at(key) ? 0) + value) }
			{ val == "*" }
			{ step.put(key, (step.at(key) ? 1) * value) }
			{ val == "/" }
			{
				if(right.isNil, {
					if(value <= 0, {
						step.put(key, 0) // division by zero
					}, {
						step.put(key, (step.at(key) ? 0) / value)
					});
				},{
					var divider = (step.at(key) ? 0);
					if(divider <= 0, {
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

JSTidyCombLeft : JSTidyComb {
	get { |cycle|
		var child = children.first.get(cycle);

		cycle.steps.do { |step|
			child.steps.do { |ch|
				if((ch.on <= step.on).and((ch.on + ch.dur) > step.on), {
					this.combine(step, ch);
				});
			}
		};

		^cycle;
	}
}

JSTidyCombRight : JSTidyComb {
	get { |cycle|
		var child = children.first.get(cycle);

		child.steps.do { |step|
			cycle.steps.do { |ch|
				if((ch.on <= step.on).and((ch.on + ch.dur) > step.on), {
					this.combine(step, ch, \right);
				});
			}
		};

		^child;
	}
}

JSTidyCombBoth : JSTidyComb {
	/*
		- create times array (start AND end times)
		- chop/split cycle/child steps using times array (deepCopy!)
		- chuck them into a priorityqueue
		- create a new cycle from the priorityqueue (combining)

		(Off works almost the same, except durations stay as they are)
	*/
	get { |cycle|
		var child = children.first.get(cycle);
		var steps = [], prev, times = OrderedIdentitySet.new;
		var pq = PriorityQueue.new;

		[cycle, child].do { |c| c.steps.do { |step|
			times.add(step.on);
			times.add(step.on + step.dur);
		}};

		cycle.steps.do { |step|
			var copy = step.deepCopy;
			times.do { |time|
				if((time > copy.on).and(time < (copy.on + copy.dur)), {
					var copy2 = copy.deepCopy;
					copy2.dur = time - copy2.on;
					pq.put(copy2.on, \cycle -> copy2);
					copy.on = time;
					copy.dur = copy.dur - copy2.dur;
				});
			};
			if(copy.dur > 0, { pq.put(copy.on, \cycle -> copy) });
		};

		child.steps.do { |step|
			var copy = step.deepCopy;
			times.do { |time|
				if((time > copy.on).and(time < (copy.on + copy.dur)), {
					var copy2 = copy.deepCopy;
					copy2.dur = time - copy2.on;
					pq.put(copy2.on, \child -> copy2);
					copy.on = time;
					copy.dur = copy.dur - copy2.dur;
				});
			};
			if(copy.dur > 0, { pq.put(copy.on, \child -> copy) });
		};

		// use pq.pop inside a loop to combine steps into steps[]
		steps = [];
		prev = nil;
		pq.pop !? { |step|
			while { step.notNil } {
				if(prev.isNil, {
					prev = step;
					step = pq.pop;
				}, {
					if(prev.value.on == step.value.on, {
						if(prev.key == \cycle, {
							this.combine(prev.value, step.value);
							steps = steps.add(prev.value);
						}, {
							this.combine(step.value, prev.value);
							steps = steps.add(step.value);
						});
						step = pq.pop;
						prev = nil;
					}, {
						steps = steps.add(prev.value);
						prev = step;
						step = pq.pop;
					});
				});
			};
			prev !? { steps = steps.add(prev.value) };
		};

		^cycle.steps_(steps);
	}
}

JSTidyPattern : JSTidyNode {
	var seq, using;

	get { |cycle|
		var seq_cycle;

		using = using ? val;

		// watch for symbols (which should exist in global "d" dict)
		// you can share patterns through a dictionary this way
		if(val[0] == $\\, {
			var curval = thisProcess.interpreter.d.at(val.drop(1).asSymbol);
			if(using != curval, {
				using = curval;
				seq = nil; // force new instantiate
			});
		});

		seq = seq ? JSMini(using); // lazy instantiate

		cycle = JSTidyCycle.new;

		// convert JSMiniSteps to JSTidySteps
		seq_cycle = seq.next_cycle;
		seq_cycle.steps.do { |step|
			var item = JSTidyStep(step.on, step.dur);

			item.put(\string, step.string);
			item.put(\number, step.number);

			cycle.add(item);
		};

		^cycle;
	}
}

// "note 0 2 4 17" - "chord 123:1 135" // str = the chord, num = strum 0 - 9
JSTidyFP_Chord : JSTidyNode {
	*new { |pattern|
		var instance = super.new("chord");
		instance.add(JSTidyPattern(pattern));
		^instance;
	}

	get { |cycle|
		var chords = children.first.get(cycle);

		cycle.steps.do { |step|
			var chord = chords.stepAt(step.on);
			step.put(\chord, chord.at(\string) ? "0");
			step.put(\strum, (chord.at(\number) ? 0).asInteger / 20);
		}

		^cycle;
	}
}

// ~a << "jux 0.6" |> "rev" | ...
JSTidyFP_Jux : JSTidyNode {
	var <>by;

	*new { |pat|
		var instance = super.new("jux");
		var by=0.5;
		if(pat.size > 0, { by = pat.split($ ).at(0).asFloat });
		^instance.by_(max(0, min(1.0, by)));
	}

	become_cur_after_add { ^true }

	get { |cycle|
		var org, alt, steps, pq=PriorityQueue.new;

		org = children.last.get(cycle); // the cycle from the JSTidyBranch
		org.steps.do { |step| pq.put(step.on, step.put(\pan, -1 * by)) };

		// calculate shifted steps for the alt cycle
		alt = org.deepCopy;
		children.drop(-1).do { |child| alt = child.get(alt) };
		alt.steps.do { |step| pq.put(step.on, step.put(\pan, by)) };

		steps = [];
		while { pq.notEmpty } { steps = steps.add(pq.pop) };

		^cycle.steps_(steps);
	}
}

// ~a << "off 0.25" |+ "n 7" | ..
JSTidyFP_Off : JSTidyNode {
	var <>shift, <>stack;

	*new { |pat|
		var instance = super.new("off");
		^instance.shift_(max(0, min(1.0, pat.split($ ).at(0).asFloat)));
	}

	become_cur_after_add { ^true }

	get { |cycle|
		var org, alt, steps, pq=PriorityQueue.new;

		stack.do { |step|
			step.on = step.on - 1;
			pq.put(step.on, step);
		};
		stack = [];

		org = children.last.get(cycle); // the cycle from the JSTidyBranch
		org.steps.do { |step| pq.put(step.on, step) };

		// calculate shifted steps for the alt cycle
		alt = org.deepCopy;
		children.drop(-1).do { |child| alt = child.get(alt) };
		alt.steps.do { |step|
			step.on = step.on + shift;
			if(step.on > 1, {
				stack = stack.add(step);
			}, {
				pq.put(step.on, step);
			});
		};

		steps = [];
		while { pq.notEmpty } { steps = steps.add(pq.pop) };

		^cycle.steps_(steps);
	}
}

// keep \on and \dur, but reverse the dictionary!
JSTidyFP_Rev : JSTidyNode {
	*new { |pattern| ^super.newCopyArgs("rev") }

	get { |cycle|
		var dicts=[];
		cycle.steps.do { |step| dicts = dicts.add(step.dict) };
		dicts = dicts.reverse;
		cycle.steps.do { |step| step.dict_(dicts.removeAt(0)) };
		^cycle;
	}
}

// ~a << "slice 8 1 2 3 4" | ...
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

	become_cur_after_add { ^true }

	// sets \begin and \end keys and chops step into slices
	// the structure comes from the slice pattern
	// so you look up what sample to play in the input cycle
	// slice will adjust duration to play the whole slice (may overlap)
	// param splice > 0 will adjust \rate instead @see \playbuf synthdef
	get { |cycle|
		var input = children.last.get(cycle); // a branch
		cycle = children.first.get(cycle); // holds structure

		cycle.steps.do { |step|
			var slice, step2 = input.stepAt(step.on);

			slice = (step.at(\string) ? 0).asInteger; // what slice to play
			step.put(\string, nil);
			step.put(\number, nil); // could use this for bufnum..
			step2 !? { step.putAll(step2.dict) }; // bufnum, degree, def etc

			step.put(\begin, max(0, min(slice, count - 1)) / count);
			step.put(\end, max(1, min(slice + 1, count)) / count);
			step.put(\legato, 1); // in order for splice to work
		};

		^cycle;
	}
}

JSTidyFP_Every : JSTidyNode {
	var <>when, turn;

	*new { |pattern|
		var instance = super.new("every");
		^instance.when_(pattern.split($ ).at(0).asInteger);
	}

	become_cur_after_add { ^true }

	get { |cycle|
		cycle = children.last.get(cycle); // should be a JSTidyBranch

		turn = (turn ? -1) + 1;
		if((turn % when) == 0, {
			children.drop(-1).do { |child| cycle = child.get(cycle) };
		});

		^cycle;
	}
}

// ~a << "note 1 2 3 4" |> "dx7 12921"
JSTidyFP_Dx7 : JSTidyNode {
	var <>preset;

	*new { |pattern|
		var instance = super.new("dx7");
		^instance.preset_(pattern.split($ ).at(0).asInteger);
	}

	get { |cycle|
		cycle.steps.do { |step|	step.put(\dx7, preset) };
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
		if(pattern.size > 0, { instance.add(JSTidyPattern(pattern)) });
		^instance;
	}

	get { |cycle|
		// return a cycle with value from your pattern filled in for val
		cycle = children.first.get(cycle);
		cycle.steps.do { |step|
			step.at(\string) !? { |str|
				if((str.size > 1).and(str.at(0) == $~), {
					// str is like ~xxx: do nodeproxy.bus.asMap
					var proxy = currentEnvironment.at(str.drop(1).asSymbol);
					proxy.kr(1); // could be a new proxy, so initialize it
					step.put(val.asSymbol, proxy.bus.asMap);
				}, {
					case
					{ val == "def" } { step.put(\def, str.asSymbol) }
					{ val == "buf" } { step.put(\buf, str.asInteger) }
					{ val == "vel" } {
						step.put(\vel, str.asInteger.clip(0, 128))
					}
					{ val == "snd" } { step.put(\snd, str.asSymbol) }
					{ val == "note" } {
						if((str.size == 2).and("abcdefg".indexOf(str[0]).notNil), {
							str = str[0].asString ++ str[1].asInteger.asString;
							step.put(\midinote, str.notemidi);
						}, {
							step.put(\note, str.asFloat);
						});
					}
					{ val == "scale" } { step.put(\scale, str.asSymbol) }
					{ val == "speed" } {
						step.put(\speed, str.asFloat.midiratio)
					}
					//{ val == "slow" } { step.put(\slow, str.asFloat) }
					{ step.put(val.asSymbol, str.asFloat) }
				});
			};

			step.at(\number) !? { |num|
				case
				{ val == "snd"   } { step.put(\buf,  num.asInteger) }
				{ val == "note"  } { step.put(\oct,  num.asInteger) }
				{ val == "def"   } { step.put(\note, num.asInteger) }
				{ val == "scale" } { step.put(\oct,  num.asInteger) }
				{ }
			};

			step.put(\string, nil);
			step.put(\number, nil);
		};
		^cycle;
	}
}


/*
	<< "juxby 0.5 (rev)" | bla bla
	jux changes the right channel
	plays original cycle panned left 0.5
	plays reversed cycle panned right 0.5
	juxby 0 would play them both in the center
	juxby 1 is the default ("juxby" or "jux")

	<< "off 0.25, rev" | bla bla
	plays the original cycle
	plays the reversed cycle on top, but 1/4 cycle later in time

	quark : "Tidy" : Tidal Cycles syntax for SuperCollider

	inline effects: how to do it: reserve some slots maybe?
*/

/*
	When evaluated in the interpreter, the "<<" operator for
	NodeProxy creates a new JSTidy object around the NodeProxy.
	As the interpreter works from left to right, the next
	operator will then be handled by that JSTidy object.
	This goes on and on as more operators are encountered, and
	this way, a tree of objects is built inside the JSTidy object.
	The last "operator" is the "printOn" message, that is called
	by the interpreter when all the code has been interpreted.
	It should return a string, to be displayed in the post window.
	During the printOn method, a new Task is started, that will
	generate and run JSTidyCycle's (bars) of JSTidyStep's, that will
	launch synths on the server.
	The Task is set as the source for the NodeProxy.
	If new code is evaluated for the NodeProxy, the Task will be
	replaced by a new one. The old one will be stopped by the
	NodeProxy.

	So, you can use JSTidy if you use the "<<" operator on a NodeProxy.
	You can still use NodeProxy the normal way.
*/

/////////////////////////////////////////////////////
// HOOKS
/////////////////////////////////////////////////////

+ NodeProxy {

	// the < operator + a function string results in a JSTidy object
	// the interpreter will move on to the next operator
	// invoking it on the JSTidy object that was just created.
	< { |input|
		
		this.ar(2); // make sure to allocate the audio bus
		
		if(input.class == String, {
			^JSTidy(this).add_branch("<<").add_func(input)
		});

		// assume array: create JSTidy object + proxy for each element
		input.postln;
	}

	fx { |func_or_symbol, extra_args|
		// avoid conflict with send slotnumbers, which are bus index + 10
		var slot = Server.default.options.numAudioBusChannels + 10;
		this.ar(2);
		this.fadeTime_(4);
		if(func_or_symbol.isFunction, {
			this.put(slot, \filterIn -> func_or_symbol);
		}, {
			// assume synthdef using ReplaceOut
			this.put(slot, func_or_symbol.asSymbol, 0, extra_args);
		});

		// add your key in the currentEnvironment to JSTidy.fxnames
		// JSTidyStep.play needs fxnames to install sends to them
		JSTidy.fxnames = (JSTidy.fxnames ?? []).add(
			currentEnvironment.findKeyForValue(this)
		);
	}

	> { |str| JSTidy.send(this, str)	}
	
	hush { JSTidy.hush(bus.index) }
}

+ ProxySpace {

	bpm { |bpm|
		if(clock.isNil, {	this.makeTempoClock });
		clock.schedAbs(clock.nextBar, { clock.tempo_(bpm / 60) });
	}

	bpb { |bpb|
		if(clock.isNil, {	this.makeTempoClock });
		clock.schedAbs(clock.nextBar, {
			clock.beatsPerBar_(bpb);
			this.quant_(bpb);
		});
	}

	config { |bpm, bpb|
		if(clock.isNil, {	this.makeTempoClock });
		clock.schedAbs(clock.nextBar, {
			clock.tempo_(bpm / 60);
			clock.beatsPerBar_(bpb);
			this.quant_(bpb);
		});
	}

	hush { |fade=8|
		Routine {
			this.stop(fade);
			fade.wait;
			JSTidy.stop;
			1.wait;
			this.clear.pop
		} .play
	}
}
