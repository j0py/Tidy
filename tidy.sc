// TODO: tidy should declare playbuf_stereo/mono by itself

JSTidy {
	classvar loglevel;

	var <>name, <tree, cur;

	*new { |name| ^super.new.name_(name) }
	
	*should_log { |level|
		loglevel ?? { loglevel = [] };
		^(loglevel.indexOf(level).notNil);
	}

	*log { |level|
		loglevel ?? { loglevel = [] };
		level ?? { loglevel = [] } !? { loglevel = loglevel.add(level) };
	}

	*end { |seconds=1|
		// TODO: end all routines
		// Library.at(\tidyar, name, \routine) !? { |r| r.stop };
	}
	
	*hush { |name, seconds=1|
		Routine({
			var beats = seconds / thisThread.clock.tempo;
			Library.put(\tidyar, name, \fade, beats);
			beats.wait;
			Library.at(\tidyar, name, \routine) !? { |routine|
				Library.put(\tidyar, name, \routine, nil);
				routine.stop
			};
		}).play;
	}

	*tempo { |tempo|
		TempoClock.tempo_(tempo);
		("\\tempo -- { DC.kr(" ++ tempo ++ ") }").interpret;
	}

	*quant { |quant| Library.put(\tidy, \quant, quant.asInteger) }
	
	printOn { |stream|
		// if tree is nil then something has gone wrong during the
		// creation of the new tree. return right here, so that the old
		// tree keeps running.
		tree ?? { "tree nil".postln; ^this };

		if(JSTidy.should_log(\tree)) { tree.log };

		name.printOn(stream);  // output to postwindow

		// make sure that only 1 routine will replace the running
		// routine at the next quantisation point. one could
		// evaluate some code twice within one quantisation period!
		Library.at(\tidyar, name, \evaluated) ?? { 

			// a logical cycle lasts 1 beat in my system, quant is in beats
			Library.put(\tidyar, name, \evaluated, Routine({
				var quant, nudge=0.001, now=thisThread.beats, wait;

				quant = (Library.at(\tidy, \quant) ? 4).asInteger;

				// stop the old routine <nudge> before starting the new
				// nudge=0 : old routine triggers one more note, while the
				// new routine also triggers -> double notes!
				wait = (now + quant).div(quant) * quant - now;
				if(wait < nudge) { wait = wait + quant };
				
				(wait - nudge).wait;
				Library.at(\tidyar, name, \routine) !? { |r| r.stop };

				nudge.wait;
				Library.put(\tidyar, name, \routine, Routine({
					var fading=false, faded, fadebeats, gain=1;

					// enable re-evaluation
					Library.put(\tidyar, name, \evaluated, nil);
					
					loop({
						var cycle = tree.get(JSTidyCycle.new, name);
						
						if(JSTidy.should_log(\cycle), { cycle.postln });

						Library.at(\tidyar, name, \fade) !? { |beats|
							Library.put(\tidyar, name, \fade, nil);
							fading = true;
							faded = 0;
							fadebeats = beats;
						};

						cycle.steps.do({ |step|
							var slow;
							if(fading) {
								gain = faded.linexp(0,fadebeats,1,0.001);
							};
							
							if(step.trig > 0) { step.play(gain) };
							slow = (step.at(\slow) ? "1").asFloat;
							(step.delta * slow).wait;

							if(fading) {
								faded = faded + (step.delta * slow)
							};
						});
					});
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
		array.do { |tidytree| cur.add(tidytree.tree) };
		cur = cur.parent;
	}
	
	// returns a JSTidyXX function. str format: "<function name> <pattern>"
	// a JSTidyXX function takes a cycle, maybe alters it, and returns it.
	func { |str|
		var func, pat, class;

		str = str.split($ );
		func = str.removeAt(0);
		pat = str.join($ ).stripWhiteSpace;

		if(func[0] == $=) {
			func = func.drop(1);
			if(Library.at(\tidyar, func.asSymbol, \bus).notNil) {
				^JSTidyOut(func, pat)
			}
		};

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
		if(obj.become_cur_after_add) { cur = obj };
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
		steps !? { steps.do { |step| step.printOn(stream) } };
	}
}

JSTidyStep {
	var <>trig, <>delta, <>dur, <>dict, <>outs;

	*new { |trig, delta, dur, str, num|
		^super.newCopyArgs(trig ? 0, delta ? 1, dur ? 1)
		.dict_(Dictionary.new)
		.outs_(Dictionary.new)
		.put(\str, str)
		.put(\num, num);
	}

	putAll { |argdict| dict.putAll(argdict) }
	put { |key, value| dict.put(key.asSymbol, value) }
	at { |key| ^dict.at(key.asSymbol) }

	out { |to, gain|
		//"step.out(%, %)".format(to, gain).postln;
		outs.put(to.asSymbol, gain.asFloat)
	}

	play { |fader=1|
		var instr, def, sustain, dx7, rootfreq;
		var scale = Scale.at((dict.at(\scale) ? \major).asSymbol);
		
		// it will be audio
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
		};

		this.at(\def) !? { |def|
			this.put(\instrument, def.asSymbol);
			this.put(\def, nil);
		};

		this.at(\note) !? { |note|
			// if > 22 then it is a midinote, right?
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
			var steps = 12;
			var mtranspose = dict.at(\mtranspose) ? 0;
			var degree = dict.at(\degree) ? 0;
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

		dict.at(\legato) ?? { dict.put(\legato, 0.8) };
		sustain = dict.at(\legato) * dur
		* (dict.at(\slow) ? 1)
		/ (dict.at(\fast) ? 1);
		dict.put(\secs, sustain / thisThread.clock.tempo);   // in seconds

		outs.keys.do { |name, i|
			var gain = outs.at(name) * fader;
			var bus_index = Library.at(\tidyar, name, \bus).index;
			//"step.play out % %".format(bus_index, gain).postln;
			dict.put(("out"++((i+1).asString)).asSymbol, bus_index);
			dict.put(("gain"++((i+1).asString)).asSymbol, gain.asFloat);
		};

		if(JSTidy.should_log(\step), { this.postln });

		//                         degree, root, octave
		//Scale.major.degreeToFreq(2, 60.midicps, 1);
		rootfreq = dict.at(\freq);

		Routine({
			var synths, strum;

			strum = dict.at(\strum) ? 0;
			synths = List.new;

			(dict.at(\chord) ? "0").do { |ch, i|
				ch = ch.asInteger - $0.asInteger;
				dict.put(\freq, scale.degreeToFreq(ch, rootfreq, 0));

				dx7 !? {
					dict.putAll(
						dx7.params(dict.at(\freq), dict.atFail(\vel, 64))
					)
				};

				Server.default.bind {
					synths.add(Synth(def.name, dict.asPairs));
				};

				(strum * i * 0.05).wait;
			};

			if(def.hasGate, {
				sustain.wait;

				synths.do { |synth, i|
					Server.default.bind { synth.set(\gate, 0) };
					(strum * i * 0.05).wait;
				};
			});
		}).play;
	}

	printOn { |stream|
		stream << "step % % % ".format(
			trig,
			delta.round(0.01).asString.padLeft(6),
			dur.round(0.01).asString.padLeft(6)
		);
		dict.keysValuesDo { |k,v| stream << "%:%,".format(k,v) };
		//outs.keysValuesDo { |k,v| stream << "~%:%,".format(k,v) };
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

	add { |child| children = children.add(child) }

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

JSTidyComb : JSTidyNode {
	// combine 2 steps, third param is where structure should come from.
	combine { |step, stepAt, right|
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

JSTidyCombLeft : JSTidyComb {
	get { |cycle, name|
		var time, child = children.first.get(cycle, name);

		time = 0;
		cycle.steps.do { |step|
			this.combine(step, child.at(time));
			time = time + step.delta;
		};

		^cycle;
	}
}

JSTidyCombRight : JSTidyComb {
	get { |cycle, name|
		var time, child = children.first.get(cycle, name);

		time = 0;
		child.steps.do { |step|
			this.combine(step, cycle.at(time), \right);
			time = time + step.delta;
		};

		^child;
	}
}

JSTidyCombBoth : JSTidyComb {
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
}

JSTidyPattern : JSTidyNode {
	var seq;

	get { |cycle, name|
		seq = seq ? JSMiniParser(val).parse; // lazy instantiate
		^JSTidyCycle(seq.next_cycle);
	}
}

JSTidyOut : JSTidyNode {
	*new { |fx, pattern|
		^super.new(fx).add(JSTidyPattern(pattern))
	}

	get { |cycle, name|
		var time, gains = children.first.get(cycle, name);
		
		time = 0;
		cycle.steps.do { |step|
			step.out(val, gains.at(time).at(\str).asFloat);
			time = time + step.delta;
		};
		^cycle;
	}
}

// play sub-sequences
JSTidyFP_Seq : JSTidyNode {
	var seq; // a queue of steps

	*new { |pattern| ^super.new("seq").add(JSTidyPattern(pattern)) }

	get { |cycle, name|
		var index;

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
		^children.at(index).get(cycle, name);
	}

	become_cur_after_add { ^true }
}

// "note 0 2 4" - "chord 123:1 135" // str = the chord, num = strum 0 - 9
// TODO: put chord in mini-notation too: <0,2,4> strum?
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

// reverse the dictionary
// TODO: i think you can also reverse the steps now
JSTidyFP_Rev : JSTidyNode {
	*new { |pattern| ^super.new("rev") }

	get { |cycle, name|
		var dicts = cycle.steps.collect { |step| step.dict };
		dicts = (dicts ? []).reverse;
		cycle.steps.do { |step| step.dict_(dicts.removeAt(0)) };
		^cycle;
	}
}

// ~a < "slice 8 1 2 3 4" | "splice 1" - ...
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
	get { |cycle, name|
		var time, input = children.last.get(cycle, name); // branch
		cycle = children.first.get(cycle, name); // holds structure

		time = 0;
		cycle.steps.do { |step|
			var slice, step2 = input.at(time);

			slice = (step.at(\str) ? 0).asInteger; // what slice to play
			step.put(\str, nil);
			step.put(\num, nil); // could use this for bufnum..
			step2 !? {
				step.putAll(step2.dict); // bufnum,degree,def etc
				step.outs = step2.outs; // what bus(es) to play to
			};

			step.put(\begin, max(0, min(slice, count - 1)) / count);
			step.put(\end, max(1, min(slice + 1, count)) / count);
			step.put(\legato, 1); // in order for splice to work

			time = time + step.delta;
		};

		^cycle;
	}
}

// do something extra every nth cycle
JSTidyFP_Every : JSTidyNode {
	var <>when, turn;

	*new { |pattern|
		^super.new("every").when_(pattern.split($ ).at(0).asInteger);
	}

	become_cur_after_add { ^true }

	get { |cycle, name|
		cycle = children.last.get(cycle, name); // should be a JSTidyBranch

		// let your children alter the cycle when it is your turn
		turn = (turn ? -1) + 1;
		if((turn % when) == 0, {
			children.drop(-1).do { |child|
				cycle = child.get(cycle, name)
			};
		});

		^cycle;
	}
}

// ~a << "note 1 2 3 4" |> "dx7 12921"
// needs DX7.init. plays a dx7 preset
// TODO: should not be intertwined with Tidy!
//
// the DX7 class is based on:
// --------------------------
// Supercollider DX7 Clone v1.0
// Implemented by Aziz Ege Gonul for more info go to www.egegonul.com
// Under GNU GPL 3 as per SuperCollider license
//
JSTidyFP_Dx7 : JSTidyNode {
	var <>preset;

	*new { |pattern|
		^super.new("dx7").preset_(pattern.split($ ).at(0).asInteger);
	}

	get { |cycle, name|
		cycle.steps.do { |step|	step.put(\dx7, preset) };
		^cycle;
	}
}

// catch-all function: the function name becomes the "thing" that you set
// in the step dictionary. it will become a direct parameter to the
// synthdef as the step gets played.
//
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
					{ val == "def" } { step.put(\def, str.asSymbol) }
					{ val == "buf" } { step.put(\buf, str.asInteger) }
					{ val == "vel" } { step.put(\vel, str.asFloat) }
					{ val == "snd" } { step.put(\snd, str.asSymbol) }
					{ val == "note" } {
						if("abcdefg".contains(str[0]), {
							// TODO: SuperCollider supports "a", "as", "ab". use that.
							// @see helpbrowser "Literals"
							step.put(\midinote, str.notemidi);
						}, {
							step.put(\note, str.asFloat);
						});
					}
					{ val == "scale" } { step.put(\scale, str.asSymbol) }
					{ val == "speed" } {
						step.put(\speed, str.asFloat.midiratio)
					}
					{ step.put(val.asSymbol, str.asFloat) };

					if(str == "~") { step.trig = 0 };
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

+ Symbol {
	-- { |in|
		var bus, node;

		case
		{ in.isFunction }
		{
			// control bus
			Routine({
				(bus = Library.at(\tidykr, this, \bus)) ?? {
					bus = Bus.control(Server.default, 1);
					Library.put(\tidykr, this, \bus, bus);
				};

				Library.at(\tidykr, this, \synth) !? { |node| node.free };

				Server.default.sync;

				node = in.play(nil, bus.index);
				Library.put(\tidykr, this, \synth, node);
			}).play;
		}
		{ in.isString }
		{
			// sequencer
			^JSTidy(this).add_branch("--").add_func(in)
		}
		{ in.isArray }
		{
			// stereo fx bus
			var out, gain, def, args;
			# out, gain, def, args = in;

			// make "=sin" syntax possible
			args = args.collect { |el, i|
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
			};
			
			Routine({
				(bus = Library.at(\tidyar, this, \bus)) ?? {
					bus = Bus.audio(Server.default, 2);
					Library.put(\tidyar, this, \bus, bus);
				};

				Server.default.sync;
				
				(node = Library.at(\tidyar, this, \synth)) ?? {
					out = Library.at(\tidyar, out.asSymbol, \bus);
					if(out.isNil) { out = 0 } { out = out.index };
					node = Synth(def.asSymbol, args ++ [
						\in, bus.index,
						\out, out,
					]);
					Library.put(\tidyar, this, \synth, node);
				};

				Server.default.sync;
				
				node.set(\gain, gain.asFloat.clip(0, 1));
				args !? { args.clump(2).do { |k| node.set(k[0], k[1]) } };
			}).play;
		}
		{
			"strange input %".format(in.class).postln;
		}
	}

	hush { |seconds=1| JSTidy.hush(this, seconds) }

	// control value, use in a UGen Function
	bus { ^Library.at(\tidykr, this, \bus) }
}

/*
	some ideas:

    \a -- "once" | <some stuff> would play <some stuff> once

    \a -- "stack" [
      \ -- <sequence 1>,
      \ -- <sequence 2>,
    ] - <other functions>

	- "rot" function: rotate the cycle left or right for certain number
	of steps, given by a pattern (0 = no rotation).

	- "slice"/"splice" : note:slice_number. rate = (note - 60).midiratio

	- "life" : bring in small random variations (wow:flutter)

	- use Shaper.ar to create distortion effects
*/

