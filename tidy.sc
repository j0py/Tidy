/*
	- give JSTidyCycle a dictionary too; functions may set things
      in it, which could influence playing steps, or the routine

    - "ser 2 3" | <sequence 1> | <sequence 2> | <default stuff>
      this would play:
      <sequence 1> | <default stuff> 2 times
      <sequence 2> | <default stuff> 3 times
      <sequence 1> | <default stuff> 2 times
      etc

    - "one 2 3" | <sequence 1> | <sequence 2> | <default stuff>
      this would play:
      <sequence 1> | <default stuff> 2 times
      <sequence 2> | <default stuff> 3 times
	  and then the routine would stop

    - "one" | <some stuff> would play <some stuff> once

    - "par" | <sequence 1> | <sequence 2> | <some stuff>
      finds out the last branch by itself (<some stuff>)
      plays the branches before it in parallell as sub-routines
      of the main routine. or you could just combine the cycles
      coming out of <sequence 1> | <some stuff> with the cycles that
      come out of <sequence 2> | <some stuff> in 1 routine. i want it
      to be possible that sequence 1 and 2 have different lengths though.

    - check differende between slice and splice. i remember that splice
      makes sure that the sample always fits exactly by adusting rate.

	- "rot" function: rotate the cycle left or right for certain number
	of steps, given by a pattern (0 = no rotation).

	- "slice"/"splice" : note:slice_number. rate = (note - 60).midiratio

	- "life" : bring in small random variations (wow:flutter)

	- use Shaper.ar to create distortion effects
*/

JSTidy : JSTidyTree {
	classvar loglevel, routines;

	var <>proxy, <>count;

	*new { |proxy| ^super.new.proxy_(proxy) }

	*show { |count| ^super.new.count_(count) }

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
	
	printOn { |stream|
		// make sure that the routines Order exists
		routines ?? { routines = Order.new };

		// if tree is nil then something has gone wrong during the
		// creation of the new tree. return right here, so that the old
		// tree keeps running.
		tree ?? { "tree nil".postln; ^this };
		
		if(JSTidy.should_log(\tree)) { tree.log };

		// with count, you can log some cycles instead of playing them
		count !? {
			if(JSTidy.should_log(\cycle)) {
				(count - 1).do { tree.get(JSTidyCycle.new) };
				tree.get(JSTidyCycle.new).postln.class.postln;
			};
		};

		// if you have a proxy and got here, then we can play
		proxy !? {
			var slot, name;
			proxy.ar(2); // make sure we have a bus
			name = currentEnvironment.findKeyForValue(proxy);
			slot = proxy.bus.index; // we need the index
			proxy.printOn(stream);  // output to postwindow
			
			Routine({
				// always create a fresh new routine on re-evaluation
				routines.at(slot) !? { |r| r.stop };
				routines.put(slot, Routine({
					loop({
						var cycle = tree.get(JSTidyCycle.new, name);
						if(JSTidy.should_log(\cycle), { cycle.postln });
						cycle.steps.do { |step|
							var slow;
							if(step.trig > 0) { step.play(proxy) };
							slow = (step.at(\slow) ? "1").asFloat;
							(step.delta * slow).wait;
						};
					});
				}).play(proxy.clock));
			}).play(proxy.clock, proxy.quant); // quant: wait for beat
		}
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
}

JSTidyTree {
	var <tree, cur;
	
	// returns a JSTidyXX function. str format: "<function name> <pattern>"
	// a JSTidyXX function takes a cycle, maybe alters it, and returns it.
	func { |str|
		var func, pat, class;

		str = str.split($ );
		func = str.removeAt(0);
		pat = str.join($ ).stripWhiteSpace;

		if(func[0] == $~) {
			^JSTidySend(func.drop(1).toLower, pat)
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
	var <>trig, <>delta, <>dur, <>dict, <>sends;

	*new { |trig, delta, dur, str, num|
		^super.newCopyArgs(trig ? 0, delta ? 1, dur ? 1)
		.dict_(Dictionary.new)
		.sends_(Dictionary.new)
		.put(\str, str)
		.put(\num, num);
	}
	
	putAll { |argdict| dict.putAll(argdict) }
	put { |key, value| dict.put(key.asSymbol, value) }
	at { |key| ^dict.at(key.asSymbol) }

	send { |to, gain| sends.put(to.asSymbol, gain.asFloat) }
	
	play { |proxy|
		var instr, def, sustain, dx7, rootfreq;
		var scale = Scale.at((dict.at(\scale) ? \major).asSymbol);

		// control proxy intercept
        if(proxy.rate == \control) {
			dict.at(\val) !? { |val|
				Server.default.setControlBusValue(proxy.bus.index, val);
				//"set bus % val %".format(proxy.bus.index, val).postln;
			};
			^this;
		};

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
		dict.put(\secs, sustain / proxy.clock.tempo);   // in seconds

		dict.put(\out, proxy.bus.index); // the main output

		sends.keys.do { |sendname, i|
			var gain = sends.at(sendname);
			var bus = currentEnvironment.at(sendname.asSymbol).bus;
			dict.put(("send"++((i+1).asString)).asSymbol, bus.index);
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
		//sends.keysValuesDo { |k,v| stream << "~%:%,".format(k,v) };
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

JSTidySend : JSTidyNode {
	*new { |fx, pattern|
		^super.new(fx).add(JSTidyPattern(pattern))
	}

	get { |cycle, name|
		var time, gains = children.first.get(cycle, name);
		var sendname = (name ++ "_" ++ val).asSymbol;

		// create a separate proxy to send signal to the fx for this proxy
		// do not call xset too often: will leave a trail of synths
		currentEnvironment.at(sendname).to(val, 1.0, \set);

		// store the sends in the step objects by calling send() method.
		// during step.play, the sends are added to the dict of the step.
		// and that way the values will be fed to the synth on the server.
		time = 0;
		cycle.steps.do { |step|
			step.send(sendname, gains.at(time).at(\str).asFloat);
			time = time + step.delta;
		};
		^cycle;
	}
}

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
JSTidyFP_Rev : JSTidyNode {
	*new { |pattern| ^super.new("rev") }

	get { |cycle, name|
		var dicts = cycle.steps.collect { |step| step.dict };
		dicts = (dicts ? []).reverse;
		cycle.steps.do { |step| step.dict_(dicts.removeAt(0)) };
		^cycle;
	}
}

// ~a < "slice 8 1 2 3 4" | ...
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
			step2 !? { step.putAll(step2.dict) }; // bufnum,degree,def etc

			step.put(\begin, max(0, min(slice, count - 1)) / count);
			step.put(\end, max(1, min(slice + 1, count)) / count);
			step.put(\legato, 1); // in order for splice to work

			time = time + step.delta;
		};

		^cycle;
	}
}

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
				if((str.size > 1).and(str.at(0) == $~), {
					// str is like ~xxx: do nodeproxy.bus.asMap
					var proxy;
					proxy = currentEnvironment.at(str.drop(1).asSymbol);
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
						if("abcdefg".contains(str[0]), {
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
				});
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


/*
	When evaluated in the interpreter, the "<" operator for
	NodeProxy creates a new JSTidyTree object around the NodeProxy.
	As the interpreter works from left to right, the next
	operator will then be handled by that JSTidyTree object.
    The parameter for that operator is a string.
    JSTidyTree executes the operator with the string parameter,
    adds something to the tree that is built inside itself,
    and returns itself.

    Then, if there is another operator + string argument, the
    process repeats.

	This goes on and on as more operators + strings are encountered, and
	this way, a tree of objects is built inside the JSTidyTree object.

	The last "operator" is the "printOn" message, that is called
	by the interpreter when all the code has been interpreted.
	It should return a string, to be displayed in the post window.

	During the printOn method, a new Routine is started, that will
	generate and run JSTidyCycle's (bars) of JSTidyStep's, that will
	launch synths on the server. The synths will receive the bus index
    of the proxy as \out parameter (and also indexes of fx buses).

	JSTidy extends JSTidyTree.
*/

/////////////////////////////////////////////////////
// HOOKS
/////////////////////////////////////////////////////

+ NodeProxy {

	< { |input|
		
		this.ar(2); // make sure to allocate the audio bus
		
		if(input.class == String, {
			^JSTidy(this).add_branch("<<").add_func(input)
		});

		input.postln; // error
	}

	// << will result in a control proxy
	<< { |input|
		
		this.kr(1); // make sure to allocate the control bus
		
		if(input.class == String, {
			^JSTidy(this).add_branch("<<").add_func(input)
		});

		input.postln;
	}

	fx { |func_or_symbol, extra_args|
		// avoid conflict with send slotnumbers, which are bus index + 10
		var slot = Server.default.options.numAudioBusChannels + 10;
		this.ar(2);
		
		if(func_or_symbol.isFunction, {
			this.put(slot, \filterIn -> func_or_symbol);
		}, {
			this.put(slot, func_or_symbol.asSymbol, 0, extra_args);
		});
	}

	to { |str, gain, how|
		var to, slot;

		this.ar(2); // make sure we have a bus index

		slot = this.bus.index + 10;

		if(str[0] == $~) { str = str.drop(1) }; // the ~ is optional
		to = currentEnvironment.at(str.asSymbol); // might create proxy
		if(to.objects.at(slot).isNil, {
			to.put(slot, \mix -> { this.ar });
		});

		// avoid calling xset lots of times, because this will result
		// in lots of synths hanging around waiting to be destroyed
		if((how ? \xset) == \xset) {
			to.fadeTime_(2);
			to.xset(("mix"++(slot)).asSymbol, gain.asFloat);
		} {
			to.set(("mix"++(slot)).asSymbol, gain.asFloat);
		};
	}
	
	> { |str|
		str.split($ ).clump(2).do { |pair|
			this.to(pair[0], pair[1].asFloat, \xset);
		}
	}

	hush { |fade=0|
		if(fade < 0.02) {
			JSTidy.hush(bus.index);
		} {
			Routine({
				// fade out the audio on the nodeproxy bus, using \filter
				// method with a Line. add 11 to the max slot number so
				// that this will also work for fx proxies,
				// who have the fx on slot number max + 10.
				var slot = Server.default.options.numAudioBusChannels + 11;
				this.put(
					slot,
					\filter -> { |in| Line.ar(1,0,fade,in,0,2) }
				);
				// wait for the fadeout to be almost done
				(fade - 0.01).wait;
				// stop the routine from triggering new steps
				JSTidy.hush(bus.index);
				// remove the fadeout so that proxy is ready for use again
				this.put(slot, nil);
			}).play
		}
	}
}

+ Symbol {
	// needed for Seq and Stack: inside an array, the Interpreter must
	// first encounter a Symbol ad operator "--" and a String parameter.
	// That will result in a new JSTidyTree object.
	-- { |str| ^JSTidyTree.new.add_branch("--").add_func(str) }
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
			this.clear.pop;
			"hushed".postln;
		} .play
	}
}
