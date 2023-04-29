/////////////////////////////////////////////////////
// PATTERN PARSER (mini notation)
/////////////////////////////////////////////////////

JSMini {
	var root, index, str, cycle, queue, time=0.0;

	*new { |spec| ^super.new.init(spec) }

	init { |spec| 
		root = JSMiniRoot.new;
		str = spec.asString;
		this.parse(root);
		^this;
	}

	next_cycle {
		var steps, dur, i, cont, newsteps;

		queue = (queue ? []); // queue of cycles

		while { queue.size < 1 } { queue = queue.add(root.get_steps) };

		steps = queue.removeAt(0);

		if(steps.size <= 0, { ^JSMiniCycle(cycle, List.new) });

		// calculate total \dur of following \space steps
		i = 0;
		dur = 0;
		while { queue.size < (i + 1) } { queue = queue.add(root.get_steps) };
		cont = (queue[i].size > 0);
		while { cont } {
			queue[i].do { |step|
				case
				{ cont.not } {}
				{ step.type == \space }
				{ dur = dur + step.dur; step.type = \rest; }
				{ cont = false };
			};
			if(cont, {
				i = i + 1;
				while { queue.size < (i + 1) } { queue = queue.add(root.get_steps) };
				cont = (queue[i].size > 0);
			});
		};

		newsteps = List.new;
		steps.reverseDo { |step|
			case
			{ step.type == \space }
			{ dur = dur + step.dur }
			{ step.type == \note }
			{ step.dur = step.dur + dur; dur = 0; newsteps.add(step) }
			{ dur = 0 };
		};

		^JSMiniCycle(cycle, newsteps.reverse);
	}

	get { ^root.get_steps }

	logroot { root.post }

	log { |number_of_cycles = 1|
		root.post;
		number_of_cycles.do { |i|
			"cycle %".format(i).postln;
			"on     dur    str num".postln;
			root.get_steps.do { |step| "%".format(step).postln };
		}
	}

	parse { |current|
		var node, node2, parsing=\string;

		index = (index ? 0);
		while
		{ index < str.size } {
			var ch = str.at(index);
			index = index + 1;

			case
			{ ch == $  } {
				if(node.notNil, { current.addChild(node); node = nil });
				parsing = \string;
			}
			{ ch == $~ } {
				if((index < str.size).and(str.at(index) != $ ), {
					// parse nodeproxy into the string value
					if(node.isNil, { node = JSMiniNote.new });
					node.string_((node.string ? "") ++ ch);
					parsing = \string;
				}, {
					node = JSMiniRest.new;
					parsing = \rest;
				})
			}
			{ ch == $_ } {
				node = JSMiniSpace.new;
				parsing = \space;
			}
			{ ch == $< } {
				node = JSMiniTurns.new;
				this.parse(node);
				parsing = \turns;
			}
			{ ch == $> } {
				if(node.notNil, { current.addChild(node); node = nil });
				^this;
			}

			{ ch == $[ } {
				node = JSMiniNested.new;
				this.parse(node);
				parsing = \nested;
			}
			{ ch == $] } {
				if(node.notNil, { current.addChild(node); node = nil });
				^this;
			}

			{ ch == ${ } {
				node = JSMiniPoly.new;
				this.parse(node);
				parsing = \poly;
			}
			{ ch == $} } {
				if(node.notNil, { current.addChild(node); node = nil });
				^this;
			}

			// euclydian syntax (3,8,2)
			{ ch == $( } { parsing = \euclid_x }
			{ ch == $) } { parsing = \none }

			{ ch == $, } {
				case
				{ parsing == \euclid_x } { parsing = \euclid_y }
				{ parsing == \euclid_y } { parsing = \euclid_r }
				{ }
			}
			{ ch == $* } { parsing = \faster }
			{ ch == $/ } { parsing = \slower }
			{ ch == $: } { parsing = \number }
			{ ch == $! } { parsing = \repeat }
			{ ch == $% } { parsing = \division }
			{ ch == $- } {
				if(node.isNil, {
					// a negative number is starting..
					node = JSMiniNote.new;
					node.string_((node.string ? "") ++ ch);
				});
			}

			{ parsing == \string } {
				if(node.isNil, { node = JSMiniNote.new });
				node.string_((node.string ? "") ++ ch);
			}
			{ parsing == \number } {
				if(node.isNil, { node = JSMiniNote.new });
				node.number_((node.number ? "") ++ ch);
			}
			{ parsing == \faster } {
				if(node.notNil, {
					node.faster_((node.faster ? "") ++ ch)
				});
			}
			{ parsing == \slower } {
				if(node.notNil, {
					node.slower_((node.slower ? "") ++ ch)
				});
			}
			{ parsing == \repeat } {
				if(node.notNil, {
					node.repeat_((node.repeat ? "") ++ ch)
				});
			}
			{ parsing == \division } {
				if(node.notNil, {
					node.division_((node.division ? "") ++ ch)
				});
			}
			{ parsing == \euclid_x } {
				if(node.notNil, {
					node.euclid_x = ((node.euclid_x ? "") ++ ch)
				});
			}
			{ parsing == \euclid_y } {
				if(node.notNil, {
					node.euclid_y = ((node.euclid_y ? "") ++ ch)
				});
			}
			{ parsing == \euclid_r } {
				if(node.notNil, {
					node.euclid_r = ((node.euclid_r ? "") ++ ch)
				});
			}
			{ }; // default no action
		};

		if(node.notNil, {	current.addChild(node) });

		^this;
	}
}

JSMiniCycle {
	var <>cycle, <>steps;

	*new { |cycle, steps| ^super.newCopyArgs(cycle, steps) }

	printOn { |stream|
		stream << "cycle %\n".format(cycle);
		stream << "  on    dur   str   num";
		steps.do { |step|
			stream << "  % % % %\n".format(
				step.on.asString.padRight(6),
				step.dur.asString.padRight(6),
				step.string.asString.padRight(6),
				step.number
			);
		}
	}
}

JSMiniRoot : JSMiniNested { }

JSMiniNote : JSMiniNode {
	var pattern;

	type { ^\note }

	get_pattern {
		pattern ?? {
			var steps = [ JSMiniStep(0, 1, string, number, this.type) ];

			// use faster/slower/euclid to calculate the repeating steps
			steps = this.get_euclid_steps(steps);
			steps.do { |step|
				step.dur = step.dur / ((faster ? "1").asFloat);
				step.dur = step.dur * ((slower ? "1").asFloat);
				step.on = step.on / ((faster ? "1").asFloat);
				step.on = step.on * ((slower ? "1").asFloat);
			};
			pattern = steps;
		};
		^pattern;
	}
}

JSMiniRest : JSMiniNote { type { ^\rest } }
JSMiniSpace : JSMiniNote { type { ^\space } }

JSMiniNested : JSMiniNode {
	get_pattern {
		var steps=[];

		children.do { |child, i| 
			steps = steps.addAll(
				child.get_steps.collect { |step|
					step.dur = step.dur / children.size; // scale
					step.on = step.on / children.size; // scale
					step.on = step.on + (i / children.size); // offset
				}
			)
		};

		// use faster/slower/euclid to calculate the repeating steps
		steps = this.get_euclid_steps(steps);
		steps.do { |step|
			step.dur = step.dur / ((faster ? "1").asFloat);
			step.dur = step.dur * ((slower ? "1").asFloat);
			step.on = step.on / ((faster ? "1").asFloat);
			step.on = step.on * ((slower ? "1").asFloat);
		};
		^steps;
	}
}

JSMiniTurns : JSMiniNode {
	get_pattern {
		var steps=[];

		turn = (turn ? 0);
		steps = steps.addAll(children.wrapAt(turn).get_steps);

		// use faster/slower/euclid to calculate the repeating steps
		steps = this.get_euclid_steps(steps);
		steps.do { |step|
			step.dur = step.dur / ((faster ? "1").asFloat);
			step.dur = step.dur * ((slower ? "1").asFloat);
			step.on = step.on / ((faster ? "1").asFloat);
			step.on = step.on * ((slower ? "1").asFloat);
		};
		^steps;
	}
}

JSMiniPoly : JSMiniNode {
	var <>division;

	get_pattern {
		var count, steps=[];

		count = children.size;
		division !? { count = division.asInteger };

		count.do { |i| 
			steps = steps.addAll(
				children.wrapAt(i).get_steps.collect { |step|
					step.dur = step.dur / count;
					step.on = step.on / count;
					step.on = step.on + (i / count);
				}
			)
		};

		// use faster/slower/euclid to calculate the repeating steps
		steps = this.get_euclid_steps(steps);
		steps.do { |step|
			step.dur = step.dur / ((faster ? "1").asFloat);
			step.dur = step.dur * ((slower ? "1").asFloat);
			step.on = step.on / ((faster ? "1").asFloat);
			step.on = step.on * ((slower ? "1").asFloat);
		};
		^steps;
	}
}

JSMiniNode {
	var <>children;
	var <>string, <>number, <>faster, <>slower, <>repeat;
	var <>euclid_x, <>euclid_y, <>euclid_r;
	var <>turn;

	// deliberately NOT cloning "repeat" !!
	deepCopy { ^super.deepCopy.repeat_(nil) }

	addChild { |node|
		children = children.add(node);

		((node.repeat ? 1).asInteger - 1).do {
			children = children.add(node.deepCopy)
		};

		^node;
	}

	post { |indent=""|
		(indent ++ this.log).postln;
		children.do({ |node| node.post(indent ++ "--") });
	}

	log {
		^format(
			"% %/%/% \"%\" % (%,%,%)",
			this.class.name,
			slower ? 0, faster ? 0, repeat ? 0,
			string ? "", number ? 0,
			euclid_x ? 0, euclid_y ? 0, euclid_r ? 0
		)
	}

	type { ^nil }

	get_steps {
		var time, result, dur;

		dur = 1 / ((faster ? "1").asFloat) * ((slower ? "1").asFloat);
		turn = (turn ? 0); // how many times i have been called before

		// turn      0     1     2     3
		// turns: |-----|-----|-----|-----|
		// dur:   |---|---|---|---|---|---|
		// dur:   |--------|--------|-----|
		//
		time = (turn.div(dur) * dur);
		result = [];
		while { time < (turn + 1) } {
			this.get_pattern.do { |step|
				var on = time + step.on;
				if((on >= turn).and(on < (turn + 0.99999)), {
					result = result.add(step.copy.on_(on - turn));
				});
			};
			time = time + dur; // rounding errors!! hence 0.99999..
		};

		turn = turn + 1;

		^result;
	}

	get_euclid_steps { |steps_in|
		var steps, slots, sum, left_over, spread;

		if(euclid_x.isNil.or(euclid_y.isNil), { ^steps_in });

		euclid_x = euclid_x.asInteger;
		euclid_y = euclid_y.asInteger;
		euclid_r = (euclid_r ? 0).asInteger;

		// https://www.lawtonhall.com/blog/euclidean-rhythms-pt1
		// distribute y things over x slots rather evenly (y > x)

		// 1: fill all slots equally with as much things as possible
		slots = Array.fill(euclid_x, euclid_y.div(euclid_x));

		// 2: calculate how many things are left over
		left_over = euclid_y - (slots[0] * euclid_x);

		// 3: distribute the left over things evenly, adding 1 to some slots
		spread = euclid_x.div(left_over);
		left_over.do { |i| i = i * spread; slots[i] = slots[i] + 1 };

		// 4: rotate
		slots = slots.rotate(euclid_r ?? 0);

		// 5: modify the given array of steps
		steps = [];

		sum = slots.sum;
		steps_in.do { |step|
			var on = step.on;
			slots.do { |slot|
				var newstep = JSMiniStep(
					on,
					step.dur * slot / sum,
					step.string,
					step.number,
					step.type
				);
				steps = steps.add(newstep);
				on = on + newstep.dur;
			}
		};

		^steps;
	}
}

JSMiniStep {
	var <>on, <>dur, <>string, <>number, <>type;

	*new { |on, dur, string, number, type|
		^super.newCopyArgs(on, dur, string ?? "", number, type);
	}

	printOn { |stream|
		var str = (string ? "").quote;
		stream << "% % % %".format(
			(on ? 0).asFloat.round(0.0001).asString.padRight(6),
			(dur ? 0).asFloat.round(0.0001).asString.padRight(6),
			str ? "",
			number ? 0
		)
	}
}
