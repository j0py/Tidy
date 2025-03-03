JSTrack : JSTidy {
	classvar tracks;

	var <>gain_bus, <>mute_bus, last_mute;
	var queue, curtree, newtree;
	var <hushed=false, <hushing=false, <once=false;
	var <>node, <>type, <>bus;
	
	*do { |func| tracks.do { |track| func.(track) } }
	
	*at { |name|
		tracks ?? { tracks = Dictionary.new };
		^tracks.at(name.asSymbol);
	}

	*atFail { |name|
		var track, server = Server.default;
		name = name.asSymbol;
		tracks ?? { tracks = Dictionary.new };
		tracks.at(name) !? { |track| ^track };
		tracks.put(name, track = JSTrack.new);
		
		case
		{ ((0..9).collect(_.asSymbol).includes(name)) }
		{ track.type_(\fx).bus_(Bus.audio(server, 2)) }
		{ (97..122).collect(_.asAscii).collect(_.asSymbol).includes(name) }
		{
			track.type_(\audio)
			.gain_bus_(JSControlBus("gain"))
			.mute_bus_(JSControlBus("mute"))
			.bus_(Bus.audio(server, 2))
		}
		{ track.type_(\control).bus_(Bus.control(server, 1)) };
		^track;
	}
	
	// \a -- "n 0 2 3" - etc has been evaluated in the Interpreter
	printOn { |stream|
		// if tree is nil then something has gone wrong while creating it.
		// in that case: stop here, so that curtree will keep going.
		tree ?? { "%: tree nil".format("name").postln; ^this };
		if(Tidy.log == \tree) { tree.log };
		"%% pattern".format("\\", "name").printOn(stream);

		once = false;
		newtree = tree;
		tree = nil; // get ready for next evaluation
		hushed = false;
		hushing = false;
		node !? { Server.default.bind { node.release }; node = nil };

		JSMainloop.start;
	}

	play { |cycle_number|
		var steps = [], delta = 1;

		Routine({
			if((cycle_number % JSQuant.quant) == 0) {
				if(curtree != newtree) { queue = nil };
				curtree = newtree; // switch trees in quantized manner
			};
            curtree !? {
				queue = queue ? List.new;

				while { delta > 0.0001 } {
					var step, clone, slow, fast;

					if(queue.size <= 0) {
						var rot, cycle;
						cycle = curtree.get(JSTidyCycle.new, "name");
						// you can do this in a cycle plugin
						cycle.steps.do({ |x| rot ?? rot = x.at(\rot) });
						cycle.rotate(rot ? 0);
						Tidy.alter_cycle(this, cycle);
						if(Tidy.log == \cycle) { cycle.postln };
						queue.addAll(cycle.steps);
					};

					step = queue.removeAt(0);

					slow = step.dict.removeAt(\slow) ? 1;
					if(slow.class == Bus) { slow = slow.getSynchronous };
					fast = step.dict.removeAt(\fast) ? 1;
					if(fast.class == Bus) { fast = fast.getSynchronous };
					slow = slow / fast;
					
					step.delta = step.delta * slow;
					step.dur = step.dur * slow;
					
					if(delta >= step.delta) {
						delta = delta - step.delta;
					} {
						// insert a rest step at head of the queue
						queue.insert(0,	JSTidyStep.rest(step.delta-delta));
						step.delta = delta;
						delta = 0;
					};

					steps = steps.add(step);
				};

				steps.do({ |step|
					step.play(this);
					step.log;
					step.at(\once) !? { once = true; "once".postln };
					step.delta.wait;
				});
			};
		}).play;
	}

	hush { |seconds=0.02|
		Routine({
			if(hushing.not) {
				gain_bus !? { gain_bus.reset(seconds) };
				hushing = true;
			};
			(seconds * TempoClock.tempo).wait;
			node !? {
				Server.default.bind { node.release };
				node = nil;
			};
			hushed = true;
			queue = List.new;
		}).play;
	}

	// \a -- [out, func_or_symbol, args, target]
	// target should be the out if that is a symbol
	old_array { |input|
		var server = Server.default;
		var out, what, gain, args, target, old, addAction;

		if(((input.class == Array) and: (input.size >= 2)).not) {
			^"%% -- [out, func/def, [], target]".format("\\", "0");
		};

		out = input[0];
		args = input[2] ? [];
		target = input[3];
		args = args ++ [\in, bus.index];
		
		Routine({
			case
			{ out.isInteger } { }
			{ (out = JSTrack.at(out.asSymbol)).notNil }
			{ out = out.bus.index }
			{ out = 0 };
			
			// make bus mapping possible
			args = args.collect({ |el, i|
				var result = el;

				if(((i % 2) == 1) and: (el.class == Symbol)) {
					JSTrack.at(el) !? { |t|	result = t.bus.asMap }
				};

				result;
			});

			old = node;
			target !? { target = JSTrack.at(target).node };
			target ?? { target = old };
			addAction = \addToHead;
			target !? { addAction = \addBefore };

			Server.default.bind {
				case
				{ input[1].isFunction }
				{
					// the function must expect an \in argument
					// the function may use the \gain argument
					node = input[1].play(
						target: target,
						addAction: addAction,
						outbus: out,
						fadeTime: 0.5,
						args: args
					);
					
					old !? { old.release };
				}
				{
					var def = input[1].asSymbol;
					
					if(SynthDescLib.at(def).notNil) {
						// the synthdef should have a sustaining envelope
						// with \gate arg (and fade in/fade out somewhat)
						// the synthdef must use an \in and \out bus argument
						// the synthdef may use the \gain argument
						node = Synth(
							defName: input[1].asSymbol,
							args: args ++ [\out, out],
							target: target,
							addAction: addAction
						);
						
						old !? { old.release };
					} {
						"synthdef % unknown".format(def).postln;
					}
				}
			}
		}).play;
	}

	// \2 -- [mix, func_or_symbol, args, target]
	array { |input|
		var server = Server.default;
		var mix, what, gain, args, target, old, addAction;

		if(((input.class == Array) and: (input.size >= 2)).not) {
			^"%% -- [mix, func/def, [], target]".format("\\", "0");
		};

		mix = input[0];
		args = input[2] ? [];
		target = input[3];
		args = args ++ [\in, bus.index];
		
		Routine({
			case
			{ mix.isInteger } {	args = args ++ [\out1, 0, \gain1, 1] }
			{
				var send=1;
				mix.asString.do { |gain, i|
					gain = gain.digit.linlin(0, 15, 0, 1).asFloat;
					if(gain > 0) {
						var track = JSTrack.at(i.asSymbol);
						track !? {
							args = args ++ [
								("out"++send).asSymbol,
								track.bus.index,
								("gain"++send).asSymbol,
								gain
							];

							send = send + 1;
						}
					}
				}
			};
			
			// make bus mapping possible
			args = args.collect({ |el, i|
				var result = el;

				if(((i % 2) == 1) and: (el.class == Symbol)) {
					JSTrack.at(el) !? { |t|	result = t.bus.asMap }
				};

				result;
			});

			old = node;
			target !? { target = JSTrack.at(target).node };
			target ?? { target = old };
			addAction = \addToHead;
			target !? { addAction = \addBefore };

			Server.default.bind {
				case
				{ input[1].isFunction }
				{
					// the function must expect an \in argument
					// the function may use the \gain argument
					/*
					node = input[1].play(
						target: target,
						addAction: addAction,
						outbus: out,
						fadeTime: 0.5,
						args: args
					);
					*/
					old !? { old.release };
				}
				{
					var def = input[1].asSymbol;
					
					if(SynthDescLib.at(def).notNil) {
						// the synthdef should have a sustaining envelope
						// with \gate arg (and fade in/fade out somewhat)
						// the synthdef must use an \in and \out bus argument
						// the synthdef may use the \gain argument
						node = Synth(
							defName: input[1].asSymbol,
							args: args.postln,
							target: target,
							addAction: addAction
						);
						
						old !? { old.release };
					} {
						"synthdef % unknown".format(def).postln;
					}
				}
			}
		}).play;
	}

	function { |func|
		JSMainloop.start;

		Server.default.bind {
			node !? { node.release };
			if(bus.notNil) {
				node = func.play(outbus: bus.index)
			} {
				node = func.play
			}
		}
	}

	// reason for mute buses: it mutes running synths.
	// the last_mute variable is cheaper than mute_bus.getSynchronous.
	set_mute_bus { |new_mute|
		if(new_mute == last_mute) { ^this };
		mute_bus.set(new_mute); // 0 or 1
		last_mute = new_mute;
	}
}

