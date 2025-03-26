JSTrack : JSTidy {
	classvar tracks;

  // TODO: protect your internal variables better than this!
	var <>gain_bus, <>mute_bus, last_mute;
	var queue, curtree, newtree;
	var <hushed=false, <hushing=false, <once=false;
	var <>node, <>type, <>bus, <>name;
	
  *initClass { tracks = Dictionary.new }

	*do { |func| tracks.do { |track| func.(track) } }
	
	*pairsDo { |func| tracks.pairsDo { |name, track| func.(name, track) } }
	
	*at { |name| ^tracks.at(name.asSymbol) }

	*atFail { |name|
		var track, server = Server.default;
		name = name.asSymbol;
		tracks.at(name) !? { |track| ^track };
		tracks.put(name, track = JSTrack.new.name_(name)); // track knows its name
		
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
	
  *busIndex { |key|
    if(key.asSymbol == \0) { ^0 };
    tracks.at(key.asSymbol) !? { |track| ^track.bus.index };
    ^nil;
  }

	// \a -- "n 0 2 3" - etc has been evaluated in the Interpreter
	printOn { |stream|
		// if tree is nil then something has gone wrong while creating it.
		// in that case: stop here, so that curtree will keep going.
		tree ?? { "%: tree nil".format(this.name).postln; ^this };
		if(Tidy.log == \tree) { tree.log };
		"%% pattern".format("\\", this.name).printOn(stream);

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
		  node !? { Server.default.bind { node.release }; node = nil };
			hushed = true;
			queue = List.new;
		}).play;
	}

	// make bus mapping possible
  mapbuses { |args|
    args = args ? [];
		^args.collect({ |el, i|
			var result = el;

			if(((i % 2) == 1) and: (el.class == Symbol)) {
				JSTrack.at(el) !? { |t|	result = t.bus.asMap }
			};

			result;
		});
  }

  // set params on running node
  // string format: "<param> <value> <param> <value> .."
  params { |input|
    // TODO maybe quantize this..
    node !? {
      var param;
      input.split($ ).do { |value, i|
        if((i % 2) <= 0) {
          param = value.asSymbol
        } {
          if(value[0] == $=) {
            value = value.drop(1).asSymbol;
			      JSTrack.at(value) !? { |t| 
              Server.default.bind { node.set(param, t.bus.asMap) }
            }
          } {
            Server.default.bind { node.set(param, value.asFloat) }
          } 
        }
      }
      ^"[%]".format(input);
    };
    ^"no node";
  }

  string { |input|
		node !? { Server.default.bind { node.release }; node = nil };

    ^this.add_branch("--").add_func(input);
  }

  // fx:
  // \2 -- [\tank, \decay, 8, ..]
  // \2 -- { |in| ... }
  //
  // audio:
  // \b -- [\tank, \decay, 8, ..]
  // \b -- "freq =melody" - "def bass" - "mix f2 f5" // mix can be overridden
  // \b -- { |in| Saw.ar(\melody.bus) * \amp.kr(0.1) }
  //
  // control:
  // \mod -- { SinOsc.kr(0.1).range(10, 20) }
  // \mod -- "cv 1 2 3 4" |+ "cv <12 23>"
  //
	array { |input, name|
		var args, target, old, addAction, def;

		JSMainloop.start;

		// the synthdef should have a sustaining envelope
		// with \gate arg (and fade in/fade out somewhat)
		// the synthdef must use an \in and \out bus argument
		// the synthdef may use the \gain argument
    def = input[0].asSymbol;
	  if(SynthDescLib.at(def).isNil) { ^"synthdef % unknown".format(def) };

    args = input.drop(1) ++ [\in, bus.index] ++ JSMix.args(name);
		args = this.mapbuses(args);	

    // figure out target and addAction for the new node (fx / audio)
		old = node;
    target = nil;
		addAction = \addToHead;
    if(type == \fx) {
      target = old; // could still be nil!
 			target !? { addAction = \addBefore };
    };

    // launch the synth
		Server.default.bind {
		  node = Synth(defName:def, args:args, target:target, addAction:addAction);
			old !? { old.release };
		};
	}

	function { |func, name|
		JSMainloop.start;

    if(type == \control) {
		  Server.default.bind {
			  node !? { node.release };
			  node = func.play(outbus: bus.index)
		  }
    } { // audio or fx
      var args, old, target, addAction;

      args = [\in, bus.index] ++ JSMix.args(name);
		  args = this.mapbuses(args);	

      // TODO: copied from above!
		  old = node;
      target = nil;
		  addAction = \addToHead;
      if(type == \fx) {
        target = old; // could still be nil!
 			  target !? { addAction = \addBefore };
      };

      Routine {
		    Server.default.bind {
          node = {
            var sig = SynthDef.wrap(func, [], [In.ar(\in.kr(0), 2)]);
            // make sure that this node can be released
            sig = sig * Env.asr(0.02, 1, 0.02).kr(2, \gate.kr(1));
            Out.ar(\out1.kr(0), sig * \gain1.kr(0));
            Out.ar(\out2.kr(0), sig * \gain2.kr(0));
            Out.ar(\out3.kr(0), sig * \gain3.kr(0));
            Out.ar(\out4.kr(0), sig * \gain4.kr(0));
          }.play(
            target: target,
            addAction: addAction,
            outbus: 0,
            fadeTime: 0.5,
            args: args
          );
        };

        // sometimes old.release is fast, and as old could be the target
        // above, we get an error. that is why the release is postponed
        // a bit.
        (TempoClock.tempo / 10).wait; // wait 1/10 second
  		  old !? { Server.default.bind { old.release } };
      }.play;
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

