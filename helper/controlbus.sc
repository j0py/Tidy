JSControlBus {
	var <>bus, node, lastval, >purpose;
	
	*new { |purpose|
		^super.new.purpose_(purpose).bus_(Bus.control(Server.default, 1))
	}

	reset { |seconds=0.02|
		this.set(0, seconds);
		lastval = nil;
	}
		
	*init { |server|
		SynthDef(\controlbus, {
			var cur = In.kr(\out.kr(0));
			var sig = Env([cur, cur, \val.kr(0)],[0, \sec.kr(0)]).kr;
			ReplaceOut.kr(\out.kr(0), sig);
		}).add;
		server.sync;
	}
	
	// launch a synth that changes the value on the
	// control bus over an amount of time in seconds.
	set { |val, sec=0.02|
		lastval ?? { sec = 0 };
		if(val != lastval) {
			"controlbus % set % -> % (%)".format(
				purpose, lastval, val, sec
			).postln;
			Server.default.bind {
				node = Synth(
					\controlbus,
					[out: bus, val: val, sec: sec],
					node,
				    if(node.isNil, \addToTail, \addReplace)
				);
			};
			lastval = val;
		}
	}
}
