+ Symbol {
	hush { |secs=0.02| JSTrack.at(this) !? { |t| t.hush(secs) } }
	mute { JSMute.mute(this) }
	unmute { JSMute.unmute(this) }
	solo { JSMute.solo(this) }
	unsolo { JSMute.unsolo(this) }

	bus { JSTrack.at(this) !? { |track| ^track.bus } }
	node { JSTrack.at(this) !? { |track| ^track.node } }
	asMap { ^this.bus.asMap }

	/*
		send MIDICLOCK to some device (24 messages per beat):

		1: \tidy .midiout
		2: connect midi with jack
		3: \clock -- "n 0!96" - "midiclock 1"
	*/
	midiout {
		if(this == \tidy) {
			Routine({
				// connect with QJackCtl to CH345 device ("OUT" means out)
				MIDIClient.init;
				Server.default.sync;
				Library.put(\tidy, \midiout, MIDIOut(0));
				"*** midiout ready : connect using jack now ***".postln;
			}).play;
		};
	}

	/*
        example                                        | rate    | bus
		-----------------------------------------------+---------+-----
		\pan -- { SinOsc.kr(0.1) }                     | control | out
		\vel -- "cv 0.2 0.8" |+ "cv 0.1"               | control | out

		\a -- [out, function, args, gain, target]      | audio   | in
		\a -- [out, symbol, args, gain, target]        | audio   | in
		\1 -- [out, function, args, gain, target]      | audio   | in
		\2 -- [out, symbol, args, gain, target]        | audio   | in

		\b -- "map bd snare" - "pan =pan" - "mix f41"  | audio   | -
		\b -- { Out.ar(\a.bus, SinOsc.ar([300,298])) } | audio   | -
	*/
	-- { |in|
		case

		// audio tracks inside an array (seq/stack)
		{ this == "".asSymbol }
		{ ^JSTidy.new.add_branch("--").add_func(in) }

		// audio or control rate sequence
		{ in.isString }
		{ ^JSTrack.atFail(this).add_branch("--").add_func(in) }

		// [out, func or symbol, args, gain, target]
        { in.isArray }
		{ JSTrack.atFail(this).array(in) }

		// audio or control rate function
        { in.isFunction }
		{ JSTrack.atFail(this).function(in) }

		{ ^"%% -- <string or func or array>".format("\\", this) };
	}
}

