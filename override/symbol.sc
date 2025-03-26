+ Symbol {
	hush { |secs=0.02| JSTrack.at(this) !? { |t| t.hush(secs) } }
	mute { JSMute.mute(this) }
	unmute { JSMute.unmute(this) }
	solo { JSMute.solo(this) }
	unsolo { JSMute.unsolo(this) }

	bus { JSTrack.at(this) !? { |track| ^track.bus } }
	node { JSTrack.at(this) !? { |track| ^track.node } }
	asMap { ^this.bus.asMap }

  << { |in|
		^JSTrack.atFail(this).params(in);
  }

	-- { |in|
		case

    // set mix values (hexadecimal) for all tracks
    { this == 'mix' } { JSMix.setGlobal(in) }

		// audio tracks inside an array (seq/stack)
		{ this == "".asSymbol }
		{ ^JSTidy.new.add_branch("--").add_func(in) }

		// audio or control rate sequence
		{ in.isString }
		{ ^JSTrack.atFail(this).string(in, this) }
		
		// audio or fx [mix, func or symbol, args, target]
    { in.isArray }
		{ JSTrack.atFail(this).array(in, this) }

		// control / audio rate function
    { in.isFunction }
		{ JSTrack.atFail(this).function(in, this) }

		{ ^"%% -- <string or func or array>".format("\\", this) };
	}
}

