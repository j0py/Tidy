JSMute {
	classvar muted;
	classvar soloed;

	*symbols { |str_or_symbol|
		var result = List.new;
		case
		{ str_or_symbol.class == String } {
			str_or_symbol.split($ ).do { |key|
				case
				{ key.isNil } { }
				{ key.asString.size <= 0 } { }
				{ result.add(key.asSymbol) }
			}
		}
		{ str_or_symbol.class == Symbol }
		{ result.add(str_or_symbol) }
		{ };
		^result;
	}
	
	*add_to_set { |in, set|
		this.symbols(in).do { |sym|
			JSTrack.at(sym) !? { |t| set.add(t) }
		}
	}

	*remove_from_set { |in, set|
		this.symbols(in).do { |sym|
			JSTrack.at(sym) !? { |t| set.remove(t) }
		}
	}

	// Tidy mute: "a b c" or Tidy mute: \a
	*mute { |str_or_symbol|
		Routine({
			JSQuant.quantize;
			muted ?? { muted = IdentitySet.new };
			if(str_or_symbol.isString) { muted = IdentitySet.new };
			this.add_to_set(str_or_symbol, muted);
			this.pr_set_mute_buses;
		}).play;
	}
 
	*solo { |str_or_symbol|
		Routine({
			JSQuant.quantize;
			soloed = IdentitySet.new;
			this.add_to_set(str_or_symbol, soloed);
			this.pr_set_mute_buses;
		}).play;
	}

	*unmute { |str_or_symbol|
		Routine({
			JSQuant.quantize;
			muted ?? { muted = IdentitySet.new };
			this.remove_from_set(str_or_symbol, muted);
			this.pr_set_mute_buses;
		}).play;
	}

	*unsolo { |str_or_symbol|
		Routine({
			JSQuant.quantize;
			soloed ?? { soloed = IdentitySet.new };
			this.remove_from_set(str_or_symbol, soloed);
			this.pr_set_mute_buses;
		}).play;
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
	// also \a should not trigger new notes.
	//
	// if \b is un-soloed, you could hear \a 's loooong note if it
	// did not finish yet.
	//
	*pr_set_mute_buses {
		muted ?? { muted = IdentitySet.new };
		soloed ?? { soloed = IdentitySet.new };

		JSTrack.do { |track|
			 track.set_mute_bus(this.should_mute(track).asInteger)
		 };
		
		//^"soloed %, muted %".format(soloed.as(Array), muted.as(Array));
	}

	*should_mute  { |track|
		soloed ?? { soloed = IdentitySet.new };
		muted ?? { muted = IdentitySet.new };

		// solo wins from mute
		if(soloed.includes(track)) { ^false };
		if(muted.includes(track)) { ^true };
		if(soloed.size > 0) { ^true };
		^false;
	}
}

