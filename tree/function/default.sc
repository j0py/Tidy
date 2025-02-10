// the default function
JSTidyFP : JSTidyNode {
	*new { |val, pattern|
		val = Tidy.abbr.at(val.asSymbol) ? val;

		if(val == "log") { pattern = "1" };
		if(val == "once") { pattern = "1" };
		if(val == "degrade" and: (pattern.size <= 0)) { pattern = "0.5" };
		if(val == "fit" and: (pattern.size <= 0)) { pattern = "1" };
		if(val == "flip" and: (pattern.size <= 0)) { pattern = "1" };
		if(val == "crop" and: (pattern.size <= 0)) { pattern = "1" };
		if(val == "stretch" and: (pattern.size <= 0)) { pattern = "1" };
		if(val == "mute" and: (pattern.size <= 0)) { pattern = "1" };
		if(val == "solo" and: (pattern.size <= 0)) { pattern = "1" };
		if(val == "rot" and: (pattern.size <= 0)) { pattern = "1" };

		if(pattern.size > 0) {
			^super.new(val).add(JSTidyPattern(pattern))
		};

		^super.new(val);
	}

	get { |cycle, name|
		// return a cycle with value from your pattern filled in for val
		cycle = children.first.get(cycle, name);

		cycle.steps.do { |step|

			step.at(\str) !? { |str|
				if(str[0] == $=) {
					step.put(val.asSymbol, this.string_to_control(str));
				} {
					// interpret str depending on the function name (val)
					case
					{ str == "~" } { step.trig = 0 }
					{ val == "buf" } { step.put(\buf, str.asInteger) }
					{ val == "map" } { step.put(\map, str.asSymbol) }

					{ val == "def" } { step.put(\def, str.asSymbol) }
					{ val == "vowel" } { step.put(\vowel, str.asSymbol) }
					{ val == "rot" } { step.put(\rot, str.asInteger) }
					{ val == "vel" } { step.put(\vel, str.asFloat) }
					{ val == "play" } { step.put(\play, str.asSymbol) }
					{ val == "late" } { step.put(\latebeats, str.asFloat) }
					{ val == "note" } {
						if("abcdefg".contains(str[0])) {
							step.put(\note, this.string_to_note(str))
						} {
							step.put(\note, str.asFloat)
						}
					}
					{ val == "degree" } { step.put(\degree, str.asFloat) }
					{ val == "scale" } { step.put(\scale, str.asSymbol) }
					{ val == "mix" } { step.put(\mix, str) }
					{ val == "set" } { step.put(\set, str.asSymbol) }
					{ step.put(val.asSymbol, str.asFloat) };
				};
			};

			step.at(\num) !? { |num|
				case
				{ val == "map"   } { step.put(\buf, num.asInteger) }
				{ val == "gain"  } { step.put(\gainsec, num.asInteger) }
				{ val == "late"  } { step.put(\latemsecs, num.asInteger) }
				// in your plugin you can use this value if you want
				{ step.put((val++"_num").asSymbol, num.asInteger) }
			};

			step.put(\str, nil);
			step.put(\num, nil);
		};

		^cycle;
	}

	// "=xxx" : value comes from the controlbus of xxx
	string_to_control { |str, step|
		JSTrack.at(str.drop(1).asSymbol) !? { |track|
			if("legato;note".contains(val)) {
				// TODO:
				// this will give same value for all steps
				// of the cycle! this is because "get" is called
				// when the cycle is generated. After playing the
				// first step of the cycle, the value on the control
				// bus likely has changed a bit, and that new value is
				// what you want for the second step of the cycle.
				// in other words: calling getSynchronous here is too soon.
				^track.bus.getSynchronous
			};
			^track.bus
		};
		^nil;
	}
	
	// "c3#"
	string_to_note { |str|
		var octave = 4;
		var add = 0;
		var note = $c;
		//
		str.asString.do { |ch, i|
			if(i <= 0) {
				if("abcdefg".contains(ch)) { note = ch };
			} {
				if(ch == $#) { add = add + 1 };
				if(ch == $b) { add = add - 1 };
				if("12345678".contains(ch)) {
					octave = (ch.ascii - $0.ascii).asInteger
				};
			}
		};

		^[\a, -3, \b, -1, \c, 0, \d, 2, \e, 4, \f, 5, \g, 7].asDict.at(
			note.asSymbol
		) + ((octave + 1) * 12) + add - 60;
	}
}

