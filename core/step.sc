JSTidyStep {
	var <>trig, <>delta, <>dur, <>dict;

	*new { |trig, delta, dur, str, num|
		^super.newCopyArgs(trig ? 0, delta ? 1, dur ? 1)
		.dict_(Dictionary.new)
		.put(\str, str)
		.put(\num, num);
	}

	*copy { |step, trig, delta, dur, str, num|
		^JSTidyStep(
			trig ? step.trig,
			delta ? step.delta,
			dur ? step.dur,
			str ? "",
			num ? 0
		).dict_(Dictionary.newFrom(step.dict));
	}
	
	*rest { |delta|	^JSTidyStep(0, delta, delta, "~", 0) }
	
	putAll { |argdict| dict.putAll(argdict) }
	put { |key, value| dict.put(key.asSymbol, value) }

	at { |key| ^dict.at(key) }
	has { |key| ^dict.includesKey(key) }
	
	removeAt { |key|
		var val = this.at(key.asSymbol);
		dict.removeAt(key.asSymbol);
		^val;
	}

	/* send your dict as parameters to a node (fx or track)
	play_set { |name, track|
		if((dict.at(\degrade) ? 1).coin.not) { ^this };
		if(trig <= 0) { ^this };

		//JSPlugins.alter(name, this); // all plugins do their thing

		dict.at(\set).asSymbol.setarr(dict.asPairs);
	}
	*/

	play_midiout {
		dict.at(\midiout) !? { |chan|
			Library.at(\tidy, \midiout).noteOn(
				chan,
				dict.at(\freq).cpsmidi.asInteger,
				(this.at(\vel) ? 0.5).linlin(0, 1, 0, 127)
			);
			^true;
		};

		dict.at(\midiclock) !? { |chan|
			Library.at(\tidy, \midiout).midiClock;
			^true;
		};

		^false;
	}
	
	/*
		all synthdefs define outx/gainx control pairs, so that some
		of the audio can be sent to effect buses:

		Out.ar(\out1.kr(0), sig * \gain1.kr(0))

		the "mix" function and also functions "0", "1" .. "9" can be
		used to set the gain for effects 0, 1, 2, 3, .. 9.

		the send values are determined and then assigned to the
		out/gain indexes. If you specify 6 sends in your patterns,
		but the synthdef only defines out1 .. out4, then the values
		of synth arguments out5, out6, gain5 and gain6 will simply
		be ignored by the synth.

		this way of sending audio to the effects has some advantages:
		- the effect synth is instantiated once, so cpu effective
		- the sends to the effects are patternable
    */
	put_sends {
		var send, mix, fx;

		// - "mix f4" - : gains for fx 0 and 1
		mix = 0!10;
		(dict.at(\mix) ? "f").do { |gain, i|
			mix[i] = gain.digit.linlin(0, 15, 0, 1).asFloat;
		};

		// - "4 0.4 0.2" -, override gain for fx 4 (patternable)
		// - "4 =cvx" -, override gain for fx 4 with controlbus value
		fx = nil!10;
		10.do { |i| dict.at(i.asSymbol) !? { |gain| fx[i] = gain } };

		// distribute the values over the available outputs of the synth
		send = 1;
		10.do { |i|
			JSTrack.at(i.asSymbol) !? { |effect|
				case
				{ fx[i].notNil } {
					this.put(("out" ++ send).asSymbol, effect.bus.index);
					this.put(("gain" ++ send).asSymbol, fx[i]);
					send = send + 1;
				}
				{ mix[i] > 0 } {
					this.put(("out" ++ send).asSymbol, effect.bus.index);
					this.put(("gain" ++ send).asSymbol, mix[i]);
					send = send + 1;
				}
				{}
			}
		}
	}

	play { |track|
		var tempo = TempoClock.tempo;
		var degrade = dict.at(\degrade) ? 1;
		if(degrade.isKindOf(Bus)) { degrade = degrade.getSynchronous };
		if(degrade.coin.not) { ^this };
		if(trig <= 0) { ^this };

		if(track.type == \control) {
			dict.at(\cv) ? dict.at(\control) !? { |cv|
				Routine({
					var latebeats=0;

					dict.at(\latemsecs) !? { |msecs|
						latebeats = (msecs.clip(0, 40) / 1000 * tempo);
					};
					
					dict.at(\latebeats) !? { |beats|
						latebeats = latebeats + (beats.clip(0, 1));
					};
					
					if(latebeats > 0) { latebeats.wait };
					
					track.bus.setSynchronous(cv.asFloat);
				}).play;
			}
			^nil;
		};

		if(JSMute.should_mute(track)) { ^this };

		// sustain is overrideable in seconds (for percussive synths)
		this.at(\sustain) ?? {
			var sustainBeats = dur * (dict.at(\legato) ? 0.8);
			dict.put(\sustain, sustainBeats / tempo)
		};

		this.put(\mute, track.mute_bus.bus.asMap);
		if(track.hushing.not) {
			track.gain_bus.set(
				(this.at(\gain) ? 0.5).asFloat,
				max(0.02, (this.at(\gainsec) ? 0).asFloat)
			);
		};
		this.put(\gain, track.gain_bus.bus);

		if(this.play_midiout) { ^this };
		Tidy.alter_step(track, this); // all plugins do their thing

		dict.at(\def) ?? { "no def".postln; ^this };
		this.put_sends;

		dict = dict.collect { |v| case {v.class == Bus} {v.asMap} {v} };

		// play note on a separate thread (because of sustain.wait)
		Routine({
			var synthDesc, node, latebeats=0;

			dict.at(\latemsecs) !? { |msecs|
				latebeats = (msecs.clip(0, 40) / 1000 * tempo);
			};
			dict.at(\latebeats) !? { |beats|
				latebeats = latebeats + (beats.clip(0, 1));
			};
			// https://www.youtube.com/watch?v=0dsjuPZsNwQ
			if(latebeats > 0) { latebeats.wait };

			Server.default.bind {
				node = Synth(dict.at(\def), dict.asPairs)
			};
			(dict.at(\sustain) * tempo).wait; // in beats
			Server.default.bind { node.release };
		}).play;
	}

	log {
		if((Tidy.log == \step) or: ((dict.at(\log) ? 0) > 0)) {
			this.postln;
		}
	}
	
	printOn { |stream|
		var width=40;
		var len=width;
		
		stream << "step\ntrig:% delta:% dur:%\n".format(
			trig,
			delta.round(0.01),
			dur.round(0.01)
		);

		dict.keys.asArray.sort.do { |k|
			var str, val = dict.at(k.asSymbol);
			//if(k != \log) {
				if(val.isFloat) { val = val.round(0.01) };
				str = "%:% ".format(k, val);
				if((len - (str.size)) < 0) { stream << "\n"; len=width; };
				stream << str;
				len = len - str.size;
			//};
		};
		
		//dict.keysValuesDo { |k,v| stream << "%:%,".format(k,v) };
		//outs.keysValuesDo { |k,v| stream << "~%:%,".format(k,v) };
		stream << "\n";
	}

	// called by JSTidyCycle.printOn
	printOn2 { |stream|
		stream << "step % % %".format(
			trig,
			delta.round(0.01).asString.padLeft(6),
			dur.round(0.01).asString.padLeft(6)
		);

		dict.keysValuesDo { |k,v| stream << "%:%,".format(k,v) };
		stream << "\n";
	}
}

