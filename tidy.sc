// TODO: document the functions
// TODO: schelp docs for SC

Tidy {
	classvar samples, buffers, <recordings, prevfreq, abbreviations;
	classvar <>log=0;   // to (de)activate logging
	classvar step_plugins, cycle_plugins, <>midi_out;

	*doc {
		[
			"abbr   : abbreviate: Tidy.abbr([\\leg, \\legato])",
			"setup  : setup plugins, synthdefs, abbreviations, etc",
			"load   : load samples: Tidy.load(\"~/samples\")",
			"sample : get sample buffer: Tidy.sample(\\bd, 2)",
			"audit  : audit samples: Tidy.audit(\\sn)",
			"def    : define synthdef: Tidy.def(\n, func, variants)",
			"bpm    : Tidy.bpm(110) or Tidy.bpm",
			"cps    : Tidy.cps(0.3) or Tidy.cps",
			"bpm    : Tidy.quant(2) or Tidy.quant",
			"buffer : alloc buffer xx cycles: Tidy.buffer(2)",
			"mic    : record mic in buffer: Tidy.mic(\\n, 2, 0.25)",
			"rec    : record bus in buffer: Tidy.rec(\\n, 2, 0.1)",
			"save   : save buffer: Tidy.save(\\n, \"~/x.wav\")",
			"end    : end all with fadeout: Tidy.end(12)",
		].do { |str| str.postln }
	}

	// <value> or #peak att dec sus rel peak curve
	// - "lpf 200" -
	// - "lpf #200 0.1 0.2 0.3 3" -
	// do NOT supply a kr default if you don't know it;
	// this gives a SynthDef a chance to do that!
	*adsr { |name, gate, doneAction=0, default|
		^Env.adsr(
			(name++1).asSymbol.kr(0),           // attack
			(name++2).asSymbol.kr(0),           // decay
			(name++3).asSymbol.kr(1),           // sustainlevel
			(name++4).asSymbol.kr(0).max(0.1),  // release
			name.asSymbol.kr(default),          // peak
			(name++5).asSymbol.kr(-4)           // curve
		).kr(doneAction, gate);
	}

	*abbr { |array|
		array ?? { ^abbreviations };
		abbreviations.putAll(array.asDict)
	}

	*add_step_plugin { |key, func|
		step_plugins = step_plugins ? JSPlugins.new;
		step_plugins.add(key, func);
	}

	*alter_step { |track, step|
		step_plugins = step_plugins ? JSPlugins.new;
		step_plugins.alter(track, step)
	}

	*add_cycle_plugin { |key, func|
		cycle_plugins = cycle_plugins ? JSPlugins.new;
		cycle_plugins.add(key, func);
	}

	*alter_cycle { |track, cycle|
		cycle_plugins = cycle_plugins ? JSPlugins.new;
		cycle_plugins.alter(track, cycle)
	}

	*setup {
		// default (overrideable) plugins and synthdefs

		abbreviations = [
			\d, \def,
			\m, \map, \snd, \map, \s, \map,
			\b, \buf,
		].asDict;

		// FREQ
		// n(ote) = midinote - 60

		this.add_step_plugin(\freq, { |track, step|
			case
			{ step.at(\freq).notNil } { /* do nothing */ }
			{ step.at(\midinote).notNil } {
				var mn = step.at(\midinote);
				if(mn.isKindOf(Bus)) { mn = mn.getSynchronous };
				step.put(\freq, mn.midicps)
			}
			{ step.at(\note).notNil } {
				var note = step.at(\note);
				var octave = (step.at(\octave) ? 4).asInteger;
				step.put(\freq, (note + ((octave + 1) * 12)).midicps)
			}
			{ step.at(\scale).notNil or: step.at(\toscale).notNil } {
				// - "scale major minor" - "degree 0 2b 4# 5" -
				// - "toscale 0 2 4 5 7 9 11" - "degree 0 2# 4 5b" -
				var add=0, mul=1, degree="", scale, note, octave;
				//
				if(step.at(\scale).notNil) {
					scale = Scale.at(step.at(\scale).asSymbol) ? Scale.major;
				} {
					scale = step.at(\toscale).asArray;
				};
				octave = (step.at(\octave) ? 4).asInteger;
				(step.at(\degree) ? "0").asString.do { |ch, i|
					if(ch == $-) { mul = -1; octave = octave -1 };
					if(ch == $#) { add = add + 1 };
					if(ch == $b) { add = add - 1 };
					if(".0123456789".contains(ch)) { degree = degree ++ ch };
				};
				degree = degree.asInteger * mul;
				note = scale.wrapAt(degree).asInteger + add;
				step.put(\freq, (note.asInteger + (12 * (octave + 1))).midicps)
			}
			{ step.put(\freq, 60.midicps) }
		});

		// needed for the glide function, but should disappear!
		this.add_step_plugin(\prevfreq, { |track, step|
			var prevfreq = Tidy.prevfreq(track);
			step.put(\prevfreq, prevfreq ? step.at(\freq));
			Tidy.prevfreq(track, step.at(\freq));
		});

		this.add_step_plugin(\sample, { |track, step|
			var buf, rate;

			rate = step.at(\rate) ? 1;

			step.at(\map) !? { |map|
				var index = (step.at(\buf) ? 1);
				if(index.isKindOf(Bus)) { index = index.getSynchronous };
				index = index.asInteger;
				if(index <= -1) { rate = rate * -1 };
				buf = Tidy.sample(map, abs(index));
				buf ?? { "buf % % unknown".format(map, index).postln };
			};

			step.at(\play) !? { |rec|
				buf = Tidy.recordings.at(rec.asSymbol);
				buf ?? { "rec buf % unknown".format(rec).postln };
			};

			buf !? {
				var s = Server.default;
				var begin, end, legato, bufbeats, bufseconds, sustainbeats;

				step.put(\bufchannels, buf.numChannels);
				step.put(\bufnum, buf.bufnum);

				rate = rate * (step.at(\speed) ? 1);
				rate = rate * buf.sampleRate / s.sampleRate;
				if((step.at(\reversed) ? 0) > 0) { rate = rate * -1 };
				begin = step.at(\begin) ? 0;
				end = step.at(\end) ? 1;

				// calculate sustainbeats
				bufseconds = buf.numFrames / buf.sampleRate;
				bufseconds = bufseconds * abs(end - begin);
				bufbeats = bufseconds * TempoClock.tempo;

				legato = bufbeats / step.dur; // default: play whole sample
				if(legato > 1) { legato = 1 }; // max step length
				legato = step.at(\legato) ? legato; // unless overridden
				step.put(\legato, legato);
				sustainbeats = step.dur * legato;

				// starting point in buffer for PlayBuf
				step.put(\begin, begin);
				if(rate < 0) { step.put(\begin, end) };

				step.at(\stretch) !? { |stretch|
					// fit step around <stretch> samples
					if(step.delta > 0) { //"n [0,2,4]"
						step.delta = bufbeats * stretch
					};
					sustainbeats = bufbeats * stretch;
				};

				step.put(\sustainbeats, sustainbeats);

				// flip: align reversed sample perfectly to the right
				if((step.at(\flip) ? 0) > 0) {
					if(rate > 0) { rate = rate * -1 };
					if(bufbeats >= sustainbeats) {
						step.put(\begin, sustainbeats / bufbeats);
					} {
						step.put(\begin, 1);
						step.put(\latebeats, sustainbeats - bufbeats);
					};
				};

				// in the synthdef, we will know the sustain in seconds
				// this can be convenient for Lag or Env
				step.put(\sustain, sustainbeats / TempoClock.tempo);
			};

			step.put(\rate, rate);
		});

		// the Vowel quark could be installed, or not
		\Vowel.asClass !? {
			this.add_step_plugin(\vowel, { |track, step|
				step.at(\vowel) !? { |vowel|
					vowel = vowel.asSymbol; // \a \o \e \i \u
					Vowel.formLib.at(vowel) !? {
						var reg = (step.at(\register) ? 0); // 0,1,2 etc
						var regs = [
							\bass, \tenor, \counterTenor, \alto, \soprano
						];
						reg = regs.at(reg.clip(0, regs.size));
						vowel = Vowel(vowel, reg).brightenExp(3);
						step.put(\vowel_freqs, vowel.freqs);
						step.put(\vowel_rqs, vowel.amps); // SuperDirt..
						step.put(\vowel_amps, vowel.amps);
					}
				}
			})
		};

		this.add_step_plugin(\bufnum, { |track, step|
			if(step.has(\bufnum)) {
				if(step.has(\def).not) {
					step.put(\def, \playbuf1);
					if((step[\bufchannels] ?? 1) > 1) {
						step.put(\def, \playbuf2)
					};
				};

				if(step.has(\rumble)) {
					step.put(\def, \rumble1);
					if((step[\bufchannels] ?? 1) > 1) {
						step.put(\def, \rumble2)
					};
				}
			}
		});

		/*
			each step can be "filled" using the <dur> of the following
			rest steps, if any. the step itself should have a value for
			"fill" (integer, min 1).
			a value of 1 for "fill" means do nothing.
			a value of 2 means "if next step is a rest, add its dur to yours"
			this wraps around the cycle.
		*/
		this.add_cycle_plugin(\fill, { |track, cycle|
			var last, lastfill, steps = List.new;
			cycle.steps.do { |step|
				var fill = (step.at(\fill) ? 1);

				if(fill.isKindOf(Bus)) { fill = fill.getSynchronous };
				fill = max(1, fill);

				case
				{ step.trig > 0 } {
					last = step;
					lastfill = fill;
				} {
					last !? {
						if(lastfill > 1) {
							lastfill = lastfill - 1;
							last.dur = lastfill * step.dur + last.dur;
						}
					};
				};
				steps.add(step);
			};

			cycle.steps_(steps.asArray);
		});

		// define synthdefs

		SynthDef(\mic, {
			var bufnum = \buf.kr(0);
			var in = In.ar(\in.kr(0), 2);

			// make mono to use it with grainbuf / tgrains
			in = [in.sum];

			// highpass to avoid mic rumble
			in = HPF.ar(in, 200);
			in = HPF.ar(in, 100);
			in = LeakDC.ar(in);

			RecordBuf.ar(in, bufnum, loop: 0, doneAction: 2);
		}).add;

		SynthDef(\rec, {
			var bufnum = \buf.kr(0);
			var in = In.ar(\in.kr(0), 2);
			RecordBuf.ar([in.sum], bufnum, loop: 0, doneAction: 2);
		}).add;

		this.def(\playbuf2, {
			arg freq, vel, gate, sustain;
			var sig, rate, bufnum, begin, att, rel, crv, trigger;
			//
			att = \att.kr(0.01);
			rel = \rel.kr(1);
			crv = \crv.kr(-4);
			rate = \rate.kr(1) * \cvrate.kr(1) * freq / 60.midicps;
			bufnum = \bufnum.kr(0);
			trigger = \trigger.tr(1);
			begin = \begin.kr(0) * BufFrames.kr(bufnum);
			sig = PlayBuf.ar(2, bufnum, rate, trigger, begin);
			sig = LeakDC.ar(sig);
			sig = Balance2.ar(sig[0], sig[1], \pan.kr(0));
			sig = sig * Env.asr(att, 1, rel, crv).kr(2, gate);
		});

		this.def(\playbuf1, {
			arg freq, vel, gate, sustain;
			var sig, rate, bufnum, begin, att, rel, crv;
			//
			att = \att.kr(0.01);
			rel = \rel.kr(1);
			crv = \crv.kr(-4);
			rate = \rate.kr(1) * \cvrate.kr(1) * freq / 60.midicps;
			bufnum = \bufnum.kr(0);
			begin = \begin.kr(0) * BufFrames.kr(bufnum);
			sig = PlayBuf.ar(1, bufnum, rate, startPos: begin);
			sig = LeakDC.ar(sig);
			sig = Pan2.ar(sig, \pan.kr(0));
			sig = sig * Env.asr(att, 1, rel, crv).kr(2, gate);
		});

		this.def(\rumble2, {
			arg freq, vel, gate, sustain;
			var sig, rate, bufnum, begin, rumble, env, delay, amount;
			var att, rel, crv;
			//
			att = \att.kr(0.01);
			rel = \rel.kr(1);
			crv = \crv.kr(-4);
			rate = \rate.kr(1) * \cvrate.kr(1) * freq / 60.midicps;
			bufnum = \bufnum.kr(0);
			begin = \begin.kr(0) * BufFrames.kr(bufnum);
			sig = PlayBuf.ar(2, bufnum, rate, startPos: begin);
			//
			amount = \rumble.kr(0);
			rumble = sig.sum * amount;
			delay = LFNoise2.kr(0.5).range(0.27, 0.51)!6 * sustain;
			rumble = CombL.ar(rumble, 1, delay, sustain * (1 + amount));
			rumble = LPF.ar(rumble.sum, 22);
			sig = LeakDC.ar(sig + (rumble!2));
			//
			sig = Balance2.ar(sig[0], sig[1], \pan.kr(0));
			env = Env([0, 1, 1, 0], [att, sustain, rel], crv).kr(2);
			sig * env;
		});

		this.def(\rumble1, {
			arg freq, vel, gate, sustain;
			var sig, rate, bufnum, begin, rumble, env, delay, amount;
			var att, rel, crv;
			//
			att = \att.kr(0.01);
			rel = \rel.kr(1);
			crv = \crv.kr(-4);
			rate = \rate.kr(1) * \cvrate.kr(1) * freq / 60.midicps;
			bufnum = \bufnum.kr(0);
			begin = \begin.kr(0) * BufFrames.kr(bufnum);
			sig = PlayBuf.ar(1, bufnum, rate, startPos: begin);
			//
			amount = \rumble.kr(0);
			rumble = sig * amount;
			//
			delay = LFNoise2.kr(0.5).range(0.27, 0.51)!6 * sustain;
			rumble = CombL.ar(rumble, 1, delay, sustain * (1 + amount));
			rumble = LPF.ar(rumble.sum, 22);
			sig = LeakDC.ar(sig + rumble);
			//
			sig = Pan2.ar(sig);
			env = Env([0, 1, 1, 0], [att, sustain, rel], crv).kr(2, gate);
			sig * env;
		});

		// some default effect synthdefs

		SynthDef(\id, {
			var sig = In.ar(\in.kr(0), 2) * \gain.kr(1, 1);
			sig = sig * Env.asr(0.5, 1, 0.5, 0).kr(2, \gate.kr(1));
			Out.ar(\out1.kr(0), sig * \gain1.kr(0));
			Out.ar(\out2.kr(0), sig * \gain2.kr(0));
			Out.ar(\out3.kr(0), sig * \gain3.kr(0));
			Out.ar(\out4.kr(0), sig * \gain4.kr(0));
		}).add;

		SynthDef(\gverb, {
			var sig = In.ar(\in.kr(0), 2) * \gain.kr(1, 1);
			sig = GVerb.ar(sig, \roomsize.kr(10), 3, drylevel: 0);
			sig = sig * Env.asr(0.5, 1, 0.5, 0).kr(2, \gate.kr(1));
			Out.ar(\out1.kr(0), sig * \gain1.kr(0));
			Out.ar(\out2.kr(0), sig * \gain2.kr(0));
			Out.ar(\out3.kr(0), sig * \gain3.kr(0));
			Out.ar(\out4.kr(0), sig * \gain4.kr(0));
		}).add;
	}

	*prevfreq { |key, value|
		prevfreq = prevfreq ? Dictionary.new;
		value ?? { ^prevfreq.at(key) };
		prevfreq.put(key, value);
	}

	*postline { |w=38| "".padLeft(w+6, "-").postln }

	*postlist { |list, sep|
		var w = 38, str = ",".ccatList(list).replace(",, ", "");
		sep = sep ? " - ";
		this.postline;
		while { str.size > w } {
			var i = w;
			while { str[i] != $  and: (i > 0) } { i = i - 1 };
			if(i > 0) {
				str.keep(i+1).replace(", ", sep).postln;
				str = str.drop(i+1);
			} {
				str.keep(w).replace(", ", sep).postln;
				str = str.drop(w);
			}
		};
		str.replace(", ", sep).postln;
		this.postline;
	}

	*load { |folder|
		Routine({
			var s = Server.default;
			samples = samples ?? Dictionary.new;
			folder = folder.standardizePath;
			(folder +/+ "*").pathMatch.do({ |map|
				var list = List.new;
				(map +/+ "*.wav").pathMatch.do({ |file|
					list.add(Buffer.read(s, file));
				});
				s.sync;
				samples.put(
					map.basename.withoutTrailingSlash.asSymbol,
					list
				);
			});
			s.sync;
			"Loaded %".format(folder.quote).postln;
			this.sample;
		}).play;
	}

	*sample { |map, index|
		case
		{ samples.isNil } { ^"** no samples **" }
		{ map.isNil } { this.postlist(
			samples.keys.asArray.sort.collect({ |key|
				var val = samples.at(key.asSymbol);
				"% %".format(key, val.size);
			});
		)}
		{ samples.at(map.asSymbol).isNil } {
			^"** unknown map : % **".format(map.asString)
		}
		{ index.isNil } {
			this.postline;
			"map % samples:".format(map.asString.quote).postln;
			samples.at(map.asSymbol).do({ |buf, i|
				"% (% sec) % % %".format(
					(i+1).asString.padLeft(3),
					buf.duration.round(0.01).asString.padLeft(5),
					buf.numChannels,
					buf.bufnum,
					PathName(buf.path).fileName
				).postln;
			});
			this.postline;
		} {
			^samples.at(map.asSymbol).wrapAt(index - 1)
		};
		^"";
	}

	*audit { |map|
		Routine({
			samples.at(map.asSymbol).do({ |buf, i|
				var filename = PathName(buf.path).fileName;
				"% %".format(i.asString.padLeft(3), filename).postln;
				case
				{ buf.numChannels == 2 } {
					{
						PlayBuf.ar(2, buf, doneAction:2);
					} .play
				} {
					{
						Pan2.ar(PlayBuf.ar(1, buf, doneAction:2), 0)
					} .play
				};
				(max(2, buf.duration) * TempoClock.tempo).wait;
			});
		}).play;
	}

	*unload { |folder| // TODO
		var s = Server.default;
		Routine({
			(5 * TempoClock.tempo).wait;
			samples.keys.do { |map|
				samples.at(map).do { |buf| buf.free }
			};
			s.sync;
			samples = nil;
			"..samples unloaded".postln;
		}).play;
	}

	*def { |name, func, variants|
		^SynthDef(name, {
			var sig, env, vel, freq, glide, sus, pf, gate, lpf;

			freq = \freq.kr(60.midicps);
			gate = \gate.kr(1);
			vel = Tidy.adsr("vel", gate, 0, 0.5); // timbre
			sus = \sustain.kr(0); // in seconds

			// glide functionality
			pf = \prevfreq.kr(60.midicps);
			glide = \glide.kr(0);
			freq = Env([pf, freq], glide * sus, \exp).kr(0, gate);

			sig = SynthDef.wrap(func, [], [freq, vel, gate, sus]);

			// gain and mute use kr buses so that you can mute / fade
			// long running synths while they are still playing
			sig = sig * Tidy.adsr("amp", gate, 0, 1); // adsr
			sig = sig * \gain.kr(1); // fade in/out ("gain 0.3:7")
			sig = sig * abs(\mute.kr(0).asInteger.clip(0,1) - 1); // inverted

			lpf = Tidy.adsr("lpf", gate, 0, 20000).clip(20, 20000);
			// key tracking (using velocity too)
			lpf = \kt.kr(0).linlin(0, 1, lpf, max(freq * (0.5+vel), lpf));
			sig = RLPF.ar(sig, lpf, \rq.kr(1).linlin(0, 1, 0.05, 1));

			// sends
			Out.ar(\out1.kr(0), sig * \gain1.kr(0));
			Out.ar(\out2.kr(0), sig * \gain2.kr(0));
			Out.ar(\out3.kr(0), sig * \gain3.kr(0));
			Out.ar(\out4.kr(0), sig * \gain4.kr(0));
		}, variants: variants).add;
	}

	*fx { |name, func|
		^SynthDef(name, {
			var sig = SynthDef.wrap(func, [], [In.ar(\in.kr(0), 2)]);
      // make sure that this node can be released
      sig = sig * Env.asr(0.2, 1, 0.2).kr(2, \gate.kr(1));
      Out.ar(\out1.kr(0), sig * \gain1.kr(0).clip(0, 1));
      Out.ar(\out2.kr(0), sig * \gain2.kr(0).clip(0, 1));
      Out.ar(\out3.kr(0), sig * \gain3.kr(0).clip(0, 1));
      Out.ar(\out4.kr(0), sig * \gain4.kr(0).clip(0, 1));
    }).add;
	}

	*bpm { |bpm|
		bpm !? {
			bpm = bpm.asInteger;
			Routine({
				JSQuant.quantize;
				TempoClock.tempo_(bpm / 60 / 4);
			}).play;
		} ?? { bpm = TempoClock.tempo * 60 * 4 };
		"bpm: % (% cps)".format(bpm, (bpm / 60 / 4).round(0.01)).postln;
	}

	*cps { |cps|
		cps !? {
			cps = cps.asFloat;
			Routine({
				JSQuant.quantize;
				TempoClock.tempo_(cps);
			}).play;
		} ?? { cps = TempoClock.tempo };
		"cps: % (% bpm)".format(cps, (cps * 60 * 4).round(0.01)).postln;
	}

	*quant { |quant|
		quant !? {
			quant = quant.asInteger;
			Routine({
				JSQuant.quantize;
				JSQuant.quant = quant;
			}).play;
		} ?? { quant = JSQuant.quant };
		"quant: %".format(quant.round(0.01)).postln;
	}

	*buffer { |cycles, channels = 1|
		buffers = buffers ? List.new;
		case
		{ cycles.notNil } {
			cycles = cycles.asFloat;
			Routine({
				var s = Server.default;
				var b = Buffer.alloc(
					s,
					cycles * s.sampleRate / TempoClock.tempo,
          channels
				);
				(0.5 * TempoClock.tempo).wait;
				b.clear;
				buffers.add(b);
				buffers.join("\n").postln;
			}).play;
			"audio buffer % cycles".format(cycles);
		} { "audio buffers:\n" ++ buffers.join("\n") }
	}

	*mic { |name, cycles=1, nudge=0|
		this.rec(name, cycles, 2, nudge);
	}

	*rec { |name, cycles, bus, nudge=0|
		recordings = recordings ? Dictionary.new;
		Routine({
			var seconds, buf, old, now, def, s = Server.default;

			case { bus == 2 } { def = \mic } { def = \rec };

			cycles = cycles.asInteger;
			seconds = cycles / TempoClock.tempo; // 1 cycle = 1 beat
			"% % cycles (% seconds)".format(def, cycles, seconds).postln;
			buf = Buffer.alloc(s, seconds * s.sampleRate, 1);
			s.sync;
			"% bufnum %".format(def, buf.bufnum).postln;

			JSQuant.quantize;

			// start synth 1 cycle from now + start countdown now
			// start synth a little later, as input signal will take some
			// time to enter supercollider.
			s.makeBundle(1 / TempoClock.tempo + nudge, {
				Synth(def, [buf: buf, in: bus], nil, \addToTail);
			});

			4.do { |i| "..%".format(4 - i).postln; (1/4).wait };
			"..go!".postln;

			// countup in post window during \rec synth lifetime
			(cycles * 4).do { |i| "--%".format(i+1).postln; (1/4).wait; };

			// store the new recording; free old one if any
			old = recordings.at(name.asSymbol);
			recordings.put(name.asSymbol, buf);
			old !? { |b| b.free };

			"record finished".postln;
		}).play;
	}

	*save { |name, saveasname|
		recordings.at(name.asSymbol) !? { |buffer|
			var path;
			saveasname = saveasname ? name;
			saveasname = saveasname.asString;

			if(saveasname.endsWith(".wav").not) {
				saveasname = saveasname ++ ".wav";
			};

			path = ("~/" ++ saveasname).standardizePath;

			"Writing %% to %".format("\\", name, path.quote).postln;
			buffer.write(path, "wav");
		};
	}

	*end { |seconds=0.02|
		Routine({
			"Tidy stopping..".postln;
			JSTrack.do { |track| track.hush(seconds) };
			(seconds * TempoClock.tempo).wait; // audio fades out
			JSMainloop.stop;
		}).play;
	}

	*solo { |str_or_symbol| JSMute.solo(str_or_symbol) }
	*unsolo { |str_or_symbol| JSMute.unsolo(str_or_symbol) }
	*mute { |str_or_symbol| JSMute.mute(str_or_symbol) }
	*unmute { |str_or_symbol| JSMute.unmute(str_or_symbol) }

	/*
		send MIDICLOCK to some device (24 messages per beat):

		1: \tidy .midiout
		2: connect midi with jack
		3: \clock -- "n 0!96" - "midiclock 1"
	*/
	*midiout {
		Routine({
			// connect with QJackCtl to CH345 device ("OUT" means out)
			MIDIClient.init;
			Server.default.sync;
			Tidy.midi_out = MIDIOut(0);
			"*** midiout ready : connect using jack now ***".postln;
		}).play;
	}
}

