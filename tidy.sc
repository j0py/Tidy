
Tidy {
    classvar samples, buffers, <recordings, prevfreq, abbreviations;
    classvar <>log=0;   // to (de)activate logging
    classvar step_plugins, cycle_plugins, <>midi_out;

    *abbr { |array|
        abbreviations ?? { abbreviations = [].asDict };
        array !? { abbreviations.putAll(array.asDict) };
        ^abbreviations;
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

        this.abbr([ \oct, \octave, \leg, \legato ]);

        // use Event's freq calculator :)
        this.add_step_plugin(\freq, { |track, step|
            step.at(\freq) ?? {
                step.put(
                    \freq,
                    Event.partialEvents.at(\pitchEvent)
                    .copy.putAll(step.dict)
                    .use { ~freq.valueEnvir }
                )
            }
        });

        // HIER

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
        //vel = JSADSR("vel", gate, 0, 0.5); // timbre
        vel = \vel.kr(0.5);
        sus = \sustain.kr(0); // in seconds

        // glide functionality
        pf = \prevfreq.kr(60.midicps);
        glide = \glide.kr(0);
        freq = Env([pf, freq], glide * sus, \exp).kr(0, gate);

        sig = SynthDef.wrap(func, [], [freq, vel, gate, sus]);

        // gain and mute use kr buses so that you can mute / fade
        // long running synths while they are still playing
        sig = sig * \amp.kr(1);
        sig = sig * \gain.kr(1); // fade in/out ("gain 0.3:7")
        sig = sig * abs(\mute.kr(0).asInteger.clip(0,1) - 1); // inverted

        lpf = \lpf.kr(20000).clip(20, 20000);
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

LR2MS {
    *ar { |sig|
        var left=sig[0], right=sig[1];

        ^[left + right, left - right];
    }
}

MS2LR {
    *ar { |sig|
        var mid=sig[0], side=sig[1];

        ^[mid + side / 2, mid - side / 2];
    }
}

// a cycle, containing steps.
JSTidyCycle {
    var <steps, <>env;

    *new { |steps| ^super.new.steps_(steps) }

    // step_array: [ [<trig>,<delta>,<dur>,<str>,<num>], .. ] or [JSTidyStep,..]
    steps_ { |steps_array|
        steps = steps_array.collect { |el|
            if(el.isArray) {
                el = JSTidyStep(el[0], el[1], el[2], el[3], el[4]);
            };
            el;
        };
        this.make_index;
    }

    // TODO move to plugin
    rotate { |rot|
        if(rot != 0) { this.steps_(steps.rotate(rot)) }
    }

    make_index {
        var indexes=[], times=[];
        steps !? {
            steps.do { |step, index|
                indexes = indexes.add(index);
                times = times.add(0);
                indexes = indexes.add(index);
                times = times.add(step.delta);
            };
            times.removeAt(0);
            env = Env(indexes, times);
        }
    }

    // use an Env to get a step index, given a time
    at { |time|
        steps ?? { ^nil };
        env ?? { ^nil };
        ^steps.at(env.at(time));
    }

    printOn { |stream|
        stream << "cycle\n";
        steps !? { steps.do { |step| step.printOn2(stream) } };
    }
}
JSMainloop {
    classvar <mainloop; // the Routine that plays all cycles
    classvar <>shift=0; // wait extra beats once during mainloop

    *start {
        mainloop ?? {
            mainloop = Routine({
                var cycle_number = 0;
                JSControlBus.init(Server.default);
                JSQuant.quantize;
                loop {
                    JSTrack.do { |track|
                        case
                        { track.hushed or: track.once } { }
                        { track.play(cycle_number) }
                    };
                    cycle_number = cycle_number + 1;
                    1.wait;	// 1 beat = 1 cycle
                    if(shift > 0) {
                        "shifted %".format(shift).postln;
                        shift.wait;
                        shift=0;
                    };
                }
            }).play;
            "..mainloop begun".postln;
        };
    }

    *stop { |seconds=0.02|
        mainloop !? { mainloop.stop };
        mainloop = nil;
        "..mainloop ended".postln;
    }
}

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
    of the audio can be sent to effect buses.

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
    /*
    put_sends {
        var send, mix, fx;

        // - "mix f4" - : gains for 0 and fx 1
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
            JSTrack.at(i.asSymbol) !? { |t|
                var index = t.bus.index;

                case
                { fx[i].notNil } {
                    //"i % send % fx % %".format(i, send, index, fx[i]).postln;
                    this.put(("out" ++ send).asSymbol, index);
                    this.put(("gain" ++ send).asSymbol, fx[i]);
                    send = send + 1;
                }
                { mix[i] > 0 } {
                    //"i % send % mix % %".format(i, send, index, mix[i]).postln;
                    this.put(("out" ++ send).asSymbol, index);
                    this.put(("gain" ++ send).asSymbol, mix[i]);
                    send = send + 1;
                }
                {}
            }
        }
    }
    */

    play { |track|
        var tempo = TempoClock.tempo;
        var degrade = dict.at(\degrade) ? 1;

        // this step might not be played after all..
        if(degrade.isKindOf(Bus)) { degrade = degrade.getSynchronous };
        if(degrade.coin.not) { ^this };
        if(trig <= 0) { ^this };

        // this track plays a control bus
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

        this.put(\mute, track.mute_bus.bus.asMap);

        if(track.hushing.not) {
            track.gain_bus.set(
                (this.at(\gain) ? 0.5).asFloat,
                max(0.02, (this.at(\gainsec) ? 0).asFloat)
            );
        };
        this.put(\gain, track.gain_bus.bus);

        if(this.play_midiout) { ^this };

        // for specific values: get them from a bus
        [\degree, \legato].do { |key|
            dict.at(key) !? { |val|
                if(val.isKindOf(Bus)) { 
                    dict.put(key, val.getSynchronous);
                }
            }
        };

        // calculate sustain if it has not been set yet
        this.at(\sustain) ?? {
            var sustainBeats = dur * (dict.at(\legato) ? 0.8);
            dict.put(\sustain, sustainBeats / tempo)
        };

        // asMap buses (before calling any plugins)
        dict = dict.collect { |v| case {v.isKindOf(Bus) } {v.asMap} {v} };

        Tidy.alter_step(track, this); // all plugins do their thing

        dict.at(\def) ?? { "no def".postln; ^this };
        JSMix.putSends(this, track.name);

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
            Server.default.bind { node.set(\gate, 0) };
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
            if(val.isFloat) { val = val.round(0.01) };
            str = "%:% ".format(k, val);
            if((len - (str.size)) < 0) { stream << "\n"; len=width; };
            stream << str;
            len = len - str.size;
        };

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
        node !? { Server.default.bind { node.set(\gate, 0) }; node = nil };

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
            node !? { Server.default.bind { node.set(\gate, 0) }; node = nil };
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
        node !? { Server.default.bind { node.set(\gate, 0) }; node = nil };

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
            old !? { old.set(\gate, 0) };
        };
    }

    // -- "sin 0.2 0 1"
    // -- "noise 1.2 -1 1"
    // HIER
    special_function { |str, name|
        var func = {
            SinOsc.kr(0.1).range(0, 1)
        };
        ^this.function(func, name);
    }

    function { |func, name|
        JSMainloop.start;

        if(type == \control) {
            Server.default.bind {
                node !? { node.set(\gate, 0) };
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
                old !? { Server.default.bind { old.set(\gate, 0) } };
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

JSTidy {
    var <tree; // the root of the tree that is grown
    var cur;   // the current leaf in the tree ("where we are")

    // if you make a mistake, you might get here.
    // printOn will do nothing if tree is nil: current sound keeps playing
    doesNotUnderstand { |selector ... args|
        tree = nil;
        JSTidyException("% not understood".format(selector)).throw;
    }

    // needed for the "stack" and "seq" functions
    -- { |array|
        array.do { |jstidy|	cur.add(jstidy.tree) };

        // the next node must be added AFTER the array of JSTidy trees
        while { cur.parent.notNil.and(cur.is_branch.not) } {
            cur = cur.parent;
        };
    }

    // return JSTidyXX function object. str = "<function name> <pattern>"
    // future: str = "<function name> <pattern1> -- <pattern2>"
    func { |str|
        var func, pat, class;

        str = str.split($ );
        func = str.removeAt(0);
        pat = str.join($ ).stripWhiteSpace;

        case
        // "xyz #1 2 3 4" --> [xyz:1, xyz1:2, xyz2:3, xyz3:4, etc] TODO 
        { pat[0] == $# } {
            ^JSTidyFP_List(func, pat.drop(1).stripWhiteSpace);
        }
        // "xyz /28d5" TODO not ideal
        { pat[0] == $/ } {
            ^JSTidyFP_HexList(func, pat.drop(1).stripWhiteSpace);
        } {
            class = "%%".format(func[0].toUpper, func.drop(1).toLower);
            class = "JSTidyFP_%".format(class).asSymbol.asClass;
            class !? { ^class.new(pat) };
            ^JSTidyFP(func, pat);
        }
    }

    add { |node|
        tree ?? { tree = cur = JSTidyBranch("tree") };
        cur.add(node);
        node.parent = cur;

        if(node.become_cur_after_add) {
            cur = node
        } {
            node.children.do { |child|
                if(child.become_cur_after_add) { cur = child }
            }
        }
    }

    add_func { |str| this.add(this.func(str)) }
    add_branch { |str| this.add(JSTidyBranch(str)) }

    | { |str| this.add_branch("|").add_func(str) }

    |<| {  |str| this.add(JSTidyCombBoth("<").add(this.func(str))) }
    < {  |str| this.add(JSTidyCombBoth("<").add(this.func(str))) }
    |< { |str| this.add(JSTidyCombLeft("<").add(this.func(str))) }
    <| { |str| this.add(JSTidyCombRight("<").add(this.func(str))) }

    |>| {  |str| this.add(JSTidyCombBoth(">").add(this.func(str))) }
    // ">" not possible in SuperCollider Interpreter
    |> { |str| this.add(JSTidyCombLeft(">").add(this.func(str))) }
    >| { |str| this.add(JSTidyCombRight(">").add(this.func(str))) }

    - { |str| this.add(JSTidyCombLeft(">").add(this.func(str))) }

    |+| {  |str| this.add(JSTidyCombBoth("+").add(this.func(str))) }
    + {  |str| this.add(JSTidyCombBoth("+").add(this.func(str))) }
    |+ { |str| this.add(JSTidyCombLeft("+").add(this.func(str))) }
    +| { |str| this.add(JSTidyCombRight("+").add(this.func(str))) }

    |*| {  |str| this.add(JSTidyCombBoth("*").add(this.func(str))) }
    * {  |str| this.add(JSTidyCombBoth("*").add(this.func(str))) }
    |* { |str| this.add(JSTidyCombLeft("*").add(this.func(str))) }
    *| { |str| this.add(JSTidyCombRight("*").add(this.func(str))) }

    |/| {  |str| this.add(JSTidyCombBoth("/").add(this.func(str))) }
    / {  |str| this.add(JSTidyCombBoth("/").add(this.func(str))) }
    |/ { |str| this.add(JSTidyCombLeft("/").add(this.func(str))) }
    /| { |str| this.add(JSTidyCombRight("/").add(this.func(str))) }

    |%| {  |str| this.add(JSTidyCombBoth("%").add(this.func(str))) }
    % {  |str| this.add(JSTidyCombBoth("%").add(this.func(str))) }
    |% { |str| this.add(JSTidyCombLeft("%").add(this.func(str))) }
    %| { |str| this.add(JSTidyCombRight("%").add(this.func(str))) }
}

// \a -- "bin f---8---4-4-----" - "s bd" |* "amp 0.4"
JSTidyFP_Bin : JSTidyNode {

    *new { |pattern|
        var instance = super.new("bin");
        if(pattern.size > 0) { instance.add(JSTidyBinPattern(pattern)) };
        ^instance;
    }

    get { |cycle, name|
        cycle = children.first.get(cycle, name);

        cycle.steps.do { |step|
            step.put(
                \amp,
                (step.at(\str) ? 0).asInteger.linlin(0, 15, 0, 1)
            );
            step.put(\str, nil);
            step.put(\num, nil);
        };

        ^cycle;
    }
}
// chop samples in N parts
// \a -- "chop 4" | "rev" | "s ride"
JSTidyFP_Chop : JSTidyNode {

    *new { |pattern| ^super.new("chop").add(JSTidyPattern(pattern ? "1")) }

    become_cur_after_add { ^true }

    get { |cycle, name|
        var pat, org, time, pq=PriorityQueue.new;

        pat = children.first.get(JSTidyCycle.new, name); // chop cycle
        org = children.last.get(cycle, name); // branch cycle

        time = 0;
        org.steps.do { |step|
            var delta, dur, chop, time2;

            chop = pat.at(time).dict.at(\str).asInteger.clip(1, 96);

            time2 = time;
            time = time + step.delta;

            step.delta_(step.delta / chop);
            step.dur_(step.dur / chop);

            // also set begin and end parameters
            chop.do { |i|
                var step2 = JSTidyStep.copy(step);
                step2.put(\begin, i / chop);
                step2.put(\end, i + 1 / chop);
                pq.put(time2, step2);
                time2 = time2 + step2.dur;
            };

            step.put(\legato, 1); // cut sample after dur
        };

        ^JSTidyCycle(this.steps_from_priority_queue(pq));
    }
}
// the default function
JSTidyFP : JSTidyNode {
    *new { |val, pattern|
        val = Tidy.abbr.at(val.asSymbol) ? val;
        pattern ?? { pattern = "1" };
        if(pattern.size <= 0) { pattern = "1" };
        ^super.new(val).add(JSTidyPattern(pattern))
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
                    { val == "play" } { step.put(\play, str.asSymbol) }
                    { val == "late" } { step.put(\latebeats, str.asFloat) }
                    { val == "degree" } {
                        if("abcdefg".contains(str[0])) {
                            step.put(\midinote, this.string_to_midinote(str))
                        } {
                            step.put(\degree, str.asFloat)
                        }
                    }
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
        //"string_to_control for %".format(str).postln;
        JSTrack.at(str.drop(1).asSymbol) !? { |track| ^track.bus };
        ^nil; // the item in the dictionary of the step will disappear
    }

    // "c3#"
    string_to_midinote { |str|
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

// \a -- "every 8" >| "b 1 2 3 4" | etc; takes action on 7, 15, 23, etc
JSTidyFP_Every : JSTidyNode {
    var <>when, turn;

    *new { |pattern|
        var split = pattern.split($ );
        ^super.new("every")
        .when_(split.at(0).asInteger);
    }

    become_cur_after_add { ^true }

    get { |cycle, name|
        cycle = children.last.get(cycle, name); // should be a JSTidyBranch

        // let your children alter the cycle when it is your turn
        turn = (turn ? -1) + 1; // zero based cycle counter

        if(((turn + 1) % when) == 0) {
            children.drop(-1).do { |child|
                cycle = child.get(cycle, name);
            };
        };

        ^cycle;
    }
}

// use hex number(s) to create structure: - "hex 4026" - "b 0" - "s bd" -
JSTidyFP_Hex : JSTidyNode {

    *new { |pattern|
        var instance = super.new("hex");
        if(pattern.size > 0) { instance.add(JSTidyHexPattern(pattern)) };
        ^instance;
    }

    get { |cycle, name|
        cycle = children.first.get(cycle, name);

        cycle.steps.do { |step|
            step.put(\hex, (step.at(\str) ? step.trig).asString);
            step.put(\str, nil);
            step.put(\num, nil);
        };

        ^cycle;
    }
}
// ~a < "jux 0.6" |> "rev" | ...
// pan original left, altered right
JSTidyFP_Jux : JSTidyNode {
    var <>by;

    *new { |pat|
        var by=0.5;
        if(pat.size > 0, { by = pat.split($ ).at(0).asFloat });
        ^super.new("jux").by_(max(0, min(1.0, by)));
    }

    become_cur_after_add { ^true }

    get { |cycle, name|
        var time, org, alt, steps, pq=PriorityQueue.new;

        org = children.last.get(cycle, name); // from the JSTidyBranch

        // put steps of the org cycle in the PriorityQueue
        time = 0;
        org.steps.do { |step|
            pq.put(time, step.put(\pan, -1 * by));
            time = time + step.delta;
        };

        // calculate steps for the alt cycle
        alt = org.deepCopy;
        children.drop(-1).do { |child| alt = child.get(alt) };

        // put steps of the alt cycle in the PriorityQueue too
        time = 0;
        alt.steps.do { |step|
            pq.put(time, step.put(\pan, by));
            time = time + step.delta;
        };

        ^JSTidyCycle(this.steps_from_priority_queue(pq));
    }
}

// \a -- "def atone" - "adsr #2 20 0.4 500"
// results in floats or buses put into the step
JSTidyFP_List : JSTidyFP {
    var <>str;

    *new { |val, str|
        var instance = super.new(val).str_(str);
        instance.add(JSTidyPattern("1")); // a single step dummy pattern
        ^instance;
    }

    get { |cycle, name|
        cycle = children.first.get(cycle, name);
        cycle.steps.do { |step|
            str.split($ ).do { |substr, i|
                // the first number/str will have key = "<val>"
                // the subsequent number/str will have key = "<val>1", etc
                var key = (case { i > 0 } { (val++i) } { val }).asSymbol;
                if(substr[0] == $=) {
                    step.put(key, this.string_to_control(substr));
                } {
                    step.put(key, substr.asFloat)
                }
            }
        };
        ^cycle;
    }
}

// \a -- "def atone" - "adsr /824d6"
// results in floats (0..1) put into the step
JSTidyFP_HexList : JSTidyFP {
    var <>str;

    *new { |val, str|
        var instance = super.new(val).str_(str);
        instance.add(JSTidyPattern("1")); // a single step dummy pattern
        ^instance;
    }

    get { |cycle, name|
        cycle = children.first.get(cycle, name);
        cycle.steps.do { |step|
            str.do({ |c, i|
                var value = c.digit.linlin(0, 15, 0, 1);
                // the first number/str will have key = "<val>"
                // the subsequent number/str will have key = "<val>1", etc
                var key = (case { i > 0 } { (val++i) } { val }).asSymbol;
                step.put(key, value.asFloat)
            })
        };
        ^cycle;
    }
}


/*
TODO
JSTidyFP_Loopat : JSTidyNode {

    *new { |pattern|
        ^super.new("loopAt").add(JSTidyPattern(pattern ? "1"))
    }

    become_cur_after_add { ^true }

    // set speed so that the steps fit perfectly in the cycle
    get { |cycle, name|
        var pat, org, time;

        pat = children.first.get(JSTidyCycle.new, name); // loopAt cycle
        org = children.last.get(cycle, name); // branch cycle

        time = 0;
        org.steps.do { |step|
            var buf, loopat;

            // this is also done in step.play.. should be done
            // function JSTidy_FP should set step \buf
            step.at(\snd) !? { |bank|
                var index = abs((step.at(\buf) ? 1).asInteger);
                buf = JSTidySamples.buf(bank, index);
            };

            loopat = pat.at(time).dict.at(\str).asInteger.clip(1, 96);
            time = time + step.delta;

            buf !? {
                var dur, speed;
                buf.debug("buf");
                dur = buf.duration.debug("duration") * TempoClock.tempo;
                dur.debug("dur1");
                dur = dur * ((step.at(\end) ? 1) - (step.at(\begin) ? 0));
                dur.debug("dur2");
                speed = dur / step.dur / loopat;
                speed.debug("speed");
                step.put(\speed, speed);
            };
        };

        ^org;
    }
}
*/
// \a -- "off 0.25" |+ "n 7" | ..
// add timeshifted, altered layer
// TODO the shift time should be patternable (e.g. "off <0.25 0.375>" - bla bla)
JSTidyFP_Off : JSTidyNode {
    var <>shift, <>stack;

    *new { |pat|
        ^super.new("off")
        .shift_(max(0, min(1.0, pat.split($ ).at(0).asFloat)));
    }

    become_cur_after_add { ^true }

    get { |cycle, name|
        var time, org, alt, steps, pq=PriorityQueue.new;

        org = children.last.get(cycle, name); // from the JSTidyBranch

        // add steps of org cycle to PriorityQueue
        time = 0;
        org.steps.do { |step|
            pq.put(time, step);
            time = time + step.delta; // slow/fast?
        };

        // add steps of the stack + alt cycle to PriorityQueue
        // TODO: this causes an extra pause when switching trees,
        // which is undesireable!
        stack ?? { stack = [ JSTidyStep(0, shift, shift, "~", 0) ] };
        time = 0;
        stack.do { |step|
            pq.put(time, step);
            time = time + step.delta; // slow/fast?
        };
        stack = [];

        // calculate shifted steps for the alt cycle
        alt = org.deepCopy;
        children.drop(-1).do { |child| alt = child.get(alt, name) };

        // continue with the time from adding stack items to PriorityQueue
        alt.steps.do { |step|
            case
            { time > 0.999 } { stack = stack.add(step) }
            { (time + step.delta) > 0.999 } {
                // split the step and add some silence to the stack
                var d = time + step.delta - 1;
                stack = stack.add(step.deepCopy.delta_(d).trig_(0));
                step.delta_(1 - time); // the duration remains longer
                pq.put(time, step);
            } {
                pq.put(time, step);
            };
            time = time + step.delta;
        };

        ^JSTidyCycle(this.steps_from_priority_queue(pq));
    }
}



// How can "rev" be used in both these sitations?
// \a -- "chop 4" | "rev" | "s ride sn"
// \a -- "jux" - "rev" | "s ride sn"
// answer: log the tree
JSTidyFP_Rev : JSTidyNode {
    *new { |pattern| ^super.new("rev") }

    get { |cycle, name| ^cycle.steps_(cycle.steps.reverse) }
}

// play sub-sequences
JSTidyFP_Seq : JSTidyNode {
    var <>pattern;

    *new { |pattern|
        var instance = super.new("seq");
        instance.pattern_(pattern);
        instance.add(JSTidyPattern(pattern));
        ^instance;
    }

    become_cur_after_add { ^true }

    get { |cycle, name|
        var index;
        var seq; // a queue of steps

        // if you change some parameters in the sequence, and
        // re-evaluate, then a new tree is built in memory.
        // this node object will be replaced by a new one then.
        // so if you then want to continue in the sequence, 
        // you cannot 'remember' where you were in the sequence
        // inside this object. i chose to remember in in the
        // global Library, and that's why i need the track name.
        //
        // if you changed the pattern and then re-evaluate, then
        // the remembered sequence has become invalid, and must
        // be cleared. so i have to remember the pattern string
        // too..
        seq = Library.at(\tidyseq, name.asSymbol, \seq);

        Library.at(\tidyseq, name.asSymbol, \pattern) !? { |pat|
            if(pat != pattern) { seq = nil };
        };
        Library.put(\tidyseq, name.asSymbol, \pattern, pattern);

        // keep a queue of steps of the sequence
        seq ?? { seq = List.new };
        if(seq.size <= 0) {
            seq.addAll(children.first.get(cycle, name).steps)
        };

        // which child branch will deliver the next cycle?
        // remember: your first child is the seq pattern
        // dur/delta of the steps is ignored. we use 1 step for each cycle.
        index = seq.removeAt(0).at(\str).asInteger;
        index = index % (children.size - 1) + 1;

        Library.put(\tidyseq, name.asSymbol, \seq, seq);

        ^children.at(index).get(cycle, name);
    }
}

JSTidyFP_Slice : JSTidyNode {
    var <>count;

    *new { |pattern|
        var instance = super.new("slice");
        var str = pattern.split($ );
        instance.count = max(1, str.removeAt(0).asInteger);
        pattern = str.join($ ).stripWhiteSpace;
        if(pattern.size > 0, { instance.add(JSTidyPattern(pattern)) });
        ^instance;
    }

    get { |cycle, name|
        cycle = children.first.get(JSTidyCycle.new, name);

        cycle.steps.do { |step|
            var slice = (step.at(\str) ? 1).asInteger; // 1 .. <count>

            if(slice <= -1) { step.put(\reversed, 1) };
            slice = abs(slice).clip(1, count);

            step.put(\begin, slice - 1 / count); // 0..1
            step.put(\end, step.at(\begin) + (1/count));

            step.put(\str, nil);
        };

        ^cycle;
    }
}

// mix-play sub-sequences
JSTidyFP_Stack : JSTidyNode {
    *new { |pattern| ^super.new("stack") }

    get { |cycle, name|
        var pq=PriorityQueue.new;

        children.do { |child|
            var time = 0;
            child.get(JSTidyCycle.new, name).steps.do { |step|
                pq.put(time, step);
                time = time + step.delta;
            }
        };

        ^JSTidyCycle(this.steps_from_priority_queue(pq));
    }

    become_cur_after_add { ^true }
}


/*
// chop each step in N identical smaller steps and weave them
JSTidyFP_Striate : JSTidyNode {
    // TODO
    *new { |pattern|
        ^super.new("striate").add(JSTidyPattern(pattern ? "1"))
    }

    become_cur_after_add { ^true }

    get { |cycle, name|
        var pat, size, steps=[];

        pat = children.first.get(JSTidyCycle.new, name);
        size = cycle.steps.size;

        pat.steps.do { |striate|
            var count = striate.dict.at(\str).asInteger.clip(1, 16);
            count.do {
                cycle.steps.do { |step|
                    steps.add(
                        step
                        .copy
                        .delta_(step.delta / striate)
                        .dur_(step.dur / striate)
                    );
                }
            }
        };

        ^JSTidyCycle.new(steps);
    }
}
*/

JSTidyFP_Toscale : JSTidyNode {
    var >scale;

    *new { |pattern|
        var instance = super.new("toscale");
        instance.add(JSTidyPattern("1"));
        ^instance.scale_(pattern.split($ ).collect { |x| x.asInteger });
    }

    get { |cycle, name|
        cycle = children.first.get(cycle, name);
        cycle.steps.do { |step|	
            step.at(\scale) ?? { step.put(\scale, scale) };
        };
        ^cycle;
    }
}

// \a -- "bin ---2--4---5" - "s sn" // = rhythm + velocity
JSTidyBinPattern : JSTidyNode {
    var seq;

    get { |cycle, name|
        var steps;
        seq ?? { seq = JSMNPattern(val) }; // lazy instantiate
        steps = [];
        seq.steps.do { |step|
            var bits, delta, dur, cur;

            delta = step[1] / step[3].size;
            dur = step[2] / step[3].size;

            step[3].do { |c|
                case
                { c == $- }
                {
                    if(cur.isNil) {
                        cur = [0, delta, dur, "~", step[4]]
                    } {
                        cur[1] = cur[1] + delta;
                        cur[2] = cur[2] + delta;
                    }
                }
                {
                    cur !? { steps = steps.add(cur) };
                    cur = [1, delta, dur, c.digit.min(15), step[4]];
                }
            };

            cur !? { steps = steps.add(cur) };
        };

        ^JSTidyCycle(steps);
    }
}


JSTidyBranch : JSTidyNode {
    get { |cycle, name|
        var last = children.last;

        if(last.notNil.and(last.is_branch)) {
            cycle = last.get(cycle, name);
            children.drop(-1).do { |child|
                cycle = child.get(cycle, name)
            };
        } {
            children.do { |child| cycle = child.get(cycle, name) };
        };

        ^cycle;
    }

    // the node added after this node should become a child of this node
    become_cur_after_add { ^true }

    is_branch { ^true }
}
JSTidyCombBoth : JSTidyNode {
    get { |cycle, name|
        ^cycle.steps_(
            this.make_steps(
                cycle.steps.asList,
                children.first.get(cycle, name).steps.asList
            )
        );
    }

    make_steps { |steps1, steps2|
        var step1, step2, steps=List.new;

        steps1 = steps1.asList;
        steps2 = steps2.asList;

        while { (steps1.size > 0) or: (steps2.size > 0) } {

            step1 = step2 = nil;

            if(steps1.size > 0) { step1 = steps1.removeAt(0) };
            if(steps2.size > 0) { step2 = steps2.removeAt(0) };

            case
            { step1.isNil } { steps.add(step2); step2 = nil }
            { step2.isNil } { steps.add(step1); step1 = nil }
            {
                case
                { abs(step1.delta - step2.delta) < 0.001 }
                {
                    this.combine(step1, step2);
                    steps.add(step1);
                }
                { step1.delta < step2.delta }
                {
                    var d = step2.delta - step1.delta;
                    var step2a = step2.deepCopy.delta_(d).dur_(d);
                    steps2.insert(0, step2a);
                    step2.delta = step1.delta;
                    step2.dur = step1.dur;
                    this.combine(step1, step2);
                    steps.add(step1);
                }
                {
                    var d = step1.delta - step2.delta;
                    var step1a = step1.deepCopy.delta_(d).dur_(d);
                    steps1.insert(0, step1a);
                    step1.delta = step2.delta;
                    step1.dur = step2.dur;
                    this.combine(step2, step1);
                    steps.add(step2);
                }
            }
        };

        ^steps;
    }

    combine { |step, stepAt, right|
        stepAt.dict.keysValuesDo { |key, value|
            var stepval = step.at(key);
            case
            { val == ">" }
            { if(right.isNil) { step.put(key, value) } }
            { val == "<" }
            { if(right.notNil) { step.put(key, value) } }
            {
                if(stepval.class == Bus) {
                    stepval = stepval.getSynchronous;
                };
                if(value.class == Bus) {
                    value = value.getSynchronous;
                };

                case
                { val == "+" }
                {
                    if((stepval.isString) or: (value.isString)) {
                        step.put(key, (stepval ? "") ++ (value ? ""));
                    } {
                        step.put(key, (stepval ? 0) + value);
                    }
                }
                { val == "*" } { step.put(key, (stepval ? 1) * value) }
                { val == "/" }
                {
                    if(right.isNil, {
                        if(value == 0, {
                            step.put(key, 0) // division by zero
                        }, {
                            step.put(key, (stepval ? 0) / value)
                        });
                    },{
                        var divider = (stepval ? 0);
                        if(divider == 0, {
                            step.put(key, 0) // division by zero
                        }, {
                            step.put(key, value / divider)
                        });
                    })
                }
                { val == "%" }
                {
                    if(right.isNil, {
                        step.put(key, (stepval ? 0) % value)
                    }, {
                        step.put(key, value % (stepval ? 0))
                    })
                }
                { }
            };
        }
    }
}


// (cycle) |> (child), (/+*%<>)
//
JSTidyCombLeft : JSTidyNode {
    get { |cycle, name|
        var time, child = children.first.get(cycle, name);

        time = 0;
        cycle.steps.do { |step|
            var other, keys;

            other = child.at(time);

            // collect all keys from both sides
            keys = step.dict.keys;
            other.dict.keys.do { |key| keys.add(key) };

            // combine the values for the keys
            keys.do { |key|
                var stepval = step.at(key);
                var otherval = other.at(key);

                case
                { stepval.isNil } { step.put(key, otherval) }
                { otherval.isNil } { }
                // now we know that both are not nil..
                { val == ">" } { step.put(key, otherval) }
                { val == "<" } { }
                {
                    if(stepval.class == Bus) {
                        stepval = stepval.getSynchronous;
                    };
                    if(otherval.class == Bus) {
                        otherval = otherval.getSynchronous;
                    };

                    case
                    { val == "+" } { step.put(key, stepval + otherval) }
                    { val == "*" } { step.put(key, stepval * otherval) }
                    { val == "/" } {
                        if(otherval == 0) {
                            step.put(key, 0)
                        } {
                            step.put(key, stepval / otherval)
                        }
                    }
                    { val == "%" } { step.put(key, stepval % otherval) }
                    { }
                }
            };

            time = time + step.delta;
        };

        ^cycle;
    }
}

// (cycle) >| (child), (/+*%<>)
//
JSTidyCombRight : JSTidyNode {
    get { |cycle, name|
        var time, child = children.first.get(cycle, name);

        time = 0;
        child.steps.do { |step|
            var other, keys;

            other = cycle.at(time);

            // collect all keys from both sides
            keys = step.dict.keys;
            other.dict.keys.do { |key| keys.add(key) };

            keys.do { |key|
                var stepval = step.at(key);
                var otherval = other.at(key);

                case
                { stepval.isNil } { step.put(key, otherval) }
                { otherval.isNil } { }
                // now we know that both are not nil..
                { val == ">" } { }
                { val == "<" } { step.put(key, otherval) }
                {
                    if(stepval.class == Bus) {
                        stepval = stepval.getSynchronous;
                    };
                    if(otherval.class == Bus) {
                        otherval = otherval.getSynchronous;
                    };

                    case
                    { val == "+" } { step.put(key, stepval + otherval) }
                    { val == "*" } { step.put(key, stepval * otherval) }
                    { val == "/" }
                    {
                        if(stepval == 0) {
                            step.put(key, 0) // division by zero
                        } {
                            step.put(key, otherval / stepval)
                        }
                    }
                    { val == "%" } { step.put(key, otherval % stepval) }
                    { }
                }
            };

            time = time + step.delta;
        };

        ^child;
    }
}

JSTidyHexPattern : JSTidyNode {
    var seq;

    get { |cycle, name|
        var steps, last;
        seq ?? { seq = JSMNPattern(val) }; // lazy instantiate
        seq = seq ? JSMNPattern(val); // lazy instantiate
        steps = List.new;
        seq.steps.do { |step|
            var bits, delta, dur, fill;

            // step: [<trig>, <delta>, <dur>, <str>, <num>]
            // str should be a hex string
            bits = step[3].collectAs({|c|
                c.digit.min(15).asBinaryDigits(4)
            }, Array).flatten;

            delta = step[1] / bits.size;
            dur = step[2] / bits.size;

            bits.do { |bit|
                steps.add([bit, delta, dur, "~1"[bit], step[4]]);
            };
        };

        ^JSTidyCycle(steps.asArray);
    }
}

JSTidyNode {
    var <>children, <val, <>parent;

    *new { |val| ^super.newCopyArgs([], val.asString) }

    add { |child|
        children = children.add(child);
        child.parent = this; // @see JSTidy -- operator
    }

    log { |indent=""|
        "%% %".format(indent, this.class, (val ? "").quote).postln;
        children.do { |child| child.log(indent ++ "--") };
    }

    get { |cycle, name| ^cycle }

    become_cur_after_add { ^false }

    is_branch { ^false }

    steps_from_priority_queue { |pq|
        var steps = List.new;
        var time = 0;

        // calculate delta times for all the steps
        while { pq.notEmpty } {
            var next_time, step = pq.pop;
            next_time = pq.topPriority ? 1;
            step.delta = next_time - time;
            time = next_time;
            steps.add(step);
        };

        ^steps.asArray;
    }
}
JSTidyPattern : JSTidyNode {
    var seq;

    get { |cycle, name|
        seq ?? { seq = JSMNPattern(val) }; // lazy instantiate
        ^JSTidyCycle(seq.steps);
    }
}
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
JSTidyException : Exception {
    reportError { this.errorString.postln }
}


JSMix {
    classvar global;
    var mix;

    *initClass { global = Dictionary.new }

    // " <track name> <mix> <track name> <mix> " --> Dictionary
    *setGlobal { |str|
        global.clear;

        (str
            .asString
            .stripWhiteSpace
            .reduce({|a,b|(a++b).replace("  ", " ")}) ? "")
            .split($ )
            .asDict
            .pairsDo { |k,v| global.put(k.asSymbol, v) };

            // set the out/gain parameters of the running nodes on all tracks
            JSTrack.pairsDo { |key, track|
                track.node !? { |node|
                    var n=1;
                    JSMix(global.at(key.asSymbol) ? "f").get.pairsDo { |fx, gain|
                        JSTrack.busIndex(fx) !? { |index|
                            Server.default.bind {
                                node.set(("out"++n).asSymbol, index);
                                node.set(("gain"++n).asSymbol, gain);
                            };
                            n = n + 1;
                        }
                    }
                }
            }
        }

        *putSends { |step, key|
            var mix, hex, n=1;

            hex = (global.at(key.asSymbol) ? "f");
            hex = step.at(\mix) ? hex; // step can override hex
            mix = JSMix(hex).get; // Dictionary

            mix.pairsDo { |k,v| if(v <= 0) { mix.removeAt(k) } };

            10.do { |i| step.at(i.asSymbol) !? { |gain| mix.put(i.asSymbol, gain) } };

            mix.pairsDo { |fx, gain|
                JSTrack.busIndex(fx) !? { |index|
                    step.put(("out"++n).asSymbol, index);
                    step.put(("gain"++n).asSymbol, gain);
                    n = n + 1;
                }
            }
        }

        *args { |key|
            var n=1, args=[];
            JSMix(global.at(key.asSymbol) ? "f").get.pairsDo { |fx, gain|
                JSTrack.busIndex(fx) !? { |index|
                    args = args ++ [("out"++n).asSymbol, index];
                    args = args ++ [("gain"++n).asSymbol, gain];
                    n = n + 1;
                }
            };
            ^args;
        }

        // object

        *new { |hex| ^super.new.init.hex(hex) }

        init { mix = Dictionary.new }

        hex { |hex|
            (hex ? "f").do { |gain, i|
                mix.put(i.asSymbol, gain.digit.linlin(0, 15, 0, 1).asFloat);
            };
        }

        set { |i, gain| mix.put(i.asSymbol, gain.asFloat) }

        get { ^mix }
    }

    // test code
    /*
    m = JSMix("f02a")
    m.set(3, 0.12)
    m.get
    m.get.keys.do { |k| k.class.postln }
    */



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

    JSPlugins {
        var >dict, >list;

        *new { ^super.new.dict_(Dictionary.new).list_(List.new) }

        // the sequence in which plugins are added..
        add { |key, func|
            if(list.includes(key).not) { list.add(key) };
            dict.put(key, func);
        }

        // .. is the sequence with which they are called
        alter { |track, step|
            list.do { |key|	dict.at(key.asSymbol).value(track, step) }
        }

        log { list.postln }
    }

    /* <value> or #peak att dec sus rel curve
    - "lpf 200"
    - "lpf #200 0.1 0.2 0.3 3"
    do NOT supply a kr default if you don't know it;
    this gives a SynthDef a chance to do that!
    */
    JSADSR {
        *new { |name, gate, doneAction=0, default|
            ^Env.adsr(
                (name++1).asSymbol.kr(0),           // attack
                (name++2).asSymbol.kr(0),           // decay
                (name++3).asSymbol.kr(1),           // sustainlevel
                (name++4).asSymbol.kr(0).max(0.1),  // release
                name.asSymbol.kr(default),          // peak
                (name++5).asSymbol.kr(-4)           // curve
            ).kr(doneAction, gate);
        }
    }

    JSQuant {
        classvar <>quant=1;

        *quantize {
            var now = thisThread.beats;
            ((now + quant).div(quant) * quant - now).wait;
        }
    }
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

            // audio or control rate sequence or special function
            { in.isString }
            {
                var split = in.split($ );

                case
                { "sin;noise".contains(split[0]) }
                { JSTrack.atFail(this).special_function(in, this) }
                { ^JSTrack.atFail(this).string(in, this) }
            }

            // audio or fx [mix, func or symbol, args, target]
            { in.isArray }
            { JSTrack.atFail(this).array(in, this) }

            // control / audio rate function
            { in.isFunction }
            { JSTrack.atFail(this).function(in, this) }

            { ^"%% -- <string or func or array>".format("\\", this) };
        }
    }

