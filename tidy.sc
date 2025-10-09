
Tidy {
    classvar samples, buffers, <recordings, abbreviations;
    classvar <>log=0; 
    classvar step_plugins, cycle_plugins, pattern_plugins, <>midi_out;
    classvar <>vosc, <common, <glide;
    classvar global_scale, global_root, global_swing, global_swing_n;

    // all playing patterns will freeze temporarily
    *freeze { |beats| JSMainloop.shift = abs(beats) }

    // playing patterns continue during the mute
    *muteall { |beats| JSMute.muteall(abs(beats)) }

    *scale { |symbol, quantize=true|
        symbol ?? { ^global_scale }; // getter / setter
        Routine({
            if(quantize) { JSQuant.quantize };
            global_scale = Scale.at(symbol) ? Scale.major;
        }).play;
    }

    *root { |integer, quantize=true|
        integer ?? { ^global_root }; // getter / setter
        Routine({
            if(quantize) { JSQuant.quantize };
            global_root = integer;
        }).play;
    }

    *swing { |float, quantize=true|
        float ?? { ^global_swing }; // getter / setter
        Routine({
            if(quantize) { JSQuant.quantize };
            global_swing = float;
        }).play;
    }

    *swing_n { |integer, quantize=true|
        integer ?? { ^global_swing_n }; // getter / setter
        Routine({
            if(quantize) { JSQuant.quantize };
            global_swing_n = integer;
        }).play;
    }

    *abbr { |array|
        abbreviations ?? { abbreviations = [].asDict };
        array !? { abbreviations.putAll(array.asDict) };
        ^abbreviations;
    }

    *add_step_plugin { |key, func|
        step_plugins = step_plugins ? JSPlugins.new;
        step_plugins.add(key, func);
    }

    *alter_step { |step, track|
        step_plugins = step_plugins ? JSPlugins.new;
        step_plugins.alter(step, track)
    }

    *add_cycle_plugin { |key, func|
        cycle_plugins = cycle_plugins ? JSPlugins.new;
        cycle_plugins.add(key, func);
    }

    *alter_cycle { |cycle, track|
        cycle_plugins = cycle_plugins ? JSPlugins.new;
        cycle_plugins.alter(cycle, track)
    }

    *add_pattern_plugin { |key, func|
        pattern_plugins = pattern_plugins ? JSPlugins.new;
        pattern_plugins.add(key, func);
    }

    *alter_pattern { |list_holding_pattern|
        pattern_plugins = pattern_plugins ? JSPlugins.new;
        pattern_plugins.alter(list_holding_pattern)
    }

    *outputs { |bus|
        bus !? { 
            if(bus.rate == \control) {
                ^{ |sig| Out.kr(\out.kr(0), sig) }
            }
        };

        ^{ |sig|
            Out.ar(\out.kr(0), sig * \gain0.kr(1).clip(0, 1));
            Out.ar(\out1.kr(0), sig * \gain1.kr(0));
            Out.ar(\out2.kr(0), sig * \gain2.kr(0));
            Out.ar(\out3.kr(0), sig * \gain3.kr(0));
            Out.ar(\out4.kr(0), sig * \gain4.kr(0));
        }
    }

    *setup {
        // default (overrideable) plugins and synthdefs

        this.abbr([ 
            \oct, \octave, 
            \leg, \legato, 
            \s, \sound, 
            \b, \buf,
            \n, \note,
            \d, \degree,
        ]);

        common = { |sig, freq, vel|
            var lpf;

            // gain and mute use kr buses so that you can mute / fade
            // long running synths while they are still playing
            sig = sig * \amp.kr(1);
            sig = sig * \gain.kr(1); // fade in/out ("gain 0.3:7")
            sig = sig * abs(\mute.kr(0).asInteger.clip(0,1) - 1); // inverted

            // this is rather expensive, don't you think?
            lpf = \lpf.kr(20000);
            lpf = \kt.kr(0).linlin(0, 1, lpf, max(freq * (0.5+vel), lpf));
            sig = LPF.ar(sig, lpf.clip(20, 20000));
        };

        glide = { |freq, sus|
            freq = Lag.kr(
                Select.kr(
                    Line.kr(0, 1, 0.005), 
                    [ \beginfreq.kr(60.midicps), freq ]
                ),
                \glide.kr(0) * sus
            );
        };

        // \a -- "amp sine 8 2 0.3 0.5" - ...
        this.add_pattern_plugin(\sine, { |list_holding_pattern|
            var pattern = list_holding_pattern.at(0);
            if(pattern.find("sine") == 0) {
                var n, sines_per_cycle, mul, add;
                pattern = pattern.split($ );
                n = (pattern.at(1) ? 8).asInteger;
                sines_per_cycle = (pattern.at(2) ? 1).asFloat.round(0.01);
                mul = (pattern.at(3) ? 1).asFloat;
                add = (pattern.at(4) ? 0).asFloat;

                pattern = n.collect { |i|
                    (sin(i*2*pi/n) * mul + add).round(mul/500)
                };
                pattern = "[" ++ pattern.join($ ) ++ "]*" ++ sines_per_cycle;
                pattern.postln;
                list_holding_pattern.put(0, pattern);
            };
        });

        /* using Event's freq calculator :)
        scale       : requested scale, default major [0 2 4 5 7 9 11]
        degree      : indexes into the scale
        note        : indexed note from the scale = semitiones
        octave      : requested octave, default 5
        root        : root note (0 == C)
        octaveRatio : ratio per octave, default 2
        midinote    : note + root / octavesteps + octave - 5 * (12 * octaveRatio.log2) + 60
        freq        : midinote.midicps

        Event.partialEvents.at(\pitchEvent).copy.putAll(
            [degree:13,note:13].asDict
        ).use { ~midinote.valueEnvir }

        so either use \degree and \scale, or use \note
        so either use \note, \root and \octave, or use \midinote
        */
        this.add_step_plugin(\freq, { |step, track|
            step.at(\freq) ?? {
                var scale, root;

                case
                { step.has(\scale) } { scale = Scale.at(step.at(\scale)) }
                { Tidy.scale.notNil } { scale = Tidy.scale };

                case
                { step.has(\root) } { root = step.at(\root) }
                { Tidy.root.notNil } { root = Tidy.root };

                step.put(
                    \freq,
                    Event.partialEvents.at(\pitchEvent)
                    .copy.putAll(step.dict)
                    .put(\scale, scale ? Scale.major)
                    .put(\root, root ? 0)
                    .use { ~freq.valueEnvir }
                )
            }
        });

        // - "sound bd" - "buf 3" - "note d4" - "degree 2"
        // - "s bd" - "b 3" - "n d4" - "d 2"
        // find sample -> set \map, find synthdef -> set \def
        this.add_step_plugin(\sound, { |step, track|
            step.at(\sound) !? { |sound|
                sound = sound.asSymbol;
                case
                { Tidy.hasMap(sound) } { 
                    step.put(\map, sound);
                    step.at(\sound_n) !? { |n| step.put(\map_n, n) };
                }
                { SynthDescLib.at(sound).notNil } { step.put(\def, sound) }
                { "Sound % unknown".format(sound.asString.quote).postln }
            }
        });

        this.add_step_plugin(\sample, { |step, track|
            var buf, rate;

            rate = (step.at(\rate) ? 1) * (step.at(\speed) ? 1);

            step.at(\map) !? { |map|
                var index = (step.at(\buf) ? (step.at(\map_n) ? 1));
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
                var s = Server.default, tempo = TempoClock.tempo;
                var begin, end, duration;

                step.put(\bufchannels, buf.numChannels);
                step.put(\bufnum, buf.bufnum);

                // slice, chop, etc will adjust begin and/or end
                begin = step.at(\begin) ? 0;
                end = step.at(\end) ? 1;

                // play whole sample (duration is in seconds)
                rate = rate * buf.sampleRate / s.sampleRate;
                duration = buf.numFrames / s.sampleRate / rate;
                duration = duration * abs(end - begin);

                if((step.at(\legato) ? 0) > 0.05) {
                    duration = step.at(\legato) * step.dur / tempo;
                };

                // loopat: adjust rate to fit the sample in n cycles
                if((step.at(\loopat) ? 0) > 0) {
                    var loopat = step.at(\loopat); // a number of cycles
                    duration = loopat / TempoClock.tempo;
                    rate = buf.numFrames / (duration * s.sampleRate);
                    duration = duration * abs(end - begin);
                };

                // flip: align reversed sample perfectly to the right
                if((step.at(\flip) ? 0) > 0) {
                    rate = rate * -1;
                    step.put(\begin, end);
                    step.put(\late, max(0, step.delta - (duration * tempo)));
                } {
                    if((step.at(\reversed) ? 0) > 0) {
                        step.put(\begin, end);
                        step.put(\end, begin);
                    }
                };

                step.put(\sustain, duration);
            };

            step.put(\rate, rate);
        });

        // the Vowel quark could be installed, or not
        \Vowel.asClass !? {
            this.add_step_plugin(\vowel, { |step, track|
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

        this.add_step_plugin(\bufnum, { |step, track|
            case
            { step.has(\_function_or_symbol) } { } // do not set \def key!
            { step.has(\bufnum).not } { } // not playing a buffer
            { step.has(\rumble) } {
                step.put(\def, \rumble1);
                if((step[\bufchannels] ?? 1) > 1) {
                    step.put(\def, \rumble2)
                };
            }
            { step.has(\def).not } {
                step.put(\def, \playbuf1);
                if((step[\bufchannels] ?? 1) > 1) {
                    step.put(\def, \playbuf2)
                };
            }
            { };

            /*
            if(step.has(\_function_or_symbol).not) {
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
            }
            */
        });

        this.add_step_plugin(\swing, { |step, track|
            (step.at(\swing) ? Tidy.swing) !? { |swing|
                var count, onset;
                swing = swing.asFloat.clip(0, 1);
                count = (step.at(\swing_n) ? Tidy.swing_n ? 16).asInteger;
                onset = (step.at(\_onset) ? 0).asFloat; // @see JSTrack.play 

                if(((onset*count).round % 2) > 0) { 
                    step.put(\swinglate, swing/count)
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
        this.add_cycle_plugin(\fill, { |cycle, track|
            var last, lastfill, steps = List.new;
            cycle.steps.do { |step|
                var fill = (step.at(\fill) ? 1);

                if(fill.isKindOf(Bus)) { fill = fill.getSynchronous };
                fill = max(1, fill);

                case
                { step.coin > 0 } {
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

        SynthDef(\cv, {
            Env.perc(0, 0.01).kr(2); // short lived..
            Out.kr(\bus.kr(0), \cv.kr(0));
        }).add;

        Tidy.def2(\playbuf2, {
            arg freq, vel, gate, sustain;
            var sig, rate, bufnum, begin, att, rel, crv, loop;
            //
            att = \att.kr(0);
            rel = \rel.kr(0.01);
            crv = \crv.kr(-4);
            loop = \loop.kr(0);
            rate = \rate.kr(1) * \cvrate.kr(1) * freq / 60.midicps;
            bufnum = \bufnum.kr(0);
            begin = \begin.kr(0) * BufFrames.kr(bufnum);
            sig = PlayBuf.ar(2, bufnum, rate, \trig.tr(1), begin, loop);
            sig = LeakDC.ar(sig);
            sig = Balance2.ar(sig[0], sig[1], \pan.kr(0), 3.dbamp);
            sig = sig * Env.asr(att, 1, rel, crv).kr(2, gate);
        });

        Tidy.def(\playbuf1, {
            arg freq, vel, gate, sustain;
            var sig, rate, bufnum, begin, att, rel, crv, env, loop;
            //
            att = \att.kr(0);
            rel = \rel.kr(0.01);
            crv = \crv.kr(-4);
            loop = \loop.kr(0);
            rate = \rate.kr(1) * \cvrate.kr(1) * freq / 60.midicps;
            bufnum = \bufnum.kr(0);
            begin = \begin.kr(0) * BufFrames.kr(bufnum);
            sig = PlayBuf.ar(1, bufnum, rate, \trig.tr(1), begin, loop);
            sig = LeakDC.ar(sig);
            sig = sig * Env.asr(att, 1, rel, crv).kr(2, gate);
        });

        this.def2(\rumble2, {
            arg freq, vel, gate, sustain;
            var sig, rate, bufnum, begin, rumble, env, delay, amount;
            var att, rel, crv;
            //
            att = \att.kr(0);
            rel = \rel.kr(1);
            crv = \crv.kr(-4);
            rate = \rate.kr(1) * \cvrate.kr(1) * freq / 60.midicps;
            bufnum = \bufnum.kr(0);
            begin = \begin.kr(0) * BufFrames.kr(bufnum);
            sig = PlayBuf.ar(2, bufnum, rate, \trig.tr(1), begin);
            //
            amount = \rumble.kr(0);
            rumble = sig.sum * amount;
            delay = LFNoise2.kr(0.5).range(0.27, 0.51)!6 * sustain;
            rumble = CombL.ar(rumble, 1, delay, sustain * (1 + amount));
            rumble = LPF.ar(rumble.sum, 22);
            sig = LeakDC.ar(sig + (rumble!2));
            //
            sig = Balance2.ar(sig[0], sig[1], \pan.kr(0), 3.dbamp);
            env = Env([0, 1, 1, 0], [att, sustain, rel], crv).kr(2, gate);
            sig * env;
        });

        this.def(\rumble1, {
            arg freq, vel, gate, sustain;
            var sig, rate, bufnum, begin, rumble, env, delay, amount;
            var att, rel, crv;
            //
            att = \att.kr(0);
            rel = \rel.kr(1);
            crv = \crv.kr(-4);
            rate = \rate.kr(1) * \cvrate.kr(1) * freq / 60.midicps;
            bufnum = \bufnum.kr(0);
            begin = \begin.kr(0) * BufFrames.kr(bufnum);
            sig = PlayBuf.ar(1, bufnum, rate, \trig.tr(1), begin);
            //
            amount = \rumble.kr(0);
            rumble = sig * amount;
            //
            delay = LFNoise2.kr(0.5).range(0.27, 0.51)!6 * sustain;
            rumble = CombL.ar(rumble, 1, delay, sustain * (1 + amount));
            rumble = LPF.ar(rumble.sum, 22);
            sig = LeakDC.ar(sig + rumble);
            //
            //sig = Pan2.ar(sig);
            env = Env([0, 1, 1, 0], [att, sustain, rel], crv).kr(2, gate);
            sig * env;
        });

        // some default effect synthdefs

        SynthDef(\gverb, {
            var sig = In.ar(\in.kr(0), 2) * \gain.kr(1, 1);
            sig = GVerb.ar(sig, \roomsize.kr(10), 3, drylevel: 0);
            sig = sig * Env.asr(0.5, 1, 0.5, 0).kr(2, \gate.kr(1));
            Out.ar(\out1.kr(0), sig * \gain1.kr(0));
            Out.ar(\out2.kr(0), sig * \gain2.kr(0));
            Out.ar(\out3.kr(0), sig * \gain3.kr(0));
            Out.ar(\out4.kr(0), sig * \gain4.kr(0));
        }).add;

        // make 8 vosc wavetables for the vosc synthdef
        Routine {
            var size = 512;
            var server = Server.default;
            var shapes = [\wel, \sin, \lin];
            var waves = [
                Wavetable.sineFill(size, [1]),
                Env([0, 1, 0, -1, 0], 0.25).asSignal(size).asWavetable, // tri
                //Env([0, 1, -1, 0], [0.5, 0, 0.5]).asSignal(size).asWavetable,
                Env([1,1,-1,-1],[0.5,0,0.5]).asSignal(size).asWavetable, // block
                Env([1, -1], [1]).asSignal(size).asWavetable, // saw
                Signal.newClear(size).overDub(
                    Env([0,1,0,0], [0.4,0.4,0.1], shapes).asSignal(size/2)
                ).reverse.neg.overDub(
                    Env([0,1,0,0], [0.4,0.4,0.1], shapes).asSignal(size/2)
                ).asWavetableNoWrap,
                Signal.newClear(size).overDub(
                    Env([0,1,0,0], [0.1,0.1,0.1], shapes).asSignal(size/2)
                ).reverse.neg.overDub(
                    Env([0,1,0,0], [0.1,0.1,0.1], shapes).asSignal(size/2)
                ).asWavetableNoWrap,
                Signal.newClear(size).overDub(
                    Env([0,1,0,0], [0.12,0.02,0.2], shapes).asSignal(size/2)
                ).reverse.neg.overDub(
                    Env([0,1,0,0], [0.12,0.02,0.2], shapes).asSignal(size/2)
                ).asWavetableNoWrap,
                Signal.newClear(size).overDub(
                    Env([0,1,0,0], [0.12,0.02,0.5], shapes).asSignal(size/2)
                ).reverse.neg.overDub(
                    Env([0,1,0,0], [0.12,0.02,0.5], shapes).asSignal(size/2)
                ).asWavetableNoWrap,
            ];
            Tidy.vosc = Buffer.allocConsecutive(waves.size, server, size*2);
            server.sync;
            waves.do { |wave, i| Tidy.vosc[i].sendCollection(wave) };

            Tidy.def(\vosc, {
                arg freq, vel, gate, sustain;
                var sig, bufnum, count, pos, att, rel;
                //
                att = \att.kr(0.01) * sustain;
                rel = \rel.kr(0.1) * sustain;
                bufnum = Tidy.vosc.first.bufnum;
                count = Tidy.vosc.size;
                pos = (\pos.kr(0) + vel).clip(0, count - 1);
                //pos = vel.linlin(0, 1, bufnum, bufnum + count - 1);
                sig = VOsc.ar(pos, freq);
                sig = LeakDC.ar(sig);
                sig = Pan2.ar(sig, \pan.kr(0));
                sig = sig * Env.asr(att, 1, rel, \crv.kr(-4)).kr(2, gate);
                // mid delay
                sig = LR2MS.ar(sig);
                sig = [DelayC.ar(sig[0], 0.1, 0.0005), sig[1]];
                sig = MS2LR.ar(sig);
                //
                sig;
            });

        }.play;

        JSMainloop.activate;
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
            // find any folder anywhere that contains WAV files
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

    *hasMap { |map| 
        if(samples.isNil) { ^false };
        if(samples.at(map.asSymbol).isNil) { ^false };
        ^true;
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
        var speakers = Server.default.options.numOutputBusChannels;

        "def %".format(name).postln;

        if(speakers == 4) {
            ^SynthDef(name, {
                var sig, env, vel, freq, sus, gate, pd;

                sus = \sustain.kr(0); // in seconds
                freq = \freq.kr(60.midicps);
                freq = SynthDef.wrap(Tidy.glide, [], [freq, sus]);
                gate = \gate.kr(1);
                vel = \vel.kr(0.5);
                pd = \pand.kr(0.8).clip(0, 1);

                sig = SynthDef.wrap(func, [], [freq, vel, gate, sus]); // mono
                sig = SynthDef.wrap(Tidy.common, [], [sig, freq, vel]);
                sig = Pan4.ar(sig, \pan.kr(0) * pd, \pany.kr(0) * pd);
                SynthDef.wrap(Tidy.outputs, [], [sig]);
            }, variants: variants).add;
        };

        // default to stereo
        ^SynthDef(name, {
            var sig, env, vel, freq, sus, gate, lpf;

            sus = \sustain.kr(0); // in seconds
            freq = \freq.kr(60.midicps);
            freq = SynthDef.wrap(Tidy.glide, [], [freq, sus]);
            gate = \gate.kr(1);
            vel = \vel.kr(0.5);

            sig = SynthDef.wrap(func, [], [freq, vel, gate, sus]); // mono
            sig = SynthDef.wrap(Tidy.common, [], [sig, freq, vel]);
            sig = Pan2.ar(sig, \pan.kr(0));
            SynthDef.wrap(Tidy.outputs, [], [sig]);
        }, variants: variants).add;
    }

    // the function delivers a stereo signal already
    *def2 { |name, func, variants|
        "def %".format(name).postln;

        // default to stereo
        ^SynthDef(name, {
            var sig, env, vel, freq, sus, gate, lpf, ratio;
            sus = \sustain.kr(0); // in seconds
            freq = \freq.kr(60.midicps);
            freq = SynthDef.wrap(Tidy.glide, [], [freq, sus]);
            gate = \gate.kr(1);
            vel = \vel.kr(0.5);
            sig = SynthDef.wrap(func, [], [freq, vel, gate, sus]); // stereo
            sig = SynthDef.wrap(Tidy.common, [], [sig, freq, vel]);
            SynthDef.wrap(Tidy.outputs, [], [sig]);
        }, variants: variants).add;
    }

    *fx { |name, func|
        "fx %".format(name).postln;
        ^SynthDef(name, {
            var sig = SynthDef.wrap(func, [], [In.ar(\in.kr(0), 2)]);
            // make sure that this node can be released
            sig = sig * \amp.kr(1);
            sig = sig * \gain.kr(1); // fade in/out ("gain 0.3:7")
            sig = sig * Env.asr(0.2, 1, 0.2).kr(2, \gate.kr(1));
            SynthDef.wrap(Tidy.outputs, [], [sig]);
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
        ^TempoClock.tempo;
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
        ^TempoClock.tempo;
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

    *buffer { |cycles, channels = 2|
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
                s.sync;
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
            JSTrack.do { |track| 
                if(track.type != \fx) { track.hush(seconds) }
            };
            (seconds * TempoClock.tempo).wait; // audio fades out
            JSMainloop.deactivate;
            //JSMainloop.stop;
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

    // step_array: [ [<coin>,<delta>,<dur>,<str>,<num>], .. ] or [JSTidyStep,..]
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

        steps ?? { ^this };
        if(steps.size > 0) {
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
    classvar activator;

    *activate {
        activator ?? {
            activator = {
                JSTrack.initClass;
                JSMainloop.start
            };
            activator.value;
            CmdPeriod.add(activator);
        }
    }

    *deactivate {
        JSMainloop.stop;
        CmdPeriod.remove(activator);
        activator = nil;
    }

    *start {
        JSMainloop.stop;
        //mainloop ?? {
            mainloop = Routine({
                var cycle_number = 0;
                JSControlBus.init(Server.default);
                JSQuant.quantize;
                loop {
                    JSTrack.do { |track| track.play_cycle(cycle_number) };
                    cycle_number = cycle_number + 1;
                    1.wait; // 1 beat = 1 cycle
                    if(shift > 0) {
                        "shifted %".format(shift).postln;
                        shift.wait;
                        shift=0;
                    };
                }
            }).play;
            "..mainloop start".postln;
        //};
    }

    *stop { |seconds=0.02|
        mainloop !? { mainloop.stop };
        mainloop = nil;
        "..mainloop stop".postln;
    }
}

JSTidyStep {
    var <>coin, <>delta, <>dur, <>dict;
    //    var <>note; // the note that this step is playing

    *new { |coin, delta, dur, str, num|
        ^super.newCopyArgs(coin ? 0, delta ? 1, dur ? 1)
        .dict_(Dictionary.new)
        .put(\str, str)
        .put(\num, num);
    }

    *copy { |step, coin, delta, dur, str, num|
        ^JSTidyStep(
            coin ? step.coin,
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

    asPairs { ^dict.asPairs }

    removeAt { |key|
        var val = this.at(key.asSymbol);
        dict.removeAt(key.asSymbol);
        ^val;
    }

    mapbuses {
        dict = dict.collect { |v| case {v.isKindOf(Bus) } {v.asMap} {v} };
    }

    should_play { |track|
        var degrade = dict.at(\degrade) ? 1;

        // this step might not be played after all..
        if(degrade.isKindOf(Bus)) { degrade = degrade.getSynchronous };
        if((degrade * coin).coin.not) { ^false };
        if(JSMute.should_mute(track)) { ^false };
        ^true;
    }

    log {
        if((Tidy.log == \step) or: ((dict.at(\log) ? 0) > 0)) {
            this.postln;
        }
    }

    printOn { |stream|
        var width=40;
        var len=width;

        stream << "step\ncoin:% delta:% dur:%\n".format(
            coin.round(0.01),
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
            coin.round(0.01).asString.padLeft(4),
            delta.round(0.01).asString.padLeft(6),
            dur.round(0.01).asString.padLeft(6)
        );

        dict.keysValuesDo { |k,v| stream << "%:%,".format(k,v) };
        stream << "\n";
    }
}

JSTrack : JSTidy {
    classvar tracks;

    var <>gain_bus, <>mute_bus, last_mute;
    var queue, curtree, newtree;
    var <hushed=false, <hushing=false, <once=false;
    var <>bus, <type, name;
    var to_launch, synth;
    var <>mono_steps, mono_synth, <>last_freq;
    var debug_on=false;

    *initClass { tracks = Dictionary.new }

    *do { |func| tracks.do { |track| func.(track) } }

    *pairsDo { |func| tracks.pairsDo { |name, track| func.(name, track) } }

    *at { |name| ^tracks.at(name.asSymbol) }

    *has { |name| ^tracks.includesKey(name.asSymbol) }

    *atFail { |name, type|
        var track;
        name = name.asSymbol;
        tracks.at(name) !? { |track| ^track };
        "new track %%, type %%".format("\\", name, "\\", type).postln;
        tracks.put(name, track = JSTrack.new.init(name, type));
        ^track;
    }

    init { |aname, atype|
        var server = Server.default;
        name = aname;
        type = atype;
        case
        { type == \audio }
        {
            gain_bus = JSControlBus("gain");
            mute_bus = JSControlBus("mute");
            bus = Bus.audio(server, 2);
            mono_steps = List.new;
        }

        { type == \control }
        { bus = Bus.control(server, 1) }

        { type == \fx }
        { 
            bus = Bus.audio(server, 2)
        }
    }

    launch { |function_or_symbol| 
        to_launch = function_or_symbol;
        if(debug_on) { "set function / synthdef to launch".postln };
    }

    //debug { |on=true| debug_on = on }

    debug { |on|
        case
        { debug_on.isNil } { debug_on = (on ? true) } // default true
        { on.isNil } { debug_on = debug_on.not }      // toggle true/false
        { debug_on = on }                             // or set
    }

    // after hush, you must evaluate the track in the interpreter to re-start
    hush { |seconds=0.02|
        if(hushing.not) {
            gain_bus !? { gain_bus.reset(seconds) };
            hushing = true;
        };
        Routine({
            (seconds * TempoClock.tempo).wait;
            hushed = true; // new cycles will not play
            this.release_synth(Server.default);
            this.release_mono_synth(Server.default, 6);
            queue = List.new;
            context = nil;
            status = \idle;
        }).play;
    }

    // the interpreter is done evaluating and wants to post something
    printOn { |stream|
        case
        { status == \error }
        {
            "%: status error".format(name).postln;
            tree = nil; // let's try again
        }
       
        { status == \idle }
        {
            // only a function or symbol set: supply default tree
            // @see Symbol -- implementation
            this.status_(\build).add_leaf("_function_or_symbol 1");
        };

        if(status == \build) {
            // feedback to user
            if(Tidy.log == \tree) { tree.log };
            "%% pattern".format("\\", name).printOn(stream);

            // preparations
            once = false; // true if only one cycle needs to be played
            status = \new;
            //context = nil; // a new tree should start with a new context
            newtree = tree; // from now on, the sequencer can grab the new tree
            tree = nil; // ready for next evaluation
            hushed = false;
            hushing = false;

            //JSMainloop.start;
        }
    }

    play_cycle { |cycle_number|
        var steps = [], delta = 1, routine;

        routine = Routine({
            if((cycle_number % JSQuant.quant) == 0) {
                if(curtree != newtree) { queue = nil };
                curtree = newtree; // switch trees in quantized manner
                if(debug_on) { "switch trees" };
            };
            curtree !? {
                queue = queue ? List.new;

                while { delta > 0.0001 } {
                    var step, clone, slow, fast;

                    if(queue.size <= 0) {
                        var rot, cycle;
                        cycle = curtree.get(JSTidyCycle.new);
                        // you can do this in a cycle plugin
                        cycle.steps.do({ |x| rot ?? rot = x.at(\rot) });
                        cycle.rotate(rot ? 0);
                        Tidy.alter_cycle(cycle, this);
                        if(Tidy.log == \cycle) { cycle.postln };
                        queue.addAll(cycle.steps);
                    };

                    if(queue.size <= 0) {
                        delta = 0; // stop this loop
                    } {
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
                    }
                };

                delta = 0;
                steps.do({ |step|
                    // a step needs to know where in the cycle it starts    
                    step.put(\_onset, delta);
                    delta = delta + step.delta;
                    this.play_step(step);
                    if((step.at(\once) ? 0) > 0) { 
                        if(once.not) { "once".postln };
                        once = true; 
                    };
                    step.delta.wait;
                });
            };
        });

        if(hushed.not and: once.not) { routine.play }
    }

    play_step { |step|
        var tempo = TempoClock.tempo;
        var server = Server.default;

        if(step.should_play(this).not) { ^this };

        if(debug_on) { "play step".postln };

        mute_bus !? { step.put(\mute, mute_bus.bus.asMap) };

        // if hushing, sequenced steps may not set gain bus
        if(gain_bus.notNil and: hushing.not) {
            gain_bus.set(
                (step.at(\gain) ? 1).asFloat,
                max(0.02, (step.at(\gain_n) ? 0).asFloat)
            );
        };

        gain_bus !? { step.put(\gain, gain_bus.bus) };

        // for specific values: get them from a bus
        [\degree, \note, \root, \legato].do { |key|
            step.at(key) !? { |val|
                if(val.isKindOf(Bus)) { 
                    step.put(key, val.getSynchronous);
                }
            }
        };

        step.mapbuses;

        Tidy.alter_step(step, this); // all plugins do their thing

        // calculate sustain if it has not been set yet
        step.at(\sustain) ?? {
            step.put(\sustain, step.dur * (step.at(\legato) ? 0.8) / tempo)
        };

        step.put(\beginfreq, last_freq ? step.at(\freq));
        last_freq = step.at(\freq);
           
        Routine({
            var late, server = Server.default;
            var sustain = step.at(\sustain);
            var args;
           
            args = this.map_sends_and_buses(step.dict.asPairs);

            if((step.at(\log) ? 0) > 0) { 
                (args ++ [\delta, step.delta, \dur, step.dur]).debug("step")
            };

            late = (step.at(\late_n) ? 0) + (step.at(\latems) ? 0) /1000*tempo;
            late = late + (step.at(\late) ? 0) + (step.at(\swinglate) ? 0);
            if(late > 0) { late.wait };

            case
            { type == \tidy }
            {
                if(debug_on) { "play \\tidy step".postln };
                if(step.has(\root)) { Tidy.root(step.at(\root), false) };
                if(step.has(\scale)) { Tidy.scale(step.at(\scale), false) };
                if(step.has(\swing)) { Tidy.swing(step.at(\swing), false) };
                if(step.has(\swing_n)) { Tidy.swing_n(step.at(\swing_n), false) };
            }

            { (type == \control) and: step.has(\cv) }
            { 
                if(debug_on) { "play \\cv step".postln };
                this.release_synth(server);
                this.release_mono_synth(server, 1);
                server.bind { Synth(\cv, [bus: bus, cv: step.at(\cv).asFloat]) }
            }

            { to_launch.notNil }
            {
                var old, addAction, to_launch_copy;

                to_launch_copy = to_launch;
                to_launch = nil;

                old = synth;

                case
                { (type == \fx) and: old.notNil }
                { addAction = \addBefore }
                { addAction = \addToHead };

                if(to_launch_copy.isFunction) {
                    if(debug_on) { 
                        "launch function".postln;
                        "old = %".format(old).postln;
                        "addAction = %".format(addAction).postln;
                    };
                    server.bind {
                        var in_func;
                        if(type == \control) {
                            in_func = { |i, c| In.kr(i, c) }
                        } {
                            in_func = { |i, c| In.ar(i, c) }
                        };

                        synth = {
                            var sig, in;
                           
                            in = SynthDef.wrap(
                                in_func,
                                [],
                                [bus.index, bus.numChannels]
                            );

                            sig = SynthDef.wrap(
                                to_launch_copy,
                                [],
                                [in]
                            );

                            // diy release because sc uses doneAction 0
                            sig = sig * Env.asr(0.05, 1, 0.05).kr(2, \gate.kr(1));
                            sig = sig * \amp.kr(1) * \gain.kr(1);
                            sig = sig * abs(\mute.kr(0).asInteger.clip(0,1) -1); 
                            SynthDef.wrap(Tidy.outputs(bus), [], [sig]);
                        }.play(
                            target: if(addAction == \addBefore, old, nil),
                            addAction: addAction,
                            outbus: 0, // not used because i use Out ugen
                            fadeTime: 0.02, // not used because i use own Env
                            args: args
                        );
                    }
                } {
                    if(debug_on) { "launch synthdef (old = %)".format(old ? "(nil)").postln };
                    server.bind {
                        synth = Synth(
                            to_launch_copy, 
                            args,
                            if(addAction == \addBefore, old, nil),
                            addAction
                        );
                    };
                };

                this.release_target(old, server);
            }

            { step.has(\def) }
            {
                var def = step.at(\def).asSymbol;
                if(SynthDescLib.at(def).isNil) {
                    if(hushing.not) { "def % unknown".format(def).postln }
                } {

                    if(debug_on) { "play note".postln };
                    this.release_synth(server);
                    this.play_note(def, step, sustain, server, args, tempo);
                }
            } 
            
            { synth.notNil }
            {
                args = args ++ [
                    \freqlag, (step.at(\glide) ? 0) * sustain,
                    //\trig, 1, // PlayBuf2 will restart if retriggered!
                ];

                if(debug_on) { "set params: %".format(args).postln };

                server.bind { synth.performList(\set, args) }
            }

            { "no def and no synth".postln }
        }).play;
    }
    
    release_target { |target, server|
        // sometimes release is fast, and as target is used while
        // starting the new synth, we may get an error if the target
        // has already been released and is gone.
        // this is why the release of the target is postponed a bit.
        Routine {
            0.1.wait;
            if(debug_on) { "release target %".format(target ? "(nil)").postln };
            target !? { server.bind { target.set(\gate, 0) } };
        } .play
    }

    release_synth { |server|
        synth !? { 
            if(debug_on) { "release synth %".format(synth).postln };
            server.bind { synth.set(\gate, 0); synth = nil };
        };
    }

    release_mono_synth { |server, n|
        mono_synth !? { 
            server.bind { mono_synth.set(\gate, 0); mono_synth = nil };
            if(debug_on) { "release mono synth %".format(n).postln };
        };
    }

    play_note { |def, step, sustain, server, args, tempo|
        var polynote; // played polyphonic note

        case
        { (step.at(\mono) ? 0) > 0 }
        {
            case
            { status == \new }
            {
                // if you (re)evaluate then the mono synth must be replaced
                this.release_mono_synth(server, 5);
                server.bind { mono_synth = Synth(def, args) };
                if(debug_on) { "launch mono synth*".postln };
                status = \run;
            }
            { mono_steps.isEmpty }
            {
                server.bind { mono_synth = Synth(def, args) };
                if(debug_on) { "launch mono synth".postln };
            } {
                // set params on the mono synth
                server.bind {
                    mono_synth.performList(\set, step.dict);
                    mono_synth.set(\trig, 1);
                    if(debug_on) { "mono synth set freq".postln };
                }
            };
            mono_steps.add(step);
        }
        {
            server.bind { polynote = Synth(def, args) };
            if(debug_on) { "launch poly synth %".format(polynote).postln };
        };

        if(debug_on) { "wait % seconds".format(sustain).postln };
        (sustain * tempo).wait; // in beats

        case
        { (step.at(\mono) ? 0) > 0 }
        {
            if(mono_steps.size == 1) { this.release_mono_synth(server, 3) };
            mono_steps.remove(step);
        }
        { 
            server.bind { polynote.set(\gate, 0) };
            if(debug_on) { "poly synth set gate 0 %".format(polynote).postln };
        }
    }

    // sends: - "> 0.2" - / - ">reverb 0.1" -
    // buses: - "rel =krx" -
    map_sends_and_buses { |args|
        var newargs = Dictionary.new;
        var sends = Dictionary.new;
        var n;

        args = (args ? []).asDict;

        if(args.includesKey(\in).not) {
            bus !? { args.put(\in, bus.index) }; // your own bus
        };

        // analyze \mix value (assumes Fx names 1,2,3 etc)
        args.at(\mix) !? { |hex|
            hex.do { |gain, i|
                var index;
                if(i <= 0) {
                    index = 0;
                } {
                    JSTrack.at(i) !? { |t| index = t.bus.index }
                };
                index !? {
                    sends.put(index, gain.digit.linlin(0, 15, 0, 1).asFloat);
                }
            };
        };

        args.keysValuesDo { |k, v|
            if(v.asString.at(0) == $=) {
                JSTrack.at(v.asString.drop(1).asSymbol) !? { |t| 
                    v = t.bus.asMap
                };
            };

            case
            { k.asString.at(0) == $> } {
                if(k.asString.size > 1) {
                    JSTrack.at(k.asString.drop(1).asSymbol) !? { |t|
                        sends.put(t.bus.index, v);
                    }
                } {
                    sends.put(0, v);
                }
            } {
                newargs.put(k, v);
            }
        };

        n = 1;
        sends.keysValuesDo { |k, v|
            if(k == 0) {
                newargs.put(\out, 0);
                newargs.put(\gain0, v); // override the default value of 1
            } {
                newargs.put(("out"++n).asSymbol, k);
                newargs.put(("gain"++n).asSymbol, v);
                n = n + 1;
            }
        };

        if(type == \control) { newargs.put(\out, bus.index) };

        ^newargs.asPairs;
    }

    // set params on running synth
    // string format: "<param> <value> <param> <value> .."
    // TODO maybe quantize this..
    params { |input|
        synth !? {
            var param, server = Server.default;
            input.split($ ).do { |value, i|
                if((i % 2) <= 0) {
                    param = value.asSymbol
                } {
                    if(value[0] == $=) {
                        value = value.drop(1).asSymbol;
                        JSTrack.at(value) !? { |t| 
                            server.bind { synth.set(param, t.bus.asMap) }
                        }
                    } {
                        server.bind { synth.set(param, value.asFloat) }
                    } 
                }
            }
            ^"[%]".format(input);
        };
        ^"no synth";
    }

    // reason for mute buses: it mutes running synths.
    // the last_mute variable is cheaper than mute_bus.getSynchronous.
    set_mute_bus { |new_mute|
        if(new_mute == last_mute) { ^this };
        mute_bus !? { mute_bus.set(new_mute) }; // 0 or 1
        last_mute = new_mute;
    }
}

JSTidy {
    var <tree; // the root of the tree that is grown
    var cur;   // the current leaf in the tree ("where we are")
    var context; // context for more persistent storage
    
    // \idle  : just created or activated
    // \build : building the tree
    // \error : something went wrong: do not use the tree
    // \new   : a new tree has been set for the mainloop to run
    // \run   : mainloop is running the tree
    var <>status = \idle;


    // if you make a mistake, you might get here.
    // printOn will do nothing if status is \error: current sound keeps playing
    doesNotUnderstand { |selector ... args|
        status = \error;
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

    // return JSTidyXX object. str = "<function name> <pattern>"
    // future: str = "<function name> <pattern1> -- <pattern2>"
    mkleaf { |str|
        var val, pat, class;

        context = context ? Dictionary.new;

        str = str.split($ );
        val = str.removeAt(0);
        pat = str.join($ ).stripWhiteSpace;

        pat = [pat].asList;
        Tidy.alter_pattern(pat);
        pat = pat.at(0);

        case
        { pat[0] == $# }
        { ^JSTidyFP_List(val, pat.drop(1).stripWhiteSpace) }
        {
            class = "%%".format(val[0].toUpper, val.drop(1).toLower);
            class = "JSTidyFP_%".format(class).asSymbol.asClass;
            class !? { ^class.new(pat, context) };
            ^JSTidyFP(val, pat);
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

    add_leaf { |str| this.add(this.mkleaf(str)) }
    add_branch { |str| this.add(JSTidyBranch(str)) }

    | { |str| this.add_branch("|").add_leaf(str) }

    |<| {  |str| this.combine("<", \both, str) }
    < {  |str| this.combine("<", \both, str) }
    |< {  |str| this.combine("<", \left, str) }
    <| {  |str| this.combine("<", \right, str) }

    |>| {  |str| this.combine(">", \both, str) }
    // ">" not possible in SuperCollider Interpreter
    |> { |str| this.combine("<", \left, str) }
    >| { |str| this.combine("<", \right, str) }

    - { |str| 
        //"% - %".format(cur.val, str).postln;
        this.combine(">", \left, str);
    }

    |+| {  |str| this.combine("+", \both, str) }
    + {  |str| this.combine("+", \both, str) }
    |+ { |str| this.combine("+", \left, str) }
    +| { |str| this.combine("+", \right, str) }

    |*| {  |str| this.combine("*", \both, str) }
    * {  |str| this.combine("*", \both, str) }
    |* { |str| this.combine("*", \left, str) }
    *| { |str| this.combine("*", \right, str) }

    |/| {  |str| this.combine("/", \both, str) }
    / {  |str| this.combine("/", \both, str) }
    |/ { |str| this.combine("/", \left, str) }
    /| { |str| this.combine("/", \right, str) }

    |%| {  |str| this.combine("%", \both, str) }
    % {  |str| this.combine("%", \both, str) }
    |% { |str| this.combine("%", \left, str) }
    %| { |str| this.combine("%", \right, str) }

    combine { |operation, direction, str|
        cur.lastchild !? { |child| 
            if(child.val == "_function_or_symbol") { direction = \right }
        };

        case
        { direction == \both }
        { this.add(JSTidyCombBoth(operation).add(this.mkleaf(str))) }
        { direction == \left }
        { this.add(JSTidyCombLeft(operation).add(this.mkleaf(str))) }
        { direction == \right }
        { this.add(JSTidyCombRight(operation).add(this.mkleaf(str))) }
        { };
    }
}

// chop samples in N parts (set \begin and \end)
// \a -- "chop 4" | "rev" | "s ride"
JSTidyFP_Chop : JSTidyNode {

    *new { |pattern| ^super.new("chop").add(JSTidyPattern(pattern ? "1")) }

    become_cur_after_add { ^true }

    get { |cycle|
        var pat, org, time, steps;

        pat = children.first.get(JSTidyCycle.new); // chop cycle
        org = children.last.get(cycle); // branch cycle
        steps = List.new;

        time = 0;
        org.steps.do { |step|
            var chop;
           
            chop = pat.at(time).dict.at(\str).asInteger.clip(1, 96);
            time = time + step.delta;

            step.delta_(step.delta / chop);
            step.dur_(step.dur / chop);

            chop.do { |i|
                var step2 = JSTidyStep.copy(step);
                step2.put(\begin, i / chop);
                step2.put(\end, i + 1 / chop);
                steps.add(step2);
            };
        };

        ^JSTidyCycle(steps.asArray);
    }
}

JSTidyFP : JSTidyNode {
    *new { |val, pattern|
        val = Tidy.abbr.at(val.asSymbol) ? val;
        // check if pattern must be interpreted from a string
        // store the pattern in this object
        pattern ?? { pattern = "1" };
        if(pattern.size <= 0) { pattern = "1" };
        ^super.new(val).add(JSTidyPattern(pattern))
    }

    get { |cycle|
        // interpret the stored string to a pattern string
        // if this pattern differs from the stored pattern, then
        // replace your child with a new JSTidyPattern(pattern)
        // store the new pattern

        // return a cycle with value from your pattern filled in for val
        cycle = children.first.get(cycle);

        cycle.steps.do { |step|

            step.at(\str) !? { |str|
                case
                { str[0] == $= }
                { step.put(val.asSymbol, this.string_to_control(str)) }
                {
                    // interpret str depending on val (or str itself)
                    case
                    { str == "~" } { step.coin = 0 }
                    { val == "buf" } { step.put(\buf, str.asInteger) }
                    { val == "map" } { step.put(\map, str.asSymbol) }
                    { val == "mix" } { step.put(\mix, str.asString) }
                    { val == "sound" } { step.put(\sound, str.asString) }

                    { val == "def" } { step.put(\def, str.asSymbol) }
                    { val == "note" } {
                        if("abcdefg".contains(str[0])) {
                            step.put(\midinote, this.string_to_midinote(str))
                        } {
                            step.put(\note, str.asFloat)
                        }
                    }
                    { val == "scale" } { step.put(\scale, str.asSymbol) }
                    { step.put(val.asSymbol, str.asFloat) };
                };
            };

            step.at(\num) !? { |n| 
                step.put((val++"_n").asSymbol, n.asInteger)
            };

            step.put(\str, nil);
            step.put(\num, nil);
        };

        ^cycle;
    }

    // "=xxx" : value comes from the controlbus of xxx
    string_to_control { |str, step|
        JSTrack.at(str.asString.drop(1).asSymbol) !? { |track| ^track.bus };
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
        ) + ((octave + 1) * 12) + add; // - 60;
    }
}

// Duplicate (like stut) N M 
// N = number of duplicates (1..)
// M = number of duplicates in 1 cycle (1/length of 1 duplicate in cycles)
// (i'd rather type "16" than "0.0625" and i cannot yet support "(1/16)".
// \a -- "dup 4 16" |* "speed 0.5" | "s bd sn"
// maybe i can make things patternable too, or fluid
JSTidyFP_Dup : JSTidyNode {
    var <>n=1, <>m=1, >context;

    *new { |pattern, context| 
        var instance = super.new("dup").context_(context);
        pattern = pattern ? "1 1";
        pattern = pattern.split($ );
        if(pattern.size > 0) {
            instance.n = pattern[0].asInteger;
            pattern = pattern.drop(1);
        };
        if(pattern.size > 0) {
            instance.m = pattern[0].asFloat;
            pattern = pattern.drop(1);
        };
        ^instance;
    }

    become_cur_after_add { ^true }

    get_1 { |cycle|
        var time, subtime, pq, steps, pq2;
        //
        cycle = children.last.get(cycle); // branch cycle
        children.drop(-1).do { |child| cycle = child.get(cycle) };
        //
        pq = context.at(\dup) ? PriorityQueue.new;
        //
        time = 0;
        cycle.steps.do { |step|
            // could fetch m from pattern here
            subtime = time;
            n.do { |i|
                pq.put(subtime, step.deepCopy);
                subtime = subtime + (1/m);
            };
            time = time + step.delta;
        };

        // take steps for the current cycle out of the PriorityQueue
        steps = List.new;
        time = pq.topPriority ? 1;
        while { pq.notEmpty and: (time <= 0.999) } {
            var top, step = pq.pop;
            if(steps.size <= 0) { steps.add(JSTidyStep.rest(time)) };
            top = pq.topPriority ? 1;
            steps.add(step.delta_(top - time));
            time = top;
        };

        if(steps.size <= 0) { steps.add(JSTidyStep.rest(1)) };

        // shift entire priorityqueue 1 cycle
        pq2 = PriorityQueue.new;
        while { pq.notEmpty } {
            var top = pq.topPriority;
            pq2.put(max(0, top - 1), pq.pop);
        };

        // store for next call to get()
        context.put(\dup, pq2);

        ^JSTidyCycle(steps.asArray);
    }

    get { |cycle|
        var time, pq, steps, pq2, cycles, shift;

        pq = context.at(\dup) ? PriorityQueue.new;

        // make n cycles, but not shifted in time yet
        cycles = List.new;
        cycles.add(cycle = children.last.get(cycle)); // branch cycle
        (n-1).do {
            cycle = cycle.deepCopy;
            children.drop(-1).do { |child| cycle = child.get(cycle) };
            cycles.add(cycle);
        };

        // put all cycles in the PriorityQueue, shifted in time
        shift = 0;
        cycles.do { |cycle|
            time = 0;
            cycle.steps.do { |step|
                pq.put(time + shift, step);
                time = time + step.delta;
            };
            shift = shift + (1/m);
        };

        // take steps for the current cycle out of the PriorityQueue
        steps = List.new;
        time = pq.topPriority ? 1;
        while { pq.notEmpty and: (time <= 0.999) } {
            var top, step = pq.pop;
            if(steps.size <= 0) { steps.add(JSTidyStep.rest(time)) };
            top = pq.topPriority ? 1;
            steps.add(step.delta_(top - time));
            time = top;
        };

        if(steps.size <= 0) { steps.add(JSTidyStep.rest(1)) };

        // shift entire priorityqueue 1 cycle
        pq2 = PriorityQueue.new;
        while { pq.notEmpty } {
            var top = pq.topPriority;
            pq2.put(max(0, top - 1), pq.pop);
        };

        // store for next call to get()
        context.put(\dup, pq2);

        ^JSTidyCycle(steps.asArray);
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

    get { |cycle|
        cycle = children.last.get(cycle); // should be a JSTidyBranch

        // let your children alter the cycle when it is your turn
        turn = (turn ? -1) + 1; // zero based cycle counter

        if(((turn + 1) % when) == 0) {
            children.drop(-1).do { |child|
                cycle = child.get(cycle);
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

    get { |cycle|
        cycle = children.first.get(cycle);

        cycle.steps.do { |step|
            step.put(\hex, step.at(\str).asString);
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

    get { |cycle|
        var time, org, alt, steps, pq=PriorityQueue.new;

        org = children.last.get(cycle); // from the JSTidyBranch

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
        instance.add(JSTidyPattern("1")); // one step per cycle
        ^instance;
    }

    get { |cycle|
        cycle = children.first.get(cycle);
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

// divide the cycle over N cycles
JSTidyFP_Loopat : JSTidyNode {
    var stack;

    *new { |pattern|
        // you should only use patterns like <1 3 5>..
        ^super.new("loopAt").add(JSTidyPattern(pattern ? "1"))
    }

    become_cur_after_add { ^true }

    fill_stack {
        var pat, n, cycle;
       
        cycle = children.last.get(JSTidyCycle.new);
        pat = children.first.get(JSTidyCycle.new); // the loopat number(s)
        n = pat.at(0).at(\str).asFloat.max(0.1);
        
        cycle.steps.do { |step|
            // divide cycle over n cycles
            step.delta = step.delta * n;
            step.dur = step.dur * n;
            step.put(\loopat, n); // @see sample plugin
            stack.add(step);
        }
    }

    get { |cycle|
        var steps, delta=1, limit=10;

        stack = stack ? List.new;

        // take steps from the stack until you have filled one cycle
        steps = List.new;
        while { (delta > 0.0001) and: (limit > 0) } {
            limit = limit - 1;
            if(stack.size <= 0) { this.fill_stack } {
                var step;
                limit = limit + 2; // restore limit
                step = stack.removeAt(0);
                if(step.delta < (delta + 0.0001)) {
                    steps.add(step);
                    delta = delta - step.delta;
                } {
                    var short = max(0, step.delta - delta);
                    stack.addFirst(JSTidyStep.rest(short));
                    step.delta = step.delta - short;
                    steps.add(step);
                    delta = 0;
                }
            }
        };

        if(limit <= 0) {
            // something went wrong.. make sure at least 1 step exists
            "loopat: limit 0".postln;
            if(steps.size <= 0) {
                steps.add(JSTidyStep.rest(1));
            };
        };

        ^JSTidyCycle.new(steps.asArray);
    }
}

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

    get { |cycle|
        var time, org, alt, steps, pq=PriorityQueue.new;

        org = children.last.get(cycle); // from the JSTidyBranch

        // add steps of org cycle to PriorityQueue
        time = 0;
        org.steps.do { |step|
            pq.put(time, step);
            time = time + step.delta; // slow/fast?
        };

        // add steps of the stack + alt cycle to PriorityQueue
        // TODO: this causes an extra pause when switching trees,
        // which is undesireable! --> use the context!
        stack ?? { stack = [ JSTidyStep(0, shift, shift, "~", 0) ] };
        time = 0;
        stack.do { |step|
            pq.put(time, step);
            time = time + step.delta; // slow/fast?
        };
        stack = [];

        // calculate shifted steps for the alt cycle
        alt = org.deepCopy;
        children.drop(-1).do { |child| alt = child.get(alt) };

        // continue with the time from adding stack items to PriorityQueue
        alt.steps.do { |step|
            case
            { time > 0.999 } { stack = stack.add(step) }
            { (time + step.delta) > 0.999 } {
                // split the step and add some silence to the stack
                var d = time + step.delta - 1;
                stack = stack.add(step.deepCopy.delta_(d).coin_(0));
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

    get { |cycle| ^cycle.steps_(cycle.steps.reverse) }
}

// play through a pattern of sub-sequences
JSTidyFP_Seq : JSTidyNode {
    var <>pattern, <>context;

    *new { |pattern, context|
        var instance = super.new("seq");
        instance.pattern_(pattern);
        instance.context_(context);
        instance.add(JSTidyPattern(pattern));
        ^instance;
    }

    become_cur_after_add { ^true }

    get { |cycle|
        var index;
        var seq; // a queue of steps

        // if you change some parameters in the sequence, and
        // re-evaluate, then a new tree is built in memory.
        // this node object will be replaced by a new one then.
        // so if you then want to continue in the sequence, 
        // you cannot 'remember' where you were in the sequence
        // inside this object. it will be remembered in a context
        // dictionary held by the Track object.
        // if you changed the pattern and then re-evaluate, then
        // the remembered sequence has become invalid, and must
        // be cleared. so i have to remember the pattern string
        // too to detect if it has changed.
        // Still, this leaves only 1 "seq" per tree..
        seq = context.at(\seq);
        context.at(\seqp) !? { |p| if(p != pattern) { seq = nil } };
        context.put(\seqp, pattern);

        // keep a queue of steps of the sequence
        seq ?? { seq = List.new };
        if(seq.size <= 0) {
            seq.addAll(children.first.get(cycle).steps)
        };

        // which child branch will deliver the next cycle?
        // remember: your first child is the seq pattern
        // dur/delta of the steps is ignored. we use 1 step for each cycle.
        //index = seq.removeAt(0).at(\str).asInteger;
        
        // support interpreter variables too using "$n" for example
        index = seq.removeAt(0).at(\str);
        if((index.size == 2) and: (index[0] == $$)) {
            index = this.string_to_interpreter(index[1]);
        };

        index = (index ? 0).asInteger;
        index = index % (children.size - 1) + 1;

        context.put(\seq, seq);

        ^children.at(index).get(cycle);
    }
}

JSTidyFP_Slice : JSTidyNode {
    var <>count;

    *new { |pattern|
        var instance = super.new("slice");
        var str = pattern.split($ );
        instance.count = max(1, str.removeAt(0).asInteger);
        pattern = str.join($ ).stripWhiteSpace;
        if(pattern.size <= 0) { pattern = "1" };
        instance.add(JSTidyPattern(pattern));
        ^instance;
    }

    get { |cycle|
        cycle = children.first.get(JSTidyCycle.new);

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

    get { |cycle|
        var pq=PriorityQueue.new;

        children.do { |child|
            var time = 0;
            child.get(JSTidyCycle.new).steps.do { |step|
                pq.put(time, step);
                time = time + step.delta;
            }
        };

        ^JSTidyCycle(this.steps_from_priority_queue(pq));
    }

    become_cur_after_add { ^true }
}

// chop pattern into n bits and play the bits, but start
// one bit further every cycle. wraps around.
JSTidyFP_Iter : JSTidyNode {
    var >count, start=0;

    *new { |pattern|
        var split = pattern.split($ );
        //"iter %".format(split).postln;
        ^super.new("iter")
        .count_(split.at(0).asInteger);
    }
    
    become_cur_after_add { ^true }

    get { |cycle|
        var parts = List.new;
        var onset = 0;
        var steps;

        //"iter::get %".format(count).postln;

        count.do { parts.add(List.new) }; // make room

        // create list of lists of steps
        cycle = children.last.get(cycle); // should be a JSTidyBranch
        cycle.steps.do { |step|
            var part = (onset / (1 / count)).asInteger;
            parts.at(part).add(step);
            onset = onset + step.delta;
        };

        //"iter::get2 %".format(count).postln;

        // create the new cycle
        steps = List.new;
        count.do { |i| steps.addAll(parts.at(start + i % count)) };

        //"iter::get3 % %".format(count, steps).postln;

        // prepare for next time
        start = start + 1 % count;

        //"iter::get4 % %".format(count, start).postln;

        ^JSTidyCycle(steps.asArray);
    }
}

/*
// chop each step in N identical smaller steps and weave them
JSTidyFP_Striate : JSTidyNode {
    // TODO
    *new { |pattern|
        ^super.new("striate").add(JSTidyPattern(pattern ? "1"))
    }

    become_cur_after_add { ^true }

    get { |cycle|
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

    get { |cycle|
        cycle = children.first.get(cycle);
        cycle.steps.do { |step|	
            step.at(\scale) ?? { step.put(\scale, scale) };
        };
        ^cycle;
    }
}

JSTidyBranch : JSTidyNode {
    get { |cycle|
        var last = children.last;

        case
        { last.isNil } { ^cycle }
        { last.is_branch } {
            cycle = last.get(cycle);
            children.drop(-1).do { |child| cycle = child.get(cycle) };
        } {
            children.do { |child| cycle = child.get(cycle) };
        };

        ^cycle;
    }

    // the node added after this node should become a child of this node
    become_cur_after_add { ^true }

    is_branch { ^true }
}

JSTidyCombBoth : JSTidyNode {
    get { |cycle|
        ^cycle.steps_(
            this.make_steps(
                cycle.steps.asList,
                children.first.get(cycle).steps.asList
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
    get { |cycle|
        var time, child = children.first.get(cycle);

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
    get { |cycle|
        var time, child = children.first.get(cycle);

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

    get { |cycle|
        var steps;
        seq ?? { seq = JSMNPattern(val) }; // lazy instantiate
        steps = List.new;
        seq.steps.do { |step|
            var bits, delta, dur, fill;

            // step: [<coin>, <delta>, <dur>, <str>, <num>]
            // str should be a hex string
            bits = step[3].collectAs({|c|
                c.digit.min(15).asBinaryDigits(4)
            }, Array).flatten * step[0];

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

    lastchild { ^children.last }

    log { |indent=""|
        "%% %".format(indent, this.class, (val ? "").quote).postln;
        children.do { |child| child.log(indent ++ "--") };
    }

    get { |cycle| ^cycle }

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
            
    string_to_interpreter { |ch|
        ^thisProcess.interpreter.tryPerform(ch.asSymbol);
    }
}

JSTidyPattern : JSTidyNode {
    var seq;

    get { |cycle|
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
        lastval ?? { sec = 0 }; // else u miss first beat!
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

JSMute {
    classvar muted;
    classvar soloed;
    classvar mute_all=false;

    // TODO: create a temporary mute for all tracks (like 1 cycle)
    // do this by setting soloed to something for a short while
    *muteall { |beats|
        Routine({
            JSQuant.quantize(0.01);
            mute_all = true;
            this.pr_set_mute_buses;
            beats.wait;
            mute_all = false;
            this.pr_set_mute_buses;
        }).play;
    }

    *symbols { |str_or_symbol|
        var result = List.new;
        case
        { str_or_symbol.isNil } { }
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

    *toggle_in_set { |in, set|
        this.symbols(in).do { |sym|
            JSTrack.at(sym) !? { |t| 
                if(set.findMatch(t).notNil) {
                    set.remove(t);
                } {
                    set.add(t);
                }
            }
        }
    }

    *remove_from_set { |in, set|
        this.symbols(in).do { |sym|
            JSTrack.at(sym) !? { |t| set.remove(t) }
        }
    }

    // Tidy .mute: "a b c" or Tidy .mute: \a or \a .mute
    *mute { |str_or_symbol|
        Routine({
            JSQuant.quantize(0.01);
            muted ?? { muted = IdentitySet.new };
            if(str_or_symbol.isString) { 
                muted = IdentitySet.new;
                this.add_to_set(str_or_symbol, muted);
            } {
                // \a .mute is now a toggle switch
                this.toggle_in_set(str_or_symbol, muted);
            };
            this.pr_set_mute_buses;
        }).play;
    }

    *solo { |str_or_symbol|
        Routine({
            var current;
            JSQuant.quantize(0.01);
            current = soloed;
            soloed = IdentitySet.new;
            this.add_to_set(str_or_symbol, soloed);
            current.do { |t|
                if(soloed.findMatch(t).notNil) {
                    soloed.remove(t)
                }
            };
            this.pr_set_mute_buses;
        }).play;
    }

    *unmute { |str_or_symbol|
        Routine({
            JSQuant.quantize(0.01);
            muted ?? { muted = IdentitySet.new };
            this.remove_from_set(str_or_symbol, muted);
            this.pr_set_mute_buses;
        }).play;
    }

    *unsolo { |str_or_symbol|
        Routine({
            JSQuant.quantize(0.01);
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

        if(mute_all) { ^true };
        if(soloed.includes(track)) { ^false }; // solo wins from mute
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
    alter { |obj, track|
        list.do { |key|	dict.at(key.asSymbol).value(obj, track) }
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
            (name++4).asSymbol.kr(0),           // release
            name.asSymbol.kr(default),          // peak
            (name++5).asSymbol.kr(-4)           // curve
        ).kr(doneAction, gate);
    }
}

JSQuant {
    classvar <>quant=1;

    *quantize { |beatsEarly=0|
        var now = thisThread.beats;
        var wait = ((now + quant).div(quant) * quant - now);
        if(beatsEarly > wait) { wait = wait + quant };
        (wait - beatsEarly).wait;
    }
}

// WaveTable loading
JSWave {
    classvar <waves; // stores the last loaded wavetables

    *load { |spec, base|
        var files;
        base = base ? "/data/sc";
        base = PathName(base).findFolder("akwf");
        base ?? { ^"Base folder for wavetables not found" };
        spec ?? { Tidy.postlist(base.folders.collect { |n| n.folderName }); ^""; };
        base = PathName(base.fullPath +/+ spec);
        if(base.isFolder.not) { ^"% is not a folder".format(base) };
        //
        waves !? { 
            var local = waves; // copy current pointer!
            Routine { local.do { |buffer| buffer.free } }.play;
        };
        //
        files = (base.fullPath +/+ "*.wtable").pathMatch;
        Routine({
            var server = Server.default;
            waves = Buffer.allocConsecutive(
                numBufs: files.size,
                server: server,
                numFrames: 1024,
                numChannels: 1,
                completionMessage: { |buf, i| buf.readMsg(files[i]) }
            );
            server.sync;
            "loaded % waves, first: %".format(files.size, waves[0]).postln;
        }).play;
        ^"";
    }
}

+ PathName {
    findFolder { |spec|
        this.folders.do { |folder|
            // "% %".format(folder, folder.folderName).postln;
            if(folder.folderName.contains(spec)) { ^folder };
            folder.findFolder(spec) !? { |f| ^f };
        };
        ^nil;
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
    debug { JSTrack.at(this) !? { |track| track.debug } }

    << { |in| ^JSTrack.atFail(this).params(in) }

    /*
    2025-08-18:

    // audio rate reads stereo input bus

    \1 -- { |in| ... }             // ar synth, addBefore if possible
    \1 -- \synthdef                // ar synth, addBefore if possible
    \a -- { |in| ... }             // ar synth, addToHead
    \a -- \synthdef                // ar synth, addToHead
    \a -- "delay 12 13" - "..."    // sequence params on synth (no def)
    \a -- "def synthdef" - "n 12"  // sequence poly/mono notes (has def)

    // control rate writes mono output bus

    \val -- { |in| ... }          // kr synth, addToHead
    \val -- \synthdef             // kr synth, addToHead
    \val -- "mono" - "p1 1"       // sequence params on synth (no def)
    \val -- "def adef" - "p1 1"   // sequence values on bus (has def)
    \val -- "cv 3 5 6 7"          // sync write value to bus (has cv)

    */

    -- { |in|
        case

        { this == \tidy } 
        {
            ^JSTrack.atFail(this, \tidy)
            .status_(\build)
            .add_branch("--")
            .add_leaf(in.asString)
        }

        // (seq/stack)
        { this == "".asSymbol }
        { ^JSTidy.new.status_(\build).add_branch("--").add_leaf(in) }

        {
            var type, track;

            case
            { this.asInteger.asString == this.asString } { type = \fx }
            { this.asString.size > 1 } { type = \control }
            { type = \audio };

            track = JSTrack.atFail(this, type).status_(\idle);
            //track.debug(false); // after re-evaluation, debug is off

            case
            { in.isString }
            { ^track.status_(\build).add_branch("--").add_leaf(in) }

            { in.isFunction or: (in.class == Symbol) }
            { 
                ^track
                .status_(\build)
                .launch(in)
                .add_branch("--")
                .add_leaf("_function_or_symbol 1")
            }

            { ^"%% -- <string or func or symbol>".format("\\", this) };
        }
    }
}

