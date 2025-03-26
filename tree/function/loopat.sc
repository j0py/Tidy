
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
