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
