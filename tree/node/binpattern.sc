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

