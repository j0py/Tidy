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

