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

