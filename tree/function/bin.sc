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
