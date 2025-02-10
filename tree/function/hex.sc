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
