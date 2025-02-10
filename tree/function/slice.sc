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

