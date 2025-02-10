JSTidyFP_Toscale : JSTidyNode {
	var >scale;
	
	*new { |pattern|
		var instance = super.new("toscale");
		instance.add(JSTidyPattern("1"));
		^instance.scale_(pattern.split($ ).collect { |x| x.asInteger });
	}

	get { |cycle, name|
		cycle = children.first.get(cycle, name);
		cycle.steps.do { |step|	step.put(\toscale, scale) };
		^cycle;
	}
}

