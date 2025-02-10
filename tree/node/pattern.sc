JSTidyPattern : JSTidyNode {
	var seq;

	get { |cycle, name|
		seq ?? { seq = JSMNPattern(val) }; // lazy instantiate
		^JSTidyCycle(seq.steps);
	}
}
