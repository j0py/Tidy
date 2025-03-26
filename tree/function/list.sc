// \a -- "def atone" - "adsr #2 20 0.4 500"
// results in floats or buses put into the step
JSTidyFP_List : JSTidyFP {
	var <>str;
	
	*new { |val, str|
		var instance = super.new(val).str_(str);
		instance.add(JSTidyPattern("1")); // a single step dummy pattern
		^instance;
	}

	get { |cycle, name|
		cycle = children.first.get(cycle, name);
		cycle.steps.do { |step|
			str.split($ ).do { |substr, i|
				// the first number/str will have key = "<val>"
				// the subsequent number/str will have key = "<val>1", etc
				var key = (case { i > 0 } { (val++i) } { val }).asSymbol;
				if(substr[0] == $=) {
					step.put(key, this.string_to_control(substr));
				} {
					step.put(key, substr.asFloat)
				}
			}
		};
		^cycle;
	}
}

// \a -- "def atone" - "adsr /824d6"
// results in floats (0..1) put into the step
JSTidyFP_HexList : JSTidyFP {
	var <>str;
	
	*new { |val, str|
		var instance = super.new(val).str_(str);
		instance.add(JSTidyPattern("1")); // a single step dummy pattern
		^instance;
	}

	get { |cycle, name|
		cycle = children.first.get(cycle, name);
		cycle.steps.do { |step|
			str.do({ |c, i|
				var value = c.digit.linlin(0, 15, 0, 1);
				// the first number/str will have key = "<val>"
				// the subsequent number/str will have key = "<val>1", etc
				var key = (case { i > 0 } { (val++i) } { val }).asSymbol;
			  step.put(key, value.asFloat)
			})
		};
		^cycle;
	}
}

