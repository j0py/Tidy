
// (cycle) |> (child), (/+*%<>)
//
JSTidyCombLeft : JSTidyNode {
	get { |cycle, name|
		var time, child = children.first.get(cycle, name);

		time = 0;
		cycle.steps.do { |step|
			var other, keys;

			other = child.at(time);

			// collect all keys from both sides
			keys = step.dict.keys;
			other.dict.keys.do { |key| keys.add(key) };
			
			// combine the values for the keys
			keys.do { |key|
				var stepval = step.at(key);
				var otherval = other.at(key);
				
				case
				{ stepval.isNil } { step.put(key, otherval) }
				{ otherval.isNil } { }
				// now we know that both are not nil..
				{ val == ">" } { step.put(key, otherval) }
				{ val == "<" } { }
				{
					if(stepval.class == Bus) {
						stepval = stepval.getSynchronous;
					};
					if(otherval.class == Bus) {
						otherval = otherval.getSynchronous;
					};

					case
					{ val == "+" } { step.put(key, stepval + otherval) }
					{ val == "*" } { step.put(key, stepval * otherval) }
					{ val == "/" } {
						if(otherval == 0) {
							step.put(key, 0)
						} {
							step.put(key, stepval / otherval)
						}
					}
					{ val == "%" } { step.put(key, stepval % otherval) }
					{ }
				}
			};
			
			time = time + step.delta;
		};

		^cycle;
	}
}

