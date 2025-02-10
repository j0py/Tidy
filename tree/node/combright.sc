// (cycle) >| (child), (/+*%<>)
//
JSTidyCombRight : JSTidyNode {
	get { |cycle, name|
		var time, child = children.first.get(cycle, name);

		time = 0;
		child.steps.do { |step|
			var other, keys;

			other = cycle.at(time);

			// collect all keys from both sides
			keys = step.dict.keys;
			other.dict.keys.do { |key| keys.add(key) };
			
			keys.do { |key|
				var stepval = step.at(key);
				var otherval = other.at(key);
				
				case
				{ stepval.isNil } { step.put(key, otherval) }
				{ otherval.isNil } { }
				// now we know that both are not nil..
				{ val == ">" } { }
				{ val == "<" } { step.put(key, otherval) }
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
					{ val == "/" }
					{
						if(stepval == 0) {
							step.put(key, 0) // division by zero
						} {
							step.put(key, otherval / stepval)
						}
					}
					{ val == "%" } { step.put(key, otherval % stepval) }
					{ }
				}
			};

			time = time + step.delta;
		};

		^child;
	}
}

