JSTidyCombBoth : JSTidyNode {
	get { |cycle, name|
		^cycle.steps_(
			this.make_steps(
				cycle.steps.asList,
				children.first.get(cycle, name).steps.asList
			)
		);
	}

	make_steps { |steps1, steps2|
		var step1, step2, steps=List.new;

		steps1 = steps1.asList;
		steps2 = steps2.asList;

        while { (steps1.size > 0) or: (steps2.size > 0) } {

			step1 = step2 = nil;

			if(steps1.size > 0) { step1 = steps1.removeAt(0) };
			if(steps2.size > 0) { step2 = steps2.removeAt(0) };

			case
			{ step1.isNil } { steps.add(step2); step2 = nil }
			{ step2.isNil } { steps.add(step1); step1 = nil }
			{
				case
				{ abs(step1.delta - step2.delta) < 0.001 }
				{
					this.combine(step1, step2);
					steps.add(step1);
				}
				{ step1.delta < step2.delta }
				{
					var d = step2.delta - step1.delta;
					var step2a = step2.deepCopy.delta_(d).dur_(d);
					steps2.insert(0, step2a);
					step2.delta = step1.delta;
					step2.dur = step1.dur;
					this.combine(step1, step2);
					steps.add(step1);
				}
				{
					var d = step1.delta - step2.delta;
					var step1a = step1.deepCopy.delta_(d).dur_(d);
					steps1.insert(0, step1a);
					step1.delta = step2.delta;
					step1.dur = step2.dur;
					this.combine(step2, step1);
					steps.add(step2);
				}
			}
		};

		^steps;
	}

	combine { |step, stepAt, right|
		stepAt.dict.keysValuesDo { |key, value|
			var stepval = step.at(key);
			case
			{ val == ">" }
			{ if(right.isNil) { step.put(key, value) } }
			{ val == "<" }
			{ if(right.notNil) { step.put(key, value) } }
			{
				if(stepval.class == Bus) {
					stepval = stepval.getSynchronous;
				};
				if(value.class == Bus) {
					value = value.getSynchronous;
				};

				case
				{ val == "+" }
				{
					if((stepval.isString) or: (value.isString)) {
						step.put(key, (stepval ? "") ++ (value ? ""));
					} {
						step.put(key, (stepval ? 0) + value);
					}
				}
				{ val == "*" } { step.put(key, (stepval ? 1) * value) }
				{ val == "/" }
				{
					if(right.isNil, {
						if(value == 0, {
							step.put(key, 0) // division by zero
						}, {
							step.put(key, (stepval ? 0) / value)
						});
					},{
						var divider = (stepval ? 0);
						if(divider == 0, {
							step.put(key, 0) // division by zero
						}, {
							step.put(key, value / divider)
						});
					})
				}
				{ val == "%" }
				{
					if(right.isNil, {
						step.put(key, (stepval ? 0) % value)
					}, {
						step.put(key, value % (stepval ? 0))
					})
				}
				{ }
			};
		}
	}
}

