// a cycle, containing steps.
JSTidyCycle {
	var <steps, <>env;

	*new { |steps| ^super.new.steps_(steps) }

	// step_array: [ [<trig>,<delta>,<dur>,<str>,<num>], .. ] or [JSTidyStep,..]
	steps_ { |steps_array|
		steps = steps_array.collect { |el|
			if(el.isArray) {
				el = JSTidyStep(el[0], el[1], el[2], el[3], el[4]);
			};
			el;
		};
		this.make_index;
	}
	
  // TODO move to plugin
	rotate { |rot|
		if(rot != 0) { this.steps_(steps.rotate(rot)) }
	}

	make_index {
		var indexes=[], times=[];
		steps !? {
			steps.do { |step, index|
				indexes = indexes.add(index);
				times = times.add(0);
				indexes = indexes.add(index);
				times = times.add(step.delta);
			};
			times.removeAt(0);
			env = Env(indexes, times);
		}
	}

	// use an Env to get a step index, given a time
	at { |time|
		steps ?? { ^nil };
		env ?? { ^nil };
		^steps.at(env.at(time));
	}

	printOn { |stream|
		stream << "cycle\n";
		steps !? { steps.do { |step| step.printOn2(stream) } };
	}
}
