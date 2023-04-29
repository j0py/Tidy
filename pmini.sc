/*
Generates steps from a mini notation string.
Each step has 4 values : on, dur, string, number.
Returns a pattern that can be used in a Pbind.

Example:
Pbind(
    [\on, \dur, \str, \num], Pmini("1 2 3 4"),
	\degree, Pfunc({ |e| e.str.asInteger }),
	)

*/
Pmini
{
	*new { |mini_notation|
		var seq = JSMini(mini_notation);
		
		^Pn(
			Plazy({
				var time=0, steps;
				
				// Insert rests where needed to completely
				// fill up the time. Pbinds like that very much.
				steps = List.new;
				
				seq.next_cycle.steps.do { |step|
					if(step.on > time, {
						steps.add([step.on - time, \rest, 0]);
					});
					
					steps.add([step.dur, step.string, step.number]);

					time = step.on + step.dur;
				};

				if(1 > time, { steps.add([1 - time, \rest, 0]) });

				// return a Pseq for the steps
				Pseq(steps.asArray, 1);
			}),
			inf
		)
	}
}
