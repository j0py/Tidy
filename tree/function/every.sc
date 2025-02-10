// \a -- "every 8" >| "b 1 2 3 4" | etc; takes action on 7, 15, 23, etc
JSTidyFP_Every : JSTidyNode {
	var <>when, turn;

	*new { |pattern|
		var split = pattern.split($ );
		^super.new("every")
		.when_(split.at(0).asInteger);
	}

	become_cur_after_add { ^true }

	get { |cycle, name|
		cycle = children.last.get(cycle, name); // should be a JSTidyBranch

		// let your children alter the cycle when it is your turn
		turn = (turn ? -1) + 1; // zero based cycle counter

		if(((turn + 1) % when) == 0) {
			children.drop(-1).do { |child|
				cycle = child.get(cycle, name);
			};
		};

		^cycle;
	}
}

