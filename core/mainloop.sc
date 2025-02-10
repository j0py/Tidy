JSMainloop {
	classvar <mainloop; // the Routine that plays all cycles
	classvar <>shift=0; // wait extra beats once during mainloop
	
	*start {
		mainloop ?? {
			mainloop = Routine({
				var cycle_number = 0;
				JSControlBus.init(Server.default);
				JSQuant.quantize;
				loop {
					JSTrack.do { |track|
						case
						{ track.hushed or: track.once } { }
						{ track.play(cycle_number) }
					};
					cycle_number = cycle_number + 1;
					1.wait;	// 1 beat = 1 cycle
					if(shift > 0) {
						"shifted %".format(shift).postln;
						shift.wait;
						shift=0;
					};
				}
			}).play;
			"..mainloop begun".postln;
		};
	}
	
	*stop { |seconds=0.02|
		mainloop !? { mainloop.stop };
		mainloop = nil;
		"..mainloop ended".postln;
	}
}

