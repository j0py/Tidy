/////////////////////////////////////////////////////
// SAMPLES LOADER
// Stores loaded buffers at Library.at(\samples, \bank, \index)
/////////////////////////////////////////////////////

JSSamples {
	*load { |folder|
		var s = Server.default;
		Routine({
			(folder.resolveRelative +/+ "*").pathMatch.do({ |bank|
				var index = 0;
				(bank +/+ "*.wav").pathMatch.do({ |file|
					Library.put(
						\samples,
						bank.basename.withoutTrailingSlash.asSymbol,
						index,
						Buffer.read(s, file)
					);
					index = index + 1;
				});
				s.sync;
			});
			s.sync;
			"Loaded %".format(folder.quote).postln;
			JSSamples.log;
		}).play;
	}

	*log {
		Library.at(\samples).keysValuesDo { |k, v|
			"(%) %".format(v.size.asString.padLeft(3), k).postln;
		};
		^"";
	}

	*buf { |bank, index|
		bank ?? { ^nil };
		index ?? { ^nil };

		if(bank == \rest, { ^nil });
		if(bank == "~", { ^nil });
		if(index == \rest, { ^nil });
		if(index == "~", { ^nil });

		bank = bank.asSymbol;
		index = index.asInteger;

		Library.at(\samples, bank) !? { ^nil };
		if(Library.at(\samples, bank).size <= 0, { ^nil });

		index = index % Library.at(\samples, bank).size;

		^Library.at(\samples, bank, index);
	}

	*bufnum { |bank, index|
		JSSamples.buf(bank, index) !? { |buf| ^buf.bufnum }
		^nil;
	}
}
