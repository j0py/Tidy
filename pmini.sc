/*
Takes a mini notation string as input.
Returns a pattern that can be used in a Pbind.
The pattern has 5 values:

\trig : 1 should trigger a note, 0 should not
\delta: how long to wait before processing the next step
\dur  : value to calculate the sustain for a step
\str  : string value for a step
\num  : integer value for a step

Example:
Pbind(
    [\trig, \delta, \dur, \str, \num], Pmini("1 2 3 4"),
	\degree, Pfunc({ |e| if(e.trig > 0) { e.str.asInteger } { \rest } }),
)

*/
Pmini {
	*new { |mini_notation|
		var parser = JSMiniParser(mini_notation).parse;
		
		^Pn(Plazy({	Pseq(parser.next_cycle, 1) }), inf)
	}
}
