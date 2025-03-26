
JSMix {
  classvar global;
  var mix;

  *initClass { global = Dictionary.new }

  // " <track name> <mix> <track name> <mix> " --> Dictionary
  *setGlobal { |str|
    global.clear;

    (str
      .asString
      .stripWhiteSpace
      .reduce({|a,b|(a++b).replace("  ", " ")}) ? "")
      .split($ )
      .asDict
      .pairsDo { |k,v| global.put(k.asSymbol, v) };

    // set the out/gain parameters of the running nodes on all tracks
    JSTrack.pairsDo { |key, track|
      track.node !? { |node|
        var n=1;
        JSMix(global.at(key.asSymbol) ? "f").get.pairsDo { |fx, gain|
          JSTrack.busIndex(fx) !? { |index|
            Server.default.bind {
              node.set(("out"++n).asSymbol, index);
              node.set(("gain"++n).asSymbol, gain);
            };
				    n = n + 1;
          }
        }
      }
    }
  }

  *putSends { |step, key|
    var mix, hex, n=1;

    hex = (global.at(key.asSymbol) ? "f");
    hex = step.at(\mix) ? hex; // step can override hex
    mix = JSMix(hex).get; // Dictionary

    mix.pairsDo { |k,v| if(v <= 0) { mix.removeAt(k) } };

    10.do { |i| step.at(i.asSymbol) !? { |gain| mix.put(i.asSymbol, gain) } };

    mix.pairsDo { |fx, gain|
      JSTrack.busIndex(fx) !? { |index|
        step.put(("out"++n).asSymbol, index);
        step.put(("gain"++n).asSymbol, gain);
				n = n + 1;
      }
    }
  }

  *args { |key|
    var n=1, args=[];
    JSMix(global.at(key.asSymbol) ? "f").get.pairsDo { |fx, gain|
      JSTrack.busIndex(fx) !? { |index|
        args = args ++ [("out"++n).asSymbol, index];
        args = args ++ [("gain"++n).asSymbol, gain];
				n = n + 1;
      }
    };
    ^args;
  }

  // object

  *new { |hex| ^super.new.init.hex(hex) }

  init { mix = Dictionary.new }

  hex { |hex|
		(hex ? "f").do { |gain, i|
			mix.put(i.asSymbol, gain.digit.linlin(0, 15, 0, 1).asFloat);
		};
  }

  set { |i, gain| mix.put(i.asSymbol, gain.asFloat) }

  get { ^mix }
}

// test code
/*
m = JSMix("f02a")
m.set(3, 0.12)
m.get
m.get.keys.do { |k| k.class.postln }
*/



