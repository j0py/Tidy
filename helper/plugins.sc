JSPlugins {
	var >dict, >list;

	*new { ^super.new.dict_(Dictionary.new).list_(List.new) }

	// the sequence in which plugins are added..
	add { |key, func|
		if(list.includes(key).not) { list.add(key) };
		dict.put(key, func);
	}

	// .. is the sequence with which they are called
	alter { |track, step|
		list.do { |key|	dict.at(key.asSymbol).value(track, step) }
	}

	log { list.postln }
}
