JSTidy {
	var <tree; // the root of the tree that is grown
	var cur;   // the current leaf in the tree ("where we are")

	// if you make a mistake, you might get here.
	// printOn will do nothing if tree is nil: current sound keeps playing
	doesNotUnderstand { |selector ... args|
		tree = nil;
		JSTidyException("% not understood".format(selector)).throw;
	}

	// needed for the "stack" and "seq" functions
	-- { |array|
		array.do { |jstidy|	cur.add(jstidy.tree) };

		// the next node must be added AFTER the array of JSTidy trees
		while { cur.parent.notNil.and(cur.is_branch.not) } {
			cur = cur.parent;
		};
	}

	// return JSTidyXX function object. str = "<function name> <pattern>"
	// future: str = "<function name> <pattern1> -- <pattern2>"
	func { |str|
		var func, pat, class;

		str = str.split($ );
		func = str.removeAt(0);
		pat = str.join($ ).stripWhiteSpace;

		if(pat[0] == $#) {
			^JSTidyFP_List(func, pat.drop(1).stripWhiteSpace);
		} {
			class = "%%".format(func[0].toUpper, func.drop(1).toLower);
			class = "JSTidyFP_%".format(class).asSymbol.asClass;
			class !? { ^class.new(pat) };
			^JSTidyFP(func, pat);
		}
	}

	add { |node|
		tree ?? { tree = cur = JSTidyBranch("tree") };
		cur.add(node);
		node.parent = cur;

		if(node.become_cur_after_add) {
			cur = node
		} {
			node.children.do { |child|
				if(child.become_cur_after_add) { cur = child }
			}
		}
	}

	add_func { |str| this.add(this.func(str)) }
	add_branch { |str| this.add(JSTidyBranch(str)) }

	| { |str| this.add_branch("|").add_func(str) }

	|<| {  |str| this.add(JSTidyCombBoth("<").add(this.func(str))) }
	< {  |str| this.add(JSTidyCombBoth("<").add(this.func(str))) }
	|< { |str| this.add(JSTidyCombLeft("<").add(this.func(str))) }
	<| { |str| this.add(JSTidyCombRight("<").add(this.func(str))) }

	|>| {  |str| this.add(JSTidyCombBoth(">").add(this.func(str))) }
	// ">" not possible in SuperCollider Interpreter
	|> { |str| this.add(JSTidyCombLeft(">").add(this.func(str))) }
	>| { |str| this.add(JSTidyCombRight(">").add(this.func(str))) }

	- { |str| this.add(JSTidyCombLeft(">").add(this.func(str))) }

	|+| {  |str| this.add(JSTidyCombBoth("+").add(this.func(str))) }
	+ {  |str| this.add(JSTidyCombBoth("+").add(this.func(str))) }
	|+ { |str| this.add(JSTidyCombLeft("+").add(this.func(str))) }
	+| { |str| this.add(JSTidyCombRight("+").add(this.func(str))) }

	|*| {  |str| this.add(JSTidyCombBoth("*").add(this.func(str))) }
	* {  |str| this.add(JSTidyCombBoth("*").add(this.func(str))) }
	|* { |str| this.add(JSTidyCombLeft("*").add(this.func(str))) }
	*| { |str| this.add(JSTidyCombRight("*").add(this.func(str))) }

	|/| {  |str| this.add(JSTidyCombBoth("/").add(this.func(str))) }
	/ {  |str| this.add(JSTidyCombBoth("/").add(this.func(str))) }
	|/ { |str| this.add(JSTidyCombLeft("/").add(this.func(str))) }
	/| { |str| this.add(JSTidyCombRight("/").add(this.func(str))) }

	|%| {  |str| this.add(JSTidyCombBoth("%").add(this.func(str))) }
	% {  |str| this.add(JSTidyCombBoth("%").add(this.func(str))) }
	|% { |str| this.add(JSTidyCombLeft("%").add(this.func(str))) }
	%| { |str| this.add(JSTidyCombRight("%").add(this.func(str))) }
}

