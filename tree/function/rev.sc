// How can "rev" be used in both these sitations?
// \a -- "chop 4" | "rev" | "s ride sn"
// \a -- "jux" - "rev" | "s ride sn"
// answer: log the tree
JSTidyFP_Rev : JSTidyNode {
	*new { |pattern| ^super.new("rev") }

	get { |cycle, name| ^cycle.steps_(cycle.steps.reverse) }
}

