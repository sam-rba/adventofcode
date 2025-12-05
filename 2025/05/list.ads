generic
	type Item is private;
	Max: Positive;
package List is
	procedure Append(x: Item);
	function Element(index: Positive) return Item;
	function Length return Natural;
end;
