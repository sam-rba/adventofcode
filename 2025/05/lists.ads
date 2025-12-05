package Lists is
	generic
		type Item is private;
		Max: Positive;
	package Generic_List is
		Max_Length: constant Positive := Max;

		type List is private;
		subtype Length_Range is Natural range 0..Max_Length;

		function Length(Source: List) return Length_Range;
		procedure Append(Source: in out List; New_Item: Item);
		function Element(Source: List; Index: Positive) return Item;

	private
		type ItemArray is array (1..Max_Length) of Item;
		type List is record
			Contents: ItemArray;
			Length: Length_Range := 0;
		end record;
	end;
end;
