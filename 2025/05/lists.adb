package body Lists is
	package body Generic_List is
		function Length(Source: List) return Length_Range is
		begin
			return Source.Length;
		end;
	
		function Element(Source: List; Index: Positive) return Item is
		begin
			if Index > Source.Length then
				raise Constraint_Error;
			end if;
			return Source.Contents(Index);
		end;

		procedure Append(Source: in out List; New_Item: Item) is
		begin
			if Source.Length >= Max_Length then
				raise Constraint_Error;
			end if;
			Source.Length := Source.Length + 1;
			Source.Contents(Source.Length) := New_Item;
		end;

		procedure Insert(Source: in out List; New_Item: in Item; Index: in Positive) is
		begin
			if Index > Source.Length then
				raise Constraint_Error;
			end if;
			Source.Contents(Index) := New_Item;
		end;
	end;
end;
