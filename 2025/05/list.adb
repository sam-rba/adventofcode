package body List is
	list: array (1..Max) of Item;
	len: Natural range 0..Max;

	procedure Append(x: Item) is
	begin
		if len >= Max then
			raise Constraint_Error;
		end if;
		len := len + 1;
		list(len) := x;
	end;

	function Element(index: Positive) return Item is
	begin
		if index > len then
			raise Constraint_Error;
		end if;
		return list(index);
	end;

	function Length return Natural is
	begin
		return len;
	end;

begin
	len := 0;
end;
