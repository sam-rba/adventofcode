with Ada.Long_Integer_Text_IO;
with Ada.Strings.Bounded;
with Ada.Text_IO; use Ada.Text_IO;

with List;

procedure Day05 is
	MAX_RANGES: constant Positive := 256;
	MAX_LINE: constant Positive := 64;

	subtype Identifier is Long_Integer range 1..Long_Integer'Last;

	type IdentifierRange is record
		lo, hi: Identifier;
	end record;

	package IdentifierRangeList is new List(IdentifierRange, MAX_RANGES);

	package B_Str is new Ada.Strings.Bounded.Generic_Bounded_Length(MAX_LINE);
	use B_Str;

	function parseIdentifierRange(s: Bounded_String) return IdentifierRange is
		i, last: Natural;
		rng: IdentifierRange;
	begin
		i := Index(s, "-"); -- delimiter
		if i = 0 then
			raise Constraint_Error;
		end if;
		Ada.Long_Integer_Text_IO.Get(Slice(s, 1, i-1), rng.lo, last); -- lower bound
		Ada.Long_Integer_Text_IO.Get(Slice(s, i+1, Length(s)), rng.hi, last); -- upper bound
		return rng;
	end;

	procedure parseIdentifierRanges is
		line: String(1..MAX_LINE);
		len: Natural;
		rng: IdentifierRange;
	begin
		while not End_Of_File loop
			Get_Line(line, len);
			exit when len < 1; -- blank line delimiting fresh and available IDs
			rng := parseIdentifierRange(To_Bounded_String(line(1..len)));
			IdentifierRangeList.Append(rng);
		end loop;
	end;

	function parseId return Identifier is
		line: String(1..MAX_LINE);
		len, last: Natural;
		id: Identifier;
	begin
		Get_Line(line, len);
		Ada.Long_Integer_Text_IO.Get(line(1..len), id, last);
		return id;
	end;

	function isFresh(id: Identifier) return Boolean is
		rng: IdentifierRange;
	begin
		for i in 1..IdentifierRangeList.Length loop
			rng := IdentifierRangeList.Element(i);
			if id >= rng.lo and id <= rng.hi then
				return True;
			end if;
		end loop;
		return False;
	end;

	id: Identifier;
	silver: Natural := 0;
begin
	parseIdentifierRanges;
	while not End_Of_File loop
		id := parseId;
		if isFresh(id) then
			silver := silver + 1;
		end if;
	end loop;
	Put_Line("Silver: " & Integer'Image(silver));
end;
