with Ada.Long_Integer_Text_IO;
with Ada.Strings.Bounded;
with Ada.Text_IO; use Ada.Text_IO;

with Lists;

procedure Day05 is
	MAX_RANGES: constant Positive := 256;
	MAX_LINE: constant Positive := 64;

	subtype Long_Natural is Long_Integer range 0..Long_Integer'Last;
	subtype Identifier is Long_Natural range 1..Long_Natural'Last;

	type IdentifierRange is record
		lo, hi: Identifier;
		-- [lo,hi)
	end record;

	package IdentifierRangeLists is new Lists.Generic_list(IdentifierRange, MAX_RANGES);
	use IdentifierRangeLists;
	subtype IdentifierRangeList is IdentifierRangeLists.List;

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
		rng.hi := rng.hi + 1; -- exclusive upper bound
		return rng;
	end;

	procedure parseIdentifierRanges(idRanges: out IdentifierRangeList)  is
		line: String(1..MAX_LINE);
		len: Natural;
		rng: IdentifierRange;
	begin
		while not End_Of_File loop
			Get_Line(line, len);
			exit when len < 1; -- blank line delimiting fresh and available IDs
			rng := parseIdentifierRange(To_Bounded_String(line(1..len)));
			Append(idRanges, rng);
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

	function isFresh(id: Identifier; freshRanges: IdentifierRangeList) return Boolean is
		fresh: IdentifierRange;
	begin
		for i in 1..Length(freshRanges) loop
			fresh := Element(freshRanges, i);
			if id >= fresh.lo and id < fresh.hi then
				return True;
			end if;
		end loop;
		return False;
	end;

	function countFreshIngredients(idRanges: IdentifierRangeList) return Long_Natural is
		id: Identifier;
		numFresh: Long_Natural := 0;
	begin
		while not End_Of_File loop
			id := parseId;
			if isFresh(id, idRanges) then
				numFresh := numFresh + 1;
			end if;
		end loop;
		return numFresh;
	end;

	procedure trimIntersection(a, b: in out IdentifierRange) is
	begin
		if a.lo > a.hi or b.lo > b.hi then
			raise Constraint_Error;
		end if;

		if a.hi > b.lo and a.hi <= b.hi then
			-- A.Hi inside B
			a.hi := b.lo;
		end if;

		if a.lo >= b.lo and a.lo < b.hi then
			-- A.Lo inside B
			a.lo := b.hi;
		end if;

		if a.lo <= b.lo and a.hi >= b.hi then
			-- A encompasses B
			b.hi := b.lo;
		end if;

		if a.lo >= b.lo and a.hi <= b.hi then
			-- B encompasses A
			a.hi := a.lo;
		end if;
	end;

	procedure trimIntersections(idRanges: in out IdentifierRangeList) is
		a, b: IdentifierRange;
	begin
		for i in 1..Length(idRanges) loop
			a := Element(idRanges, i);
			for j in i+1..Length(idRanges) loop
				b := Element(idRanges, j);
				trimIntersection(a, b);
				Insert(idRanges, b, j);
			end loop;
			Insert(idRanges, a, i);
		end loop;
	end;

	function width(rng: IdentifierRange) return Long_Natural is
	begin
		return rng.hi - rng.lo;
	end;

	function totalWidth(idRanges: IdentifierRangeList) return Long_Natural is
		rng: IdentifierRange;
		w: Long_Natural := 0;
	begin
		for i in 1..Length(idRanges) loop
			rng := Element(idRanges, i);
			w := w + width(rng);
		end loop;
		return w;
	end;

	idRanges: IdentifierRangeList;
	silver, gold: Long_Natural := 0;
begin
	parseIdentifierRanges(idRanges);

	silver:= countFreshIngredients(idRanges);
	trimIntersections(idRanges);
	gold := totalWidth(idRanges);

	Put_Line("Silver: " & Long_Natural'Image(silver) & ", "
		& "Gold: " & Long_Natural'Image(gold));
end;
