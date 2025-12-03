with Ada.Strings.Bounded;
with Ada.Text_IO; use Ada.Text_IO;

procedure Day03 is
	MAX_BANK_SIZE: constant Positive := 128;

	package B_Str is new
		Ada.Strings.Bounded.Generic_Bounded_Length
			(Max => MAX_BANK_SIZE);
	use B_Str;

	subtype Joltage is Long_Integer;
	subtype Bank is Bounded_String;

	procedure readBank(bnk: out Bank; eof: out Boolean) is
		line: String(1..B_Str.Max_Length);
		len: Natural;
	begin
		if not End_Of_File then
			Get_Line(line, len);
			bnk := To_Bounded_String(line(1..len));
			eof := False;
		else
			eof := True;
		end if;
	end;

	function maxCharIdx(s: Bounded_String) return Positive is
	-- Index of greatest character in string.

		maxIdx: Positive := 1;
		max, c: Character;
	begin
		max := Element(s, 1);
		for i in 2..Length(s) loop
			c := Element(s, i);
			if c > max then
				max := c;
				maxIdx := i;
			end if;
		end loop;
		return maxIdx;
	end;

	function maxJoltage(bnk: Bank; ncells: Positive) return Joltage is
	-- Maximum possible joltage by enabling N cells of the bank.

		i: Positive;
		cell: Joltage;
	begin
		i := maxCharIdx(Head(bnk, Length(bnk)-(ncells-1)));
		cell := Joltage'Value((1=>Element(bnk, i)));

		if ncells > 1 then
			return cell * 10**(ncells-1)
				+ maxJoltage(Tail(bnk, Length(bnk)-i), ncells-1);
		else
			return cell;
		end if;
	end;

	bnk: Bank;
	eof: Boolean;
	silver, gold: Joltage := 0;
begin
	loop
		readBank(bnk, eof);
		exit when eof;

		silver := silver + maxJoltage(bnk, 2);
		gold := gold + maxJoltage(bnk, 12);
	end loop;

	Put_Line("Silver: " & Joltage'Image(silver)
		& ", Gold: " & Joltage'Image(gold));
end;
