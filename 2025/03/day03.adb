with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

procedure Day03 is
	subtype Joltage is Natural;
	subtype Bank is Unbounded_String;

	procedure readBank(bnk: out Bank; eof: out Boolean) is
		line: String(1..256);
		len: Natural;
	begin
		if not End_Of_File then
			Get_Line(line, len);
			bnk := To_Unbounded_String(line(1..len));
			eof := False;
		else
			eof := True;
		end if;
	end;

	function maxJoltage(bnk: Bank) return Joltage is
		max1, max2: Character;
		max1idx: Natural;
		batt: Character;
	begin
		max1 := Element(bnk, 1);
		max1idx := 1;
		for i in 2..Length(bnk)-1 loop
			batt := Element(bnk, i);
			if batt > max1 then
				max1 := batt;
				max1Idx := i;
			end if;
		end loop;

		max2 := Element(bnk, max1idx+1);
		for i in max1idx+2..Length(bnk) loop
			max2 := Character'Max(max2, Element(bnk, i));
			batt := Element(bnk, i);
			if batt > max2 then
				max2 := batt;
			end if;
		end loop;

		return Joltage'Value((1=>max1)) * 10 + Joltage'Value((1=>max2));
	end;

	bnk: Bank;
	eof: Boolean;
	silver: Joltage := 0;
begin
	loop
		readBank(bnk, eof);
		exit when eof;

		silver := silver + maxJoltage(bnk);
	end loop;

	Put_Line("Silver: " & Natural'Image(silver));
end;
