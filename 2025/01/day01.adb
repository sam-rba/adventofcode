with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Dial; use Dial;
with Parse;



procedure Day01 is
	Silver, Gold: Natural;
	Pos: Dial.Position;
	Rot: Dial.Rotation;
	Eof: Boolean;

	function Zero_Crossings(Pos: Dial.Position; Rot: Dial.Rotation) return Natural is
	begin
		return 0;
	end;

begin
	Silver := 0;
	Gold := 0;
	Pos := Dial.Start;

	-- For each line of file
	loop
		-- Parse next line of file
		Parse.Parser.Next(Rot, Eof);
		exit when Eof;

		-- Count zero crossings
		-- This is very stupid
		for I in 1..Rot.Distance loop
			case Rot.Dir is
				when Dial.L => Pos := Pos - 1;
				when Dial.R => Pos := Pos + 1;
			end case;
			if Pos = 0 then
				Gold := Gold + 1;
			end if;
		end loop;
		if Pos = 0 then
			Silver := Silver + 1;
		end if;
	end loop;

	-- Print result
	Put_Line("Silver: " & Trim(Natural'Image(Silver), Ada.Strings.Left)
		& ", Gold: " & Trim(Natural'Image(Gold), Ada.Strings.Left));
end;
