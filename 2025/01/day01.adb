with Ada.Integer_Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

procedure Day01 is
	-- Types
	Num_Pos : constant Integer := 100;
	type Position is mod Num_Pos;

	type Direction is (LEFT, RIGHT);

	type Rotation is record
		Dir: Direction;
		Distance: Natural;
	end record;
	-- Rotation is a line of the input file.

	-- Constants
	Start: constant Position := 50;

	-- Functions
	function Direction_From_Char(C: in Character) return Direction is
	begin
		case C is
			when 'L' => return LEFT;
			when 'R' => return RIGHT;
			when others => raise Constraint_Error;
		end case;
	end;

	task Parser is
		entry Next(Rot: out Rotation; Eof: out Boolean);
	end;
	task body Parser is
		Line: String(1..8);
		Len: Natural;
		Last: Positive;
		Done: Boolean := False;
	begin
		while not Done loop
			accept Next(Rot: out Rotation; Eof: out Boolean) do
				if not End_Of_File then
					Get_Line(Line, Len);
					Rot.Dir := Direction_From_Char(Line(1)); -- L/R
					Ada.Integer_Text_IO.Get(Line(2..Len), Rot.Distance, Last);
					Eof := False;
				else
					Eof := True;
					Done := True;
				end if;
			end;
		end loop;
	end;

	-- Variables
	Silver, Gold: Natural := 0;
	Pos: Position := Start;
	Rot: Rotation;
	Eof: Boolean;

begin
	-- For each line of file
	loop
		-- Parse next line
		Parser.Next(Rot, Eof);
		exit when Eof;

		-- Count zero crossings
		-- This is very stupid
		for I in 1..Rot.Distance loop
			case Rot.Dir is
				when LEFT => Pos := Pos - 1;
				when RIGHT => Pos := Pos + 1;
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
