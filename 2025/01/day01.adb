with Ada.Integer_Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

procedure Day01 is
	--
	-- Types
	--
	NUM_POS: constant Natural := 100; -- number of dial positions
	type Position is mod NUM_POS; -- dial position

	type Direction is (LEFT, RIGHT);

	type Rotation is record
		Dir: Direction; -- L/R
		Distance: Natural; -- number of clicks
	end record;
	-- Rotation is a line of the input file.

	--
	-- Constants
	--
	START: constant Position := 50;

	--
	-- Functions
	--
	function Direction_From_Char(C: Character) return Direction is
	begin
		case C is
			when 'L' => return LEFT;
			when 'R' => return RIGHT;
			when others => raise Constraint_Error;
		end case;
	end;

	-- Count the number of times the dial touches zero during a rotation.
	function Zero_Crossings(Pos: Position; Rot: Rotation) return Natural is
		Dist_To_Zero: Natural;
	begin
		-- Compute distance to zero
		if Pos /= 0 then
			case Rot.Dir is
				when LEFT => Dist_To_Zero := Natural(Pos);
				when RIGHT => Dist_To_Zero := NUM_POS - Natural(Pos);
			end case;
		else
			Dist_To_Zero := NUM_POS;
		end if;

		-- Count number of times dial touches zero
		if Rot.Distance >= Dist_To_Zero then
			return 1 + (Rot.Distance - Dist_To_Zero) / NUM_POS;
		else
			return 0;
		end if;
	end;

	-- Rotate the dial, return its new position.
	function Rotate(Pos: Position; Rot: Rotation) return Position is
	begin
		case Rot.Dir is
			when LEFT => return Position((Integer(Pos) - Rot.Distance) mod NUM_POS);
			when RIGHT => return Position((Integer(Pos) + Rot.Distance) mod NUM_POS);
		end case;
	end;

	--
	-- Tasks
	--
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

	--
	-- Variables
	--
	Silver, Gold: Natural := 0;
	Pos: Position := START;
	Rot: Rotation;
	Eof: Boolean;

begin
	-- For each line of file
	loop
		-- Parse next line
		Parser.Next(Rot, Eof);
		exit when Eof;

		-- Rotate dial and count zero crossings
		Gold := Gold + Zero_Crossings(Pos, Rot);
		Pos := Rotate(Pos, Rot);
		if Pos = 0 then
			Silver := Silver + 1;
		end if;
	end loop;

	-- Print result
	Put_Line("Silver: " & Trim(Natural'Image(Silver), Ada.Strings.Left)
		& ", Gold: " & Trim(Natural'Image(Gold), Ada.Strings.Left));
end;
