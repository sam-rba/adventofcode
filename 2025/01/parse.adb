with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Dial;

package body Parse is
	function Direction_From_Char(C : in Character) return Dial.Direction is
	begin
		case C is
			when 'L' => return Dial.L;
			when 'R' => return Dial.R;
			when others => raise Constraint_Error;
		end case;
	end Direction_From_Char;

	task body Parser is
		Line : String(1..8);
		Len : Natural;
		Last : Positive;
		Distance : Integer;
		Done : Boolean;
	begin
		Done := False;
		while not Done loop
			accept Next(R: out Dial.Rotation; Eof : out Boolean) do
				if not Ada.Text_IO.End_Of_File then
					Ada.Text_IO.Get_Line(Line, Len);
					R.Dir := Direction_From_Char(Line(1)); -- L/R
					Ada.Integer_Text_IO.Get(Line(2..Len), Distance, Last);
					R.Distance := Dial.Position(Distance mod Dial.Num_Pos);
				else
					Eof := True;
					Done := True;
				end if;
			end Next;
		end loop;
	end Parser;
end Parse;
