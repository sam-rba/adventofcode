with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Dial; use Dial;
with Parse;

procedure Day01 is
	Pw : Natural; -- password
	Pos : Dial.Position;
	Rot : Dial.Rotation;
	Eof : Boolean;
begin
	Pw := 0;
	Pos := Dial.Start;

	loop
		Parse.Parser.Next(Rot, Eof);
		exit when Eof;

		Pos := Dial.Rotate(Pos, Rot);
		if Pos = 0 then
			Pw := Pw + 1;
		end if;
	end loop;

	Put_Line(Trim(Natural'Image(Pw), Ada.Strings.Left));
end Day01;
