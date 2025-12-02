package Dial is
	Num_Pos : constant Integer := 100;

	type Direction is (L, R);
	type Position is mod Num_Pos;

	type Rotation is record
		Dir : Direction;
		Distance : Natural;
	end record;

	Start : constant Position := 50;
end Dial;
