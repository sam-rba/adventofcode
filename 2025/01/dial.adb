package body Dial is
	function Rotate(Pos : in Position; Rot : in Rotation) return Position is
	begin
		case Rot.Dir is
			when L => return Pos - Rot.Distance;
			when R => return Pos + Rot.Distance;
		end case;
	end Rotate;
end Dial;
