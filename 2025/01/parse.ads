with Dial;

package Parse is
	task Parser is
		entry Next(R : out Dial.Rotation; Eof : out Boolean);
	end Parser;
end Parse;
