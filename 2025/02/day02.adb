with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Long_Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Day02 is
	ParseError: exception;

	procedure readId(id: out Long_Integer) is
	-- Read a number and the character following it from stdin.
		c: Character;
		s: Unbounded_String;
		last: Positive;
	begin
		-- Read number into buffer
		while not End_Of_File loop
			Get(c);
			exit when c = '-';
			exit when c = ',';

			if Is_Digit(c) then
				s := s & c;
			else
				Raise_Exception(ParseError'Identity, "unexpected character '" & c & "'");
			end if;
		end loop;
		Ada.Long_Integer_Text_IO.Get(To_String(s), id, last);
		-- Parse
	end;

	function numDigits(x: Long_Integer) return Positive is
	-- Number of decimal digits in X.
	begin
		return Natural(Float'Floor(Log(Float(x), 10.0))) + 1;
	end;

	function double(x: Long_Integer) return Long_Integer is
	-- Duplicate the digits of X to produce an invalid ID.
	begin
		return x * 10**(numDigits(x)) + x;
	end;

	function invalidIdSum(lo, hi: Long_Integer) return Long_Integer is
	-- Sum of all invalid IDs in the given range.
		sum, full: Long_Integer;
		nDigStart, nDigStop: Positive;
	begin
		sum := 0;
		nDigStart := numDigits(lo) + (numDigits(lo) mod 2);
		nDigStop := numDigits(hi) - (numDigits(hi) mod 2);
		for nDig in nDigStart..nDigStop loop
			for half in Long_Integer(10**(nDig/2-1)) .. 10**(nDig/2) loop
				full := double(half); -- produce invalid ID
				if full >= lo and full <= hi then
					-- It's an invalid ID in the range
					sum  := sum + full;
				end if;
			end loop;
		end loop;
		return sum;
	end;

	lo, hi: Long_Integer;
	silver: Long_Integer := 0;
begin
	while not End_Of_File loop
		readId(lo);
		readId(hi);
		silver := silver + invalidIdSum(lo, hi);
	end loop;
	Put_Line("Silver: " & Long_Integer'Image(silver));
end;
