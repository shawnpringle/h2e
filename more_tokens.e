include std/text.e
include std/get.e
global constant c_dc_ops = { "/*", ">=", "<=", "!=" }

public type alpha_char(integer c)
	c = lower(c)
	return 'a' <= c and c <= 'z'
end type

public type digit_char(integer x)
	x = lower(x)
	return ('0' <= x and x <= '9') or ('a' <= x and x <= 'f') 
end type

public type decimal_digit_char(integer x)
	x = lower(x)
	return ('0' <= x and x <= '9')
end type

public type id_char(integer c)
	return c = '_' or digit_char(c) or alpha_char(c)
end type

public type ws_char(integer c)
	return find(c," \t\r\n")
end type

public type delim_char(integer c)
	return find(c,"[](){}.;+-=!/*<>?!,:&|@")
end type

public type number_string(sequence s)
	integer decimal = 1
	s = lower(s)
	if match("0x",s)=1 then
		decimal = 0
		s = s[3..$]
	end if
	if decimal and not decimal_digit_char(s[1]) then
		return 0
	end if
	for i = 1 to length(s)-1 do
		if not digit_char(s[i]) then
			return 0
		end if
	end for
	return digit_char(s[$]) or s[$] = 'l'
end type

public type id_string(sequence s)	
	if alpha_char(s[1]) or s[1] = '_' then
		for i = 2 to length(s) do
			if not id_char(s[i]) then
				return 0
			end if
		end for
		return 1
	end if
	return 0
end type


global function find_key(object x, integer element, integer start_i, integer end_i, sequence haystack)
	if length(haystack)=0 then
		return 0
	elsif start_i < end_i then
		for i = start_i to end_i do
			if equal(haystack[i][element],x) then
				return i
			end if
		end for
	elsif end_i < start_i then
		for i = start_i to end_i by -1 do
			if equal(haystack[i][element],x) then
				return i
			end if
		end for
	else
		return equal(x,haystack[start_i][element])*start_i
	end if
	return 0
end function

public function fix_number(sequence s)
	sequence buf
	if s[$] = 'L' then
		s = s[1..$-1]
	end if
	buf = value(s)
	if buf[1] = GET_SUCCESS then
		return s
	end if                                                      
end function

public type quoted_string(sequence s)
	if length(s)>1 and s[1]=s[$] and s[1]='\"' then
		return 1
	else
		return 0
	end if
end type

public type quoted_char(sequence s)
	if length(s) = 3 and s[1]=s[3] and s[1]='\'' then
		return 1
	end if
	return 0
end type
