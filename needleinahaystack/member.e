include std/error.e
-- Finds a record with a key value x in a haystack starting at position start_i
-- and terminating at position end_i.
-- 
--
--
public function find(object x, integer start_i=-1, integer end_i=-1, sequence haystack)
	if start_i = -1 and end_i = -1 then
		start_i = 1
		end_i = length(haystack)
	elsif start_i = -1 and end_i = 1 then
		start_i = length(haystack)
	elsif end_i = -1 then
		end_i = length(haystack)
	end if
	if start_i = -1 or end_i = -1 then
		crash("Must specify position parameters in key.e:find()\n")
	end if
	if length(haystack)=0 then
		return 0
	elsif start_i < end_i then
		for i = start_i to end_i do
			if equal(haystack[i],x) then
				return i
			end if
		end for
	elsif end_i < start_i then
		for i = start_i to end_i by -1 do
			if equal(haystack[i],x) then
				return i
			end if
		end for
	else
		return equal(x,haystack[start_i])*start_i
	end if
	return 0
end function

public function find_any(sequence keys, integer start_i=-1, integer end_i=-1, sequence haystack)
	if start_i = -1 and end_i = -1 then
		start_i = 1
		end_i = length(haystack)
	elsif start_i = -1 and end_i = 1 then
		start_i = length(haystack)
	elsif end_i = -1 then
		end_i = length(haystack)
	end if
	if start_i = -1 or end_i = -1 then
		crash("Must specify position parameters in key.e:find()\n")
	end if
	if length(haystack)=0 then
		return 0
	elsif start_i < end_i then
		for i = start_i to end_i do
			for j =  1 to length(keys) do
				object x = keys[j]
				if equal(haystack[i],x) then
					return i
				end if
			end for
		end for
	elsif end_i < start_i then
		for i = start_i to end_i by -1 do
			for j =  1 to length(keys) do
				object x = keys[j]
				if equal(haystack[i],x) then
					return i
				end if
			end for
		end for
	else
		return (eu:find(haystack[start_i],keys)!=0)*start_i
	end if
	return 0
end function

