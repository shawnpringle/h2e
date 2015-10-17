include euphoria/tokenize.e
include std/get.e

constant LEAF_MAGIC = rand(#FFFF_FFFF), VERTEX_MAGIC=rand(#FFFF_FFFF)

type operator(integer x)
	return x >= T_LTEQ and x != T_QPRINT
end type

function is_operator(sequence tok)
	return operator(tok[TTYPE])
end function

type leaf(sequence s)
	if length(s)!=2 or s[1] != LEAF_MAGIC then
		return 0
	end if
	-- s[2] will be a string representation of the value.  In
	-- a number format recognized by ANSI C.
	return 1
end type

type vertex(sequence s)
	-- if this is supposed to be a vertex, 
	-- we will have the size and magic number
	-- correct.
	if length(s)!=3 or s[1] != VERTEX_MAGIC then
		return 0
	end if
	-- s[2] will be one of the EUPHORIA operators
	-- 
	operator op = s[2]
	--
	-- s[3] will be a sequence of leaf type values
	for i = 1 to length(s[3]) do
		leaf le = s[3][i]
	end for
	return 1
end type

type tree(sequence s)
	return vertex(s) or leaf(s)
end type

function new_leaf(object x)
	return {LEAF_MAGIC,x}
end function

-- parse a EUPHORIA expression
-- return 0 on failure
-- or a parse-tree on success
public function parse_expression(sequence tokens, integer pos)
	if tokens[pos][TTYPE] = T_NUMBER and 
		(length(tokens)=pos or is_operator(tokens[pos+1])) then
		sequence buf = value(tokens[pos][TDATA])
		if buf[1] = GET_FAIL then
			return 0
		end if
		-- put into C's format.
		return new_leaf(sprintf("0x%x",{buf[2]}))
	end if
	return 0
end function


-- evaluates a parse tree
-- return {GET_SUCCESS,value} on success or
-- {GET_FAIL} on failure.
public function evaluate_expression(tree s)
	if leaf(s) then
		return {GET_SUCCESS,s[2]}
	else
		return {GET_FAIL}
	end if
end function
