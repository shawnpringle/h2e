include euphoria/tokenize.e
include joy.e as joy
include std/map.e as stdmap
include preprocessor/tokenize.e

without define EMULATE_BUILTIN_SYMBOLS

enum s_name, s_args, s_arguments=s_args, s_definition 
type symbol_struct(sequence s)
	joy:truth = length(s)=4
	string_of_integers name = s[s_name]
	if sequence(s[s_args]) then
		joy:string_of_strings sos = s[s_args]
	end if
	truth =	joy:stringof(routine_id("my_token_type"),s[s_definition])
	return 1
end type

type symbol_list(sequence s)
	return joy:stringof(routine_id("symbol_struct"),s[ud_start_pos..$])
end type

public function new()
	return stdmap:new()
end function

-- symbols
-- list of preprocessor ids and what their values are this is determined by target interpreter
ifdef EMULATE_BUILTIN_SYMBOLS then
symbol_list symbols = {
	{"__LINE__", 0, 0, 0},
	{"defined", 0, {{}}, 0}
}
constant LINE = 1
constant DEFINED = 2
constant ud_start_pos = 3
elsedef
symbol_list symbols = {
}
constant ud_start_pos = 1
end ifdef
-- add an preprocessor id
public procedure put(map sym, sequence tok, object arguments, sequence list)
	if not atom(arguments) then
		string_of_strings ss = arguments
	end if
	if length(list) and my_token_type(list[1]) then
		for i = 1 to length(list) do
			list[i] = list[i][TDATA]
		end for
	end if
	string_of_strings ss = list
	if my_token_type(tok) then
		tok = tok[TDATA]
	end if
	stdmap:put(sym, tok, {tok, arguments, list})
end procedure

-- find an preprocessor id with token matching tok
public function get(map sym, sequence tok)
	if my_token_type(tok) then
		tok = tok[TDATA]
	end if
	return map:get(sym,tok)
end function

public function get_symbol_args(sequence s)
	return s[1]
end function

-- get the definition of this preprocessor id ( as sequence of strings )
public function get_symbol_value(sequence sym, object arguments)
	if atom(arguments) then
		return sym[s_definition]
	else
		sequence out = {}
		sequence def = sym[s_definition]
		for j = 1 to length(def) do
			integer lc = find(def[j], sym[s_args])
			if lc then
				out &= arguments[j]
			else
				out = append(out,def[j])
			end if
		end for
		return out
	end if
end function

public procedure remove_symbol(map syms, sequence tok)
	if my_token_type(tok) then
		tok = tok[TDATA]
	end if	
	map:remove(syms,tok)
end procedure

-- stub functions for setting scope of the symbols...
public function get_scope(integer i)
	return "public"
end function

public procedure set_scope(integer i, sequence scope)
end procedure
