include std/pretty.e
include std/regex.e
include symbol.e as symbol
include std/filesys.e
include std/error.e
include std/console.e
include std/sequence.e as seq
include euphoria/tokenize.e
include more_tokens.e
include std/cmdline.e
include set.e as set
include error.e
include std/io.e
include std/regex.e as regex
include std/map.e as map
include euphoria/tokenize.e as ctok
include std/dll.e
include std/get.e
include needleinahaystack/key.e as key

constant C_VOID = 100
constant C_UNSUPPORTED = 1

type enum calling_convention_type
	__cdecl=1, dllimport, __stdcall, __fastcall, __pascal, __fortran, _System, __watcall
end type

constant calling_convention_type_names = { 
	"__cdecl", "dllimport", "__stdcall", "__fastcall", "__pascal", "__fortran", "_System", "__watcall"
}

--include set.e as set
constant hash_pattern = regex:new("\\s*#", regex:ANCHORED)
public constant common_ops = {">", ">=", "<=", "<", "!=", "+", "*", "-", "(", ")" }

object 
-- this is a map to maps.  Each basic kind int, long, char, etc are mapped to 
-- some different map.  Each of these maps will have in their preimage:
-- "unsigned", "signed" and "".  
-- Example: long_map = map:get(c_types,"long")
-- map:get(long_map,"unsigned") -- is C_ULONG.
c_types = 0, 

-- This is the set of valid keywords.
c_keywords = 0

-- If this is true and we encouter syntax we cannot handle,
-- report error
integer c_errors

-- If 0, errors skip to the next statement.
constant die_on_error = 0

-- Display non-errors as-well.  Things useful for debugging.
constant verbose = 0

object c_structs = 0, c_unions = 0
--------------------
--- WITH FLAGS  ----
--------------------
with trace
without define VALIDATE_TYPEDEF
with define PARSE_C_FUNCTIONS

function parse_a_type_descriptor(sequence statement_location, sequence tokens)
	integer is_signed, is_unsigned, there_is_a_star
	integer looking_for = 1 -- true if we haven't found the unknown symbol
	sequence modifier
	sequence unknown_locations = {}
	there_is_a_star = key:find(T_MULTIPLY,TTYPE,1,length(tokens),tokens)
	is_signed = key:find("signed",TDATA,1,length(tokens),tokens)
	is_unsigned = key:find("unsigned",TDATA,1,length(tokens),tokens)

	object kind = {}
	object name = 0

	if key:find("union",TDATA,1,length(tokens),tokens) or
	key:find("struct",TDATA,1,length(tokens),tokens) then
		trace(1)
	end if

	if is_signed xor is_unsigned then
		modifier = tokens[ is_signed + is_unsigned ][TDATA]
	else
		modifier = ""
	end if
	
	for j = 1 to length(tokens) do
		if tokens[j][TTYPE] = T_IDENTIFIER then
			integer possible_types = map:get(c_types,tokens[j][TDATA])
			if possible_types != 0 and sequence(kind) then
				if c_errors then
					if is_signed and is_unsigned then
						ifdef VALIDATE_TYPEDEF then
							printf(2,"Error in %s:%d.  %s is declared as both signed and unsigned.\n",
								statement_location & tokens[j][TDATA..TDATA])
							abort(1)
						elsedef
							exit
						end ifdef
					end if
				end if
				kind = map:get(possible_types,modifier)
			elsif j = is_signed then	
			elsif j = is_unsigned then
			elsif equal(tokens[j][TDATA],"const") then
			elsif looking_for or eu:compare(kind,C_POINTER) = 0 then
				-- here, the unknown symbol is a newly declared variable, function or type definition

				-- special case: signed or unsigned alone.
				if sequence(kind) and (is_signed xor is_unsigned) then
					possible_types = map:get(c_types,"")	
					kind = map:get(possible_types,modifier)
				end if
				
				looking_for = 0
				name = tokens[j][TDATA]
			else -- must be a function pointer?
				--printf( 2,"Unrecognized syntax in %s:%d.  More than one unknown symbol.\n  Tokens are:\n", 
				--statement_location )
				--ctok:show_tokens(2,tokens)
				unknown_locations = append(unknown_locations,j)
			end if -- possible_types = 0
		elsif tokens[j][TTYPE] = T_MULTIPLY then
			kind = C_POINTER
			name = 0
		elsif tokens[j][TTYPE] = T_LBRACKET or tokens[j][TTYPE] = T_LPAREN then
			kind = C_POINTER
			exit
		end if -- T_IDENTIFIER
	end for
	if looking_for then 
		-- no unknown symbols found.
		-- it is perfectly legal to typedef the same thing twice
		-- hmm...
		return 0
	elsif looking_for or sequence(kind) then
		return -1
	else
		return {name,kind}
	end if
end function

public function get_invalid_symbol_location(sequence tokens, integer c_errors)
	for j = 1 to length(tokens) do
		switch tokens[j][TTYPE] do
			case T_IDENTIFIER then
				if not set:has(c_keywords,tokens[j][TDATA]) and 
					not map:has(c_types,tokens[j][TDATA]) then
					return j
				end if
		end switch
	end for
	return 0
end function


-- levels of comprehension:
-- DECLARATION = it is declared and the user will have to write the body
-- DEFINITION = it is declared and the translator recognizes its definition
public type enum comprehension_level NONE=0, DECLARATION=1, DEFINITION=2 
end type
include std/text.e

function could_parse_macro_constant(sequence tokens)
	if length(tokens)=1 and tokens[1][TTYPE] = T_NUMBER then
		return DEFINITION
	end if
	return NONE
end function

function parse_function_declaration(sequence statement_location, sequence tokens)
	object pd
	sequence comment, s = "/*"
	for i = 1 to length(tokens) do
		s &= ' ' & tokens[i][TDATA]
	end for	
	integer r = key:find("WINAPI",TDATA,1,length(tokens),tokens)
	if r != 0 then
		tokens = eu:remove(tokens,r)
	end if
	s = s & "*/\n"
	
	-- lpl -- location of left parenthesis
	integer lpl = key:find(T_LPAREN,TTYPE,1,length(tokens),tokens)
	integer rpl = key:find(T_RPAREN,TTYPE,lpl+1,length(tokens),tokens)
	if lpl = 0 or rpl = 0 then
		return s
	end if
	comment = s
	s = s & "public constant " & tokens[lpl-1][TDATA] & " = define_c_func( 0, \"" &  tokens[lpl-1][TDATA] & "\", {"
	-- comma location
	integer oldcl = lpl
	if lpl+1 <= rpl-1 then
		-- arguments
		integer cl = key:find(T_COMMA,TTYPE,lpl+1,rpl-1,tokens)
		while cl != 0 do
			pd = parse_a_type_descriptor(statement_location, tokens[oldcl+1..cl-1])
			if atom(pd) then
				pd = {"C_POINTER", C_POINTER}
			end if
			s = s & sprintf("%s%d, ", pd)
			oldcl = cl
			cl = key:find(T_COMMA,TTYPE,cl+1,rpl-1,tokens)
		end while
		pd = parse_a_type_descriptor(statement_location, tokens[oldcl+1..rpl-1])
		if atom(pd) then
			pd = { "C_POINTER", C_POINTER}
		end if
		s = s & sprintf("%s%d", pd)
	end if
	pd = parse_a_type_descriptor(statement_location, tokens[lpl-2..lpl-2] )
	if atom(pd) then
		return { "C_POINTER", C_POINTER }
	end if
	s &= sprintf("}, %d )\n",  pd[2..2])
	return s
end function

constant line_info_matcher = regex:new(`#line (\d+) "(.+)"`)
constant pragma_matcher = regex:new(`\s+#pragma`),
	delim_matcher = regex:new("}[^};]*;?$")

public function process_header(sequence file_name, sequence processed_name, map syms, map:map with_flags)
	c_errors = map:get(with_flags,"c_errors")
	sequence lines = read_lines(file_name)
	if c_keywords = 0 then
		c_keywords = set:new()
		set:add(c_keywords,"static")
		set:add(c_keywords,"const")
		set:add(c_keywords,"union")
		set:add(c_keywords,"unsigned")
		set:add(c_keywords,"signed")
		set:add(c_keywords,"struct")
		set:add(c_keywords,"typedef")
		set:add(c_keywords,"__stdcall")
		set:add(c_keywords,"__cdecl")
		
		c_types = map:new()
		
		map:map this_map = map:new()
		map:put(this_map,("unsigned"),C_ULONG)
		map:put(this_map,("signed"),C_LONG)
		map:put(this_map,(""),C_LONG)
		map:put(c_types,"long",this_map)
		
		this_map = map:new()		
		map:put(this_map,(""),C_INT)
		map:put(this_map,("unsigned"),C_UINT)
		map:put(this_map,("signed"),C_INT)
		map:put(c_types,"int",this_map)

		this_map = map:new()		
		map:put(this_map,(""),C_SHORT)			
		map:put(this_map,("unsigned"),C_USHORT)
		map:put(this_map,("signed"),C_SHORT)
		map:put(c_types,"short",this_map)			
		
		this_map = map:new()
		map:put(this_map,(""),C_UCHAR)
		map:put(this_map,("unsigned"),C_UCHAR)
		map:put(this_map,("signed"),C_CHAR)
		map:put(c_types,"char",this_map)
		
		this_map = map:new()
		map:put(this_map,"",C_DOUBLE)
		map:put(c_types,"double",this_map)
		
		this_map = map:new()
		map:put(this_map,"",C_FLOAT)
		map:put(c_types,"float",this_map)
		
		this_map = map:new()
		map:put(this_map,"",C_VOID)
		map:put(c_types,"void",this_map)
		
		ifdef EU4_0 then
			-- __int64 not supported on EUPHORIA
			-- we make it DOUBLE so we the headers wont kill us.
			this_map = map:new()
			map:put(this_map,"",C_DWORDLONG)
			map:put(c_types,"__int64",this_map)
		end ifdef
		
		-- unsigned or signed alone means int
		this_map = map:new()
		map:put(this_map,"signed",C_LONG)
		map:put(this_map,"unsigned",C_LONG)
		map:put(c_types,"",this_map)
		
		c_structs = map:new()
		
		c_unions = map:new()
		
	end if	
	sequence statements = {}
	integer line_number = 0, h1_line_number = 0
	sequence hashed_file_name = file_name
	sequence goal_stack = ""
	sequence this_statement = ""
	for i = 1 to length(lines) do
		sequence line = lines[i]
		label "line_loop"
		object line_info = regex:matches(line_info_matcher, lines[i])
		if sequence(line_info) then
			sequence buf = value(line_info[2])
			h1_line_number = buf[2]
			hashed_file_name = line_info[3]			
			--hashed_file_name = filebase(hashed_file_name) & `.h`
			h1_line_number -= 1
		else
			h1_line_number += 1
			line_number += 1
		end if
		if regex:has_match(pragma_matcher, line) then
			for j = i to length(lines) do
				if length(lines[j]) and lines[j][$] != '\\' then
					lines[j] = ""
					exit
				end if
				lines[j] = ""
			end for
		end if
		if regex:has_match(regex:new("^\\s*#"), line) then
			line = ""
		end if
		for j = 1 to length(line) do
			-- get rid of hash mark statements
			
			switch line[j] do
				case '{' then
					goal_stack = prepend(goal_stack,'}')
				case '(' then
					goal_stack = prepend(goal_stack,')')
				case '[' then
					goal_stack = prepend(goal_stack,']')
				case ';' then
					if equal(goal_stack,"") then
						this_statement = this_statement & line[1..j] & 10
						statements = append( statements, { hashed_file_name, line_number, this_statement } )
						line = line[j+1..$]
						this_statement = ""
						goto "line_loop"
					end if
				case '}',')',']' then
					if not length(goal_stack) then
						crash("Unhandled Error")
					end if
					if line[j] = goal_stack[1] then
						goal_stack = goal_stack[2..$]
					end if					
			end switch			
		end for
		if eu:compare(goal_stack,"") then
			this_statement = this_statement & line
		end if
	end for
	lines = {}
	integer oefd = open(processed_name,"w") 
	for i = 1 to length(statements) label "statement_loop" do
		sequence line = statements[i][3]
		sequence tokens = ctok:tokenize_string(line[1..$-1])
		tokens = tokens[1]

		if match("#pragma",line) then
			continue
		end if
		if length(tokens) > 1 and tokens[$][TTYPE] = T_NEWLINE then
			tokens = eu:remove(tokens,length(tokens))
		end if
		if length(tokens) < 2 then	
		elsif tokens[1][TTYPE] = T_IDENTIFIER and equal(tokens[1][TDATA],"typedef") then
			integer possible_types
			integer inval			
			if eu:find(tokens[2][TDATA],{"struct","enum","union"}) then
				-- we cannot send this to define_c_func but by giving this an entry
				-- pointer typedefs that use it will work.
				if length(tokens)>2 then
					if verbose then
						printf(2,"Adding %s as a new type from %s:%d\n", {tokens[3][TDATA]} & statements[i][1..2])
					end if
					map:map this_map = map:new()
					if equal(tokens[2][TDATA],"enum") then
						map:put(this_map,"",C_USHORT)
					else
						map:put(this_map,"",C_UNSUPPORTED)
					end if
					if tokens[3][TTYPE] = T_IDENTIFIER then
						map:put(c_types,tokens[3][TDATA],this_map)
					else
						sequence random_string = rand(repeat(26,26)) +'a' - 1
						map:put(c_types,random_string,this_map)
						tokens = tokens[1..2] & {{ T_IDENTIFIER, random_string, 0, 0, 0 }} & tokens[3..$]
					end if
				else
					if verbose then
						printf(2,"Ignorning structure declartion on %s:%d\n",statements[i][1..2])
						printf(2,"statement = %s", {line})
					end if
					continue "statement_loop"
				end if
				
				object post_struct = regex:matches(delim_matcher,line)
				if sequence(post_struct) then
					post_struct = post_struct[1][2..$-1]
					sequence TOK
					TOK = tokenize_string(post_struct)
					if TOK[ET_ERROR] then
						continue "statement_loop"
					end if
					tokens = {tokens[1],tokens[3]} & eu:remove(TOK[ET_TOKENS],length(TOK[ET_TOKENS]))
				else
					if verbose then
						printf(2,"Ignorning structure declartion on %s:%d\n",statements[i][1..2])
						printf(2,"statement = %s", {line})
					end if
					continue "statement_loop"				
				end if
			end if
			if match("__stdcall",line) then
				continue
			end if
			ifdef VALIDATE_TYPEDEF then
				integer count = 0
				while inval with entry do
					if c_errors then
						printf(2,"Error in %s:%d.  %s has not been declared.\n",
						statements[i][1..2] & tokens[inval][TDATA..TDATA])
						show_tokens(2,tokens)
						if die_on_error then
							abort(1)
						else
							continue "statement_loop"
						end if
					else
						if length(tokens) <= 3 then
							-- invalid symbols go to next statement.
							continue "statement_loop"
						end if
					end if
				entry
					count += 1
					inval = get_invalid_symbol_location(tokens[1..$-1],c_errors)
					if count > 1000 then
						continue "statement_loop"
					end if
				end while
			elsedef
				inval = 0
			end ifdef
			integer token_count = length(tokens)
			object buf = parse_a_type_descriptor(statements[i][1..2],tokens[2..$])
			if atom(buf) and buf != 0 then
				printf(2,"Error in %s:%d.  Cannot parse the definition of %s.\n",
					statements[i][1..2] & tokens[$][TDATA..TDATA])
				puts(2, line)
				puts(2, "The tokens are:\n")
				ctok:show_tokens(2,tokens)
				printf(2,"Skip/Die/Trace:",{})
				if not c_errors then
					continue "statement_loop"
				else
					abort(1)
				end if
				buf = gets(0)
				switch buf[1] do
					case  't' then
						trace(1)
						buf = parse_a_type_descriptor(statements[i][1..2],tokens[2..$])
					case 's' then
						continue "statement_loop"
					case 'd' then
						abort(1)
				end switch
			end if
			if sequence(buf) then
				integer this_c_type = buf[2]
				sequence type_name = buf[1]
				map:map this_map = map:new()
				map:put(this_map,"",this_c_type)
				map:put(c_types,type_name,this_map)
				printf(oefd,"public constant C_%s = %x\n",  { type_name, this_c_type } )
			elsif buf = -1 and (verbose or c_errors) then
				puts(2, "The tokens are:\n")
				ctok:show_tokens(2,tokens)
				if die_on_error then
					die(statements[i][1], line, statements[i][2], "Cannot determine what to typedef.\n", {})
				end if
				continue "statement_loop"
			else
				continue "statement_loop"
			end if
		elsif tokens[1][TTYPE] = T_IDENTIFIER and equal(tokens[1][TDATA],"static") and 
				equal("const",tokens[2][TDATA]) then
			integer len = length(tokens)
			integer comprehension = could_parse_macro_constant(tokens[6..$])
			if comprehension = DEFINITION then
				printf(  oefd,  "global constant %s = %s\n", { tokens[4][TDATA], tokens[6][TDATA] }  )
			end if
		else
			ifdef PARSE_C_FUNCTIONS then
				sequence expected = {}
				sequence return_type = {}
				integer return_type_value = -1
				for j = 1 to length(tokens) do
					calling_convention_type cct = __stdcall
					sequence jdat = tokens[j][TDATA]
					if length(expected) and expected[1] = tokens[j][TTYPE] then
						integer handled = expected[2] 
						expected = expected[3..$]
						if handled then
							continue
						end if
					end if
					integer t = eu:find(jdat,calling_convention_type_names)
					
					if equal("__declspec",jdat) then
						puts(oefd,parse_function_declaration({file_name,i},tokens[j+4..$]))
					elsif equal("DECLSPEC_IMPORT",jdat) then
						puts(oefd,parse_function_declaration({file_name,i},tokens[j+1..$]))
						continue
					elsif t != dllimport and t != 0 then
						cct = t
					elsif eu:find(jdat,{"extern","const"}) then							
					elsif map:get(c_types,jdat,-1)!=-1 or eu:find(jdat,{"unsigned","signed"}) or tokens[j][TTYPE] = T_MULTIPLY then
						return_type = append(return_type,tokens[j])
					elsif length(return_type) then
						return_type = parse_a_type_descriptor(statements[i][1..2], 
						append(return_type,tokens[j]))
						return_type_value = return_type[2]
						integer k = j+2
						if tokens[j+1][TTYPE] != T_LPAREN then
							if verbose then			
								printf(2, "%s:%d Expecetd ( but got something else!\n", statements[i][1..2])
							end if
							exit
						end if
						sequence name = return_type[1]
						if get_key() != -1 or statements[i][2] = 6328 then
							if statements[i][2] = 6328 then
--								verbose = 1
							end if
							any_key()
						end if
						if verbose then
							printf(2, "I could parse this thing with a little help....\n %s:%d %s", 
								statements[i][1..2]&{line} )
							printf(2, "return type is :", {})
							ctok:show_tokens(2, return_type)
							printf(2, "Function name is: %s\n", {name})
						end if
						exit
					elsif equal(jdat,"dllimport") then
						-- should seek function in the dlls it knows...
					else
						if verbose or c_errors then
							puts(2, "Unexpected entry found in function definition!\n")
							puts(2, "token is: ")
							show_tokens(2,tokens[j..j])
							printf(2, "file %s line is: %d : %s", {file_name, i, line}  )
							if die_on_error then
								abort(1)
							else
								continue "statement_loop"
							end if
						end if
						exit
					end if
				end for
			elsedef
				integer return_type_value = -1
			end ifdef
			if verbose and return_type_value = -1 then
				printf(2,"Ignoring a statement on: %s:%d %s",statements[i][1..2] & {line})
			end if
			-- parse a function declaration
		end if
	end for
	
	close(oefd)
	return processed_name
end function


