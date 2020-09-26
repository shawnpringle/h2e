include std/pretty.e
include std/regex.e
include symbol.e as symbol
include std/filesys.e
include std/error.e
include std/console.e
include std/sequence.e as seq
constant id_pattern = regex:new("[A-Za-z_][A-Za-z_0-9]*")
constant define_that_is_also_EUPHORIA_pattern = regex:new(`^(\s*)#(\s*)define(\(?)\s+([A-Za-z_][A-Za-z_0-9]*)( (\s+|[A-Za-z_][A-Za-z_0-9]*|[0-9]+L?|0x[0-9a-fA-F]+L?|\(|\)|\+|-|\*|<|>|<=|>=|/\*.*\*/)+)?$`, regex:ANCHORED & regex:EXTRA )
constant define_pattern = regex:new(`^(\s*)#(\s*)define(\(?)\s+([A-Za-z_][A-Za-z_0-9]*)\s+(.+)$`, regex:ANCHORED & regex:EXTRA )
constant semicolon_pattern = regex:new(";")
constant number_pattern = regex:new("([0-9]+|(0x[0-9a-fA-F]+))L?", regex:DEFAULT)
constant undef_pattern = regex:new("(\\s*)#(\\s*)undef " & id_pattern, regex:ANCHORED)
constant ifdef_pattern = regex:new("(\\s*)#(\\s*)(ifdef|ifndef)\\s+(" & id_pattern & ')', regex:ANCHORED)
constant if_pattern = regex:new("(\\s*)#(\\s*)if\\b", regex:ANCHORED)
constant else_pattern = regex:new(`(\s*)#(\s*)else`, regex:ANCHORED)
constant endif_pattern = regex:new("(\\s*)#(\\s*)endif" & id_pattern, regex:ANCHORED)
constant include_pattern = regex:new("(\\s*)#(\\s*)include <([/_a-zA-Z0-9\\.\\\\]+)>", regex:ANCHORED)
--include std/map.e
include preprocessor/tokenize.e
include euphoria/tokenize.e
include more_tokens.e
include std/cmdline.e
include set.e as set
include error.e
include joy.e

object shortest_include_dir = 0
set:set opened_files = set:new()

with trace
with define ALLOW_MISSING_FILES

-- retrieve a line joining lines that end in backslash
-- and also returning the number of new lines encountered
-- The form is: {line,line_count}
-- If an EOF is encountered before data can be read line will be -1.
-- Line will contain all of the original line feeds.
function getcs(integer in)
	sequence ret = ""
	object line = ""
	integer line_number = 0
	loop do
		line = gets(in)
		if atom(line) then
			-- EOF
			if line_number then
				return {ret,line_number}
			else
				return {line,0}
			end if
		end if
		line_number += 1
		ret = ret & line
		if length(line)>2 and line[$-1] = '\\' then
			retry
		end if
	until 1
	end loop
	return {ret, line_number}
end function

/*
 calls a preprocessor in such a way that it includes the pre-included-files in <i>pre_included_files</i>,
 a sequence of strings, each string is a file name; processes <i>prepared_file_name</i>, and the
 preprocessed output it written to a new file named by <i>out_file</i> 
 */
public function preprocess_header(sequence pre_included_files, sequence prepared_file_name, sequence out_file, integer verbose = 0 )
	if string_of_atoms(pre_included_files) then
		pre_included_files = {pre_included_files}
	end if
	object PATH = getenv("PATH")
	sequence list = {"owcc.exe","-E"}
	if atom(PATH) or compare(locate_file("owcc.exe",PATH),"owcc.exe")=0 then
		-- couldn't find it...
		puts(2,"Error no C preprocessor found.")
		maybe_any_key("Press any key to Close.")
		abort(1)
	end if
	for i = 1 to length(pre_included_files) do
		list = list & {"-include-file",pre_included_files[i]}
	end for
	list &= {prepared_file_name,"-o", out_file}
	if verbose then
		puts(2,"Executing:\n")
		puts(2,seq:join(list," "))
	end if
	integer pps = system_exec(build_commandline(list) & "",0)
	if pps then
		abort(pps)
	end if
	return out_file
end function

/*
<table>
<tr><th>variable</th><th>Definition</th></tr>
<tr><td><i>header_file_name</i> </td><td> the C header file name to prepare. </td></tr>
<tr><td><i>prepared_file_name</i></td><td> the prepared_file_name to write to in the first intermediate language</td></tr>
<tr><td><i>syms</i><td>A map of objects that include variables included with global or with #define</td></tr>
</table>
*/
public function prepare_header(sequence header_file_name, sequence prepared_file_name, object syms)
	object long_name = 0
	integer hfd = -1
	
	trace(1)
	long_name = locate_file(header_file_name,getenv("INCLUDE"))
	if not set:has(opened_files,long_name) then
		hfd = open(long_name,"r")
		
		if hfd = -1 then
			return 2
		end if
	else
		return prepared_file_name
	end if	
	
	opened_files = set:add(opened_files,long_name)
	create_directory(pathname(long_name))
	integer h1fd = open(prepared_file_name,"w")
	if h1fd = -1 then
		return 3
	end if
	sequence buf
	buf = getcs(hfd)
	object line = buf[1]
	integer line_number = buf[2]
	
	sequence preprocessor_stack = {}
	
	while sequence(line) do
		
		sequence token_out = preprocessor_tokenize_string(line, line_number)
		if token_out[ET_ERROR] = ERR_EOL_STRING then
			sequence ret = line
			loop do
				integer c = getc(hfd)
				if c = -1 then				
					token_out = preprocessor_tokenize_string(ret, line_number)
					token_out[ET_ERROR] = ERR_EOF
					exit
				end if
				if c = '\n' then
					line_number += 1
				end if
				ret &= c
				if compare("*/",ret[$-1..$]) = 0 then
					token_out = preprocessor_tokenize_string(ret, line_number)
					line = ret
					exit
				end if
			until 0
			end loop
		end if
			
		sequence token_list = token_out[ET_TOKENS]
		if length(token_list) = 0 then
			goto "next_line"
		end if
		for k = 1 to length(token_list) do
			if token_list[k][TTYPE] = T_NUMBER then
				token_list[k][TDATA] = fix_number(token_list[k][TDATA])
			end if
		end for
		sequence tok1 = token_list[1]
		if tok1[TTYPE] = T_KEYWORD then
			switch tok1[TFORM] do
				case CMD_FUNCTION_DEFINE then
					-- do nothing. ;)
				case CMD_NUMBER_DEFINE then
					string_of_strings definition = repeat("",length(token_list))
					for k = 3 to length(token_list)-1 do
						definition[k] = token_list[k][TDATA]
					end for
					definition = definition[2..$]
					symbol:put(syms, token_list[2][TDATA], 0, definition)
					string_of_integers joined_definition
					joined_definition = seq:join(definition, "")
					if token_list[2][TTYPE] = T_IDENTIFIER and token_list[3][TTYPE] = T_NEWLINE then
						--printf(h1fd,"with define %s;\n",token_list[2][TDATA..TDATA] & {joined_definition})
					else					
						printf(h1fd,"static const int %s = %s;\n",token_list[2][TDATA..TDATA] & {joined_definition})
					end if
				case CMD_INCLUDE then					
					-- do nothing.
					object included_file = regex:matches(include_pattern,line)
					if atom(included_file) or length(included_file)<4 then
						die(header_file_name, line, line_number, "Cannot see an include line here.",{})						
					end if
					object include_dir = getenv("INCLUDE")
					sequence full_path = locate_file(included_file[4],include_dir)
					ifdef not ALLOW_MISSING_FILES then
						if not file_exists(full_path) then
							die(header_file_name, line, line_number, "Cannot open %s", {full_path})
						end if
					end ifdef
				case CMD_IFDEF then
					object ifdef_word = regex:matches(ifdef_pattern,line)
					sequence polarity = ""
					if atom(ifdef_word) then
						die(header_file_name, line, line_number, "Cannot see a word after this ifdef.",{})
					end if
					if equal(ifdef_word[3],"ifndef") then
						polarity = "not"
					end if
					--printf(h1fd,"%sifdef %s %s then\n",{ifdef_word[2],polarity,ifdef_word[4]})
					preprocessor_stack = append(preprocessor_stack, "def")
				case CMD_ENDIF then
					object endif_word = regex:matches(endif_pattern,line)
					if sequence(endif_word) then
						sequence last = preprocessor_stack[$]
						preprocessor_stack = preprocessor_stack[1..$-1] 
						if equal(last,"def") then
							--printf(h1fd,"%s%send ifdef\n",endif_word[2..3])
						end if
					end if
				case CMD_IF then
					if regex:has_match(if_pattern,line) then
						preprocessor_stack = append(preprocessor_stack, "value") 
					end if
				case CMD_ELSE then
					if regex:has_match(else_pattern,line) then
						if equal(preprocessor_stack,"") then
							die(header_file_name, line, line_number, "No #if,#ifdef,#ifndef for this #else",{})					
						end if
						if equal(preprocessor_stack[$],"def") then
							--printf(h1fd,"%s%selsedef\n",else_word[2..3])
						end if
					end if
				case CMD_UNDEF then
					remove_symbol(syms, token_list[2])
			end switch
		end if
		label "next_line"
		puts(h1fd,line)
		buf = getcs(hfd)
		line = buf[1]
		line_number += buf[2]
	end while
	close(h1fd)
	close(hfd)
	loop do
		h1fd = open(prepared_file_name,"r")
	until h1fd != -1
	end loop
	close(h1fd)
	return prepared_file_name
end function
