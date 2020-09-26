include euphoria/tokenize.e
include more_tokens.e
include error.e
constant define_len = length("#define ")
sequence immediate_defines = { {"__cplusplus", 0} } -- list of preprocessor ids and whether they are defined or not used by this translator
include ../joy.e

type command_string(sequence s)
	if s[1]='#' and id_string(s[2..$]) then
		return 1
	end if
	return 0
end type



public type my_token_type(sequence s)
	if length(s) = TFORM and string_of_integers(s[TDATA]) then
		return 1
	else
		return 0
	end if
end type

constant cmds = {"##", "#define()", "#define","#undef","#ifdef","#ifndef","#endif",
				"#if", "#include", "#else", "#"}

public enum     CMD_CONCAT, CMD_FUNCTION_DEFINE, CMD_NUMBER_DEFINE, CMD_UNDEF, CMD_IFDEF, CMD_IFNDEF, 
	CMD_ENDIF, CMD_IF, CMD_INCLUDE, CMD_ELSE, CMD_STRING


public function preprocessor_new_token(sequence s, integer line_number, integer pos)
	object t
	sequence buf
	sequence r
	t = repeat(0,TFORM)
	integer key
	t[TDATA] = s
	t[TLNUM] = line_number
	t[TLPOS] = {pos,length(s)+pos-1}
	if match("/*",s)=1 and match("*/",s)=length(s)-1 then
		t[TTYPE] = T_COMMENT
		t[TFORM] = TF_COMMENT_MULTIPLE
	elsif id_string(s) then
		t[TTYPE] = T_IDENTIFIER
		return t
	-- elsif command_string(s) and (handled by preprocessor_tokenize_string)
	elsif number_string(s) then
		t[TTYPE] = T_NUMBER
	elsif delim_char(s[1]) then
		t[TTYPE] = T_DELIMITER
	elsif quoted_string(s) then
		t[TTYPE] = T_STRING
		t[TFORM] = 0
	elsif quoted_char(s) then
		t[TTYPE] = T_CHAR
		t[TFORM] = TF_STRING_SINGLE
	elsif equal(s,"\n") then
		t[TTYPE] = T_NEWLINE
	else
		t = ERR_UNKNOWN
	end if
	return t
end function
with trace

-- always should have white space on the end...
public function preprocessor_tokenize_string(sequence string_data, integer line_number)
	sequence tokens = {}
	sequence ret
	object tok
	integer error = ERR_NONE
	integer starti = 1,i = 1
	integer len = length(string_data)
	ret = repeat(0,TFORM)
	if not ws_char(string_data[$]) and not delim_char(string_data[$]) then
		string_data = string_data
	end if
	while i <= length(string_data) do
		if string_data[i] = '\n' then
			tok = repeat(0,TFORM)
			tok[TDATA] = string_data[i..i]
			tok[TTYPE] = T_NEWLINE
			tok[TLNUM] = line_number
			tok[TLPOS] = {i,i}
			tok = preprocessor_new_token(string_data[i..i], line_number, i)
		elsif ws_char(string_data[i]) then
			while i <= length(string_data) and ws_char(string_data[i]) do
				i += 1
			end while
			starti = i
			continue
		elsif i < length(string_data) and equal(string_data[i..i+1],"/*") then
			integer end_of_c = match("*/", string_data, i+2 )
			if end_of_c = 0 then
				tok = ERR_EOL_STRING
				i = length(string_data)
			else	
				tok = preprocessor_new_token(string_data[i..end_of_c+1], line_number, i)
				i = end_of_c+1				
			end if
		elsif i < length(string_data) and find(string_data[i..i+1],c_dc_ops) then
			tok = preprocessor_new_token(string_data[i..i+1], line_number, i)
			i += 1
		elsif string_data[i] = '\'' then
			if i+2 > len then
				tok = ERR_EOL_STRING
			else
				tok = preprocessor_new_token(string_data[i..i+2], line_number, i)
			end if
			i += 2
		elsif string_data[i]='#' then
			integer k = 0
			loop do
				k += 1										
			until eu:match(cmds[k],string_data,i)
			end loop
			tok = repeat(0,TFORM)
			-- CMD_FUNCTION_DEFINE will not be found this way.
			-- We must check for a parenthesis below.
			tok[TDATA] = cmds[k]
			tok[TTYPE] = T_KEYWORD
			tok[TFORM] = k
			tok[TLNUM] = line_number
			tok[TLPOS] = {i,i+length(cmds[k])-1}
			i += length(cmds[k])-1
		elsif string_data[i]='(' then
			if length(tokens)>1 and tokens[$-1][TTYPE] = T_KEYWORD and
				tokens[$-1][TFORM] = CMD_NUMBER_DEFINE then
				tokens[$-1][TFORM] = CMD_FUNCTION_DEFINE
			end if
			tok = preprocessor_new_token(string_data[starti..i], line_number, starti )
		else
			loop do
				i += 1
			until ws_char(string_data[i]) or delim_char(string_data[i]) 
			end loop
			i -= 1
			tok = preprocessor_new_token(string_data[starti..i], line_number, starti )
		end if
		tokens = append(tokens, tok)
		if atom(tok) then
			error = tok
			tokens = tokens[1..$-1]
			ret[ET_ERR_LINE] = line_number
			ret[ET_ERR_COLUMN] = {starti,i}
		end if		
		i += 1
		starti = i
	end while
	ret[ET_TOKENS] = tokens
	ret[ET_ERROR] = error
	return ret
end function

public function preprocessor_tokenize_file(sequence file_name)
	integer in = open(file_name,"r")
	object line
	integer ln = 0
	sequence tokens = {0,0,0,{0,0}}
	tokens[ET_TOKENS] = {}
	line = gets(in)
	while sequence(line) do
		sequence new_toks = preprocessor_tokenize_string( line, ln )
		tokens[ET_ERR_LINE] = new_toks[ET_ERR_LINE]
		tokens[ET_ERR_COLUMN] = new_toks[ET_ERR_COLUMN]
		tokens[ET_ERROR] = new_toks[ET_ERROR]
		tokens[ET_TOKENS] = tokens[ET_TOKENS] & new_toks[ET_TOKENS] 
		if new_toks[ET_ERROR] != ERR_NONE then
			return tokens
		end if
		line = gets(in)
	end while
	close(in)
	return tokens
end function

public procedure Serious(sequence system_state, my_token_type this_token, integer error_number, sequence message, sequence args)
	sequence file = system_state[1]
	integer interactive_value = system_state[3]
	if interactive_value = 0 then -- just continue..
		return 
	end if
	printf(2,"%s:%d\n",{file,this_token[TLNUM]})
	printf(2,"<%04d>:: %s:\n",{error_number,message})
	puts(2,"\n")
	sequence buf = sprintf("    %s (%d): ",{file,this_token[TLNUM]})
	printf(2,"%s%s\n", {buf,this_token[TDATA]})
	puts(2,repeat(' ',length(buf)))
	puts(2,repeat('^',1+this_token[TLPOS][2]-this_token[TLPOS][1]))
	puts(2,"\n")
	maybe_edit(file, this_token[TLNUM])	
end procedure
