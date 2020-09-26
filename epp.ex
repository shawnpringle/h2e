include std/map.e as map
include euphoria/tokenize.e as eutok
include std/datetime.e -- now() and format()
include std/io.e       -- read_file() and write_file()
include std/search.e  as search -- match_replace()
include std/error.e
include preprocessor.e
include c.e
include std/filesys.e as fs
include euphoria.e
include std/get.e
include std/text.e

with define DEBUG

sequence cmds = command_line()
sequence inFileName, outFileName
sequence included_files
map:map syms
map:map with_flags

for i = 3 to length(cmds) do
    switch cmds[i] do
        case "-i" then
            inFileName = cmds[i+1]
            ifdef DEBUG then
            	puts(2, "Processing file : " & inFileName & '\n')
            end ifdef
        case "-o" then
            outFileName = cmds[i+1]
    end switch
end for

constant temp_dir = getenv("TEMP")
constant product_dir = canonical_path(`C:\Users\Public\Documents\dev\h1`)

-- return the same with backslashes before special characters
-- and replacing certian characters with escape sequences appropriate
-- for C.
function escape_string(sequence s)
	return s
end function
with trace
public function convert_header(sequence pre_include_file, sequence header_file_name, sequence temp_directory, object syms)
	sequence shortest_include_dir_list = upper(getenv("WATCOM") & `\h\`)
	sequence can_header_file_name
	sequence prepared_name

	if not file_exists(header_file_name) then
		header_file_name = locate_file(header_file_name,getenv("INCLUDE"))
	end if
	
	integer regenerate = 0
	if begins(shortest_include_dir_list,upper(header_file_name)) then
		prepared_name = pathname( product_dir & SLASH & header_file_name[length(shortest_include_dir_list)+1..$] ) & SLASH & filebase(header_file_name) & ".h1"
		object header_data = fs:dir(header_file_name)
		object prepared_header_data = fs:dir(prepared_name)
		-- header_data must be a sequence
		if sequence(prepared_header_data) then
			-- create if the C header is newer than the prepared header.
			if length(prepared_header_data) != 1 then
				crash("Internal Error: prepared_header %s returns more than one file in dir().", {prepared_name})
			end if
			regenerate = eu:compare(header_data[1][fs:D_YEAR..fs:D_SECOND],prepared_header_data[1][fs:D_YEAR..fs:D_SECOND]) > 0
		else
			-- prepared name doesn't exist
			-- create directory and create.
			integer sloc = search:rfind(SLASH,prepared_name)
			if sloc and create_directory(prepared_name[1..sloc-1]) then
			end if
			regenerate = 1
		end if
	else
		-- use temp file
		prepared_name = temp_file(,,"h1",0)	
		regenerate = 1
	end if
	
	
	sequence preprocessed_name = temp_dir & SLASH & filebase(header_file_name) & ".h2"
	sequence processed_name = temp_dir & SLASH & filebase(header_file_name) & ".e"
	if regenerate then
		-- create the .h1 file
		if atom(prepare_header(header_file_name, prepared_name, syms)) then
			crash("Cannot prepare header file.")
		end if
	end if
	ifdef DEBUG then
		printf(2,"%s=>%s=>%s=>%s\n", { header_file_name, prepared_name, preprocessed_name, processed_name} )
	end ifdef
	preprocess_header(pre_include_file, prepared_name, preprocessed_name)
	return process_header(preprocessed_name, processed_name,syms,with_flags)
end function

global object PATH = getenv("PATH")
if atom(PATH) or equal(locate_file("owcc.exe",PATH),"owcc.exe") then
	-- couldn't find Preprocessor for C
	crash("This version of EUPHORIA requires a C Preprocessor but none was found.")
end if


-- parse a .euphoria file.
with_flags = map:new()
map:put(with_flags,"c_errors",1)
syms = map:new()
eutok:keep_newlines(1)
eutok:string_numbers(1)
sequence content = eutok:tokenize_file(inFileName)
sequence base_header_name = temp_file(,,"h")
ifdef DEBUG then
	printf(2, "base_header_name is %s\n", {base_header_name})
end ifdef
sequence header_list = {base_header_name}
integer base_header = open(base_header_name, "w")
included_files = {base_header_name}
integer ofd = open(outFileName,"w")
integer len = length(content[ET_TOKENS])
sequence tokens = content[ET_TOKENS] & repeat({T_EOF, {}, 0, 0, 0},4)
integer i = 1
while i <= len do
	sequence tok1 = tokens[i]
	object tdat1 = tok1[TDATA]
	
	if tok1[TTYPE] = T_KEYWORD  and (i < len) then
	 	sequence tok2 = tokens[i+1]
	 	sequence tdat2 = tok2[TDATA]
	 	sequence tdatlast
	 	if len > 1 then	
	 		tdatlast = tokens[len][TDATA]
	 	end if
	 	integer withorwithouti = find(tdat1,{"without","with"})
	 	if equal(tok1[TDATA],"include") then
			object ret
			if ends(".h",tdat2) or ends(".h",tdatlast) then
				tdat2 = search:find_replace( "\\", tdat2, {SLASH})
				printf(base_header,"#include <%s>\n", {tdat2})
				close(base_header)
				ret = convert_header(header_list, tok2[TDATA], temp_dir, syms)
				sequence full_path = locate_file(tdat2,getenv("INCLUDE") & ";.")
				header_list = append(header_list,full_path)
				base_header = open(base_header_name, "a")
			else
				ret = tok2[TDATA]
			end if
			if atom(ret) then
				crash("Cannot open \'%s\' for reading.",{tok2[TDATA]})
			end if
			tokens[i+1][TDATA] = ret
			-- we want these modified tokens to go to the .ex file
			-- leave i alone.
	 	elsif withorwithouti then
	 		map:put(with_flags,tdat2,withorwithouti-1)
	 	        if find(tdat2,{"c_errors"})!=0 then
	 	        	-- we don't want EUPHORIA 4 to see this
		 		i+=2
		 		continue
		 	end if
	 	elsif equal(tdat1,"with") and find(tdat2,{"c_errors","c_error"})!=0 then
	 		map:put(with_flags,"c_errors",1)
	 		i+=2
	 		continue
	 	elsif i+2 <= len then	 	
	 		sequence tdat3 = tokens[i+2][TDATA]
	 		if equal(tok1[TDATA],"with") and equal(tok2[TDATA],"define") then
				map:put(syms,tdat3,{})
				printf(base_header,"#define %s\n", {tdat3})
	 		elsif i+3 <= len then
	 			sequence tdat4 = tokens[i+3][TDATA]
	 			if i+4 <= len and 
	 				equal(tok1[TDATA],"global") and 
	 				equal(tok2[TDATA],"constant") and 
	 				tokens[i+4][TTYPE] = T_NUMBER and 
					 tokens[i+3][TTYPE] = T_EQ then
						object tbuf = parse_expression(tokens,i+4) -- value
						if sequence(tbuf) then
							tbuf = evaluate_expression(tbuf)
							if tbuf[1] = GET_SUCCESS then
								map:put(syms,tdat3,tbuf[2])
								printf(base_header,"#define %s %s\n", {tdat3, tbuf[2]})
							end if								
						end if
	 			end if
	 		end if
	 	end if
	end if	
	sequence pd = tok1[TDATA..TDATA]
	switch tok1[TTYPE] do
		case eutok:T_NEWLINE then
			puts(ofd,'\n')
		case eutok:T_STRING then
			switch tok1[TFORM] do
				case eutok:TF_STRING_BACKTICK then
					printf(ofd,"`%s`",pd)
				case eutok:TF_STRING_SINGLE then
					printf(ofd,"\"%s\"",pd)
				case eutok:TF_STRING_TRIPLE then
					printf(ofd,"\"\"\"%s\"\"\"",pd)
				case else
					crash("Unknown kind of string found in tokenizer...")
			end switch
		case eutok:T_CHAR then
			printf(ofd,"\'%s\'",pd)
		case else
			puts(ofd,tok1[TDATA] & ' ')
	end switch
	i += 1
end while
puts(ofd,tokens[$][TDATA])
close(base_header)
close(ofd)




