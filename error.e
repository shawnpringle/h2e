-- error 
include std/cmdline.e
include std/console.e
include std/filesys.e

public procedure maybe_edit(sequence file_name, integer line_number)
	if equal(locate_file("jedit.bat",PATH),"jedit.bat") then
		printf(2,"We suggest you download jEdit for easier development.",{})
		return
	end if
	printf(2,"Edit this file? (y/n)",{})
	integer k = get_key()
	while k = -1 do
		k = get_key()
	end while
	puts(2,10)
	if k = 'y' then
		puts(2,"Opening editor program...  Press Enter when you finish editing.\n")
		system(build_commandline({"jedit",file_name,sprintf("+line:%d",{line_number})}),1)
	end if
end procedure

public procedure die(sequence file_name, sequence line, integer line_number, sequence fmt, sequence args) 
	printf(2,"Parse Error: in %s line %d.\n",{file_name,line_number})
	printf(2,"->%s",{line})
	printf(2,fmt & 10,args)
	maybe_edit(locate_file(file_name, getenv("INCLUDE")), line_number)	
	any_key()
	abort(1)							
end procedure



