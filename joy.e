--****
-- == Joy Library
--
-- <<LEVELTOC level=2 depth=4>>
--

-- various functions that brought me joy: $Revision$. 
-- modified $Date: 2020-09-23 12:25:00 -0300 (Wed, 23 Sep 2020) $ 

include misc.e
include file.e
include svn.e as svn
--****
-- === Constant
--

--**
-- A constant for describing the maximum svn version of this file and all files included in it.
--
public constant file_version =  svn:extract_version( "$Revision: 113 $" )


type truth_t(object x)
	return compare(x,0)!=0
end type

--****
-- === Variable
--

--# Creole or EUDOC doesn't process this in to anything here

--****
-- <eucode>
-- public truth_t truth
-- </eucode>
--
-- Description:
-- This variable must always be assigned to true or there will be a type-check error.
--


--**
-- @nodoc@
public truth_t truth
--
--

--****
-- === Types
--

--**
-- True if x is 0 or 1. (true or false)
--

public enum type boolean false=0, tru
end type                                                            

--# non-empty-sequence
type nonemptyseq( sequence s)
	return length(s)
end type

--**
-- True if s is not empty
--

public type non_empty_sequence( sequence s )
	return length(s)
end type


--**
-- True if x is a positive mathematical integer
--
-- Though EUPHORIA integers must be in the range between -(2^^30^^) and 2^^30^^-1 inclusive, the only requirement here is 
-- that the value has no fraction part and be a positive number.
-- 
public type positive_integer( object x )
	if compare(0,x) < 0 and compare(x,{}) < 0 then
		return floor(x) = x
	else
		return 0
	end if
end type

--**
-- True if i is a routine type
--
public type routine_id_type( integer i ) 
	return i > 0
end type

--**
-- True if o is a sequence and each member is an atom.
--
public type string_of_atoms(object o)
    if atom(o) then
	return 0
    end if
    for i = 1 to length(o) by 1 do
	if sequence(o[i]) then
	    return 0
	end if
    end for
    return 1
end type

--**
-- True if o is a sequence and each member is a mathematical integer.
public type string_of_integers(object o)
    if atom(o) then
	return 0
    end if
    for i = 1 to length(o) do		
		if sequence(o[i]) or floor(o[i]) != o[i] then
	    return 0
	end if
    end for
    return 1
end type

--**
-- return true if x is a sequence and each member is a string of atoms.
--
public type string_of_strings( object x )
	if atom(x) then
		return 0
	end if
	for i = 1 to length(x) do
		if not string_of_atoms(x[i]) then
			return 0
		end if
	end for
	return 1
end type

public type sequence_of_strings( object x )
	return string_of_strings(x)
end type

--**
-- Description:
-- A way to quickly produce complicated sequence types.
-- Returns true iff x is a sequence of which each member is of type type_id
-- where type_id is a routine_id of a type routine or the type name itself.
-- 
-- **Example string_of_type 1**\\
-- <eucode>
-- type ascii_char( atom  a  ) 
--     return integer(a) and between_inclusive(0,a,127)
-- end type
--
-- type sequence_of_ascii_char( sequence s )
--     return sequence_of_type( s, routine_id("ascii_char") )
-- end type
--
-- sequence_of_ascii_char s
--
-- s = "Hello World" -- okay
-- s = { 129 } -- type check failure here.
-- s = 32 -- type check failure here too.
-- </eucode>
-- 
-- see [[:string_of_atoms]] [[:string_of_integers]] [[:string_of_strings]]
--
public function string_of_type( object x, routine_id_type type_id )
	integer return_value, rid
	sequence s

	if atom(x) then
		return 0
	end if
	s = x
	
	if stringASCII(type_id) then
		rid = routine_id( type_id )
	elsif atom(type_id) then
		rid = type_id
	else
		return 0
	end if
	
	return_value = 1
	for i = 1 to length(s) do
	    return_value = return_value and call_func( rid, { x[i] } )
	end for
	
	return return_value
end function

--****
-- === Routines
--

--**
-- Description:
-- returns the minimal object of all the members in s.
--
-- An empty sequence results in a type-check error
public function min(nonemptyseq s)
	object m
	m = s[1]
	for i = 2 to length(s) do
		if compare(s[i],m) < 0 then
			m = s[i]
		end if
	end for
	return m
end function

--**
-- Description:
-- returns the maximal object of all the members in s.
--
-- An empty sequence results in a type-check error
public function max(nonemptyseq s)
	object m
	m = s[1]
	for i = 2 to length(s) do
		if compare(s[i],m) > 0 then
			m = s[i]
		end if
	end for
	return m
end function
	
--**
-- Description:
-- This returns true if m is between a and z or m is identical to either a or z.
-- This is useful because it changes a relatively complex condition like this:
-- <eucode>
--      1 <= ya + f * l and ya + f * l <= sqroot
-- </eucode>
-- to this:
-- <eucode>
--      between_inclusive( 1, ya + f * l, sqroot )
-- </eucode>
public function between_inclusive(object a, object m, object z)
    return compare(a,m)<=0 and compare(m,z)<=0
end function

--**
-- This returns true if m is between a and z but identical to neither a nor z.
public function between_exclusive(object a, object m, object z)
    return compare(a,m)<0 and compare(m,z)<0
end function

--**
-- centre a string in whitespace and return it.
public function sputs_centre( sequence s, integer len )
    while length(s) + 2 <= len do
	s = " " & s & " "
    end while
    if length(s) < len then
	s &= " "
    end if
    return s[1..len]
end function

public type pair_of_integers( sequence p ) return length(p)=2 and integer(p[1]) and integer(p[2]) end type

--#
--# A sequence where each member could be an index for another sequence.
--#
type index_sequence(sequence s)
	for i = 1 to length(s) do
		if not integer(s[i]) then return 0 end if
	end for
	return 1
end type

--**
--
public function sum( sequence s ) 
	object o
	o = 0
	for i = 1 to length(s) do o += s[i] end for
	return o
end function

--**
--
public function average( sequence s )
	object o
	o = 0
	for i = 1 to length(s) do o += s[i] end for
	return o / length(s)
end function


--**
--
--
-- returns -1 if x is negative
-- returns 1 otherwise.
--
-- 
public function sign( atom x )
	if x < 0 then
		return -1
	 end if
	return 1
end function

--**
--
-- rounds the object up
--
public function ceil(object x)
	return -floor(-x)       
end function

--**
-- 
-- returns the fraction part of x
public function frac( atom x )
	return x - floor(x)
end function

public function stringof( atom typeid, object x )
	if atom(x) then
		return 0
	end if
	for i = 1 to length(x)  do
		if not call_func(typeid, {x[i]}) then
			return 0
		end if
	end for
	return 1
end function

--**
--
-- Returns the location of object x in s
-- Searching from the end.
public function rfind( object x, sequence s )
	for i = length(s) to 1 by -1 do
		if compare(s[i],x)=0 then
			return i
		end if
	end for
	return 0
end function

--**
-- returns the last match of x in s
public function rmatch( sequence x, sequence s )
	for i = length(s)-length(x)+1 to 1 by -1 do
		truth = length(x) = length(s[i..i+length(x)-1])
		if equal(x,s[i..i+length(x)-1]) then
			return i
		end if
	end for
	return 0
end function

public type string32bit(object o)
	return string_of_integers(o)
end type

public type string16bit(object o )
	if not string_of_integers(o) then return 0 end if
	return string_of_integers(o) and 
	sum(o<#10000)=length(o) and sum(0<=o)=length(o) 
end type

--**
--
-- ASCII characters are only 7 bits long.  Look it up.
public type stringASCII( object o )
	if not string_of_integers(o) then return 0 end if
	return  sum( 0 <= o ) = length( o ) 
	and sum( o < 128 ) = length(o)
end type

public type ASCIIstring( object o ) return stringASCII( o ) end type

--**
--
-- you might use this type to show you are using unportable strings
public type string8bit(sequence o)
	if not string_of_integers(o) then return 0 end if
	return string_of_integers(o) and sum( o < 256 ) = length(o)
	and sum( 0 < o ) = length(o)
end type

public type string_of_sequences( object x )
	if atom(x) then return 0 end if
	for i = 1 to length(x) do if atom(x[i]) then return 0 end if end for
	return 1
end type

--**
-- returns the same sequence with an element missing from the end,
-- in the case of an empty sequence it returns an empty sequence
public function chop( sequence s )
	integer len
	len = length(s)
	if len=0 then
		return s
	else
		return s[1..len-1]
	end if
end function

--**
-- returns the last element of the sequence
public function end_of( nonemptyseq s )
	return s[length(s)]
end function

--**
-- returns the same sequence with the last element changed to o,
-- becareful not to send empty sequences into this.
-- Instead of 
-- <eucode>
-- foo[length(foo)] = baz 
-- </eucode> you can do 
-- <eucode> 
-- foo = set_end_of(foo,baz)
-- </eucode>
public function set_end_of( nonemptyseq s, object o )
	s[length(s)]=o
	return s
end function

--**
-- returns the same with out the newline character on the end of a string
public function chomp( string_of_atoms o )
    if length(o) and o[length(o)]='\n' then
	return o[1..length(o)-1]
    end if
    return o
end function


--**
-- joins a sequence of sequences together with foo
-- returns {{{
-- strings[1] & foo & ... & foo & strings[length(strings)]
-- }}}
-- the following will return the sequence
-- 
-- "C:\Documents and Settings\User" :
-- 
-- <eucode>
-- join("\\",{"C:","Documents and Settings","User"})
-- </eucode>
public function join(object foo, string_of_sequences strings)
    sequence ret
    ret = {}
    if length(strings) then
	ret = strings[1]
    end if
    for i = 2 to length(strings) do
	ret = ret & foo & strings[i] 
    end for
    return ret
end function



--**
-- Description:
--
-- returns foo & strings[1] & foo & strings[2] & .. strings[length(strings)] & foo 
-- 
-- like 
-- <eucode> foo & join(foo, strings ) & foo 
-- </eucode>.
--
public function enclose(object foo, sequence strings )
    return foo & join(foo, strings) & foo
end function

--**
-- Description:
--
-- returns the depth of a sequence
-- The depth is how many times can you index this sequence
-- Strings of characters have a depth 1.
-- sequences of strings of characters have a depth 2.
-- an empty sequence is considered to have a depth of 1.
--
function depth(sequence s)
	integer d, challenge
	d = 1
	for i = 1 to length(s) do
		if not atom(s[i]) then
			challenge = 1 + depth(s[i])
			if challenge > d then
				d = challenge
			end if
		end if
	end for
	return d
end function

--@nodoc@
--# True if s[1] could be a member needle for a haystack s[2]
--#    {'e', "Hello") is a valid needle_haystack_pair,
--#    {  5, "Hello") is a valid needle_haystack_pair,
--#    {"Fred", {"Mary", "Susan"}} is a valid needle_haystack_pair.
--#
--#  However, {"Fred", "Sam"} is not a valid needle_haystack_pair.
--#
--#  An empty sequence could potentially be a haystack for any kind of object.
type needle_haystack_pair(sequence s)
	-- needle must be of the same degree as the haystack elements
	if length(s[2]) and depth(s[1])<depth(s[2]) then 
		return 0
	end if
	return 1
end type


--**
-- Does the opposite of join:
-- Finds occurences of needle in haystack and returns a sequence 
-- of sequences where each entry is before or after an occurance of needle.
-- NOTE:
-- To split a path up you need to pass "/"  as foo instead of '/'!.
-- Example: 
-- {{{
-- The expression split( "\\", "C:\Documents and Settings\User" ) will return the sequence :
-- {"C:","Documents and Settings","User"}
-- }}}
-- 
-- 
-- 
--
public function sequence_split(sequence needle, sequence haystack )
    integer loc
    object ret
	needle_haystack_pair check 
	check = { needle, haystack } -- type check arguments
    --printf(1,"\nsplit(",{})print(1,needle)puts(1,",")print(1,haystack)puts(1,")=")
    loc = match(needle, haystack )
    --printf(1,"loc=%d\n",{loc})
    if loc then
	ret = {haystack[1..loc-1]} & sequence_split(needle, haystack[loc+1..length(haystack)])
    else
	ret = {haystack}
    end if
    --print(1,ret)    puts(1,"\n")
    return ret
end function

--**
-- short name for sequence_split
--
-- See [[:sequence_split]]
public function split(sequence needle, sequence haystack)
    return sequence_split(needle,haystack)
end function

--**
-- changes an ASCII string to upper case.  It will not covert
-- characters that are not ASCII.
public function toupper_ASCII( object x )
    if atom(x) then
	if between_inclusive('a',x,'z') then
	    return x - 'a' + 'A'
	else
	    return x
	end if
    end if
    for i = 1 to length(x) do
	if between_inclusive('a',x[i],'z') then
	    x[i] += ('A'-'a')
	end if
    end for
    return x
end function

--**
-- changes an ASCII string to lower case.  It only
-- converts ASCII strings
public function tolower_ASCII(object x)
     if atom(x) then
	 if between_inclusive('A',x,'Z') then
		x += 'a' - 'A'
	end if
	return x
    end if
    for i = 1 to length(x) do
	if compare('A',x[i])<=0 and compare(x[i],'Z')<=0 then
	    x[i] += ('a'-'A')
	end if
    end for
    return x
end function

--**
-- returns n!.
function factorial(integer n)
    integer m
    if n < 1 then
	return 1
    end if
    m = 1
    while n > 1 do
	m *= n
	n -= 1
    end while
    return m
end function

--**
-- returns count objects from the sequence S at random.
public function choose_noreplacement( integer count, object S )
    sequence outset
    object choice
    if 0 > count  then
	return 0
    end if
    -- here count >= 0
    if sequence(S) then
	if count > length(S) then
	    -- there are no sets of count size that we can take from
	    -- a smaller set.
	    return 0
	end if
	-- here 0 <= count < length(S)
	outset = {}
	while count > 0 do
	    choice = rand(length(S))
	    outset = append( outset, S[choice] )
	    S = S[1..choice-1] & S[choice+1..length(S)]
	    count -= 1
	end while
	return outset
    elsif integer(S) then       
	-- In this case it will return the number of such possible
	-- sets by choosing count from a set of length S.
	if not between_inclusive(0,count,S) then
	  return 0
	end if
	return factorial(S)/(factorial(count)*factorial(S-count))
    else
	-- here S is a non-integer number
	-- not defined.  Maybe you should call a Gamma function?
	return -1
    end if
end function

--**
-- If s is a sequence and i a sequence of indicies, then 
-- index_get(s,i) returns 
-- s[i[1]]....[i[length(i)].
-- 
-- if i is empty, it returns s.
-- 
-- Each i[j] must be an index of {{{ s[i[1]][...][i[j-1]] }}}
--  for j > 1 and i[1] must be an index of s or the program will crash.
-- This is useful especially for getting information out of structures returned without putting the return value into a variable first.
--  
public function index_get( sequence s, index_sequence i )
	while length(i) > 2 and string_of_integers(i) do
		s = s[i[1]][i[2]][i[3]]
		i = i[4..length(i)]
	end while
	if length(i) = 2 then
		return s[i[1]][i[2]]
	elsif length(i) = 1 then
		return s[i[1]]
	elsif length(i) = 0 then
		return s        
	end if
end function

--**
-- If s is a sequence and i a sequence of indicies then index_set(s,i,foo) 
-- executes s[i[1]]....[i[length(i)]=foo and returns the modified sequence
-- For example:
-- s = index_set( s, {3,4}, 8.777 ) is equivalent to: s[3][4] = 8.777
public function index_set( sequence s, index_sequence i, object v )
	if length(i) = 0 then
		return v
	elsif length(i) = 1 then
		s[i[1]] = v
		return s
	elsif length(i) = 2 then
		s[i[1]][i[2]] = v
		return s
	else
		s[i[1]][i[2]] = index_set( s[i[1]][i[2]], i[3..length(i)], v )
	end if
	return s          
end function


constant ps  = { ';', ';' } & repeat( ':' , 7 )
constant slash = { '\\', '\\' } & repeat( '/', 7 )
constant SLASH = slash[platform()]


--**
-- The next are like slash and split with the
-- OS specific file path seperators entered for you
public function slashjoin(string_of_sequences s)
	return join({SLASH},s)
end function

public function slashsplit(sequence s)
	return sequence_split({SLASH},s)
end function

--**
-- Search the string var for locations the file named by s might be.
-- var is seperated by semicolons in Windows and colons for other 
-- platforms.  
--
-- For example.  Under UNIX you can find the location of grep using:
-- <eucode>
-- which( "grep", getenv("PATH") )
-- </eucode>
--
-- Under MSDOS find the location of fdisk.exe with:
-- <eucode>
-- which("fdisk.exe", getenv("PATH") )
-- </eucode>
--
-- Under any platform find the location of  a EUPHORIA library file with:
-- <eucode>
-- which("unicode.ew", getenv("EUINC") )
-- </eucode>
public function which( string8bit s, object V )
    sequence dirs
    if not stringASCII(V) then
	return ""
    end if
    dirs = split({ps[platform()]}, V )
    for i = 1 to length(dirs) do
	if sequence(dir(dirs[i] & SLASH & s)) then
	    return dirs[i] & SLASH & s
	end if
    end for
    return s
end function

--**
-- Description:
-- Returns a consecutive number sequence starting from 1 and ending in k:
-- example: 
-- <eucode>
-- sequence p = make_id_permutation(6) -- p is { 1,2,3,4,5,6 }
-- </eucode>
public function make_id_permutation(integer k)
    sequence result
    result = repeat( 0, k )
    for i = 1 to k  do
	result[i] = i
    end for
    return result
end function

--**
-- return an integer interval [beg,last] as a sequence
-- for example: make_integer_interval( 3, 5 ) returns {3,4,5}
public function make_integer_interval( integer beg, integer last )
	sequence result
	result = repeat( 0, last - beg + 1 )
	for i = 1 to last - beg + 1 do
		result[i] = i + beg - 1
	end for
	return result
end function

--**
-- perl style || operator.  Take a sequence and perl or them together.
-- the result is the first non-zero entry in s is returned.  This is useful for constant declarations.
public function ior( non_empty_sequence s )
	object result
	result = s[1]
	while length(s) do
		if compare(s[1],0)!=0 then 
			return result
		end if
		s = s[2..length(s)]
	end while
	return 0
end function


--**
-- Treats s1 and s2 as sets and returns ( s1 \ s2 )
-- s1 and s2 may have repeated entries.  
-- In other words, what is returned a sequence of members of s1
-- that are also not members of s2.
public function set_diff( sequence s1, sequence s2 )
	integer loc
	for i = 1 to length(s2) do
		loc = find( s2[i], s1 )
		while loc do
			s1 = s1[1..loc-1] & s1[loc+1..$]
			loc = find( s2[i], s1 )
		end while
	end for
	return s1
end function

--**
-- returns a the sequence s with all of the members of s equal to x removed.
-- For example:  remove-objects( {5,4,3,2,4,5,1,{5} }, 5 ) = { 4,3,2,4,1,{5}}
-- See [[:remove_0s]]
public function remove_objects( sequence s, object x )
	integer p
	p = find( x, s )
	while p do
		s = s[1..p-1] & s[p+1..length(s)]
		p = find( x, s )
	end while
	return s
end function

--**
-- Description:
-- returns a sequence without any 0 members
-- Comment:
-- Useful with find and min/max functions
-- <eucode>
-- k = max( min( remove_0s( { find('.',s), match("foo",s) } ) ) & 0 )
-- </eucode>
-- k is the first occurance of either '.' or "foo", or 0 if there is there is neither a
-- '.' nor a "foo".
-- <eucode>
-- j = max( remove_0s( { rfind('.',s), match("bar",s) } ), 0 )
-- </eucode>
-- j is the first occurance of either . or bar or 0 if there is neither.
-- <eucode>
-- location = file_location[1..max({ find('\\',file_location)-1, find('/', file_location)-1,0 })]
-- </eucode>
-- location is the directory part of file_location
-- see [[:remove_objects]]
public function remove_0s( sequence s )
	return remove_objects( s, 0 )
end function

--**
-- Returns true iff x is a sequence of length i.
public function seq_of_len( object x, integer i )
	return sequence(x) and length(x) = i
end function

--**
-- Description:
-- Returns the directory where the calling Euphoria program is located.
-- Comment:
-- The directory string returned although guaranteed correct are probably not cononical.
public function get_programs_resident_directory()
	sequence s,s2
	integer sl,fd
	
	s = command_line()
	s2 = s[2]
	sl = length(s2)
	while sl and s2[sl] != SLASH do
		sl -= 1
	end while
	if sl then
		--printf(1,"Changing to directory %s...\n",{s2[1..sl-1]})
		return s2[1..sl-1]
	else -- no slashes we are in the directory already or the shell is giving it to us 
		-- via PATH...
		fd = open(s2,"r")
		if fd = -1 then
		    -- in the PATH
		    return which(s2, getenv("PATH") )
		else
		    close(fd)
		    -- in the same directory
			return "."
		end if          
	end if  
end function

--**
-- Description:
-- Prompt the user to press enter and wait forever for it before exiting.
public procedure PETC()
	object line
	puts(1,"Press Enter to Continue...")
	line = gets(0)
end procedure


--**
-- restricts a value to a bounding value or values.
--
-- If depth is 0, this is taken as a shallow operation.  If x compares as less than minimum constrain returns the minimum.  If x compares as more than maximums it returns maximums otherwise it returns x.  We can consider this as bounding points to a line segment from an infinite line whereas the space is not real but the space of sequences.  In this way of thinking we talk about "The Complex Line."
--
-- If depth is 1, we are going deeper, and we are bounding x's members according to the
-- members of minimum and maximums.  Here we are bounding in an n-dimmensional rectangle
-- what ever field x[] is in.
--
-- In general the return value r, will be such that for any index set: i_1, i_2, ... i_depth r[i_1][i_2]...[i_depth] will be  minimum[i_1][i_2]...[i_depth] if that first value was less than the second value and similar for the maximum and x[i_1][i_2]...[i_depth] otherwise.
--
-- Example:  constrain({3,3},{5,1},{6,6},0) is {5,1} because these are considered one dimmentional on the space of EUPHORIA objects with order defined by compare().  So the minimum is {3,3} and the maximum is {6,6} and {5,1} is between.  Yet, constrain({3,3},{5,1},{6,6},1) is {5,3} for here we are constraining the members on a two dimmensional rectangle over the space of EUPHORIA objects with order defined by compare().  The number 5 is between 3 and 6 but 1 is not between 3 and 6.
--
--
public function constrain( object minimum, object x, object maximums, integer depth )
	if depth = 0 then
		if compare(minimum,x)>0 then
			return minimum
		elsif compare(x,maximums)>0 then
			return maximums
		else
			return x
		end if
	elsif atom(x) or atom(minimum) or atom(maximums) then		
		printf(1,"Error: constrain depth too large for this value", {})
		PETC()
	elsif compare({},remove_objects({length(x),length(minimum),length(maximums)},length(x))) 
		!= 0 then
		printf(1, "Error: Lengths are not the same.", {})
		PETC()
	else
		for i = 1 to length(x) do
			x[i] = constrain(minimum[i],x[i],maximums[i], depth-1)
		end for
		return x
	end if
end function

public function find_replace( sequence needles, sequence replace_needles, sequence haystack )
	integer nl
	for i = 1 to length(needles) do
		while 1 do
			nl = rfind(needles[i],haystack)
			if nl = 0 then
				exit
			end if
			haystack = find_replace(needles[i..i],replace_needles[i..i],haystack[1..nl-1]) &
				 replace_needles[i] & haystack[nl+1..$]
		end while
	end for
	return haystack
end function

public function match_replace( string_of_strings needles, string_of_strings replace_needles, string_of_integers haystack )
	integer nl, nf, limit
	for i = 1 to length(needles) do
		limit = length(haystack)
		while 1 do
			nl = rmatch(needles[i],haystack[1..limit])
			if nl = 0 then
				exit
			end if
			nf = nl + length(needles[i]) - 1
			truth = equal(haystack[nl..nf],needles[i])

			haystack = haystack[1..nl-1] & replace_needles[i] & haystack[nf+1..$]

			limit = nl-1  -- prevent matches on modified part of the string
				-- this should prevent infinite loops with rpelacements like "van" => "vanguard"			
		end while
	end for
	return haystack
end function

public function interpreter_name()
	sequence cmdl
	integer spos, dpos
	object eudir
	cmdl = command_line()
	cmdl = "\\" & cmdl[1]
	spos = length(cmdl)
	dpos = 0
	while cmdl[spos] != '\\' do
		if cmdl[spos] = '.' then
			dpos = spos
		end if
		spos -= 1
	end while
	if dpos != 0 then
		cmdl = cmdl[1..dpos-1]
	end if
	cmdl = cmdl[spos+1..length(cmdl)]
	if find(cmdl,{"exw","eui"}) then
		return cmdl
	end if
	eudir = getenv("EUDIR")
	if atom(eudir) then
		-- assume 4.0 +
		return "eui"
	elsif sequence(dir(eudir &"\\" & "eui.exe")) then
		return "eui"
	else
		return "exw"
	end if	
end function

--**
-- return true if the OS uses forward slashes
public function is_unix()
	return platform() > WIN32
end function

--**
-- Description:
-- Assign to a EUPHORIA structure in 4.0.  How do we do structures in 4.0?
-- <eucode>
-- include std/io.e
-- include joy.e
--
-- type enum st_foo_index
--        name,
--        income,
--        gender
-- end type
--
-- type st_foo(object s)
--     if sequence(s) and length(s) = 3 then
--         return 1
--     else
--         return 0
--     end if
-- end type
-- 
-- st_foo a_foo = struct_assign({
--	name, "John Smith", 
--	gender, "male", 
--	income, 50_000})
--
-- sprintf(io:STDOUT, "%s is a %s and he makes %d per year.", {a_foo[name], a_foo[gender], a_foo[income]})
-- </eucode>
-- The routine removes the need for us to know the ordering of the structure members to assignment to it.  st_foo can be made as strict as we want.  
public function struct_assign(sequence s)
	integer maxi
	sequence ret
	maxi=0
	for i = 1 to length(s) by 2 do
		if s[i] > maxi then
			maxi = s[i]
		end if
	end for
	ret = repeat(0,maxi)
	for i = 1 to length(s) by 2 do
		ret[s[i]] = s[i+1]
	end for
	return ret
end function

public function member_set(sequence s, integer i, object x)
	if i > length(s) then
		s &= repeat(0,i-length(s))
	end if
	s[i] = x
	return s
end function

public function set_member(sequence s, integer i, object x)
	return member_set(s,i,x)
end function	

public function range(integer i1, integer i2)
	sequence s
	s = repeat( i1 - 1, i2-i1+1 )
	for i = 1 to length(s) do
		s[i] += i
	end for
	return s
end function

--**
-- Description: 
-- Returns the date in the form { YYYY, MM, DD, HH, MM, SS, DoW, DoY } with YYYY being the number of years counting from the Birth of Christ,
-- MM being the month number counting from 1 being January
-- DD being the day of the month
-- HH, MM, SS, being hours, minutes and seconds, respectively.
-- DoW being the day of week, Sunday = 1
-- DoY being the julian date
-- See [[:date]]
public function gregorian_date()
	sequence s = date()
	s[1] += 1900
	return s
end function
--#
