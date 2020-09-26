include get.e as get

-- Function extracts a version out a string such as $Revision: 86 $
-- A file that you want to keep track of its version should start in the following manner:
--    include svn.e as svn
--    export constant file_version = extract_version( "$Revision$" )
--
-- A program can have its svn version tracked as the maximum of the file_versions of all of the 
-- files it includes like this:
--
-- include svn.e as svn
-- include joy.e as joy
-- include otherlongnamed.e as foo
-- include bar.e as bar
-- constant file_version = extract_version( "$Revision$" )
-- constant program_version = joy:max( file_version & svn:file_version & 
--                                     foo:file_version & 
--                                     bar:file_version & joy:file_version)
--
--
-- Both the include file and program file must be set with the svn property for substituting
-- the version number into strings like $Revision: 86$
--
global function extract_version( sequence s )
	integer loc
	loc = match("$Revision: ",s)
	s = get:value( s[loc+1..$] )
	if s[1] = GET_SUCCESS then
		return s[2]
	else
		return 0
	end if
end function
-- A constant for describing the maximum svn version of this file and all files included in it.
global constant file_version =  extract_version( "$Revision: 86 $" )
