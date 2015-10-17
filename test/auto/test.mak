.ERASE
	
.EXTENSIONS:

.EXTENSIONS:  .e .euphoria
	
.euphoria.e : 
	copy $*.euphoria short.sex
	eui ..\..\..\epp.ex -i short.sex -o short.e
	del short.sex
	move short.e $@

.BEFORE : .symbolic
	set INCUDE = .;%INCLUDE%
	
distclean : .symbolic
	del *.e
