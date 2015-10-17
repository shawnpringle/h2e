test : .symbolic t_c_notseeable.e t_allconstants.e
	eutest 

t_c_notseeable.e : t_c_notseeable.euphoria ..\..\..\epp.ex ..\..\..\*.e

t_allconstants.e : t_allconstants.e ..\..\..\epp.ex ..\..\..\*.e

.BEFORE : 
	set INCUDE = .;%INCLUDE%
	
!include ..\test.mak

