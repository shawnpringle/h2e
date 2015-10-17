test : .symbolic t_manyconstants.e
	eutest 

t_manyconstants.e : t_manyconstants.euphoria ..\..\..\epp.ex ..\..\..\*.e
	copy t_manyconstants.euphoria short.sex
	eui ..\..\..\epp.ex -i short.sex -o short.e
	-del short.sex
	move short.e $@
	

!include ..\test.mak

