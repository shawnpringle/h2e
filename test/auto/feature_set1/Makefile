test : .symbolic t_empty.e t_manyconstants.e t_winver.e
	eutest 

t_manyconstants.e : t_manyconstants.euphoria ..\..\..\epp.ex ..\..\..\*.e
	copy t_manyconstants.euphoria short.sex
	eui ..\..\..\epp.ex -i short.sex -o short.e
	-del short.sex
	move short.e $@
	
t_empty.e : t_empty.euphoria  ..\..\..\epp.ex ..\..\..\*.e

t_winver.e : t_winver.euphoria  ..\..\..\epp.ex ..\..\..\*.e

!include ..\test.mak

