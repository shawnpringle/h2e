test : .symbolic t_empty.e t_manyconstants.e t_functions.e
	eutest 

PREPROCESSOR = ..\..\..\epp.ex ..\..\..\*.e

t_manyconstants.e : t_manyconstants.euphoria $(PREPROCESSOR)

t_functions.e : t_functions.euphoria $(PREPROCESSOR)

!include ..\test.mak

