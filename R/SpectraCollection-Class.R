# TODO: Add comment
# 
# Author: acizmeli
###############################################################################


setClass("SpectraCollection",representation(header="list"), contains="list")#representation(spectras = "list"))

setValidity("SpectraCollection", function(object){
			if(!any(sapply(object, is, "Spectra")) ){
				return("SpectraCollection should contain at least one Spectra object")
			}
			return(TRUE)
		})

