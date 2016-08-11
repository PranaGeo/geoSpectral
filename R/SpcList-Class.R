#########################################################################
# Class : SpcList
#########################################################################
setClass("SpcList",contains="list",
		representation=representation(by="character"), 
		prototype=prototype(by="VariousVariables")
)

setValidity("SpcList", function(object){
#			cat("---------SpcList::setValidity\n")
			if(!all(sapply(object, inherits,"Spectra"))) {
#				return("All SpcList elements should inherit from the Spectra class")
			}
			return(TRUE)
		})
