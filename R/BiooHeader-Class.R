# TODO: Add comment
# 
# Author: acizmeli
###############################################################################
#removeClass("Spectra")
if (1) {
	setClass("BiooHeader", contains="list",
			prototype=prototype(list(Station="",Cruise="")))
	
}
setMethod("initialize",
		signature(.Object = "BiooHeader"),
		function (.Object, ...) 
		{
#			cat("---------BiooHeader::Initialize\n")						
			.Object <- callNextMethod()

			#			validObject(.Object)
			return(.Object)
		}
)

setValidity("BiooHeader", function(object){
#			cat("---------Bioo::setValidity\n")
#			if(! class(object@myName)=="character"){
#				return(" myName should be a character object")
#			}
			return(TRUE)
		})