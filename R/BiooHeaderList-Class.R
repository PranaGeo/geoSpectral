# TODO: Add comment
# 
# Author: acizmeli
###############################################################################
#removeClass("Spectra")
if (1) {
	setClass("BiooHeaderList", contains="list",
			prototype=prototype(list(new("BiooHeader"))))
	
}
setMethod("initialize",
		signature(.Object = "BiooHeaderList"),
		function (.Object, ...) 
		{
#			cat("---------BiooHeader::Initialize\n")						
			.Object <- callNextMethod()

			#			validObject(.Object)
			return(.Object)
		}
)

setValidity("BiooHeaderList", function(object){
#			cat("---------Bioo::setValidity\n")
#			if(! class(object@myName)=="character"){
#				return(" myName should be a character object")
#			}
			return(TRUE)
		})
setIs("BiooHeader", "BiooHeaderList")
#setIs("BiooHeaderList", "BiooHeader")