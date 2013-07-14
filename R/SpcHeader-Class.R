# TODO: Add comment
# 
# Author: acizmeli
###############################################################################

#########################################################################
# Class : BiooHeader	
#########################################################################
setClass("BiooHeader", contains="list",
		prototype=prototype(list(Station=NA,Cruise=NA,Latitude=NA,Longitude=NA)))
setMethod("initialize",
		signature(.Object = "BiooHeader"),
		function (.Object, ...) 
		{
#			cat("---------BiooHeader::Initialize\n")						
			.Object <- callNextMethod()
			#			validObject(.Object)
			return(.Object)
		})

setValidity("BiooHeader", function(object){
#			cat("---------Bioo::setValidity\n")
			return(TRUE)
		})


#########################################################################
# Class : BiooHeaderList	
#########################################################################
setClass("BiooHeaderList", contains="list",
		prototype=prototype(list(new("BiooHeader"))))

setMethod("initialize",
		signature(.Object = "BiooHeaderList"),
		function (.Object, ...) 
		{
#			cat("---------BiooHeader::Initialize\n")						
			.Object <- callNextMethod()
			#			validObject(.Object)
			return(.Object)
		})

setValidity("BiooHeaderList", function(object){
#			cat("---------Bioo::setValidity\n")
			return(TRUE)
		})