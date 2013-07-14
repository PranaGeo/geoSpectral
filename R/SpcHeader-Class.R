# TODO: Add comment
# 
# Author: acizmeli
###############################################################################

#########################################################################
# Class : SpcHeader	
#########################################################################
setClass("SpcHeader", contains="list",
		prototype=prototype(list(Station=NA,Cruise=NA,Latitude=NA,Longitude=NA)))
setMethod("initialize",
		signature(.Object = "SpcHeader"),
		function (.Object, ...) 
		{
#			cat("---------SpcHeader::Initialize\n")						
			.Object <- callNextMethod()
			#			validObject(.Object)
			return(.Object)
		})

setValidity("SpcHeader", function(object){
#			cat("---------Bioo::setValidity\n")
			return(TRUE)
		})


#########################################################################
# Class : SpcHeaderList	
#########################################################################
setClass("SpcHeaderList", contains="list",
		prototype=prototype(list(new("SpcHeader"))))

setMethod("initialize",
		signature(.Object = "SpcHeaderList"),
		function (.Object, ...) 
		{
#			cat("---------SpcHeader::Initialize\n")						
			.Object <- callNextMethod()
			#			validObject(.Object)
			return(.Object)
		})

setValidity("SpcHeaderList", function(object){
#			cat("---------Bioo::setValidity\n")
			return(TRUE)
		})