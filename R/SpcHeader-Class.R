
#########################################################################
# Class : SpcHeader	
#########################################################################
#'  \code{SpcHeader} class.
#' @description Definition for \code{SpcHeader}. This class is required
#' for the @header slot of \code{Spectra} object.
#' 
#' @examples 
#' new("SpcHeader")
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
#			cat("---------Spc::setValidity\n")
			return(TRUE)
		})


#########################################################################
# Class : SpcHeaderList	
#########################################################################
#'  \code{SpcHeaderList} class.
#' @description Definition for \code{SpcHeaderList}. This class provides
#' a collection of multiple \code{SpcHeader} objects inside a list.
#' 
#' @examples 
#' h1 = new("SpcHeader")
#' h2 = new("SpcHeader")
#' as(list(h1, h2), "SpcHeaderList")
#' new("SpcHeaderList") 
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
#			cat("---------Spc::setValidity\n")
			return(TRUE)
		})