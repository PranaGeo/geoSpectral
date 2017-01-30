#########################################################################
# Method : SpcHeaderAdd	
#########################################################################
setGeneric("SpcHeaderAdd",function(object,Name,Value,...)
		{standardGeneric("SpcHeaderAdd")})
setMethod("SpcHeaderAdd", signature="SpcHeader", function(object,Name,Value){	
			templist = list()
			if (length(Value)>1){
				#We will return SpcHeaderList instead of SpcHeader
				out = 	lapply(Value, function(x) {
							templist[Name]=x
							as(modifyList(object,templist),"SpcHeader")})
				out = as(out, "SpcHeaderList")
			} else{
				templist[[Name]]=Value
				out = as(modifyList(object,templist),"SpcHeader")
			}
			return(out)
		})

#########################################################################
# Method : show	
#########################################################################
#' Show a SpcHeader object
#' @description Display a SpcHeader object
#'
#' @usage 
#' show(object)
#' 
#' @param object of class SpcHeader
#' @seealso \code{\link{show}}
#' 
#' @examples 
#' x=spc.example_spectra()
#' show(x@header)
#' 
setMethod("show", signature="SpcHeader", function(object){
			fieldnames = names(object)
			sapply(1:length(fieldnames), function(x) cat(paste(fieldnames[x], " : ", 
										object[fieldnames[x]], "\n")))
		})