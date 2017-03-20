
#########################################################################
# Method : SpcHeaderAdd	
#########################################################################
#'  Set a field of the @header slot of a \code{SpcHeader} class object
#' @description Function add the value of a field in the header slot of \code{SpcHeader} class object
#'
#' @usage 
#' SpcHeaderAdd (object,Name,Value,...)
#' 
#' @param object of class SpcHeader
#' @param Name a character variable  
#' @param Value a numeric variable
#' @param ... arguments to be passed to or from other methods
#' @examples 
#' sp=spc.example_spectra()
#' sp@header
#' sp@ShortName
#' sp@header=SpcHeaderAdd(sp@header,sp@ShortName,10)
#' sp@header
#' @rdname SpcHeaderAdd
#' @export
setGeneric("SpcHeaderAdd",function(object,Name,Value,...)
		{standardGeneric("SpcHeaderAdd")})
#' @rdname SpcHeaderAdd
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
#' @param object of class SpcHeader
#' @seealso \code{\link{show}}
#' 
#' @examples 
#' x=spc.example_spectra()
#' show(x@header)
#' 
#' @export
setMethod("show", signature="SpcHeader", function(object){
			fieldnames = names(object)
			sapply(1:length(fieldnames), function(x) cat(paste(fieldnames[x], " : ", 
										object[fieldnames[x]], "\n")))
		})
setGeneric(name="spc.export.text",
           def=function(input,filename,writeheader=TRUE,sep=";",...) {standardGeneric("spc.export.text")})
setMethod("spc.export.text", signature="SpcHeader", definition=function(input,filename,append=F,sep=";",...){
  nms = names(input)
  nms = paste("Spectra|header",sep,nms,sep="")
  browser()
  out1 = lapply(input,function(x){
    #If the separator character exists in the header, then eliminate it 
    x<-gsub(sep,"",x)
    if(length(x)>1)
      x<-paste(x,collapse=sep)
    else
      x<-as.character(x)
  })
  out1 = cbind(nms,out1)
  write.table(out1,filename,row.names=F,col.names=F,append=append,quote=F,sep=sep)
})
