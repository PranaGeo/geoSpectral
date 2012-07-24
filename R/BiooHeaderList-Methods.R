# TODO: names(), colnams()
# 
# Author: acizmeli
###############################################################################

#########################################################################
# Method : BiooHeaderAdd	
#########################################################################
setGeneric("BiooHeaderListAdd",function(HeaderList,Header,...)
		{standardGeneric("BiooHeaderListAdd")})
setMethod("BiooHeaderListAdd", signature="BiooHeaderList", function(HeaderList,Header,...){
			output = new("BiooHeaderList")
			output@.Data = c(output@.Data, list(Header))
	
			browser()
			if(identical(HeaderList,new("BiooHeaderList")))
				output@.Data = output@.Data[-1]			
			
			
			return(output)
		})

#########################################################################
# Method : BiooHeaderAdd	
#########################################################################
#setGeneric("BiooHeaderAdd",function(object,Name,Value,...)
#		{standardGeneric("BiooHeaderAdd")})
setMethod("BiooHeaderAdd", signature="BiooHeaderList", function(object,Name,Value){	
			if (length(Value)==1){
				Value = rep(Value, length(object))				
			} 
			if(length(object)==length(Value)){
				#We will return BiooHeaderList
				out = lapply(1:length(object), function(x) BiooHeaderAdd(object@.Data[[x]], Name, Value[x]))
				out = as(out, "BiooHeaderList")
				
			} else {
				stop("The size of field value should either be equal to one or be the same as that of the input BiooHeaderList")
			}
			return(out)
		})
#########################################################################
# Constructor function : BiooHeaderList()
#########################################################################
BiooHeaderList = function (spclist){
		new("BiooHeaderList", spclist)	
}