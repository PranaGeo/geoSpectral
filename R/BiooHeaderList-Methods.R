# TODO: names(), colnams()
# 
# Author: acizmeli
###############################################################################

#########################################################################
# Method : biooHeaderAdd	
#########################################################################
setMethod("biooHeaderAdd", signature="BiooHeaderList", function(object,Name,Value){	
			if (length(Value)==1){
				Value = rep(Value, length(object))				
			} 
			if(length(object)==length(Value)){
				#We will return BiooHeaderList
				out = lapply(1:length(object), function(x) biooHeaderAdd(object@.Data[[x]], Name, Value[x]))
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