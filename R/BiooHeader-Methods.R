# TODO: names(), colnams()
# 
# Author: acizmeli
###############################################################################

#########################################################################
# Method : BiooHeaderAdd	
#########################################################################
setGeneric("BiooHeaderAdd",function(object,Name,Value,...)
		{standardGeneric("BiooHeaderAdd")})
setMethod("BiooHeaderAdd", signature="BiooHeader", function(object,Name,Value){	
			templist = list()
			if (class(object)=="BiooHeader" & length(Value)>1){
				#We will return BiooHeaderList instead of BiooHeader
				out = 	lapply(Value, function(x) {
							templist[Name]=x
							as(c(object,templist),"BiooHeader")})
				out = as(out, "BiooHeaderList")
			} else{
				templist[Name]=(Value[1])
				out = as(c(object,templist),"BiooHeader")				
			}
			return(out)
		})

#########################################################################
# Method : show	
#########################################################################
setMethod("show", signature="BiooHeader", function(object){
			fieldnames = names(object)
			sapply(1:length(fieldnames), function(x) cat(paste(fieldnames[x], " : ", 
										object[fieldnames[x]], "\n")))
		})