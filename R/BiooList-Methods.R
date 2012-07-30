# TODO: Add comment
# 
# Author: acizmeli
###############################################################################
#########################################################################
# Method : Conversion from Bioo to BiooList using a data field (factor)
#########################################################################
Bioo2BiooList = function(myobj, name){
	if(!is.factor(myobj[[name]]))
		stop(paste("The data/ancillary '", name, "' should be a factor" ))
	#Get the indexes of each row :
	idx = lapply(levels(myobj[[name]]),function(x) which(x==myobj[[name]]))
	output = as(lapply(idx,function(x) myobj[x,]),"BiooList")
	output@by = name
	return(output)
}

#########################################################################
# Method : plot.grid
#########################################################################
setGeneric (name= "plot.grid",
		def=function(x,FUN, nnrow, nncol,...){standardGeneric("plot.grid")})
setMethod("plot.grid", "BiooList", function (x,FUN, nnrow, nncol, ...){
			nb_spc = length(which(sapply(x, inherits, "Bioo")))
			mypar = par()
			nrow = ceiling(nb_spc/nncol)
			
#			FUN <- match.fun(FUN)
			
			mar = c(4,4,1,0.5)
			oma = c(0,0,0,0)#c(1.5,2,1,1)
			par(mfrow=c(nnrow,nncol), mar=mar, oma=oma)
			
			for (I in 1:length(x)) {
				if(nrow(x[[I]])>1){
					eval_txt = paste(FUN, "(x[[1]],...)",sep="")
					eval(parse(text=eval_txt))

					if(x@by!="VariousVariables")
						try(title(paste(x@by, ":", as.character(x[[I]]$STATION[1]))),silent=TRUE)
					if (par()$mfg[1]==par()$mfg[3] & par()$mfg[2]==par()$mfg[4] & I<length(x)) {
						dev.new()
						par(mfrow=c(nnrow,nncol), mar=mar, oma=oma)
					}				
				}
			}
			par(mfrow=mypar$mfrow,mar=mypar$mar,oma=mypar$oma)
		})

#########################################################################
# Method : [[
#########################################################################
#setMethod("[[", signature="BiooList",
#		function(x, i, j, ...) {
#			myn = sapply(x, function(tt) {
#						if(class(x)=="Bioo") "Bioo"	else tt@ShortName[1]
#					})
#			
##			if(any(grepl(name,myn)))
##				x[[grep(name,myn)[1]]]
##			else stop("Could not match any object name")
#		})

#########################################################################
# Method : names
#########################################################################
setMethod("names", "BiooList", function(x){
			sapply(x, function(mobject) {
						if(class(mobject)=="Bioo") "Bioo"	else mobject@ShortName[1]
					})
		})

#########################################################################
# Method : $
#########################################################################
setMethod("$", signature = "BiooList", 
		function(x, name) {
			myn = names(x)
			if(any(grepl(name,myn)))
				x[[grep(name,myn)[1]]]
			else stop("Could not match any object name")
		})

#########################################################################
# Method : show
#########################################################################
setMethod("show", "BiooList", function(object){
			if(length(object)>0)
				sapply(1:length(object), function(x) {
							cat(paste("Element", x, ":"))
							show(object[[x]])
						})
			else cat("Empty BiooList\n")
		})

#########################################################################
# Constructor function : BiooList()
#########################################################################
BiooList = function (spclist){
	new("BiooList", spclist)
}
#########################################################################
# Method : biooInvalidDetect
#########################################################################
setMethod("biooInvalidDetect", signature = "BiooList", def=function(source12){
			out = lapply(source12, function(x) {SetInvalidIdx(x)<-biooInvalidDetect(x)})
			return(out)
})