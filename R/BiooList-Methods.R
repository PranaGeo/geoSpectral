# TODO: Add comment
# 
# Author: acizmeli
###############################################################################

#########################################################################
# Method : plot
#########################################################################
setMethod("plot", "BiooList", function (x,Y, nnrow, nncol, ...){
			nb_spc = length(which(sapply(x, is, "Spectra")))
			mypar = par()
			nrow = ceiling(nb_spc/nncol)
			
			mar = c(4,4,0.5,0.5)
			oma = c(0,0,0,0)#c(1.5,2,1,1)
			par(mfrow=c(nnrow,nncol), mar=mar, oma=oma)
			
			for (I in 1:length(x)) {				
				if(is(x[[I]], "Spectra")) {
					plot(x[[I]],...)
				}
				if (par()$mfg[1]==par()$mfg[3] & par()$mfg[2]==par()$mfg[4] & I<length(x)) {
					dev.new()
					par(mfrow=c(nnrow,nncol), mar=mar, oma=oma)
				}				
			}
			par(mfrow=mypar$mfrow)
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
setMethod("names", "BiooList", function(object){
			sapply(object, function(x) {
						if(class(x)=="Bioo") "Bioo"	else x@ShortName[1]
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
			sapply(1:length(object), function(x) {
						cat(paste("Element", x, ":"))
						show(object[[x]])
					})
		})

#########################################################################
# Constructor function : BiooList()
#########################################################################
BiooList = function (spclist){
	new("BiooList", spclist)
}