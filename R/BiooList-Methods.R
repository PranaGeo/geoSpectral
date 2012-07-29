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
# Method : names
#########################################################################
setMethod("names", "BiooList", function(object){
			sapply(object, function(x) x@ShortName)
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