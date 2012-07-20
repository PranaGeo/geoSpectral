# TODO: Add comment
# 
# Author: acizmeli
###############################################################################

#########################################################################
# Method : plot
#########################################################################
setMethod("plot", "SpectraCollection", function (x,Y, nnrow, nncol, ...){
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
# Method : show
#########################################################################
setMethod("show", "SpectraCollection", function(object){
			sapply(1:length(object), function(x) {
						if(is(object[[x]], "Spectra")) {
							cat(paste("Element", x, ":"))
							show(object[[x]])
						} else if(!is(object[[x]], "Spectra"))
							cat(paste("Element", x, ":", "Not a Spectra object\n\n"))
					})
		})

#########################################################################
# Constructor function : SpectraCollection()
#########################################################################
SpectraCollection = function (spclist, header=list()){
	if(missing("header"))
		new("SpectraCollection", spclist)
	else
		new("SpectraCollection", spclist, header=header)	
}