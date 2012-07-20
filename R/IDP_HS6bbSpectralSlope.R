# TODO: to be normalized (support of list of df's)
# 
# Author: acizmeli
###############################################################################

IDP_HS6bbSpectralSlope = function(inHS6) {
# Compute nu (spectral slope) of the power law
#   bb = bb555 * (555/wl) ^ nu
	
	for (J in 1:length(inHS6)) {
		if (nrow(inHS6[[J]])>1) {
			wl = attr(inHS6[[J]],"Wavelengths")
			x = 555/wl
			
			#z = apply(bb,2,nls(bb~b*x^z, start = list(b = bb[,4], z = 2),data=data.frame(x,bb[,])))
			minx=1; 
			maxx=nrow(inHS6[[J]])
			nb=maxx-minx+1
			bb555=rep(0,nb)
			nu=rep(0,nb)
			bbp555=rep(0,nb)
			nup=rep(0,nb)
			
			bb_idx = grep(glob2rx("bb???"), names(inHS6[[J]]))
			if (length(bb_idx)!=6){
				e <- simpleError(paste('Unable to find the six bb columns', sep="")) 		
				stop(e)
			}
			bb = as.matrix(inHS6[[J]][,bb_idx])
			bbw = 0.00117*(525/wl)^4.32
			
			# create progress bar
			pb <- txtProgressBar(min = 0, max = nb, style = 3)
			for (i in 1:nb) {
				setTxtProgressBar(pb, i)
				y = bb[i+minx-1,]			
				z = nls(y~b*x^z, start = list(b = y[4], z = 3),data=data.frame(x,y), control=nls.control(minFactor=1/8192))
				bb555[i]=coef(z)[1]
				nu[i]=coef(z)[2]
				
				y = bb[i+minx-1,] - bbw
				z = nls(y~b*x^z, start = list(b = y[4], z = 1),data=data.frame(x,y),
						control=nls.control(minFactor=1/8192))
				bbp555[i]=coef(z)[1]
				nup[i]=coef(z)[2]	
			}
			close(pb)
			
			inHS6[[J]]$bbp555 = bbp555
			inHS6[[J]]$nu = nu
			inHS6[[J]]$nup = nup
			print('Added the columns "bbp555", "nu" and "nup" in the data.frame')
		} # End if(!isempty)
	} #End for all list elements
	return(inHS6)
} #End function
#output$HS6=lapply(output$HS6, IDP_HS6bbSpectralSlope)