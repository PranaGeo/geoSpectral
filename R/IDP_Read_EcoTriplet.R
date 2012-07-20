# TODO: Add comment
# 
# Author: acizmeli
###############################################################################

#The EcoTriplet data file needs to be corrected for incomplete lines

IDP_Read_EcoTriplet = function(infile,return.data.frame=T) {
	
	if (!file.exists(infile)) {
		e <- simpleError(paste("File ", infile, " does not exist!", sep=""))
		stop(e)
	}
	print(paste("Reading ", infile, sep=""))
	
	out = read.table(infile,colClasses=c("character","character",rep("numeric",7)))	
	out$TIME=strptime(paste(out$V1,out$V2), "%m/%d/%Y %H:%M:%S")
	colnames(out)=c("","","LAMBDA1","SIGNAL1","LAMBDA2","SIGNAL2",
				"LAMBDA3","SIGNAL3","THERMISTOR","TIME")
	out = out[,-(1:2)]	
	
	Wavelengths=as.numeric(out[1,c(1,3,5)])
	out = out[,c(2,4,6:8)]
	attr(out,"Wavelengths") = Wavelengths
	attr(out,"ShortName")="flraw"
	attr(out,"LongName")="Flurescence raw signal"
	attr(out,"Units")=rep("[ ]", ncol(out))
	
	if (!return.data.frame){
		out = as(out,"Spectra")
	}
	
	return(out)
} 

#fnm = file.path(system.file(package = "BiOpticaR"), "test_data","FL002.TXT.gz")
#b = Read_EcoTriplet(fnm)
