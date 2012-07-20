# TODO: Add comment
# 
# Author: acizmeli
###############################################################################

#The EcoTriplet data file needs to be corrected for incomplete lines

Read_EcoTriplet = function(infile) {
	
	if (!file.exists(infile)) {
		e <- simpleError(paste("File ", infile, " does not exist!", sep=""))
		stop(e)
	}
	print(paste("Reading ", infile, sep=""))
	
	out = read.table(infile,colClasses=c("character","character","numeric"))	
	out$TIME=strptime(paste(out$V1,out$V2), "%m/%d/%Y %H:%M:%S")
	colnames(out)=c("","","LAMBDA1","SIGNAL1","LAMBDA2","SIGNAL2",
				"LAMBDA3","SIGNAL3","THERMISTOR","TIME")
	out = out[,-(1:2)]	
	return(out)
} 

#fnm = file.path(system.file(package = "BiOpticaR"), "test_data","FL002.TXT.gz")
#b = Read_EcoTriplet(fnm)
