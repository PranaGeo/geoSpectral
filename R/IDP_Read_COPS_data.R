# TODO: Add comment
# 
# Author: acizmeli
###############################################################################

IDP_Read_COPS_data = function(infile) {
	
	if (!file.exists(infile)) {
		e <- simpleError(paste("File ", infile, " does not exist!", sep=""))
		stop(e)
	}
	print(paste("Reading ", infile, sep=""))
	
	out = read.table(infile,colClasses=c("character","numeric"),
			sep="\t",header=T)	

	colnames(out) = sub("X.","",colnames(out))
	
	#out$TIME=strptime(out$GpsTime, "%d/%m/%Y %I:%M:%S %p")
	out$TIME=strptime(out$BioGPS.Time, "%d/%m/%Y %I:%M:%S %p")

	return(out)
}
#fnm = file.path(system.file(package = "BiOpticaR"), "test_data","ARCN11_110727_1750_data_002.tsv.gz")
#a=IDP_Read_COPS_data(fnm)

IDP_Read_COPS_RCOPS = function(infile, ParName) {
	if (!file.exists(infile)) {
		e <- simpleError(paste("File ", infile, " does not exist!", sep=""))
		stop(e)
	}
	print(paste("Reading ", infile, sep=""))
	
	load(infile)
	out = as.data.frame(cops[[ParName]])
	
	if (class(cops[[ParName]])=="matrix"){
		lbd = as.numeric(names(out))
		attr(out, "Wavelengths") = lbd
	}
	if(grepl("Ed0",ParName) | grepl("EdZ",ParName))
		Units = "W/m2/nm"
	if(grepl("LuZ",ParName))
		Units = "W/m2/sr/nm"
	if(grepl("K0",ParName) | grepl("KZ",ParName))
		Units = "1/m"

	attr(out, "Units") = Units
	attr(out, "ShortName") = ParName
	TIME = cops$Others[,1]
	TIME = as.POSIXct(TIME, tz = "UTC")
	return(out)
}
#fnm = file.path(system.file(package = "BiOpticaR"), "test_data","ARCN11_110727_1750_data_002.tsv.RData")
#a=IDP_Read_COPS_RCOPS(fnm, "Ed0")

Read_COPS_gps = function(infile) {
	
	if (!file.exists(infile)) {
		e <- simpleError(paste("The file ", infile, " does not exist!", sep=""))
		stop(e)
	}
	print(paste("Reading ", infile, sep=""))
	
	out = read.table(infile,colClasses=c("character","integer","character","numeric","numeric","integer"),
			sep="\t",header=T)	
	
	colnames(out) = sub("X.","",colnames(out))	
	colnames(out) = sub("\\.","",colnames(out))	
	
	out$TIME=strptime(out$GpsTime, "%d/%m/%Y %I:%M:%S %p")
	return(out)
}