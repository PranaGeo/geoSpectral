# TODO: Add comment
# 
# Author: acizmeli
###############################################################################

IDP_Read_Hydroscat6 = function(infile) {
	
	if (!file.exists(infile)) {
		e <- simpleError(paste("File ", infile, " does not exist!", sep=""))
		stop(e)
	}
	print(paste("Reading ", infile, sep=""))
	
	out = readLines(infile)	
	
	#Deal with the header
	header_idx = grep(glob2rx("[*"), out)
	HEADER = out[(header_idx[1]+1):(header_idx[2]-1)]
	SigmaParams =  out[(header_idx[2]+1):(header_idx[3]-1)]
	bbparams =  out[(header_idx[3]+1):(header_idx[4]-1)]
	Channels =  out[(header_idx[4]+1):(header_idx[5]-1)]
	out = out[-(1:header_idx[5])]
	out = out[-grep(glob2rx("[Data]"), out)]
		
	#Read the data
	tc=textConnection(out)
	out2=read.table(tc,colClasses="numeric",sep=",",row.names=NULL,header=F,skip=1)
	close(tc)
	tc=textConnection(out)
	header = read.table(tc,colClasses="character",sep=",",row.names=NULL,header=F,nrows=1)
	close(tc)
	colnames(out2) = header
	out2 = out2[,-ncol(out2)]
	#Compute Time
	out2$TIME = as.POSIXct(out2$Time*24*60*60,origin="1900-01-01")	
	
	#Determine bb Wavelengths
	lbd = header[grep(glob2rx("bb*"),header)][1:6]
	lbd = as.numeric(sub("bb","",lbd))
	temp = out2[,2,drop=F]
	out2 = cbind(out2, temp)
	names(out2)[ncol(out2)]="DEPTH"
	out2 = out2[,-(1:2)]

	#Sort wrt wavelengths
	isort = sort(lbd, index.return = T)
	temp=out2[,isort$ix]
	out2 = out2[,-(1:6)]
	out2 = cbind(temp, out2)
	
	attr(out2, "Wavelengths")=lbd[isort$ix]
	Units = c(rep("1/m",6),rep("[ ]",2),rep("1/m",6),rep("[ ]",2),
			rep("1/m",6),rep("[ ]",2),rep("1/m",6),rep("[ ]",2),"TIME","m")
	attr(out2, "Units")= Units
	
	attr(out2, "ShortName")="bb"
	attr(out2, "LongName")="backscattering coefficient"
	
	return(out2)
} 
#fnm = file.path(system.file(package = "BiOpticaR"), "test_data","HS6002.dat.gz")
#b = Read_Hydroscat6(fnm)