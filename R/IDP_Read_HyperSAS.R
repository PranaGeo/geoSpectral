# TODO: Add comment
# 
# Author: acizmeli
###############################################################################

IDP_Read_HyperSAS.L2 = function(infile) {
	
	if (!file.exists(infile)) {
		e <- simpleError(paste("File ", infile, " does not exist!", sep=""))
		stop(e)
	}

	#Read the header (metadata)
	myFile = readLines(infile)
	print(paste("Reading", infile))
	INTTIME_rows = grep("INTTIME",myFile)
	nrows1 = INTTIME_rows[1]-2
	
	#Get the number of fields in each line
	zz=textConnection(myFile)
#	FDNB = count.fields(zz)
	FDNB = count.fields(zz, blank.lines.skip = F)
#	
	header1_start = which(FDNB>10)
#	header1_start = header1_start[1]+2
	header1_start  = grep("INTTIME",myFile)[1]
	
	spvar1_start = header1_start+2
	spvar1_length = diff(FDNB[spvar1_start:length(FDNB)])!=0
	spvar1_length =	which(spvar1_length)[1]
	
	spvar2_start = spvar1_start+spvar1_length
	spvar2_length = diff(FDNB[spvar2_start:length(FDNB)])!=0
	spvar2_length =	which(spvar2_length)[1]
	
	spvar3_start = spvar2_start+spvar2_length+3
	
	#Read the header
	#Reformat the header containing TIME information
	myFile[1:header1_start]= gsub(" : ", ";", myFile[1:header1_start])	
	#Read the file header
	zz=textConnection(myFile)
	header = read.table(zz,header=F,sep=";",nrows=header1_start-2)
	
	#Read the column names of the first spectral variable
	zz=textConnection(myFile)
	CN = read.table(zz, skip=header1_start-1,nrows=1,as.is=TRUE)
	CN1 = CN[seq(1,ncol(CN),2)]
	CN2 = CN[seq(2,ncol(CN),2)]
	zz=textConnection(myFile)
	CN3 = read.table(zz, skip=header1_start,nrows=1)	
#browser()	
	#Read the first spectral variable
	zz=textConnection(myFile)
	data1 = read.table(zz, header=F,skip=spvar1_start-1,nrows=spvar1_length)
		
	#Read the second spectral variable
	zz=textConnection(myFile)
	data2 = read.table(zz,header=F,skip=spvar2_start-1,nrows = spvar2_length)

	zz=textConnection(myFile)
	CN = read.table(zz, skip=spvar3_start-3,nrows=1,as.is=TRUE)
	CN4 = CN[seq(1,ncol(CN),2)]
	CN5 = CN[seq(2,ncol(CN),2)]
	zz=textConnection(myFile)
	CN6 = read.table(zz, skip=spvar3_start-2,nrows=1)
	
	zz=textConnection(myFile)
	FDNB = count.fields(zz, skip=spvar3_start-1)
	if (FDNB[length(FDNB)]!=median(FDNB))
		myFile=myFile[-length(myFile)]
	
	zz=textConnection(myFile)
	data3 = read.table(zz,header=F,skip=spvar3_start-1)
	
	#Compute the wavelengths
	lbd = as.character((CN6))
	lbd[lbd=="1"]=NA
	lbd = as.numeric(lbd)

	#Put the wavelength information into column names
	CN4 = paste(CN4, formatC(lbd,format="f",digits=0), sep="_")
	CN4 = gsub("_NA","", CN4)
	colnames(data3) = CN4
	
	TIME = paste(as.character(data3$DATETAG), data3$TIMETAG2)
	TIME = strptime(TIME, "%Y%j %H:%M:%S", tz="UTC")
	
	Units = as.character(CN5)
	Units = gsub("\\(","", Units)
	Units = gsub("\\)","", Units)
	Units = gsub("\\'","", Units)
	
	#Add the TIME Column
	data3$TIME = TIME; lbd = c(lbd, NA); Units = c(Units,"TIME")
	
	attr(data3, "Wavelengths") = lbd
	attr(data3, "Units") = Units
				
#	cbind(t(CN1), t(CN2),t(CN3))
#	cbind(t(CN4),t(CN5),t(CN6))
	return(data3)
} 

#fnm = file.path(system.file(package = "BiOpticaR"), "test_data","2011-216-204558_L2.dat.gz")
#a = IDP_Read_HyperSAS.L2(fnm)