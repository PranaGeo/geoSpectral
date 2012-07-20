IDP_Read_Asphere = function (infile,return.data.frame=T) {
	if (!file.exists(infile)) {
		e <- simpleError(paste("File ", infile, " does not exist!", sep=""))
		stop(e)
	}
	print(paste("Reading ", infile, sep=""))
	
	
	outS = readLines(infile)
	#myindex =  which(count.fields(infile)==1) 
	myindex = grep(glob2rx("DS*Cal*nm"),outS)
	if (length(myindex)<=0) {
		e <- simpleError(paste("Could not find the line : DS*Cal*nm", sep=""))
		stop(e)
	}
	
	fc = textConnection(outS)
	timedepth = read.table(fc, colClasses="numeric",nrows=myindex-3,header=T)
	close(fc)
	fc = textConnection(outS)
	lbd = read.table(fc, colClasses="numeric",skip=myindex,header=F,nrows=1)
	close(fc)
	fc = textConnection(outS)
	out = read.table(fc, colClasses="numeric",skip=myindex+1,header=F,fill=T)
	close(fc)
	
	timedepth[,1] = as.POSIXct(timedepth[,1], origin="1904-01-01")
	names(timedepth) = c("TIME","ASPHERE_DEPTH")
	lbd = as.numeric(lbd)
	
	colnames(out) = lbd
	Units = rep("m-1",ncol(out))
	
	out = cbind(out,timedepth)
	Units = c(Units, "TIME","m")
	lbd = c(lbd, NA,NA)
	attr(out, "Wavelengths") = lbd[!is.na(lbd)]
	attr(out, "Units") = Units
	attr(out, "ShortName") = "anw"
	attr(out, "LongName") = "Non-water absorption coefficient"
	
	#Eliminate invalid records
	inv_idx = which(apply(apply(out, 2, is.infinite)==T,1,sum)>0)
	if (length(inv_idx)>0)
		out = out[-inv_idx,]
	
	if (!return.data.frame){
		out = as(out,"Spectra")
	}
		
	return(out)
} 

#fnm = file.path(system.file(package = "BiOpticaR"), "test_data","ASPH002.TXT.gz")
#a=IDP_Read_Asphere(fnm)