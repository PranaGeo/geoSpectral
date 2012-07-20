
ReadNOMAD = function (fnm) {
	
#Read data off disk
	print(paste("Reading the NOMAD file", fnm, "off disk."))
	mydata=read.table(fnm, header=T,sep=",", comment.char = "!")
	
#Date-time conversion
	c=paste(mydata$year, mydata$month,mydata$day,sep="/")
	b=paste(mydata$hour, mydata$minute,mydata$second,sep=":")
	a=paste(c,b)
	a=strptime(a,"%Y/%m/%d %H:%M:%S")
	nms = names(mydata)
	nms = gsub("lat","latitude",nms)
	nms = gsub("lon","longitude",nms)
	names(mydata)=nms
	mydata["datetime"]=as.POSIXct(a)
		
	mydata[mydata==-999]=NA
	return (mydata)
}