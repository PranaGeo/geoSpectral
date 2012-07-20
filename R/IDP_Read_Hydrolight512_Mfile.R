# TODO: Add comment
# 
# Author: acizmeli
###############################################################################

readHL_SpectraComponents = function(myindex, myFile){
	out=list()
	OUT = list()
	
	zz=textConnection(myFile[myindex-2])
	temp = read.table(zz,colClasses="character")
	out$ShortName = temp$V1
	out$LongName2 = temp$V2
	out$nrows = as.numeric(temp$V3)
	out$ncols= as.numeric(temp$V4)
	
	myFile2 = myFile[(myindex):(myindex+out$nrows)]
	myFile2 = gsub("in air", "-0.1", myFile2)
	myFile2 = gsub("wavelen", "-1", myFile2)
	myFile2 = gsub("wavel", "-1", myFile2)
	myFile2 = gsub("\"","",myFile2)
	
	#HL gives first the bulk IOP, then the components
	#Get the positions of the IOP components
	comp_idx = grep("component", myFile2)
	
	#Get the bulk IOP
	zz=textConnection(myFile2[1:(comp_idx[1]-2)])
	output = read.table(zz,colClasses="numeric",header=F)
	DEPTH=as.numeric(output[1,]); DEPTH=DEPTH[-1]
	output=output[-1,]
	out$Wavelengths = output[,1]
	output = data.frame(t(output[,-1]))
	colnames(output) = paste(out$ShortName, out$Wavelengths, sep="_")
	rownames(output)=NULL
	output$DEPTH = DEPTH
	
	#Get the units and other attributes
	temp = strsplit(out$LongName2,"\\(")
	units = strsplit(temp[[1]][2],"\\)")
	out$Units = units[[1]]
	if (grepl("backscat ratio", out$LongName))
		out$Units = "[]"
	attr(output, "Units") = out$Units
	attr(output, "Invalid") = F
	attr(output, "LongName") = out$LongName
	attr(output, "ShortName") = out$ShortName
	attr(output, "ShortName") = out$ShortName
	OUT = c(OUT, list(output))
	
	#Get the components
	for (I in 1:length(comp_idx)) {
		out$LongName = as.character(myFile2[comp_idx[I]])
		myFile3 = myFile2[(comp_idx[I]+1):(comp_idx[I]+length(out$Wavelengths)+1)]
		zz=textConnection(myFile3)
		output = read.table(zz,colClasses="numeric",header=F)
		DEPTH=as.numeric(output[1,]); DEPTH=DEPTH[-1]
		output=output[-1,]
		out$Wavelengths = output[,1]
		output = data.frame(t(output[,-1]))
#browser()    
		colnames(output) = paste(out$ShortName, out$Wavelengths, sep="_")
		rownames(output)=NULL
		output$DEPTH = DEPTH
		attr(output, "Units") = out$Units
		attr(output, "Invalid") = F
		attr(output, "LongName") = out$LongName
		attr(output, "ShortName") = out$ShortName
		attr(output, "Wavelengths") = out$Wavelengths
		
		OUT = c(OUT, list(output))
	}
	return(OUT)
}

readHL_Edl =  function(myindex, myFile){
	
	out=list()
	zz=textConnection(myFile[myindex-2])
	temp = read.table(zz,colClasses="character")
	out$ShortName = temp$V1
	out$LongName2 = temp$V2
	out$nrows = as.numeric(temp$V3)
	out$ncols= as.numeric(temp$V4)

	myFile2 = myFile[(myindex):(myindex+out$nrows)]
	myFile2 = gsub("in air", "-0.1", myFile2)
	myFile2 = gsub("wavelen", "-1", myFile2)
	myFile2 = gsub("wavel", "-1", myFile2)
	myFile2 = gsub("\"","",myFile2)

	zz=textConnection(myFile2[2:length(myFile2)])
	output = read.table(zz,colClasses="numeric",header=F)
	out$Wavelengths = output[,1]
	output = data.frame(t(output[,-1]))
	rownames(output)=NULL
	
	temp = strsplit(out$LongName2,"\\(")
	units = strsplit(temp[[1]][2],"\\)")
	out$Units = units[[1]]
	
	attr(output, "Units") = out$Units
	attr(output, "Invalid") = out$Invalid
	attr(output, "Wavelengths") = out$Wavelengths
	
	zz=textConnection(myFile2[1])
	CN = read.table(zz,colClasses="character",header=F); CN=CN[-1]
	zOUT=list()
	for (J in 1:nrow(output)) { 
#		if (length(out$Wavelengths))
			
		zOUT = c(zOUT,list(output[J,]))
		
		names(zOUT[[J]]) = paste(CN[J], out$Wavelengths, sep="_")
		attr(zOUT[[J]], "LongName") = as.character(CN[J])
		attr(zOUT[[J]], "ShortName") = as.character(CN[J])
		
		if(CN[J]=="Ed")
			attr(zOUT[[J]], "Units") = "W/m^2 nm"
		if(CN[J]=="Lw" | CN[J]=="Lu")
			attr(zOUT[[J]], "Units") = "W/m^2 nm sr"
	}
	
	return(zOUT)
}
readHL_wwavel = function(myindex, myFile){
	out=list()
	
	zz=textConnection(myFile[myindex-2])
	temp = read.table(zz,colClasses="character")
	out$ShortName = temp$V1
	out$LongName2 = temp$V2
	out$nrows = as.numeric(temp$V3)
	out$ncols= as.numeric(temp$V4)
	
	if (length(out$nrows)) {
#		browser()
		myFile2 = myFile[(myindex):(myindex+out$nrows)]
		myFile2 = gsub("in air", "-0.1", myFile2)
		myFile2 = gsub("wavelen", "-1", myFile2)
		myFile2 = gsub("wavel", "-1", myFile2)
		myFile2 = gsub("\"","",myFile2)
		
		#if (any(grepl("Ed_diffuse", myFile2))){
		#zz=textConnection(myFile2)
		#output = read.table(zz,colClasses="numeric",header=T)
		if (!any(grepl("component", myFile2))) {
			zz=textConnection(myFile2)
			output = read.table(zz,colClasses="numeric",header=F)

			DEPTH=as.numeric(output[1,]); DEPTH=DEPTH[-1]
			output=output[-1,]
			out$Wavelengths = output[,1]
			output = data.frame(t(output[,-1]))
			colnames(output) = paste(out$ShortName, out$Wavelengths, sep="_")
			rownames(output)=NULL
			output$DEPTH = DEPTH
				
			if (grepl("NOT CALCULATED FOR THIS RUN", myFile[myindex-1])) {
			  out$Invalid = T
			}else{
			  out$Invalid = T		
			}
			temp = strsplit(out$LongName2,"\\(")
			units = strsplit(temp[[1]][2],"\\)")
			out$Units = units[[1]]

			attr(output, "Units") = out$Units[1]
			attr(output, "Invalid") = out$Invalid
			attr(output, "LongName") = out$LongName
			attr(output, "ShortName") = out$ShortName
			attr(output, "Wavelengths") = out$Wavelengths
			
		} else {
		  output = "component=1"
		}
	} else {
	  output = data.frame()	
	}
	return(output)
	
}

readHL_wdepth  = function(myindex, myFile){
	out=list()
	zz=textConnection(myFile[myindex])
	temp = read.table(zz,colClasses="character")
#browser()
	out$LongName = temp$V3
	out$ColNames = as.character(temp[2:ncol(temp)])
	out$ColNames = gsub("depth", "DEPTH", out$ColNames)
	
	zz=textConnection(myFile[myindex-2])
	temp = read.table(zz,colClasses="character")
	out$ShortName = temp$V1
	out$LongName2 = temp$V2
	out$nrows = as.numeric(temp$V3)
	out$ncols= as.numeric(temp$V4)
#browser()		
	myFile2 = myFile[(myindex+1):(myindex+out$nrows)]
	myFile2 = gsub("in air", "-0.1", myFile2)
	myFile2 = gsub("\"","",myFile2)
	zz=textConnection(myFile2)
	output = read.table(zz,colClasses="numeric",header=F)
	
	if (grepl("NOT CALCULATED FOR THIS RUN", myFile[myindex-1])) {
		out$Invalid = T
	}else{
		out$Invalid = T		
	}	
	temp = strsplit(out$LongName2,"\\(")
	units = strsplit(temp[[1]][2],"\\)")
	out$Units = units[[1]]
	
	attr(output, "Units") = out$Units
	attr(output, "Invalid") = out$Invalid
	attr(output, "LongName") = out$LongName
	attr(output, "ShortName") = out$ShortName
	
	colnames(output) = out$ColNames
	return(output)
}

IDP_Read_Hydrolight512_Mfile = function(infile) {
	if (!file.exists(infile)) {
		e <- simpleError(paste("File ", infile, " does not exist!", sep=""))
		stop(e)
	}
	
	data3 = list()
	myFile = readLines(infile)
	
	zz=textConnection(myFile[2])
	temp = read.table(zz)
	nb_wave = temp$V1
	nb_sheets = temp$V12
	
	#Type 1 : Extract the first two (non-spectral) variables KPAR and PAR
	idx = grep("depth",myFile)
	data = readHL_wdepth(idx[2], myFile)
	data3 = c(data3, list(data))
	data = readHL_wdepth(idx[3], myFile)
	data3 = c(data3, list(data))
	
	#Extract the spectral variables (except Ed0+ components)
	idx = grep("wavel",myFile)	
	ed_components =grep("Ed_diffuse", myFile[idx])
	idx = idx[-ed_components]
	
	for (II in 3:length(idx)) {
		#Type 2 Extract regular spectra
		data = readHL_wwavel(idx[II], myFile)	
		if (is.character(data)) {
			#Type 4 : Extract spectral input IOP & components
			data = readHL_SpectraComponents(idx[II], myFile)
		}
				
		if(class(data)=="data.frame" & length(data)>0)
			data3 = c(data3, list(data))
		if(class(data)=="list" & length(data)>0)
			data3 = c(data3, data)
	}

	#Type 3 : get in air AOPs
	reflectances = grep("Rrs", myFile)
	data = readHL_Edl(reflectances[2], myFile)
	data3 = c(data3, data)
	
#	#Type 3 : Extract the spectral Ed0+ components
	ed_components =grep("Ed_diffuse", myFile)
	data = readHL_Edl(ed_components, myFile)
	data3 = c(data3, data)
	
#	TIME = paste(as.character(data3$DATETAG), data3$TIMETAG2)
#	TIME = strptime(TIME, "%Y%j %H:%M:%S", tz="UTC")
#	#Add the TIME Column
#	data3$TIME = TIME; lbd = c(lbd, NA); Units = c(Units,"TIME")
#	
#	attr(data3, "Wavelengths") = lbd
#	attr(data3, "Units") = Units
#			
return(data3)
}
#fnm = file.path(system.file(package = "BiOpticaR"), "test_data","MUGEx3.txt.gz")
#a = IDP_Read_Hydrolight512_Mfile(fnm)