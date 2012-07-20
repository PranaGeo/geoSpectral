# TODO: Add comment
# 
# Author: acizmeli
###############################################################################

ReadCageBioOptiqueStation = function(CastDIR) {
	#This functions implements the IOP cage as configured by hobilabs (the CAST.HDS script)
	#made for autonomous logging onboard MiniDAS
	#This reads the 4 instrument data files
	#It can also read the CASTS.LOG file (default behavior)
	
	#Names of the functions that read the instrument data and return it as a
	#data.frame
	instrument = list()
	instrument$HS6$prefix = "HS6"
	instrument$HS6$read_fcn = "Read_Hydroscat6"
	instrument$HS6$extension = ".dat"
	instrument$CTD$prefix = "CTD"
	instrument$CTD$read_fcn = "Read_SeaBird_19p"
	instrument$CTD$extension = ".TXT"
	instrument$FL$prefix = "FL"
	instrument$FL$read_fcn = "Read_EcoTriplet"
	instrument$FL$extension = ".TXT"
	instrument$ASPH$prefix = "ASPH"
	instrument$ASPH$read_fcn = "Read_Asphere"
	instrument$ASPH$extension = ".TXT"
	
	out = list()
	out$HS6 = list(); out$CTD = list(); out$FL = list(); out$ASPH = list()
	mycast = data.frame()

	#Retrieve the station names from the list name
	StatName2 = IDP_GetStationName(CastDIR)
	for (I in 1:length(CastDIR)) {
		
		print("********************************************")
		print(paste("Reading station ", StatName2[I], sep=""))
	
		print(paste("Detected station ", StatName2[I], sep=""))

		#Dermine Cast number from filenames
		CastNO = ReadCageBioOptiqueCASTDetect(CastDIR[I],instrument)
		if (is.na(CastNO)) {
			e <- simpleWarning(paste("Could not determine CastNO : ", CastNO, sep=""))
			warning(e)
		}
		else
			print(paste("Detected cast no ", CastNO, " for station ", StatName2[I], sep=""))
		
		#Determine the names of the instrument files
		instrument = lapply(instrument, function(x)
					modifyList(x, list(filename=file.path(CastDIR[I], paste(x$prefix,CastNO,x$extension,sep="")))))
		
		#Prepare the commands that will call the instrument read functions to read data
		instrument = lapply(instrument, function(x)
					modifyList(x, list(call=paste("out$", x$prefix, "$Station", StatName2[I], "=", 
											x$read_fcn, '("', file.path(x$filename), '")',sep=""))))

		#Read the file CASTS.LOG (produced by the MiniDAS controller)  
		mycast = rbind(mycast, ReadCageBioOptiqueCASTS.LOG(CastDIR[I], CastNO))

		#Read Instrument files by evaluating the commands. Return an empty data.frame()
		#if the anticipated files are missing
		for (J in 1:length(instrument)) {
			mycall = instrument[[J]]$call
			myfilename = instrument[[J]]$filename
			
			if (!file.exists(myfilename)) {
				mycall = paste(strsplit(mycall, "=")[[1]][1], "=data.frame()")

				e <- simpleWarning(paste("The file ", myfilename, " does not exist!", sep=""))
				warning(e)
				eval(parse(text=mycall))
			} else{
				eval(parse(text=mycall))
				#Append the station name as a new column
				out[[J]][[I]]$Station=StatName2[I]
				out[[J]][[I]]$CastNO=CastNO				

				#Bring the startTIME of all instruments to the time indicated in file CASTS.LOG
				timeDIFF = out[[J]][[I]]$TIME[1]-mycast$startTIME[I]		
				out[[J]][[I]]$TIME = out[[J]][[I]]$TIME - timeDIFF							
			}
		}		

		#Checking the result of TIME adjustment
		print("CASTS.LOG says :")
		print(mycast)

		#Make a list of different data.frames (for the current station I)
		mystation = lapply(out, function(x)x[[I]])
		#Eliminate empty instrument files if any
		mystation=mystation[sapply(mystation, nrow)>0]
		print(vapply(mystation, function(x)as.character(ReadCageBioOptiqueTimeCheck(x)), c("A","B")))
		
		print(" ")
		print(" ")
	}
	
	return(out)
}
ReadCageBioOptiqueTimeCheck = function(input) {
	#Checks time for one station only
	ttimes = c(input$TIME[1], input$TIME[nrow(input)])
	names(ttimes) = c("startTIME", "endTIME")
	return(ttimes)
#	
}
ReadCageBioOptiqueCASTS.LOG = function(CastDIR, CastNO) {
	
	print(paste("Reading ", file.path(CastDIR, "CASTS.LOG"), sep=""))
	casts = read.table(file.path(CastDIR, "CASTS.LOG"), sep=",",colClasses="character",fill=T)
	casts = cbind(casts, strptime(paste(casts$V2,casts$V3,sep=" "), "%m/%d/%y %H:%M:%S"))
	casts = cbind(casts, strptime(paste(casts$V4,casts$V5,sep=" "), "%m/%d/%y %H:%M:%S"))
	casts = casts[,-(2:5)]
	colnames(casts) = c("CASTNO", "startTIME", "endTIME")
	
	cast_idx = which(casts$CASTNO==as.numeric(CastNO))
	if (length(cast_idx)==0) {
		e=simpleError(paste("Could not find the cast no ", CastNO, " in the provided CASTS.LOG file", sep=""))
		stop(e)	
	}
	casts = casts[cast_idx,]
	return(casts)
}
ReadCageBioOptiqueCASTDetect = function(CastDIR,fn){
	fnms = sapply(fn, function(x)x$prefix)
	fnms = unlist(sapply(fnms, function(x)
						Sys.glob(file.path(CastDIR, paste(x, "*",sep="")))))
	
	out = strsplit(fnms[1], "\\.")[[1]][1]
	out = strsplit(out, fn$HS6$prefix)[[1]][2]
	
	if (length(out)==0){
		e <- simpleError(paste("Could not extract the Cast Number from instrument filenames", sep=""))
		stop(e)
	}
	return(out)
}

ReadCageBioOptique_COPS_GPS = function(input, CastDIR2) {
	GPS = list()
	for (mystat in CastDIR2) {
		instat = strsplit(basename(mystat), "_")[[1]][2]
		#Check if the file exists
		instrument$GPS$filename = Sys.glob(paste(mystat, "/*gps.tsv",sep=""))
		if (file.exists(mystat) & length(instrument$GPS$filename)>0)
			GPS[instat] = list(Read_COPS_gps(instrument$GPS$filename))
		else {
			GPS[instat] = list(data.frame())
			e <- simpleWarning(paste("Either the directory ", mystat, " or the file ", 
							instrument$GPS$filename, "does not exist!", sep=""))
			warning(e)
		}
	}
	return(GPS)
}