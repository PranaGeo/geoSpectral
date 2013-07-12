# TODO: names(), colnams()
# 
# Author: acizmeli
###############################################################################

#########################################################################
# Method : Conversions from and to data.frame
#########################################################################
setAs(from="Spectra", to="data.frame", def=function(from){
			if(ncol(from@data)>0)
				output = cbind(as.data.frame(from@Spectra),from@data)
			
			delidx = match(c("LON","LAT","TIME","ENDTIME"),names(output))
			output = output[,-delidx[!is.na(delidx)]]
			output$LON = from@sp@coords[,"LON"]
			output$LAT = from@sp@coords[,"LAT"]
			output$TIME=as.POSIXct(time(from@time))
			output$ENDTIME=from@endTime
			
			attr(output,"ShortName") = from@ShortName
			attr(output,"LongName") = from@LongName
			attr(output,"Wavelengths") = from@Wavelengths
			attr(output,"Units") = from@Units
			attr(output,"header") = as(from@header,"list")
			names(attr(output,"header")) = names(from@header)
			
			return(output)
		})
setAs(from="data.frame", to="Spectra", def=function(from){
			#This function makes use of Spectral::Spectra()
			if(!any(grepl("Wavelengths", names(attributes(from))))) 
				stop("The required data.frame attribute was not found : Wavelengths")
			
			if(!any(grepl("Units", names(attributes(from)))))
				stop("The required data.frame attribute was not found : Units")
			
			if(!any(grepl("ShortName", names(attributes(from)))))
				stop("The required data.frame attribute was not found : ShortName")
			
			Wavelengths= attr(from, "Wavelengths") 
			Units=attr(from,"Units") 
			ShortName = attr(from, "ShortName")
			
			if (any(grepl("LongName", names(attributes(from))))){
				LongName = attr(from, "LongName")
			} else {
				LongName = ShortName
			}
			
			#Create Spectra matrix
			Spectra = as.matrix(from[,1:length(Wavelengths)])
			if(prod(dim(Spectra))==0)
				stop("The Spectra matrix is empty. Cannot create a spectra object")
			if(!all(sapply(Spectra,class)=="numeric"))
				stop(paste("Cannot create a numeric matrix from", length(Wavelengths),
								"columns of the input data.frame. Cannot create a spectra object"))
			
			#Create ancillary data.frame
			if (ncol(from)>length(Wavelengths)) {
				myidx = (length(Wavelengths)+1):ncol(from)
				data = from[myidx,drop=F]
			} else {
				data = data.frame(1:nrow(Spectra))
			}
			
			#Extract the header
			if(!is.null(attr(from,"header")))
				header = as(attr(from,"header"),"BiooHeader")
			else
				header = new("BiooHeader")
			
			if(!is.timeBased(from$TIME))
				stop("The TIME column does not contain time-based data")
			TIME = as.xts(1:length(from$TIME), from$TIME)
			if(!is.timeBased(from$ENDTIME)){
				endTime = from$TIME
			}else{
				endTime = from$ENDTIME
			}
			outS =Spectral::Spectra(data,Spectra,Wavelengths,Units=Units,
					header=header,ShortName=ShortName)
#			outS = new("Spectra", time = TIME, endTime = endTime,
#					Spectra=Spectra, data=data,
#					Wavelengths=Wavelengths, Units=Units[1], 
#					LongName = LongName, ShortName = ShortName,header=header)
			
			validObject(outS)
			return(outS)
		})

#########################################################################
# Method : dim
#########################################################################
setMethod("dim", signature = "Spectra", 
		def = function (x){
			return(dim(x@Spectra))  
		})
#########################################################################
# Method : ncol
#########################################################################
setMethod("ncol", signature = "Spectra", 
		def = function (x){  return(ncol(x@Spectra))  })
########################################################################
# Method : nrow
#########################################################################
setMethod("nrow", signature = "Spectra", 
		def = function (x){  return(nrow(x@Spectra))  })
#########################################################################
# Method : names
#########################################################################
setMethod("names", signature = "Spectra", 
		def = function (x){ 
#			if(ncol(x@data)>1)
				return(c(colnames(x@Spectra),names(x@data)))
#			else                
#				return(names(x@data)) 
		})

#########################################################################
# Method : head
#########################################################################
setMethod("head", signature = "Spectra", 
		def = function (x){  return(head(x@Spectra)) })
#########################################################################
# Method : show
#########################################################################
setMethod("show", "Spectra", function(object){
			if(ncol(object)==0){
				LongName = character()
				Units = character()
				LbdStr = character()
			} else {
				LongName = object@LongName[1]
				Units = object@Units[1]
				LbdStr = paste("[",min(object@Wavelengths),",",max(object@Wavelengths), "] ->",sep=" ")                    
			}
			bbx=bbox(object@sp)
			if (length(object@time)>1){
				period = paste(as.character(periodicity(object@time))[1],
						as.character(periodicity(object@time))[5])
				timerange = as.character(range(time(object@time)),usetz=T)
			} else { 
				period = "Not enough data"
				timerange = "NA"
			}
			cat("\n", paste(object@ShortName[1], ' : An object of class "Spectra"\n', 
							length(object@Wavelengths),"spectral channels in columns and", nrow(object@data), 
							"observations in rows"), "\n",
					"LongName: ", LongName, "\t", "Units: ", Units, "\n",
					"Wavelengths : ", length(object@Wavelengths), "channels with units of",object@WavelengthsUnit,  LbdStr, head(object@Wavelengths)," ...\n",
					"Spectra Columns: ", head(colnames(object@Spectra)), "...\n",
					"Ancillary Columns: ", head(names(object@data)),"...\n",
					"Bounding box:", "LON(",bbx[1,],") LAT(",bbx[2,],")\n",
					"Time : periodicity of ", period, " between (", timerange,")")			
		})		

#########################################################################
# Method : $
#########################################################################
setMethod("$", signature="Spectra",
		function(x, name) {
			if (name %in% colnames(x@Spectra)){
				Boutput = x@Spectra[,name]
			} 
			if (name %in% names(x@data)){
				Boutput = x@data[,name]				
			}
			if(!exists("Boutput"))
				stop("Could not match any Spectral or Ancillary (@data) columns")
			return(Boutput)
		})
setReplaceMethod("$", signature = "Spectra", 
		function(x, name, value) {
			x[[name]]=value
			#validObject(x) will be called by the [[ method
			return(x)
		})

#########################################################################
# Method : spc.colnames
#########################################################################
setGeneric("spc.colnames",function(x,Y,...){standardGeneric("spc.colnames")})
setMethod("spc.colnames", signature = "Spectra", 
		def = function (x){ return(colnames(x@Spectra)) })
setGeneric("spc.colnames<-",function(x,Y,...){standardGeneric("spc.colnames<-")})
setReplaceMethod("spc.colnames", signature = "Spectra", def = function (x,value){
			colnames(x@Spectra) = value
			validObject(x)
			return(x) 
		})

#Creates a STIDF function from longstable. If not provided, assumes LAT,LON and TIME columns as 1.
Spectra = function(inDF,Spectra,Wavelengths,Units,space,time,endTime,header,...){
	longcol="";latcol="";timecol=""
	if(missing(space)){
		if ("LAT" %in% names(inDF))
			latcol = "LAT"
		if ("lat" %in% names(inDF))
			latcol = "lat"
		if ("latitude" %in% names(inDF))
			latcol = "latitude"
		if ("LATITUDE" %in% names(inDF))
			latcol = "LATITUDE"
		if ("LON" %in% names(inDF))
			longcol = "LON"
		if ("LONG" %in% names(inDF))
			longcol = "LONG"
		if ("lon" %in% names(inDF))
			longcol = "lon"
		if ("long" %in% names(inDF))
			longcol = "long"
		if ("longitude" %in% names(inDF))
			longcol = "longitude"
		if ("LONGITUDE" %in% names(inDF))
			longcol = "LONGITUDE"
		
		if (!(longcol %in% names(inDF))) {
			inDF$LONG=1
			longcol="LONG"
			warning("Could not find a longitude column named either of: lon,long,LON,LONG,longitue,LONGITUDE. Assigning LONG=1.0 to all rows")
		} 
		if(!(latcol %in% names(inDF))){
			inDF$LAT=1
			latcol="LAT"
			warning("Could not find a latitude column named either of: lat,LAT,latitude,LATITUDE. Assigning LAT=1.0 to all rows")
		}
	}
	if(missing(time)){
		if ("time" %in% names(inDF))
			timecol = "time"
		if ("TIME" %in% names(inDF))
			timecol = "TIME"
		if (!timecol %in% names(inDF)){
			inDF$TIME=1:nrow(inDF)
			timecol="TIME"
			warning("Could not find a time column named either of : time or TIME. Assigning TIME=1.0 to all rows")
		}
	}
	if(missing(endTime)){
		if("ENDTIME" %in% names(inDF)){
			endTime = inDF$ENDTIME
		} else{
			endTime = inDF$TIME
		}
	}
	
	out = stConstruct(inDF,c(longcol,latcol),timecol,endTime=endTime)
	#I think stConstruct does not take endTime into account. Force it again
	out@endTime = endTime
	#Extract Wavelengths from data frame attributes
	if(missing(Wavelengths)){
		Wavelengths = attr(inDF,"Wavelengths")
		lbd.idx = !is.na(Wavelengths)
		Wavelengths = Wavelengths[lbd.idx]
		
		#Extract Spectra from data frame attributes
		if(missing(Spectra)){
			Spectra = as.matrix(out@Spectra[,lbd.idx])		
		}
		if(missing(data)){
			#Extract Ancillary data
			data = out@Spectra[,-which(lbd.idx)]
		}
	}
	if(missing(Units)){
		#Extract Units
		Units = attr(inDF,"Units")[1]
	}
	if(missing(header)){
		#Extract Units
		header = new("BiooHeader")
	}
	out = new("Spectra",out, Spectra=Spectra,Wavelengths=Wavelengths,Units=Units,header=header,...)
	validObject(out)
	return(out)
}

#########################################################################
# Method : spc.plot
#########################################################################
setGeneric("spc.plot",function(x,Y,...){standardGeneric("spc.plot")})
setMethod("spc.plot", "Spectra", function (x, Y, maxSp, lab_cex,xlab,ylab,type="l",lwd=2,...){						
			if (length(x@InvalidIdx)==0)
				x@InvalidIdx = rep(FALSE,nrow(x@Spectra))
			
			if(!missing(maxSp) && ncol(x)>maxSp)
				idx = seq(1,nrow(x),length.out=maxSp	)
			else
				idx = 1:nrow(x)
			
			Xidx = rep(FALSE, nrow(x@Spectra))
			Xidx[idx] = TRUE
			
			if(any(x@InvalidIdx)){
				Xidx[x@InvalidIdx]=FALSE
			}
#			if(any(x@SelectedIdx)){
#				mycol = rep("gray", nrow(x@Spectra))
#				mycol[x@SelectedIdx]="red"
#			} else
			
#			if(missing(col)) 
#				col = 1:10
			
			x@Units = gsub("\\[\\]","",x@Units)
			x@Units = gsub("\\[ \\]","",x@Units)
			
			if(missing(lab_cex))
				lab_cex = 1
#			if(missing(type))
#				type="l"
#			if(missing(lty))
#				lty=1
			
#			if(any(grepl("col",names(match.call())))) {
#				matplot(x@Wavelengths,t(x@Spectra[Xidx,]),#lab=x@Wavelengths,#xaxt="n",
#						ylab= "",xlab="", type="l", pch=19,cex=0.3, cex.axis=lab_cex, ...)
#			} else {
			matplot(x@Wavelengths,t(x@Spectra[Xidx,]),#lab=x@Wavelengths,#xaxt="n",
					ylab= "",xlab="",type=type, pch=19,cex=0.3,cex.axis=lab_cex,lwd=lwd,...)
#			}
			if(missing(ylab)){
				if(x@LongName[1]=="spvar2 longname")
					ylab = bquote(.(x@ShortName)*", ["*.(x@Units[1])*"]")
				else
					ylab = bquote(.(x@LongName[1])*", ["*.(x@Units[1])*"]")
				#	ylab = "Scalar~quantum~irradiance~mu .mol.m^{-2}~s^{-1}"
			}
			if(missing(xlab))
				xlab=bquote("Wavelength ["*.(x@WavelengthsUnit)*"]")
			
			mtext(xlab,side=1,line=2,cex=lab_cex)			
			mtext(ylab,side=2,line=2,cex=lab_cex)
			
			abline(h=0)
			grid(col="black")
		})
#########################################################################
# Method : spc.lines
#########################################################################
setGeneric("spc.lines",function(x,...){standardGeneric("spc.lines")})
setMethod("spc.lines",signature = "Spectra",definition = function(x,...){
			a=sapply(1:nrow(x@Spectra), function(S) {
						lines(x@Wavelengths, x@Spectra[S,],...)})
		})

#########################################################################
# Method : spc.rbind
#########################################################################
setGeneric (name= "spc.rbind",def=function(...){standardGeneric("spc.rbind")})
setMethod("spc.rbind", signature = "Spectra", def = function (a,...){
			
			#Check that column names match
			DFL=sapply(list(...),function(x) names(x@data),simplify=F)
			if(!all(sapply(1:length(DFL),function(x) all(DFL[[x]]==DFL[[1]]))))
				stop("Names of all Ancillary data columns should be the same")
			
			#Check that column names match
			DFL=sapply(list(...),function(x) colnames(x@Spectra),simplify=F)
			if(!all(sapply(1:length(DFL),function(x) all(DFL[[x]]==DFL[[1]]))))
				stop("Names of all Spectral data columns should be the same")
			
			#Check that the number of columns match 
			DFL=sapply(list(...), function(x) ncol(x@Spectra),simplify=F)
			if(!all(sapply(1:length(DFL),function(x) all(DFL[[x]]==DFL[[1]]))))
				stop("All Spectra arrays should have the same number of columns")
			DFL=sapply(list(...), function(x) ncol(x@data),simplify=F)
			if(!all(sapply(1:length(DFL),function(x) all(DFL[[x]]==DFL[[1]]))))
				stop("All Ancillary arrays should have the same number of columns")
			
			DFL=sapply(list(...),spc.getwavelengths)
			#Check that all Wavelengths are equal
			if(!all(apply(DFL,1,diff)==0))
				stop("Wavelengths of all input Spectra objects should be the same")
			#Create the output variable
			outt = ..1
			
			#Error if does not contain SpatialPoints
			if(class(outt@sp)!="SpatialPoints")
				stop("Only support ST* inherited object based on SpatialPoints")
			
			#Get a list of all input arguments
			allinargs = aa=match.call(expand.dots = F)$...
			
			#For all input arguments
			for(I in 2:length(allinargs)){
				#Get the slot Names
				sltn = slotNames(..1)
				#Slots to omit in the rbind process 
				sltn = sltn[sltn!="ShortName"]
				sltn = sltn[sltn!="LongName"]
				sltn = sltn[sltn!="Wavelengths"]
				sltn = sltn[sltn!="WavelengthsUnit"]
				sltn = sltn[sltn!="Units"]
				if(!inherits(eval((allinargs[[I]])),"STI"))
					stop("The input argument should inherit from class STI")
				#For all slots
				for(J in 1:length(sltn)){
					myslot = slot(eval((allinargs[[I]])),sltn[J])
					if(class(myslot)[1]=="BiooHeader"){
						aa=as.data.frame(rbind(slot(outt,sltn[J]),myslot))
						rownames(aa)=NULL
						bb = as.list(aa)
						bb = lapply(bb,function(x){names(x)<-NULL;x})
						outt@header = as(bb,"BiooHeader")
					}
#					if (length(myslot)==0)
#						myslot=NA
					if(class(myslot)[1]=="matrix"|class(myslot)[1]=="data.frame")
						slot(outt,sltn[J])<- rbind(slot(outt,sltn[J]),myslot)
					if(class(myslot)[1]=="logical"|class(myslot)[1]=="numeric"|
							class(myslot)[1]=="character"|class(myslot)[1]=="POSIXct")
						if(class(myslot)[1]=="POSIXct"){
							mytz = attr(outt@endTime,"tzone")
							slot(outt,sltn[J])<-as.POSIXct(as.POSIXlt(c(slot(outt,sltn[J]),myslot),tz=mytz))
						}
					if(class(myslot)[1]=="xts"){
						slot(outt,sltn[J])<-c(slot(outt,sltn[J]),myslot)
						slot(outt,sltn[J])<-xts(1:length(slot(outt,sltn[J])),time(slot(outt,sltn[J])))
					}	
					if(class(myslot)[1]=="SpatialPoints"){
						prj = slot(outt,sltn[J])@proj4string
						if (!identical(prj@projargs,myslot@proj4string@projargs))
							stop("proj4strings do not match!")
						#rbind the coordinates
						coords = rbind(coordinates(slot(outt,sltn[J])),coordinates(myslot))
						#Create a SpatialPoints object
						slot(outt,sltn[J])<-SpatialPoints(coords,proj4string=prj)
					}
				} #end for all slots
			} #end for all input arguments			
			validObject(outt)
			return(outt) 
		})

#########################################################################
# Method : spc.rbind
#########################################################################
setMethod("spc.rbind", signature = "STIDF", def = function (...){
			#Create the output variable
			outt = ..1
			
			#Get a list of all input arguments
			allinargs = aa=match.call(expand.dots = F)$...
			
			#For all input arguments
			for(I in 2:length(allinargs)){
				#Get the slot Names
				sltn = slotNames(..1)
				
				#Error if does not inherit from STI or contain SpatialPoints 
				if(class(eval(allinargs[[I]])@sp)!="SpatialPoints")
					stop("Only support ST* inherited object based on SpatialPoints")				
				if(!inherits(eval((allinargs[[I]])),"STI"))
					stop("The input argument should inherit from class STI")
				#For all slots
				for(J in 1:length(sltn)){
					myslot = slot(eval((allinargs[[I]])),sltn[J])
					if(class(myslot)[1]=="matrix"|class(myslot)[1]=="data.frame")
						slot(outt,sltn[J])<- rbind(slot(outt,sltn[J]),myslot)
					if(class(myslot)[1]=="logical"|class(myslot)[1]=="numeric"|
							class(myslot)[1]=="character"|class(myslot)[1]=="POSIXct")
						if(class(myslot)[1]=="POSIXct"){
							mytz = attr(outt@endTime,"tzone")
							slot(outt,sltn[J])<-as.POSIXct(as.POSIXlt(c(slot(outt,sltn[J]),myslot),tz=mytz))
						}
					if(class(myslot)[1]=="xts"){
						slot(outt,sltn[J])<-c(slot(outt,sltn[J]),myslot)
						slot(outt,sltn[J])<-xts(1:length(slot(outt,sltn[J])),time(slot(outt,sltn[J])))
					}
					if(class(myslot)[1]=="SpatialPoints"){
						prj = slot(outt,sltn[J])@proj4string
						if (!identical(prj@projargs,myslot@proj4string@projargs))
							stop("proj4strings do not match!")
						#rbind the coordinates
						coords = rbind(coordinates(slot(outt,sltn[J])),coordinates(myslot))
						#Create a SpatialPoints object
						slot(outt,sltn[J])<-SpatialPoints(coords,proj4string=prj)
					}
				} #end for all slots
			} #end for all input arguments			
			validObject(outt)
			return(outt) 
		})

#########################################################################
# Method : spc.getwavelengths
#########################################################################
setGeneric (name= "spc.getwavelengths",
		def=function(object){standardGeneric("spc.getwavelengths")})
setMethod("spc.getwavelengths", signature = "Spectra", 
		def = function (object){
			return(object@Wavelengths)
		})
#########################################################################
# Method : spc.setwavelengths
#########################################################################
setGeneric("spc.setwavelengths<-",function(object,value)
		{standardGeneric("spc.setwavelengths<-")})
setReplaceMethod(f="spc.setwavelengths", signature="Spectra",
		definition=function(object,value){
			object@Wavelengths <-value
			validObject(object)
			return (object)
		})
#########################################################################
# Method : spc.cname.construct
#########################################################################
setGeneric("spc.cname.construct",function(object,value)
		{standardGeneric("spc.cname.construct")})
setMethod(f="spc.cname.construct", signature="Spectra",
		definition=function(object,value){
			if(missing(value))
				value = object@ShortName
			return(paste(value,round(spc.getwavelengths(object)),sep="_"))
		})

#########################################################################
#spc.make.stindex 
#########################################################################
#Takes a n-element list of Spectra objects and outputs an n-rows ST object. Each row 
#of the ST object has a time interval that starts from the beginning of the first measurement
#and ends at the endTime of the last measurement of the corresponding input list element. 
spc.make.stindex = function(input) {
	if(!inherits(input,"list"))
		stop("The input dataset should inherit from a list (can also be a BiooList)")
	#Convert to to STIDF (dropping Spectral data, if any)
	input = lapply(input,as,"STIDF")
	#Save the endTime into a variable
	endTime = lapply(input,function(x) x@endTime[length(x@endTime)])
#	input = lapply(input,function(x){
#				x@sp@coords<-t(as.matrix(x@sp@coords[1,]))
#				x@time<-x@time[1]
#				x@endTime<-x@endTime[1]
#				x
#			})
	#Take only the first elements of the entire measurements
	input = lapply(input,function(x){x[1]})
	#Set the endTime of the row as the endTime of the last measurement
	input = lapply(1:length(input),function(x) {
				input[[x]]@endTime = endTime[[x]]
				input[[x]]
			})
	#Call spc.rbind to convert the list of STIDF to one STIDF object 
	input = do.call(spc.rbind,input)
	
	#Eliminate "LAT","LON","TIME" columns, if any
	cidx = match(c("LAT","LON","TIME"), names(input@data))
	cidx = cidx[!is.na(cidx)]
	if(length(cidx)>0)
		input@data = input@data[,-cidx]
	
	#Put the time and endTime slots as data columns
	input@data = cbind(data.frame(ENDTIME=as.character(input@endTime,usetz=T)),input@data)
	input@data = cbind(data.frame(TIME=as.character(time(input@time),usetz=T)),input@data)
	validObject(input)
	return(input)
}
##############################################################################
#Another version of spacetime::timeMatch(). It finds the nearest measurement 
spc.timeMatch.nearest = function(master,searched,returnList=FALSE,report=FALSE) {
	if(!is.timeBased(master))
		if(!(inherits(master,"ST")) & is.timeBased(master))
			stop("Input argument needs to either inherit from spacetime::ST class or be a timeBased variable")
	if(inherits(master,"ST"))
		master = time(master)
	if(!is.timeBased(searched))
		if(!(inherits(searched,"ST")) & is.timeBased(master))
			stop("Input argument needs to either inherit from spacetime::ST class or be a timeBased variable")
	if(inherits(searched,"ST"))
		searched = time(searched)
	out = sapply(master,function(x){mymin = which.min(abs(searched-x))})
	if(returnList)
		out = lapply(out,function(x)x)
	
	if(report){
		
	}
	return(out)
}
##############################################################################
#Reports the space and time distance of each row of the STI-inherited object
#searched to the corresponding row of the STI-inherited object master. Outputs 
#a data.frame, with two columns : time2master ("difftime", in seconds) and 
#distance2master ("numeric", in meters) 
spc.STI.stdistance = function(master,searched){
	stopifnot(length(master)==length(searched))
	
	if(inherits(master,"STI"))
		mastertime = time(master)
	if(is.timeBased(master))
		mastertime = maste	
	if(inherits(searched,"STI"))
		searchedtime = time(searched)
	if(is.timeBased(searched))
		searchedtime = searched
	output = data.frame(time2master = as.numeric(searchedtime - mastertime))
	
	if(inherits(master,"STI") && inherits(searched,"STI"))
		distn = sapply(1:length(master), function(x) {
					spDistsN1(t(as.matrix(coordinates(master)[x,])),t(as.matrix(coordinates(searched)[x,])))*1000
				})
	output = cbind(output,data.frame(distance2master=distn))
}

#########################################################################
# Method : spc.plot2
#########################################################################
setGeneric("spc.plot2",function(x,Y,...){standardGeneric("spc.plot2")})
setMethod("spc.plot2", "Spectra", function (x, Y, maxSp, lab_cex,xlab,ylab,type="l",lwd=2,...){
		})

#########################################################################
# Method : Arith
#########################################################################
setMethod("Arith", signature(e1 = "Spectra", e2 = "Spectra"),function (e1, e2) {
			result <- callGeneric(e1@Spectra, e2@Spectra)
			output = e1
			output@Spectra = result
			validObject(output)
			return(output)
		})

#########################################################################
# Method : Arith
#########################################################################
setMethod("Arith", signature(e1 = "Spectra", e2 = "numeric"),function (e1, e2) {
			result <- callGeneric(e1@Spectra, e2)
			output = e1
			output@Spectra = result
			validObject(output)
			return(output)
		})
setMethod("Math", signature("Spectra"),function (x) {
			x@Spectra <- callGeneric(x@Spectra)
			validObject(x)
			return(x)
		})
setMethod("colMeans", signature("Spectra"),function (x) {
			#Computes the mean along the rows of Spectra (@Spectra). The method finds the measurement
			#closest in time to the mean time and keeps the spatial/time attributes as well as Ancillary
			#data table (@data) associated to that measurement as that of the mean spectra
			x@Spectra <- t(as.matrix(callGeneric(x@Spectra)))
#			x@data <- as.data.frame(t(callGeneric(x@data)))
			#Find the mean time
			meantime <- xts(1,mean(time(x@time)),tzone=attr(x@time,"tzone"))
			#Find the row index closer in time to meantime
			min.idx = which.min(abs(as.numeric(time(meantime)-time(x@time))))
			x@sp <- x@sp[min.idx]
			x@time <-x@time[min.idx]
			x@data <- x@data[min.idx,]
			x@endTime <- mean(x@endTime)
			x@InvalidIdx <- logical()
			x@SelectedIdx <- logical()
			validObject(x)
			return(x)
		})

#Constructs a rectangle of sp::Lines using the bounding box of a Spatial object
setGeneric (name= "spc.bbox2lines",def=function(object){standardGeneric("spc.bbox2lines")})
setMethod("spc.bbox2lines",signature="Spatial",definition=function(object){
			bb = bbox(object)
			pt = bb[,1]
			pt = rbind(pt, c(bb[1,1],bb[2,2]))
			pt = rbind(pt, c(bb[1,2],bb[2,2]))
			pt = rbind(pt, c(bb[1,2],bb[2,1]))
#				pt = rbind(pt, bb[,1])
			row.names(pt)<-NULL
			out = Lines(list(Line(pt[1:2,]),Line(pt[2:3,]),
							Line(pt[3:4,]), Line(pt[c(4,1),])),ID="spc.bbox2lines")
			return(out)
		})
setMethod("spc.bbox2lines",signature="STI",definition=function(object){
			return(callGeneric(object@sp))
		})
setMethod("spc.bbox2lines",signature="Spectra",definition=function(object){
			return(callGeneric(object@sp))
		})

#########################################################################
# Method : spc.invalid.detect
#########################################################################
setGeneric(name= "spc.invalid.detect",
		def=function(source1){standardGeneric("spc.invalid.detect")})
setMethod("spc.invalid.detect", signature = "Spectra", def=function(source1){
			out = apply(source1@Spectra, 2,is.na)
			if(is.null(dim(out))& nrow(source1@Spectra)==1)
				dim(out)<-c(1,ncol(source1@Spectra))
			out = apply(out,1,all)
		})
#########################################################################
# Method : spc.getheader
#########################################################################
setGeneric (name= "spc.getheader",
		def=function(object,name){standardGeneric("spc.getheader")})
setMethod("spc.getheader", signature = "Spectra", 
		def = function (object,name){
			if(missing(name)){
				out = object@header
			}else {
				if(is.null(object@header[[name]])){
					out = NA
				}else{
					out = object@header[[name]]
				}
				return(out)
			}
		})
#########################################################################
# Method : spc.setheader
#########################################################################
setGeneric (name="spc.setheader<-",
		def=function(object,value,...){standardGeneric("spc.setheader<-")})
setReplaceMethod(f="spc.setheader", signature="Spectra",
		definition=function(object,value,...){
			object@header<-value
			validObject(object)
			return(object)
		})

#########################################################################
# Method : spc.updateheader
#########################################################################
setGeneric (name="spc.updateheader<-",
		def=function(object,Name,value,...){standardGeneric("spc.updateheader<-")})
setReplaceMethod(f="spc.updateheader", signature="Spectra",
		definition=function(object,Name,value,...){
			hdr=spc.getheader(object)
			hdr[[Name]]=value
			spc.setheader(object)<-hdr
			validObject(object)
			return(object)
		})

#########################################################################
# Method : spc.getselected.idx
#########################################################################
setGeneric (name= "spc.getselected.idx",
		def=function(object){standardGeneric("spc.getselected.idx")})
setMethod("spc.getselected.idx", signature = "Spectra", 
		def = function (object){
			return(object@SelectedIdx)
		})
#########################################################################
# Method : spc.setselected.idx	
#########################################################################
setGeneric("spc.setselected.idx<-",function(object,value)
		{standardGeneric("spc.setselected.idx<-")})
setReplaceMethod(f="spc.setselected.idx", signature="Spectra",
		definition=function(object,value){
			if(is.numeric(value)){
				idx = spc.getinvalid.idx(object)
				if(length(idx)==0)
					idx = rep(FALSE,nrow(object))
				idx[value]=TRUE
				value=idx
			}
			object@SelectedIdx<-value
			validObject(object)
			return (object)
		})

#########################################################################
# Method : spc.getinvalid.idx
#########################################################################
setGeneric (name= "spc.getinvalid.idx",
		def=function(object){standardGeneric("spc.getinvalid.idx")})
setMethod("spc.getinvalid.idx", signature = "Spectra", 
		def = function (object){
			return(object@InvalidIdx)
		})
#########################################################################
# Method : spc.setinvalid.idx
#########################################################################
setGeneric("spc.setinvalid.idx<-",function(object,value)
		{standardGeneric("spc.setinvalid.idx<-")})
setReplaceMethod(f="spc.setinvalid.idx", signature="Spectra",
		definition=function(object,value){
			if(is.numeric(value)){
				idx = spc.getinvalid.idx(object)
				if(length(idx)==0)
					idx = rep(FALSE,nrow(object))
				idx[value]=TRUE
				value=idx
			}
			object@InvalidIdx<-value
			validObject(object)
			return (object)
		})

#########################################################################
# Method : spc.data2header
#########################################################################
setGeneric(name= "spc.data2header",
		def=function(object,headerfield,dataname,compress,...){standardGeneric("spc.data2header")})
setMethod("spc.data2header", signature = "Spectra", 
		def=function(object,headerfield,dataname,compress=TRUE,...){
			if(missing(headerfield))
				headerfield = dataname
			object@header[[headerfield]]=object[[dataname]]
			if(compress )
				object@header[[headerfield]]=object[[dataname]][1]
			
			return(object)
		})
