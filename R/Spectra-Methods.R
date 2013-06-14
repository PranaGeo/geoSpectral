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
# Method : head
#########################################################################
setMethod("head", signature = "Spectra", 
		def = function (x){  return(head(x@Spectra)) })

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
setMethod("spc.rbind", signature = "Spectra", def = function (...){
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
setMethod("spc.rbind", signature = "STI", def = function (...){
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
	input = lapply(input,as,"STI")
	endTime = lapply(input,function(x) x@endTime[length(x@endTime)])
	input = lapply(input,function(x){
				x@sp@coords<-t(as.matrix(x@sp@coords[1,]))
				x@time<-x@time[1]
				x@endTime<-x@endTime[1]
				x
			})
	input = lapply(1:length(input),function(x) {
				input[[x]]@endTime = endTime[[x]]
				input[[x]]
			})
	input = do.call(spc.rbind,input)
}
