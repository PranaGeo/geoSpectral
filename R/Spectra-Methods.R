#########################################################################
# Method : Conversions from and to data.frame
#########################################################################
setAs(from="Spectra", to="data.frame", def=function(from){
			if(ncol(from@data)>0)
				output = cbind(as.data.frame(from@Spectra),from@data)
			
			delidx = match(c("LON","LAT","TIME","ENDTIME"),names(output))
			delidx = delidx[-which(is.na(delidx))]
			if(length(delidx)>0)
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
				period = paste(as.character(xts::periodicity(object@time))[1],
						as.character(xts::periodicity(object@time))[5])
				timerange = as.character(range(time(object@time)),usetz=F)
				tz = format(time(object@time[1]),format="%Z")
				timestr = paste("Time : periodicity of ", period, " between (", 
						timerange[1]," - ",timerange[2],"), tz=", tz ,sep="")
			} 
			if (length(object@time)==1) { 
				timestr = paste("Time : ", as.character(time(object@time),usetz=T))
			}
			if (length(object@time)==0) { 
				timestr = paste("Time : NA")
			}
			if(ncol(object)==0)
				Str = c("\n","Empty Spectra object","\n")
			else
				Str = c("\n", paste(object@ShortName[1], ' : An object of class "Spectra"\n', 
								length(object@Wavelengths),"spectral channels in columns and", nrow(object@data), 
								"observations in rows"), "\n",
						"LongName: ", LongName, "\t", "Units: ", Units, "\n",
						"Wavelengths : ", length(object@Wavelengths), "channels with units of",object@WavelengthsUnit,  LbdStr, head(object@Wavelengths)," ...\n",
						"Spectra Columns: ", head(colnames(object@Spectra)), "...\n",
						"Ancillary Columns: ", head(names(object@data)),"...\n",
						"Bounding box:", "LON(",bbx[1,],") LAT(",bbx[2,],")\n",
						timestr, "\n")
			if(length(object@Wavelengths)==1)
				Str = gsub(Str,"spectral channels","spectral channel")
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

#Creates a Spectra object from longtable (uses stConstruct()). If not provided, assumes LAT,LON and TIME columns as 1.
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
	
	#Extract Wavelengths from data frame attributes
	if(missing(Wavelengths)){
		Wavelengths = attr(inDF,"Wavelengths")
		lbd.idx = !is.na(Wavelengths)
		Wavelengths = Wavelengths[lbd.idx]
		
		#Extract Spectra from data frame attributes
		if(missing(Spectra)){
			Spectra = as.matrix(inDF[,lbd.idx])		
		}
	}
	#Extract Spectra from data frame attributes
	if(missing(Spectra)){
		Spectra = as.matrix(inDF[,1:length(Wavelengths)])
		inDF = cbind(data.frame(idx=1:nrow(inDF)), inDF[,-(1:length(Wavelengths))])
	}
	
	if(missing(Units)){
		#Extract Units
		Units = attr(inDF,"Units")[1]
	}
	if(missing(header)){
		#Extract Units
		header = new("BiooHeader")
	}
	
	#First construct a STIDF object using stConstruct()
	out = stConstruct(inDF,c(longcol,latcol),timecol,endTime=endTime)
	
	#I think stConstruct does not take endTime into account. Force it again
	out@endTime = endTime
	out = new("Spectra",out, Spectra=Spectra,Wavelengths=Wavelengths,Units=Units,header=header,...)
	validObject(out)
	return(out)
}

#########################################################################
# Method : spc.plot
#########################################################################
setGeneric("spc.plot",function(x,Y,...){standardGeneric("spc.plot")})
setMethod("spc.plot", "Spectra", function (x, Y, maxSp, lab_cex,xlab,ylab,type="l",
				pch=19,lwd=2,cex=0.3,...){						
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
			
			YY = x@Spectra[Xidx,]
			if(class(YY)=="matrix" && nrow(YY)!=length(x@Wavelengths))
				YY = t(YY)
			
			matplot(x@Wavelengths,YY,#lab=x@Wavelengths,#xaxt="n",
					ylab= "",xlab="",type=type, pch=pch,cex=cex,cex.axis=lab_cex,lwd=lwd,...)
			
			if(missing(ylab)){
				if(1)#(x@LongName[1]=="spvar2 longname")
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
#compressHeader=T Compress the header (make multiple all-equal header elements as ONE	
setGeneric (name= "spc.rbind",def=function(...){standardGeneric("spc.rbind")})
setMethod("spc.rbind", signature = "Spectra", def = function (...,compressHeader=T){
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
			
			if(length(allinargs)>1){
				#For all input arguments
				for(I in 2:length(allinargs)){
					if(!inherits(eval((allinargs[[I]])),"STI"))
						stop("The input argument should inherit from class STI")
					#Get the slot Names
					sltn = slotNames(..1)
					#Slots to omit in the rbind process 
					sltn = sltn[sltn!="ShortName"]
					sltn = sltn[sltn!="LongName"]
					sltn = sltn[sltn!="Wavelengths"]
					sltn = sltn[sltn!="WavelengthsUnit"]
					sltn = sltn[sltn!="Units"]
					#For all slots
					for(J in 1:length(sltn)){
						myslot = slot(eval((allinargs[[I]])),sltn[J])
						if(class(myslot)[1]=="BiooHeader"){
							aa=rbind(as.data.frame(slot(outt,sltn[J]),stringsAsFactors=F), as.data.frame(myslot,,stringsAsFactors=F))
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
			} #end for if(length(allinargs)>1)
			#Compress the header (make multiple all-equal header elements as ONE)
			if(compressHeader){
				for(J in names(outt@header)){
					if(length(outt@header[[J]])>1){
						myO = sapply(2:length(outt@header[[J]]),function(x){
									outt@header[[J]][x]==outt@header[[J]][1]})
						try(if(all(myO)) outt@header[[J]]=outt@header[[J]][1],silent=T)
						if(all(is.na(outt@header[[J]])))
							outt@header[[J]]=outt@header[[J]][1]
					}
				}
			}
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
				sltn = slotNames(outt)
				
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
#Takes a n-element list of Spectra objects and outputs one STIDF object. Each row 
#of the ST object has a time interval depending of the input argument rowSimplify.
#rowSimplify : "none", "spc.colMeans","firstRow" or "lastRow"
#none: length of the output object equals the sum of all rows of all elements of the input list object
#spc.colMeans: length of the output object equals the number of rows of the input list object. 
#This option returns the measurement nearest to the average time of the input list element
#firstRow and lastRow : length of the output object equals the number of rows of the input list object.
#These two options return the first and last measurements of the input list element
spc.make.stindex = function(input,what2include="",rowSimplify="none",
		includeTIME=FALSE,includeLATLON=FALSE) {
	
	if(!(rowSimplify %in% c("spc.Colmeans","firstRow","lastRow","none")))
		stop(simpleError(paste("rowSimplify should be one of",paste(c("spc.Colmeans","firstRow","lastRow","none"),collapse=","))))
	
	if(!inherits(input,"list"))
		stop("The input dataset should inherit from a list (can also be a BiooList)")
	
	MyOutput = lapply(1:length(input),function(x){
				if(nrow(input[[x]])>0){
					try(what2include<-get("what2include",envir=parent.frame(2)),silent=T)
#what2include=c("Rrs_805","INTTIME")					
					#Save the endTime into a variable
					endTime<-input[[x]]@endTime
					
					#Convert to STIDF (dropping Spectral and Ancillary data, if any)
					if(rowSimplify=="spc.Colmeans"){
						my = spc.colMeans(input[[x]])
						my@endTime = endTime[length(endTime)]
					}
					if(rowSimplify=="firstRow"){
						my = input[[x]][1]
						my@endTime = endTime[length(endTime)]
					}
					if(rowSimplify=="lastRow"){
						my = input[[x]][nrow(input[[x]])]
					}
					if(rowSimplify=="none"){
						my = input[[x]]
					}
					if(!(length(what2include)==1 && what2include==""))
						w2i = input[[x]][[what2include]]
					
					w2i2 = data.frame(Index=1:nrow(my),ListIndex=rep(x,nrow(my)))
					if(exists("w2i"))
						w2i2 = cbind(w2i2,w2i)
					my@data = w2i2
					
					my<-as(my,"STIDF")					
					#Put the time and endTime slots as data columns
					if(includeTIME){
						my[["TIME"]]=time(my)
						my[["ENDTIME"]]=my@endTime
					}
					if(includeLATLON){
						my@data[["LON"]]=coordinates(my)[,"LON"]
						my@data[["LAT"]]=coordinates(my)[,"LAT"]
					}
					#my[["TIME"]]=as.character(time(input@time),usetz=T)
					#my[["ENDTIME"]]=as.character(input@endTtime,usetz=T)
				} else {
					#Empty variable
					my<-NA
				}
				return(my)
			})
#Eliminate NAs (invalid records, index kept in $ListIndex)
	myWarn = options()$warn
	options(warn=-1)
	MyOutput = MyOutput[!sapply(MyOutput,is.na)]
	options(warn=myWarn)
	
	#Call spc.rbind to convert the list of STIDF to one STIDF object  xxx
	MyOutput = do.call(spc.rbind,MyOutput)
	validObject(MyOutput)
	return(MyOutput)
}
##############################################################################
#Another version of spacetime::timeMatch(). 
#method="over" uses the simple over technique. Same as of spacetime::timeMatch().  
#method="nearest" finds the nearest measurement. Matches only one data for all elements of master
#method="within" finds the measurements that are within the interval limits=c(upper,lower) (in seconds)

#setGeneric("spc.timeMatch",function(master,searched,returnList=FALSE,method="over",limits,report=FALSE)
#		{standardGeneric("spc.timeMatch")})
#setMethod(f="spc.timeMatch", signature=c("Spectra","Spectra"),
#		definition=function(master,searched,returnList,method,limits,report){
#		})
spc.timeMatch = function(master,searched,returnList=FALSE,method="over",limits,report=FALSE) {
	if(!is.timeBased(master))
		if(!(inherits(master,"ST")) & is.timeBased(master))
			stop("Input argument 'master' needs to either inherit from spacetime::ST class or be a timeBased variable")
	stopifnot(inherits(master,"ST"))
	if(!is.timeBased(searched))
		if(!(inherits(searched,"ST")) & is.timeBased(master))
			stop("Input argument 'searched' needs to either inherit from spacetime::ST class or be a timeBased variable")
	stopifnot(inherits(searched,"ST"))
	if(method=="over")
		out = spacetime::timeMatch(master,searched,returnList=returnList)
	if(method=="nearest"){
		out = sapply(time(master),function(x){mymin = which.min(abs(time(searched)-x))})
		if(returnList)
			out = lapply(out,function(x)x)
	}
	if(method=="within"){
		if(missing(limits))
			stop(simpleError("The input argument 'limits' is required if method=='within'"))
		if(length(limits)==0 || length(limits)>2)
			stop(simpleError("The input argument 'limits' needs to have a length of 1 or 2"))
		if(length(limits)==1)
			limits = c(limits,limits)
		out = which(time(searched)>time(master)[1]-limits[1] & 
						time(searched)<master@endTime[length(master)]+limits[2])
	}
	if(report){
		print(paste(time(master)[1],master@endTime[length(master)],paste(out,collapse=" ")))
	}
	return(out)
}
##############################################################################
#Reports the space and time distance of each row of the STI-inherited object
#searched to the corresponding row of the STI-inherited object master. Outputs 
#a data.frame, with two columns : time2master ("difftime", in seconds) and 
#distance2master ("numeric", in meters) 
spc.STI.stdistance = function(master,searched,report=F){
	stopifnot(length(master)==length(searched))
	
	if(inherits(master,"STI"))
		mastertime = time(master)
	if(is.timeBased(master))
		mastertime = maste	
	if(inherits(searched,"STI"))
		searchedtime = time(searched)
	if(is.timeBased(searched))
		searchedtime = searched
	output =  difftime(searchedtime,mastertime,units="secs")
	output = data.frame(time2master = as.numeric(output))
	
	if(inherits(master,"STI") && inherits(searched,"STI"))
		distn = sapply(1:length(master), function(x) {
					spDistsN1(t(as.matrix(coordinates(master)[x,])),t(as.matrix(coordinates(searched)[x,])))*1000
				})
	output = cbind(output,data.frame(distance2master=distn))
	
	if(report){
		a=hist(distn,breaks=50);a$breaks
		plot(master@sp)
		lines(spc.bbox2lines(master@sp))
	}	
	return(output)
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
setGeneric (name= "spc.colMeans",def=function(object){standardGeneric("spc.colMeans")})
setMethod("spc.colMeans", signature("Spectra"),function (x) {
			#Computes the mean along the rows of Spectra (@Spectra). The method finds the measurement
			#closest in time to the mean time and keeps the spatial/time attributes as well as Ancillary
			#data table (@data) associated to that measurement as that of the mean spectra
			x@Spectra <- t(as.matrix(colMeans(x@Spectra)))
#			x@data <- as.data.frame(t(callGeneric(x@data)))
			#Find the mean time
			meantime <- xts(1,mean(time(x@time)),tzone=attr(x@time,"tzone"))
			#Find the row index closer in time to meantime
			min.idx = which.min(abs(as.numeric(time(meantime)-time(x@time))))
			x@sp <- x@sp[min.idx]
			x@time <-x@time[min.idx]
			x@data <- x@data[min.idx,,F]
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
				if(all(is.na(out)))
					out=FALSE
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
#########################################################################
# Method : spc.header2data
#########################################################################
#If header element has length >1, its type is checked. If it is "character",
#its elements will be pasted using paste(...,collapse="|"). If it is another 
#type, only the first element will be taken.  
setGeneric(name= "spc.header2data",
		def=function(object,headerfield,dataname,...){standardGeneric("spc.header2data")})
setMethod("spc.header2data", signature = "Spectra", 
		def=function(object,headerfield,dataname,compress=TRUE,...){
			if(missing(dataname))
				dataname = headerfield
			if (headerfield %in% names(object@header)){
				if(class(object@header[[headerfield]])=="character")
					object[[dataname]] = object@header[[headerfield]][1]
				else
					object[[dataname]] = paste(object@header[[headerfield]],collapse="|")
			}
			else
				stop(simpleError("Could not match a header field"))
			
			return(object)
#			if(compress )
#				object[[dataname]]=object@header[[headerfield]][1]
		})
#########################################################################
# Method : [
#########################################################################
setMethod("[", signature(x = "Spectra"), function(x, i, j) {
			OUT_ANC = 0
			if(missing(i))
				i =  1:nrow(x@Spectra)
			if(missing(j))
				j =  1:ncol(x@Spectra)
			
			if (class(j)=="numeric" | class(j)=="character"){
				if (class(j)=="numeric"){
					j.new = match(j,x@Wavelengths)
				}
				if (class(j)=="character"){
					if (!exists("j.new") & any(match(j, colnames(x@Spectra),nomatch=F))) {
						j.new = match(j, names(x))
					}
					if (!exists("j.new") & any(match(j, names(x@data),nomatch=F))) {
						OUT_ANC = 1
						j.new = match(j, names(x@data))						
					}
					if (!exists("j.new") && length(j)==1 && grepl("::",j)) {					
						#The requested input is in format lbd1::lbd2
						temp = strsplit(j, "::")
						mylower = as.numeric(temp[[1]][1])
						myupper = as.numeric(temp[[1]][2])					
						j.new = which(x@Wavelengths>=mylower & x@Wavelengths<=myupper)
					}
					if (!exists("j.new"))
						stop("Could not recognize the wavelength selection format. Use the operator :: or provide spectra or inDF data column indexes or names")
					
				}			
				if (all(is.na(j.new)))
					stop("Could not find matching wavelengths or inDF data")
				if (any(na.idx <-(is.na(j.new)))) {
					j.new=j.new[!is.na(j.new)]
#					warning(paste("Could not match wavelengths or inDF data :", j[which(na.idx)]))
				}
				if (!all(is.finite(j.new)))
					stop("Could not find matching wavelengths or inDF data")
				j = j.new
			}
			InvalidIdx = x@InvalidIdx
			if (!OUT_ANC) {				
				x@Spectra=x@Spectra[i,j,drop=F]
				if(nrow(x@data)>0)
					x@data=x@data[i,,drop=F]
				x@Wavelengths = x@Wavelengths[j]
			} else{
				x@data = x@data[i,j,drop=F]				
			}
			if (length(x@InvalidIdx)>1)
				x@InvalidIdx = x@InvalidIdx[i] 
			
			x@SelectedIdx = logical()			
			return(x)
		})

#setMethod("[", signature=c("Spectra","numeric","missing"), function(x, i, j, ...) {
#			x@data<-x@data[i,]
#			x@sp<-x@sp[i,]
#			x@time<-x@time[i]
#			x@endTime<-x@endTime[i]
#			ttemp = x@Spectra[i,]
#			if(class(ttemp)!="matrix")
#				ttemp = t(as.matrix(ttemp))
#			x@Spectra = ttemp
#			
#			if(length(x@InvalidIdx)>0)
#				x@InvalidIdx = x@InvalidIdx[i]
#			if(length(x@SelectedIdx)>0)
#				x@SelectedIdx = x@SelectedIdx[i]
#			validObject(x)
#			return(x)
#		})
#########################################################################
# Method : [[
#########################################################################
setMethod("[[", signature=c("Spectra","character","missing"),
		function(x, i, j, ...) {
			Boutput = list()
			for (II in 1:length(i)){
				
				if (i[II] %in% colnames(x@Spectra)){
#					idx = which(i[II]==colnames(x@Spectra))
					Boutput[[II]] = as.data.frame(x@Spectra[,i[II]])
					names(Boutput[[II]])<-i[II]
				}
				if (i[II] %in% names(x@data)){
#					idx = which(i[II]==names(x@data))
					Boutput[[II]] = x@data[i[II]]				
					names(Boutput[[II]])<-i[II]
				}
			}
			names(Boutput)<-i
			Boutput = as.data.frame(Boutput)
			row.names(Boutput)<-NULL
			if(ncol(Boutput)==0)
				stop("Could not match any Spectral or ancillary data columns")
			
			return(Boutput)
		})
setReplaceMethod("[[",  signature=c("Spectra","character","missing"), definition=function(x, i, j, value) {
#			matched = 0
			if(class(value)=="data.frame")
				stop("The input variable 'value' cannot be a data.frame")
			if (i %in% colnames(x@Spectra))
				stop(simpleError("Matched a Spectra column. Use spc.add.channel() to add a spectral channel"))
#			if (i %in% names(x@data)){
#				matched = 1
			x@data[[i]] <- value				
#			}
#			if(!matched)
#				stop("Could not match any Spectral or ancillary data columns")
			validObject(x)
			return(x)
		})

setMethod("rep", signature(x = "Spectra"),
		function(x, times, length.out, each, ...) {
			if(!missing(length.out))
				stop("The argument 'length.out' is not supported yet")
			if(!missing(each))
				stop("The argument 'each' is not supported yet")
			SP = sapply(1:ncol(x), function(y) rep(x@Spectra[1,y], times))
			
			if(prod(dim(x@data))!=0){
				DT = as.data.frame(matrix(rep(matrix(NA,1,ncol(x@data)), times), ncol = ncol(x@data)))
				for (I in 1:ncol(DT))
					DT[,I] = rep(x@data[,I],times)
				names(DT)<-names(x@data)
			}
			
			if (length(x@InvalidIdx)>1)
				x@InvalidIdx = rep(x@InvalidIdx,times)
			
			crds = matrix(rep(x@sp@coords,times),ncol=ncol(x@sp@coords),byrow=T)
			colnames(crds)<-c("LON","LAT")
			x@time = xts(rep(x@time,times),rep(time(x@time),times))
			x@endTime = rep(x@endTime,times)
			x@sp@coords <- crds
			if(prod(dim(x@data))!=0)
				x@data = DT 
			x@Spectra = SP	
			x@SelectedIdx = logical()
			validObject(x)
			return(x)
		})

#########################################################################
# Method : spc.interp.spectral
#########################################################################
setGeneric (name= "spc.interp.spectral",
		def=function(source1,target_lbd,...){standardGeneric("spc.interp.spectral")})
setMethod("spc.interp.spectral", signature = "Spectra", 
		def = function (source1,target_lbd,show.plot=FALSE){
			if(missing(target_lbd))
				stop("The input argument 'target_lbd' is missing")
			
			out = source1
			lbd_source1 = spc.getwavelengths(source1)
			DF = matrix(nrow=nrow(source1),ncol=length(target_lbd))
			my = list()
			for(x in 1:nrow(DF)) {
				my[[x]] = approx(lbd_source1, source1@Spectra[x,],xout=target_lbd,rule=2)
				DF[x,] = t(my[[x]]$y)
			}
			if(show.plot){
				plot(lbd_source1, source1@Spectra[1,],type="b",ylab=source1@LongName,xlab="Wavelength",pch="o")
				points(my[[x]]$x,my[[x]]$y,col="green",cex=0.4)
				grid(col="black")
			}
			out@Spectra = DF
			out@Wavelengths = target_lbd
			spc.colnames(out) <- spc.cname.construct(out)
			validObject(out)
			return(out)
		})

#########################################################################
# Method : spc.export.text
#########################################################################
#spc.export.text(out.Rrs[[5]]@Rrs,"test.txt")
#aa=spc.import.text("test.txt")
#dev.new();spc.plot(aa)
setGeneric(name="spc.export.text",
		def=function(input,filename,writeheader=TRUE,sep=";",...) {standardGeneric("spc.export.text")})
setMethod("spc.export.text", signature="Spectra", definition=function(input,filename,writeheader,sep,...){
			data = as(input,"data.frame")
			idx.idx = which(colnames(data) == "idx")
			if(length(idx.idx)>0){
				data = data[,-idx.idx]
			}
			data = cbind(data.frame(idx=1:nrow(data)),data)
			clmnnames = colnames(data)
			data$TIME = as.character(data$TIME,usetz=TRUE)
			data$ENDTIME = as.character(data$ENDTIME,usetz=TRUE)
			
			written=0
			if(writeheader){
				spc.export.text(input@header,filename,append=F)
				written=length(input@header)
			}
			slotInfos = .spc.slot.infos(input,sep)
			for(I in 1:length(slotInfos)){
				if(length(slotInfos[[I]])==1)
					mysl=paste(names(slotInfos)[I],slotInfos[[I]],sep=sep)
				else
					mysl = paste(names(slotInfos)[I],paste(slotInfos[[I]],collapse=sep),sep=sep)
				if(written==0)
					write.table(mysl,filename,row.names=F,col.names=F,append=F,quote=F)
				else
					write.table(mysl,filename,row.names=F,col.names=F,append=T,quote=F)
				written = written+1
			}
			
			#Write column names
			write.table(paste(clmnnames,collapse=sep), filename, row.names=F, col.names=F,append=T, quote=F,eol="\n")
			#Write Spectra+Ancillary data
			write.table(data, filename, sep=sep, row.names=F, col.names=F,append=T,quote=F)
			print(paste("Wrote", filename ))			
		})
.spc.slot.infos = function(input,sep){
	out=list('Spectra|ShortName'=input@ShortName,
			'Spectra|LongName'=input@LongName,
			'Spectra|Units'=input@Units,'Spectra|proj4string'=input@sp@proj4string@projargs,
			'Spectra|WavelengthsUnit'=input@WavelengthsUnit,
			'Spectra|Wavelengths'=spc.getwavelengths(input))
	return(out)
}
setMethod("spc.export.text", signature="BiooHeader", definition=function(input,filename,append=F,sep=";",...){
			nms = names(input)
			nms = paste("Spectra|header",sep,nms,sep="")
			out1 = lapply(input,function(x){
						#If the separator character exists in the header, then eliminate it 
						x<-gsub(sep,"",x)
						if(length(x)>1)
							x<-paste(x,collapse=sep)
						else
							x<-as.character(x)
					})
			out1 = cbind(nms,out1)
			write.table(out1,filename,row.names=F,col.names=F,append=append,quote=F,sep=sep)
		})

#########################################################################
# Method : spc.import.text
#########################################################################
spc.import.text = function(filename,sep=";",...){
	myT = readLines(con=filename)
	
	#Extract the header
	header.idx = grep("Spectra\\|header",myT)
	if(length(header.idx)>0){
		hdr = strsplit(myT[header.idx],sep)
		
		nms = sapply(hdr,function(x)x[2])
		header = sapply(hdr,function(x){
					if(length(x)>2)
						x[3:length(x)]
					else
						""
				})
		names(header)<- nms
		header = .spc.header.infos(header) 
		
		if(any(grepl("StationType",nms)))
			if(is.logical(header$StationType))
				header$StationType = "T"
		header = as(header,"BiooHeader")
		myT = myT[-header.idx]
		
	} else {
		header = new("BiooHeader")
	}
	#Extract the Spectra slots
	Slots.idx = grep("Spectra\\|",myT)
	if(length(Slots.idx)>0){
		Slots = strsplit(myT[Slots.idx],sep)
		idx = grep("LongName",Slots)
		LongName = Slots[[idx]][2]
		idx = grep("ShortName",Slots)
		ShortName = Slots[[idx]][2]
		idx = grep("Units",Slots)
		Units= Slots[[idx]][2]
		idx = grep("proj4string",Slots)
		proj4string=Slots[[idx]][2]
		if(grepl("NA",proj4string))
			proj4string = NA
		idx = grep("WavelengthsUnit",Slots)
		WavelengthsUnit=Slots[[idx]][2]
		idx = which("Spectra|Wavelengths"==sapply(Slots,function(x)x[1]))
		try(Wavelengths<-as.numeric(Slots[[idx]][2:length(Slots[[idx]])]),silent=T)
		if(!exists("Wavelengths"))
			stop(simpleError("Could not find Wavelength information"))
		myT = myT[-Slots.idx]
		con = textConnection(myT)
		Spec = read.table(con,header=T,sep=sep)
		close(con)
		
		#Eliminate the first (idx) column
		idx = which(names(Spec)=="idx")
		if(length(idx)>0){
			Spec = Spec[,-idx]
		}
		
		Spec$TIME<-as.character(Spec$TIME)
		tz = strsplit(Spec$TIME[1]," ")[[1]][3]
		Spec$TIME<-as.POSIXct(strptime(Spec$TIME,"%Y-%m-%d %H:%M:%S",tz=tz))
		Spec$ENDTIME<-as.character(Spec$ENDTIME)
		Spec$ENDTIME<-as.POSIXct(strptime(Spec$TIME,"%Y-%m-%d %H:%M:%S",tz=tz))
		Spec = Spectra(Spec,ShortName=ShortName,Wavelengths=Wavelengths,Units=Units,
				LongName=LongName,header=header)
	} else {
		stop("Cannot find information for Spectra object slots")
	}
	return(Spec)
}

#This internal function takes as input the Spectra header as a list and 
#1)converts its elements to numbers (when possible)
#2)evals its elements in case the text contains some R code
.spc.header.infos = function(header){ 
	#Suppress warnings for the below operation (as.numeric creates warnings)
	myWarn = options()$warn
	options(warn=-1)
	header = lapply(header,function(x) {
				try(y<-as.numeric(x),silent=T)
				if(!is.na(y))
					x<-y
				return(x)
			})
#	header = lapply(1:length(header),function(x) {
	##				if(names(header)[x]=="Rsky750")
#					try(y<-eval(parse(text=header[[x]])),silent=T)
#				if(exists("y"))
#					header[[x]]<-y
#				return(header[[x]])
#			})
	options(warn=myWarn)
	return(header)
}
#########################################################################
# Method : spc.export.xlsx
#########################################################################
#spc.export.xlsx(out.Rrs[[5]]@Rrs,"test.xlsx")
setGeneric(name="spc.export.xlsx",
		def=function(input,filename,sheetName,writeheader=TRUE,append=F,sep=";",...) {standardGeneric("spc.export.xlsx")})
setMethod("spc.export.xlsx", signature="Spectra", definition=function(input,filename,sheetName,writeheader,append,sep,...){
			if(missing(sheetName))
				sheetName = input@ShortName
			
			data = as(input,"data.frame")
			data$TIME = as.character(data$TIME,usetz=TRUE)
			data$ENDTIME = as.character(data$ENDTIME,usetz=TRUE)
			data = cbind(data.frame(idx=1:nrow(data)),data)
			
			slotInfos = .spc.slot.infos(input,sep)
			if(!append){
				#Create an empty excel workbook and start writing into it
				wb <- xlsx::createWorkbook()
			}else{
				#Create an empty excel workbook and start writing into it
				wb <- xlsx::loadWorkbook(file=filename)
			}
			sheet <- xlsx::createSheet(wb, sheetName=sheetName)
			if(writeheader){
				for(I in 1:length(input@header)){					
					if(length(input@header[[I]])==1)
						myH=cbind("Spectra|header",names(input@header)[I],input@header[[I]])
					else		
						myH = cbind("Spectra|header",names(input@header)[I],t(input@header[[I]]))
					xlsx::addDataFrame(myH, sheet,row.names=F,col.names=F,,startRow=I,startColumn=1)
				}
			}
			written = length(input@header)
			for(I in 1:length(slotInfos)){
				if(length(slotInfos[[I]])==1)
					mysl=cbind(names(slotInfos)[I],slotInfos[[I]])
				else
					mysl = cbind(names(slotInfos)[I],t(slotInfos[[I]]))
				xlsx::addDataFrame(mysl,sheet,row.names=F,col.names=F,startRow=written+1,startColumn=1)
				written = written+1
			}
			xlsx::addDataFrame(data, sheet,row.names=F,startRow=written+1,startColumn=1)
			xlsx::saveWorkbook(wb, filename)
			print(paste("Wrote sheet", sheetName, "to", filename))
		})

#########################################################################
# Method : subset
#########################################################################
setMethod("subset",  signature="Spectra",
		definition=function(x, subset, select, drop = FALSE, ...) {
			if (missing(subset)) 
				mycall <- TRUE
			else {
				mycall <- substitute(subset)
				if(any(sapply(as.character(mycall),function(y) {y %in% colnames(x@Spectra)})))
					try(xidx <- eval(mycall, as.data.frame(x@Spectra), parent.frame()),silent=T)
				if(any(sapply(as.character(mycall),function(y) {y %in% names(x@data)})))
					try(xidx <- eval(mycall, x@data, parent.frame()),silent=T)		
				if (!exists("xidx") || !is.logical(xidx)) 
					simpleError(stop("'subset' must evaluate to logical"))				
				xidx <- xidx & !is.na(xidx)
				if (length(x@SelectedIdx)>0)
					x@SelectedIdx = x@SelectedIdx[xidx]
				if (length(x@InvalidIdx)>0)
					x@InvalidIdx = x@InvalidIdx[xidx]
				if (nrow(x@data)>0)
					x@data = x@data[xidx,,drop=drop]
				x@Spectra = x@Spectra[xidx,]
				x@sp = x@sp[xidx,]
				x@time= x@time[xidx,]
				x@endTime= x@endTime[xidx]
			}	
			
			if (missing(select)) 
				vars <- TRUE
			else {				
				nl <- as.list(1:ncol(x@Spectra))
				names(nl) <- colnames(x@Spectra)					
				vars <- eval(substitute(select), nl, parent.frame())				
				if(vars %in% colnames(x@Spectra)){
					y_idx = as.integer(nl[vars])
					
					x@Wavelengths = x@Wavelengths[y_idx]
					x@Spectra = x@Spectra[,y_idx,drop=F]
				}					

				nl <- as.list(1:ncol(x@data))
				names(nl) <- colnames(x@data)
				vars <- eval(substitute(select), nl, parent.frame())
				if(vars %in% colnames(x@data)){
					y_idx = as.integer(nl[vars])	
					x@data = x@data[,y_idx,drop=F]
				}
			}
			
			validObject(x)
			return(x)
		})
