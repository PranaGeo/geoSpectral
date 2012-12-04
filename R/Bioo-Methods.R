# TODO: names(), colnams()
# 
# Author: acizmeli
###############################################################################

#########################################################################
# Method : Conversions from and to data.frame
#########################################################################
setAs(from="Bioo", to="data.frame", def=function(from){
			if(any(grepl("Units", names(attributes(from)))))
				output = from@DF
			
#			attr(output,"ShortName") = from@ShortName
			attr(output,"LongName") = from@LongName
			attr(output,"Units") = from@Units
			return(output)
		})
setAs(from="data.frame", to="Bioo", def=function(from){
			outS = new("Bioo", DF=from)
			Units = attr(from,"Units")
			if(length(Units)==1)
				Units = rep(Units, ncol(outS))
			if (any(grepl("Units", names(attributes(from)))))
				outS@Units=Units 
			if (any(grepl("LongName", names(attributes(from)))))
				outS@LongName = rep(attr(from, "LongName"),ncol(outS))				
			return(outS)
		})

#########################################################################
# Method : Conversion from Bioo to BiooList using a data field (factor)
#########################################################################
#Later add the functionality with FUN (i.e. taking means)
Bioo2BiooList = function(myobj, name,FUN){
	#Get the indexes of each DF row :
	idx = lapply(unique(myobj[[name]]),function(x) which(x==myobj[[name]]))
	#For each row index in the list, subset the DF, return a list
	output = lapply(idx,function(x) myobj[x,])
	output = as(output,"BiooList")
	output@by = name
	return(output)
}
#########################################################################
# Method : show
#########################################################################
setMethod("show", "Bioo", function(object){
			cat("\n", paste('An object of class "Bioo"\n', 
							ncol(object@DF),"variables in columns and", nrow(object@DF), 
							"observations in rows"), "\n",
					"LongName : ",head(object@LongName), "\n",					
					"Units : ", head(object@Units), "\n",
					"Columns : ", head(colnames(object@DF)), "...\n")
#			cat("\nHeader: \n")
#			show(object@header)
		})

#########################################################################
# Method : names
#########################################################################
setMethod("names", signature = "Bioo", 
		def = function (x){ return(names(x@DF)) })
#########################################################################
# Method : dim
#########################################################################
setMethod("dim", signature = "Bioo", 
		def = function (x){  return(dim(x@DF))  })
#########################################################################
# Method : ncol
#########################################################################
setMethod("ncol", signature = "Bioo", 
		def = function (x){  return(ncol(x@DF))  })
########################################################################
# Method : nrow
#########################################################################
setMethod("nrow", signature = "Bioo", 
		def = function (x){  return(nrow(x@DF))  })
#########################################################################
# Method : head
#########################################################################
setMethod("head", signature = "Bioo", 
		def = function (x){  return(head(x@DF)) })
#########################################################################
# Method : cbind
#########################################################################
setMethod("cbind", signature = "Bioo", def = function (..., deparse.level = 1){
			stop("Not implemented yet!")
			return() 
		})

#########################################################################
# Method : spc.rbind
#########################################################################
setGeneric (name= "spc.rbind",
		def=function(...){standardGeneric("spc.rbind")})
setMethod("spc.rbind", signature = "Bioo", def = function (...){
			DFL=sapply(list(...),function(x) names(x),simplify=F)
			if(!all(sapply(1:length(DFL),function(x) all(DFL[[x]]==DFL[[1]]))))
				stop("Names of all columns should be the same")
#			dots <-match.call(expand.dots=F)$...

			#Exctract the input arguments as a list of data frames
			DFL=sapply(list(...),function(x) x@DF,simplify=F)
			
			#Check that all the inputs are Bioo objects
			if(!all(sapply(list(...), class)=="Bioo"))
				stop("All input arguments should be Bioo objects")
			
			outt = ..1
			#apply rbind() on the DF slot of the input arguments 
			outt@DF = do.call(rbind,DFL)
			
			outt@SelectedIdx = logical()
			outt@InvalidIdx = logical()
			validObject(outt)
			return(outt) 
		})

#########################################################################
# Method : $
#########################################################################
setMethod("$", signature = "Bioo", 
		function(x, name) {
			x@DF[[name]]
		})
setReplaceMethod("$", signature = "Bioo", 
		function(x, name, value) {
			if (length(value)!=nrow(x))
				stop("Replace value must have the same number or rows as the input object")
			x@DF[[name]] = value
			validObject(x)
			return(x)
		})

#########################################################################
# Method : [[
#########################################################################
setMethod("[[", signature="Bioo",
		function(x, i, j, ...) {
			x@DF[[i]]
		})
setReplaceMethod("[[",  signature="Bioo",
		function(x, i, j, value) {
			if (length(value)!=nrow(x))
				stop("Replace value must have the same number or rows as the input object")
			x@DF[[i]] <- value
			validObject(x)
			return(x)
		})

#########################################################################
# Method : [
#########################################################################
setMethod("[", signature(x = "Bioo"),
		function(x, i, j) {
			if(missing(i))
				i =  1:nrow(x@DF)
			if(missing(j))
				j =  1:ncol(x@DF)
			
			if(is.character(j))
				j = match(j, names(x))
			
			x@DF=x@DF[i,j,drop=F]
			x@Units = x@Units[j]
			x@LongName= x@LongName[j]
			x@SelectedIdx = logical()
			
			if (length(x@InvalidIdx)>1)
				x@InvalidIdx = x@InvalidIdx[i] 
			return(x)
		})

#########################################################################
# Method : sort
#########################################################################
setMethod("sort", signature="Bioo", definition= function (x, which.col, decreasing = FALSE, ...){
			if(!(is.numeric(x[[which.col]]) | all(is.finite(x[[which.col]]))))
				stop(paste("The column", which.col, "should be numeric with finite values."))
			
			if(length(which.col)>1)
				stop(paste("Sorting with respect to only one is supported"))
			
			#Sort with respect to the selected input column
			d_idx = sort.int(x[[which.col]], decreasing=decreasing, index.return = TRUE)
			x@DF= x@DF[d_idx$ix,,drop=F]
			if (nrow(x@Ancillary)>1)
				x@Ancillary@DF = x@Ancillary@DF[d_idx$ix,]
			if(length(x@SelectedIdx)>0)
				x@SelectedIdx = x@SelectedIdx[d_idx$ix]
			if(length(x@InvalidIdx)>0)
				x@InvalidIdx = x@InvalidIdx[d_idx$ix]
			return(x)
		})

#########################################################################
# Method : bioo.getheader
#########################################################################
setGeneric (name= "bioo.getheader",
		def=function(object,name){standardGeneric("bioo.getheader")})
setMethod("bioo.getheader", signature = "Bioo", 
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
# Method : bioo.setheader
#########################################################################
setGeneric (name="bioo.setheader<-",
		def=function(object,value,...){standardGeneric("bioo.setheader<-")})
setReplaceMethod(f="bioo.setheader", signature="Bioo",
		definition=function(object,value,...){
			object@header<-value
			validObject(object)
			return(object)
		})

#########################################################################
# Method : bioo.getselected.idx
#########################################################################
setGeneric (name= "bioo.getselected.idx",
		def=function(object){standardGeneric("bioo.getselected.idx")})
setMethod("bioo.getselected.idx", signature = "Bioo", 
		def = function (object){
			return(object@SelectedIdx)
		})
#########################################################################
# Method : bioo.setselected.idx	
#########################################################################
setGeneric("bioo.setselected.idx<-",function(object,value)
		{standardGeneric("bioo.setselected.idx<-")})
setReplaceMethod(f="bioo.setselected.idx", signature="Bioo",
		definition=function(object,value){
			if(is.numeric(value)){
				idx = bioo.getinvalid.idx(object)
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
# Method : bioo.getinvalid.idx
#########################################################################
setGeneric (name= "bioo.getinvalid.idx",
		def=function(object){standardGeneric("bioo.getinvalid.idx")})
setMethod("bioo.getinvalid.idx", signature = "Bioo", 
		def = function (object){
			return(object@InvalidIdx)
		})
#########################################################################
# Method : bioo.setinvalid.idx
#########################################################################
setGeneric("bioo.setinvalid.idx<-",function(object,value)
		{standardGeneric("bioo.setinvalid.idx<-")})
setReplaceMethod(f="bioo.setinvalid.idx", signature="Bioo",
		definition=function(object,value){
			if(is.numeric(value)){
				idx = bioo.getinvalid.idx(object)
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
# Method : plot.time
#########################################################################
setGeneric (name= "plot.time",
		def=function(object, ...){standardGeneric("plot.time")})
setMethod("plot.time", signature="Bioo", function (object,Y,maxSp=50, ...){
			idx = as(1:ncol(object@DF), "logical")
			
			if (length(object@InvalidIdx)==0)
				object@InvalidIdx = rep(FALSE,nrow(object@DF))		
			
			x = 1:nrow(object)
			xlb = "Observation number"
			if (class(object)=="Spectra")
				ylb = object@LongName[1]
			else
				ylb = ""
			
			if(missing(Y)){
				if(!missing(maxSp) && ncol(object)>maxSp)
					Y = seq(1,ncol(object),length.out=maxSp)
				else
					Y = names(object)
			}
			matplot(x[!object@InvalidIdx], 
					object@DF[!object@InvalidIdx,Y], type="l", pch=19,cex=0.3,
					xlab=xlb, ylab=ylb, ...)
			grid(col="black")			
		})

#########################################################################
# Method : plot.depth
#########################################################################
setGeneric (name= "plot.depth",
		def=function(object, ...){standardGeneric("plot.depth")})
setMethod("plot.depth", signature="Bioo", function (object,X,maxSp=20, 
				title, add=FALSE, xlab=NULL, ylab=NULL, ylim=NULL,...){
			
			idx = as(1:ncol(object@DF), "logical")
			depth=object$DEPTH
			
			if (length(object@InvalidIdx)==0)
				object@InvalidIdx = rep(FALSE,nrow(object@DF))		
			
			if(missing(ylab))
				ylab = "Depth [m]"
			
			if(missing(xlab)) {
				if (class(object)=="Spectra") {
					xlab = paste(object@LongName[1], " [", object@Units,"]",sep="") 					
				} else {
					xlab = ""
				}
			}
			if(missing(X)){
				if(ncol(object)>maxSp)
					X = seq(1,ncol(object),length.out=maxSp)
				else
					X = names(object)
			}
			if (is.numeric(X))
				X = names(object)[X]
			
			if(missing(ylim)){
				ylim = rev(range(pretty(depth[!object@InvalidIdx],n=10)))
				ylim[2]=-0.1	
			}
			#If any, do not draw these parameters
			X = gsub("DEPTH","",X,fixed=T)
			X = gsub("VOLTAGE","",X,fixed=T)
			X = gsub("TIME","",X,fixed=T)
			X=X[X!=""]
			
			myunits = object@Units[match(X,names(object))]
			mynames = names(object@DF)[match(X,names(object))]
			u_units = unique(myunits)
			my_sides = rep(c(1,3), ceiling(length(u_units)/2))
			
			#Extract the data to plot
			myX = object@DF[!object@InvalidIdx,X,drop=F]
			myY = depth[!object@InvalidIdx]
			#Sort with respect to depth
			d_idx = sort.int(myY,index.return = TRUE)
			myY = d_idx$x
			myX = myX[d_idx$ix,,drop=F]
			#Eliminate rows full with zeros
			idx = !apply(myX==0,1,all)
			myY = myY[idx]
			myX = myX[idx,,drop=F]
			#Eliminate NAs in depth
			idx = !is.na(myY)
			myY = myY[idx]
			myX = myX[idx,,drop=F]
			
			if (!all(diff(myY)==0) & !(length(myY)<2)) {
				if(length(u_units)==1){	
					if(add)
						matlines(myX,myY,type="l",xlab="",ylab="",ylim=ylim,...)
					else
						matplot(myX,myY,type="l",xlab="",ylab="",ylim=ylim,...)
					
					matpoints(myX,myY,xlab="",ylab="",pch=19,cex=0.4,ylim=ylim,...)
					
					mtext(ylab,side=2,line=3,cex=1.6)
					mtext(xlab,side=1,line=3,cex=1.6)
					grid(col="black")		
				}else{
#			for (I in 1:length(u_units)){
					for (I in 1:2){
						if (I!=1)
							par(new=T)
						col_idx = match(u_units[I],myunits)
						xlab = paste(mynames[col_idx[1]], " [", myunits[col_idx[1]],"]",sep="")
						
						plot(myX[,col_idx],myY,type="l", axes=F,pch=19,cex=0.3, ylim=ylim,col=I,xlab="",ylab="",...)
						axis(my_sides[I], col=I, pretty(range(myX[,col_idx]),10))
						mtext(my_sides[I],text=xlab,line=2)
						if (I==1)
							box(); 	
					}
					axis(2,pretty(range(myY),10))
					mtext(2, text=ylb,line=2)
					grid(col="black")	
				}
				if(!missing(title))
					title(title)
			} else{
				return(0)
			}
		})
		
#########################################################################
# Method : plot.depth.by.station
#########################################################################
# setGeneric (name= "plot.depth.by.station",
# 				def=function(input,fname){standardGeneric("plot.depth.by.station")})
# setMethod("plot.depth.by.station", signature="Bioo", function(input,fname) { 
# 			Stat=character()
# 			mymax = sapply(input,function(x) max(x$X440,na.rm=TRUE));mymax=max(mymax,na.rm=TRUE)*1.1
# 			mymin = sapply(input,function(x) min(x$X440,na.rm=TRUE));mymin=min(mymin,na.rm=TRUE)*0.9
# 			dmax = max(sapply(input,function(x) max(x$DEPTH,na.rm=TRUE)),na.rm=TRUE)
# 			fname = paste(fname, "_", gsub("/","_ov_",input[[I]]@ShortName),".png",sep="")
# 			png(file=fname)
# 			for (I in 1:length(input)) {
# 				xlab = paste(input[[I]]@ShortName, " [ ", input[[I]]@Units[1], " ]",sep="")
# 				if (I==1) {
# 					plot(input[[I]][["X440"]],input[[I]][["DEPTH"]],
# 							col=I,lwd=3,xlim=c(mymin,mymax),ylim=c(dmax,-0.1),type="l",xlab="",ylab="",cex.axis=1.4)
# 					points(input[[I]][["X440"]],input[[I]][["DEPTH"]],col=I,bg=I,pch=21)
# 				}
# 				else
# 					plot.depth(input[[I]], "X440",add=T,"col"=I,lwd=3,xlab=xlab)
# 				Stat[I] = input[[I]]$STATION[1] 
# 			}
# 			legend("bottomright",Stat,col=1:I,fill=1:I)
# 			dev.off()
# 			print(paste("Saved",fname))
# 		})

#########################################################################
# Method : bioo.interp.time
#########################################################################
setGeneric (name= "bioo.interp.time",
		def=function(source1,target1,column,show.plot){standardGeneric("bioo.interp.time")})
setMethod("bioo.interp.time", signature = "Bioo", 
		def = function (source1,target1,column,show.plot=FALSE){
			my = approx(source1$TIME, source1[[column]],xout=target1$TIME)
			if(show.plot){
				plot(source1$TIME, source1[[column]],type="l",ylab=column,xlab="TIME")
#				plot.time(source1[,c("TIME",column)])
				points(my$x,my$y,col="green",cex=0.4)
				grid(col="black")
			}
			return(my$y)
		})

#########################################################################
# Method : bioo.invalid.detect
#########################################################################
setGeneric(name= "bioo.invalid.detect",
		def=function(source1){standardGeneric("bioo.invalid.detect")})
setMethod("bioo.invalid.detect", signature = "Bioo", def=function(source1){
			out = apply(source1@DF, 2,is.na)
			if(is.null(dim(out))& nrow(source1@DF)==1)
				dim(out)<-c(1,ncol(source1@DF))
			out = apply(out,1,all)
		})

#########################################################################
# Method : bioo.data2header
#########################################################################
setGeneric(name= "bioo.data2header",
		def=function(object,headerfield,dataname,compress,...){standardGeneric("bioo.data2header")})
setMethod("bioo.data2header", signature = "Bioo", 
		def=function(object,headerfield,dataname,compress=TRUE,...){
			if(missing(headerfield))
				headerfield = dataname
			object@header[[headerfield]]=object[[dataname]]
			if(compress )
				object@header[[headerfield]]=object[[dataname]][1]
			
			return(object)
		})

#########################################################################
# Method : subset
#########################################################################
setMethod("subset",  signature="Bioo",
		definition=function(x, subset, select, drop = FALSE, ...) {   
			if (missing(subset)) 
				mycall <- TRUE
			else {
				mycall <- substitute(subset)
				xidx <- eval(mycall, x@DF, parent.frame())
				if (!is.logical(xidx)) 
					stop("'subset' must evaluate to logical")
				xidx <- xidx & !is.na(xidx)
				if (length(x@SelectedIdx)>0)
					x@SelectedIdx = x@SelectedIdx[xidx]
				if (length(x@InvalidIdx)>0)
					x@InvalidIdx = x@InvalidIdx[xidx]
			}	
			if (missing(select)) 
				vars <- TRUE
			else {
				nl <- as.list(seq_along(x@DF))
				names(nl) <- names(x)
				vars <- eval(substitute(select), nl, parent.frame())
				y_idx = as.integer(nl[vars])
				x@Units = x@Units[y_idx]
				x@LongName = x@LongName[y_idx]
			}
			x@DF = x@DF[xidx, vars, drop = drop]
			validObject(x)
			return(x)
		})

#########################################################################
# Method : bioo.add.column
#########################################################################
setGeneric(name= "bioo.add.column",
		def=function(object, name, value, units,longname){standardGeneric("bioo.add.column")})
setMethod("bioo.add.column", signature="Bioo", definition= function (object, name, value, units,longname) {
			if (name %in% names(object)){
				stop(paste("The column", name, "already exists. Consider using the methods $, [ or [["))
			} 		  
			if(!is.data.frame(value)){
				if(length(value)==1)
					value = rep(value, nrow(object))
				
				value = data.frame(value)
				names(value) = name
			}			
			if (missing(units) && length(unique(object@Units))==1)
				units = object@Units[1]			
			if (length(units)!= ncol(value))
				stop(paste('The input variable "units" should have the same lengths as the number of columns of "value"'))
			if (missing(longname))
				longname = name
				
			if(nrow(object)>0 & nrow(object)!=nrow(value))
				stop(paste('The number of rows do not match'))			
				
			if(nrow(object)==0){
				object@DF = value
				object@Units = c(object@Units, units)
				object@LongName = c(object@LongName,longname)
			} else{
				object@DF = cbind(object@DF,value)
				object@Units = c(object@Units, units)
				object@LongName = c(object@LongName,longname)
			}
			validObject(object)
			return(object)
		})