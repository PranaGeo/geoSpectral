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
			x@SelectedIdx = logical()
			
			if (length(x@InvalidIdx)>1)
				x@InvalidIdx = x@InvalidIdx[i] 
			return(x)
		})

#########################################################################
# Method : GetBiooHeader
#########################################################################
setGeneric (name= "GetBiooHeader",
		def=function(object){standardGeneric("GetBiooHeader")})
setMethod("GetBiooHeader", signature = "Bioo", 
		def = function (object){
			return(object@header)
		})
#########################################################################
# Method : SetBiooHeader
#########################################################################
setGeneric (name="SetBiooHeader<-",
		def=function(object,value,...){standardGeneric("SetBiooHeader<-")})
setReplaceMethod(
		f="SetBiooHeader",
		signature="Bioo",
		definition=function(object,value){
			object@header<-value
			validObject(object)
			return(object)
		})

#########################################################################
# Method : GetSelectedIdx
#########################################################################
setGeneric (name= "GetSelectedIdx",
		def=function(object){standardGeneric("GetSelectedIdx")})
setMethod("GetSelectedIdx", signature = "Bioo", 
		def = function (object){
			return(object@SelectedIdx)
		})
#########################################################################
# Method : SetSelectedIdx	
#########################################################################
setGeneric("SetSelectedIdx<-",function(object,value)
		{standardGeneric("SetSelectedIdx<-")})
setReplaceMethod(
		f="SetSelectedIdx",
		signature="Bioo",
		definition=function(object,value){
			if(is.numeric(value)){
				idx = GetInvalidIdx(object)
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
# Method : GetinvalidIdx
#########################################################################
setGeneric (name= "GetInvalidIdx",
		def=function(object){standardGeneric("GetInvalidIdx")})
setMethod("GetInvalidIdx", signature = "Bioo", 
		def = function (object){
			return(object@InvalidIdx)
		})
#########################################################################
# Method : SetInvaliddidx	
#########################################################################
setGeneric("SetInvalidIdx<-",function(object,value)
		{standardGeneric("SetInvalidIdx<-")})
setReplaceMethod(
		f="SetInvalidIdx",
		signature="Bioo",
		definition=function(object,value){
			if(is.numeric(value)){
				idx = GetInvalidIdx(object)
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
setMethod("plot.depth", signature="Bioo", function (object,X,maxSp=50, ...){
			idx = as(1:ncol(object@DF), "logical")
			
			if (length(object@InvalidIdx)==0)
				object@InvalidIdx = rep(FALSE,nrow(object@DF))		
			
			ylb = "Depth [m]"
			if (class(object)=="Spectra") {
				xlb = object@LongName[1]
				depth_idx = grep("DEPTH",names(object@Ancillary))
				if (length(depth_idx)==1){
					depth = object@Ancillary@DF[,depth_idx]
				} else{
					stop("Cannot match the DEPTH column")
				}
			} else {
				xlb = ""
				depth_idx = grep("DEPTH",names(object@DF))
				if (length(depth_idx)==1){
					depth = object@DF[,depth_idx]
				} else{
					stop("Cannot match the DEPTH column")
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

			myylim = rev(range(pretty(depth[!object@InvalidIdx],n=10)))
			myylim[2]=-0.1	
			#If any, do not draw these parameters
			X = gsub("DEPTH","",X,fixed=T)
			X = gsub("VOLTAGE","",X,fixed=T)
			X = gsub("TIME","",X,fixed=T)
			X=X[X!=""]
			
			myunits = object@Units[match(X,names(object))]
			mynames = names(object@DF)[match(X,names(object))]
			u_units = unique(myunits)
			my_sides = rep(c(1,3), ceiling(length(u_units)/2))
browser()			
			if(length(u_units)==1){
				myX = object@DF[!object@InvalidIdx,X]
				myY = depth[!object@InvalidIdx]
				matplot(myX,myY,type="l", pch=19,cex=0.3, xlab=xlb,ylab=ylb,ylim=myylim,...)
				grid(col="black")		
			}else{
#			for (I in 1:length(u_units)){
				for (I in 1:2){
					if (I!=1)
						par(new=T)
					myX = object@DF[!object@InvalidIdx,X]
					myY = depth[!object@InvalidIdx]
					col_idx = match(u_units[I],myunits)
					xlb = paste(mynames[col_idx[1]], " [", myunits[col_idx[1]],"]",sep="")
					
					plot(myX[,col_idx],myY,type="l", axes=F,pch=19,cex=0.3, ylim=myylim,col=I,xlab="",ylab="",...)
					axis(my_sides[I], col=I, pretty(range(myX[,col_idx]),10))
					mtext(my_sides[I],text=xlb,line=2)
					if (I==1)
						box(); 	
				}
				axis(2,pretty(range(myY),10))
				mtext(2, text=ylb,line=2)
				grid(col="black")	
			}
		})
#########################################################################
# Method : biooInterpTime
#########################################################################
setGeneric (name= "biooInterpTime",
		def=function(source1,target1,column,plot=F){standardGeneric("biooInterpTime")})
setMethod("biooInterpTime", signature = "Bioo", 
		def = function (source1,target1,column){
			my = approx(source1$TIME, source1[[column]],xout=target1$TIME)
			
			if(plot){
				plot(source1$TIME, source1[[column]],type="l",ylab=column,xlab="TIME")
#				plot.time(source1[,c("TIME",column)])
				points(my$x,my$y,col="green",cex=0.4)
				grid(col="black")
			}
			return(myout)
		})
