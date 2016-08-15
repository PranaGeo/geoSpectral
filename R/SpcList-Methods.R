
#########################################################################
# Method : Conversion from SpcList to Spc
#########################################################################
#setAs(from="SpcList", to="Spc", def=function(from){		
#	dims = t(sapply(from,dim))[,2]
#	if(!all(dims==dims[1]))
#		stop("All SpcList elements should have the same number of rows (including Ancillary data for Spectra objects)")
#	nms = sapply(from,names)
#	if(!all(sapply(1:nrow(nms), function(x) all(nms[x,1]==nms[x,]))))
#		stop("All SpcList elements should have the same names (including Ancillary data for Spectra objects)")
#	
#	if (all(sapply(from,class)=="Spectra") |(all(sapply(from,class)=="Spc"))) {
#		if (all(sapply(from,class)=="Spectra")){
#			DF = do.call(rbind, lapply(from,function(x) x@Spectra))
#			DFAnc = do.call(rbind, lapply(from,function(x) x@data))
#			Anc = new("Spc",DF=DFAnc,Units=from[[1]]@Ancillary@Units)
#			output = new("Spectra",DF=DF,Units=from[[1]]@Units,Wavelengths=from[[1]]@Wavelengths,
#					ShortName=from[[1]]@ShortName,LongName=from[[1]]@LongName,Ancillary=Anc)
#		}
#		if (all(sapply(from,class)=="Spc")){
#			DF = do.call(rbind, lapply(from,function(x) x@Spectra))
#			output = new("Spc",DF=DF,Units=from[[1]]@Ancillary@Units)
#		}
#	} else {
#		stop("All SpcList elements should be of class 'Spectra' of 'Spc'")
#	}
#	return(output)
#})

#########################################################################
# Method : spc.plot.grid
#########################################################################
setGeneric (name= "spc.plot.grid",
		def=function(x,FUN, nnrow, nncol,...){standardGeneric("spc.plot.grid")})
setMethod("spc.plot.grid", "SpcList", function (x,FUN, nnrow, nncol, mar=c(4,4.5,1,0.5), 
				oma = c(0,0,0,0), lab_cex, ...){
			nb_spc = length(which(sapply(x, inherits, "Spectra")))
			mypar = par()
			nrow = ceiling(nb_spc/nncol)
			
#			FUN <- match.fun(FUN)
			if(missing(mar))
				mar = c(4,4.5,1,0.5)
			if(missing(oma))
				oma = c(0,0,0,0)#c(1.5,2,1,1)
			if(missing(lab_cex))
				lab_cex = 1
			
			par(mfrow=c(nnrow,nncol), mar=mar, oma=oma)
			
			for (I in 1:length(x)) {
				if(1){ #(nrow(x[[I]])>1){
					if(x@by!="VariousVariables"){
						#tit = paste(x@by, ":", as.character(spc.getheader(x[[I]],x@by)))
						tit = paste(as.character(spc.getheader(x[[I]],x@by)))
					}
					else{
						tit=""#paste(x[[I]]@ShortName)
					}
					eval_txt = paste(FUN, "(x[[I]],lab_cex=lab_cex,...)",sep="")
					eval(parse(text=eval_txt))
					title(main=tit,mgp=c(2,1,0))
					
					if (par()$mfg[1]==par()$mfg[3] & par()$mfg[2]==par()$mfg[4] & I<length(x)) {
						dev.new()
						par(mfrow=c(nnrow,nncol), mar=mar, oma=oma)
					}				
				}
			}
			par(mfrow=mypar$mfrow,mar=mypar$mar,oma=mypar$oma)
		})

#########################################################################
# Method : spc.plot.overlay
#########################################################################
setGeneric (name= "spc.plot.overlay",
		def=function(object, ...){standardGeneric("spc.plot.overlay")})
setMethod("spc.plot.overlay", "SpcList", function (object, lab_cex=1,leg_idx=T, type="l", lty=1,lwd=1, col, ...){
			if(missing(col))
				col = 1:length(object)
			if(length(leg_idx)==1)
				leg_idx = rep(leg_idx,length(object))
			if(length(lty)==1)
				lty = rep(lty,length(object))
			if(length(lwd)==1)
				lwd = rep(lwd,length(object))
			if(length(type)==1)
				type = rep(type,length(object))
			
			all_x = unlist(lapply(object,function(t) t@Wavelengths))
			all_y = unlist(lapply(object,function(t) t@Spectra))
#			browser()
			xlim = range(all_x)
			ylim = range(all_y)
			
			if(any(grepl("xlim",names(match.call())))){
				xlim = eval(match.call(expand.dots = T)$xlim)
			}
			if(any(grepl("ylim",names(match.call())))){
				ylim = eval(match.call(expand.dots = T)$ylim)
			}
			tit=""
			#Check object names to see if they can be put in the legend
			nms = sapply(names(object), function(x) x==names(object)[1])
			nms = nms[-1]
			
			for (I in 1:length(object)) {
				if(object@by!="VariousVariables"){
					#tit[I] = paste(object@by, ":", as.character(spc.getheader(object[[I]],object@by)))
					tit[I] = paste(as.character(spc.getheader(object[[I]],object@by)))
				}
				else{
					if(all(!nms))
						tit[I]=names(object)[I]					
					else
						tit[I]=as.character(I)#paste(object[[I]]@ShortName)
				}
				if(I==1)
					eval_txt = paste("spc.plot", "(object[[I]],lab_cex=lab_cex,col=col[I],lty=lty[I],lwd=lwd[I],type=type[I],...)",sep="")
				else
					eval_txt = paste("spc.lines", "(object[[I]],col=col[I],lty=lty[I],lwd=lwd[I],type=type[I],...)",sep="")
				if (!any(grepl("xlim",names(match.call()))))
					eval_txt = gsub("object\\[\\[I\\]\\],","object\\[\\[I\\]\\],xlim=xlim,",eval_txt)
				if (!any(grepl("ylim",names(match.call()))))
					eval_txt = gsub("object\\[\\[I\\]\\],","object\\[\\[I\\]\\],ylim=ylim,",eval_txt)
#			print(eval_txt)	
				eval(parse(text=eval_txt))				
				#title(main=tit,mgp=c(2,1,0))
			}#end for
			if(!all(diff(lty)==0))
				legend("bottomright",tit[leg_idx],col=col[leg_idx],cex=lab_cex,bty="n",lty=lty[leg_idx],lwd=lwd[leg_idx])	
			else
				legend("bottomright",tit[leg_idx],col=col[leg_idx],fill=col[leg_idx],cex=lab_cex,bty="n")	
		})

#########################################################################
setGeneric (name= "spc.plot.depth.overlay",
		def=function(object,X,...){standardGeneric("spc.plot.depth.overlay")})
setMethod("spc.plot.depth.overlay", "SpcList", function (object, X, lab_cex, ...){
			if(missing(lab_cex))
				lab_cex = 1
#			browser()
			all_y = unlist(lapply(object,function(t) t$DEPTH))
			all_x = unlist(lapply(object,function(t) t@Spectra[X]))
			xlim = range(all_x)
			ylim = range(all_y)
			tit=""
			for (I in 1:length(object)) {
				if(object@by!="VariousVariables"){
					#tit[I] = paste(object@by, ":", as.character(spc.getheader(object[[I]],object@by)))
					tit[I] = paste(as.character(spc.getheader(object[[I]],object@by)))
				}
				else{
					tit[I]=as.character(I)#paste(object[[I]]@ShortName)
				}
				if(I==1)
					eval_txt = paste("spc.plot.depth","(object[[I]],X,lab_cex=lab_cex,xlim=xlim,col=I,...)",sep="")
				else
					eval_txt =  paste("spc.plot.depth","(object[[I]],X,add=T,lab_cex=lab_cex,xlim=xlim,col=I,...)",sep="")
				eval(parse(text=eval_txt))				
				#title(main=tit,mgp=c(2,1,0))
			}#end for
			legend("bottomright",tit,col=1:I,fill=1:I,cex=lab_cex,bty="n")
			
		})
#########################################################################
# Method : subset
#########################################################################
#The argument "select" is not implemented yet. Use "[]"
setMethod("subset",  signature="SpcList",
		definition=function(x, subset, select, drop = FALSE, ...) {                   
			
			if(class(subset)=="list") { 
				if(length(subset)>1 & length(subset)!=length(x))
					stop('The argument "subset" should be a list of length one or the same length of the SpcList object')          
				
				if(!missing(select)) 
					temp = lapply(1:length(x), function(t) subset(x[[t]], subset=subset[[t]], select=select, drop=drop, ...))
				else
					temp = lapply(1:length(x), function(t) subset(x[[t]], subset=subset[[t]], drop=drop, ...))
				
			} else {
				stop('The input argument "subset" should be a list element containing indexes')
			}
			
			x@.Data = temp
			return(x)
		})

#########################################################################
# Method : Arith
#########################################################################
#setMethod("Arith",signature(e1 = "SpcList", e2 = "SpcList"),function (e1, e2) {
#			browser()
#			if(length(e1)!=length(e2))
#				stop("Lengths of input SpcList object should match")
#			
#			result <- callGeneric(e1[[1]], e2@Spectra[[1]])
#			output <- new("Spectra",DF=result,Wavelengths=e1@Wavelengths,Units=e1@Units,
#					ShortName = "Arith", LongName="Arith")			
#			return(output)
#		})
#
#lapply(1:length(e1), function(x){
#			browser()
#			callGeneric(e1[[x]]@Spectra, e2[[x]]@Spectra)
#		})

#########################################################################
# Method : names
#########################################################################
setMethod("names", "SpcList", function(x){
			sapply(x, function(mobject) {
						if(class(mobject)=="Spectra") mobject@ShortName[1]	else class(mobject) 
					})
		})

#########################################################################
# Method : $
#########################################################################
setMethod("$", signature = "SpcList", 
		function(x, name) {
			myn = names(x)
			if(any(grepl(name,myn))){
				x[[match(name,myn)[1]]]}
			else stop("Could not match any object name")
		})

#########################################################################
# Method : show
#########################################################################
setMethod("show", "SpcList", function(object){
			if(length(object)>0)
				sapply(1:length(object), function(x) {
							if(object@by!="VariousVariables") {
								byName = paste(object@by, spc.getheader(object[[x]],object@by), ":")								
							}
							else { 
								byName = paste("Element", x, ":")								
							}
							
							cat(byName)
							show(object[[x]])
						})
			else cat("Empty SpcList\n")
		})

#########################################################################
# Constructor function : SpcList()
#########################################################################
SpcList = function (spclist){
	new("SpcList", spclist)
}
#########################################################################
# Method : spc.invalid.detect
#########################################################################
setMethod("spc.invalid.detect", signature = "list", def=function(source1){
			out = lapply(source1, function(x) {SetInvalidIdx(x)<-spc.invalid.detect(x)})
			return(out)
		})

#########################################################################
# Method : spc.getheader
#########################################################################
setMethod("spc.getheader", signature = "list", def = function (object,name){
			sapply(object, spc.getheader,name)
		})

#########################################################################
# Method : spc.setheader<-
#########################################################################
setReplaceMethod(f="spc.setheader", signature="list",
		definition=function(object,value,...){
			if(inherits(value,"Spectra"))
				stop("It is forbidden to set a SpcHeader an object that inherits from the Spectra class")
			if(length(value)==1)
				value = rep(value,length(object))
			stopifnot(length(value)==length(object))			
			
			a=sapply(1:length(object), function(x) {
						object[[x]] = spc.setheader(object[[x]],value[x])
					})
			validObject(object)
			return(object)
		})
#########################################################################
# Method : spc.updateheader<-
#########################################################################
setReplaceMethod(f="spc.updateheader", signature="list",
		definition=function(object,Name,value,...){
			if(inherits(value,"Spectra"))
				stop("It is forbidden to place in a SpcHeader an object that inherits from the Spectra class")
			if(length(value)==1)
				value = rep(value,length(object))
			stopifnot(length(value)==length(object))			
			
			for(xx in 1:length(object)){
				spc.updateheader(object[[xx]],Name)<-value[xx]
			}
			validObject(object)
			return(object)
		})

#########################################################################
# Method : spc.data2header
#########################################################################
setMethod("spc.data2header", signature = "list", 
		def=function(object,dataname,headerfield,compress=TRUE,...){
			temp = lapply(object, spc.data2header, headerfield,dataname,compress,...)
			object@.Data=temp
			return(object)
		})
#########################################################################
# Method : sort
#########################################################################
setMethod("sort", signature="list", definition= function (x, which.col, decreasing = FALSE, ...){
			newdata = lapply(x, sort, which.col=which.col, decreasing=decreasing, ...)
			x@.Data = newdata
			return(x)
		})

#########################################################################
# Method : spc.lapply
#########################################################################
setGeneric (name= "spc.lapply",
		def=function(X, FUN,...){standardGeneric("spc.lapply")})
setMethod("spc.lapply", signature="SpcList", definition= function (X, FUN, ...) {
			by = X@by
			X = lapply(as(X,"list"),FUN,...)
			X = as(X, "SpcList")
			X@by = by
			validObject(X)
			return(X)
		})
#########################################################################
# Method : spc.header2data
#########################################################################
.h2d = function(object,headerfield,dataname,compress=TRUE,...) {
	if(missing(dataname))
		dataname=headerfield	

	X = lapply(object,function(x,...){ #
				if(headerfield %in% names(x@header))
					x = spc.header2data(x,headerfield=headerfield,dataname=dataname,compress=compress,...)
				else
					x
			})
	return(X)
}
setMethod("spc.header2data", signature="list", definition=.h2d)
setMethod("spc.header2data", signature="SpcList", 
		definition=function(object,headerfield,dataname,compress=TRUE,...){
			by = object@by	
			X <- .h2d(object,headerfield=headerfield,dataname=dataname,compress=compress,...)
			X <- as(X, "SpcList")
			X@by = by
			validObject(X)
			return(X)
		})

#########################################################################
# Method : subset
#########################################################################
#setMethod("subset",  signature="SpcList",
#		definition=function(x, subset, select, drop = FALSE, ...) {
##			myby = x@by
#			
#			for(AA in 1:length(x)) {
#				browser()
#				mycall <- substitute(subset)
#				xidx <- eval(mycall, x[[AA]], parent.frame())
#			}
##			x[[AA]] = subset(x[[AA]], subset,select,drop)
#			x = lapply(x, subset, subset,select,drop,...)
#			x = as(x, "SpcList")
#			x@by = myby
#		})

#' Create a spatio-temporal index based on a list of \code{Spectra} objects
#' @description
#' Given a list of \code{Spectra} objects, this function creates a STIDF object summarizing the
#' spatial and temporal variability of the input dataset. Upon request, it also includes
#' data columns.
#'
#' @param input  An object of class \code{spectra}
#' @param what2include A character variable giving the data columns to be included in the output
#' @param rowSimplify  Either of "none", "spc.colMeans","firstRow" or "lastRow". Default is "none"
#' @param includeTIME  Logical. Whether of not to include TIME data in the output STIDF object. Default is FALSE.
#' @param includeLATLON Logical. Whether of not to include LAT&LON data in the output STIDF object. Default is FALSE.
#' 
#' @details 
#' This function accepts a list of \code{Spectra} objects and outputs one STIDF object summarizing 
#' spatial and temporal variation of the input dataset. 
#' 
#' If rowSimplify="none", length of the output object will be equal to the sum of all rows of 
#' all elements of the input list object.
#' 
#' If rowSimplify="spc.colMeans", length of the output object will be equal to the number of rows of 
#' the input list object. 
#' This option returns the measurement nearest to the average time of each element of the input list.
#' 
#' firstRow and lastRow : length of the output object equals the number of rows of the input list object.
#' These two options return the first and last measurements of the input list element
#' 
#' @return An object of class \code{STIDF}. Each row of the output object has a space and time 
#' characteristics depending of the input argument \code{rowSimplify}.
#' @seealso \code{\link{spc.makeSpcList}}
#' @examples 
#' sp = spc.example_spectra()
#' BL = spc.makeSpcList(sp,"STATION")
#' stidx = spc.make.stindex(BL)
#' dim(stidx)
#' stidx = spc.make.stindex(BL, what2include = "CAST")
#' head(stidx@data)
#' stidx = spc.make.stindex(BL, rowSimplify="spc.colMeans")
#' dim(stidx)
spc.make.stindex = function(input,what2include="",rowSimplify="none",
                            includeTIME=FALSE,includeLATLON=FALSE) {
  
  if(!(rowSimplify %in% c("spc.colMeans","firstRow","lastRow","none")))
    stop(simpleError(paste("rowSimplify should be one of",paste(c("spc.colMeans","firstRow","lastRow","none"),collapse=","))))
  
  if(!inherits(input,"list"))
    stop("The input dataset should inherit from a list (can also be a SpcList)")
  
  MyOutput = lapply(1:length(input),function(x){
    if(nrow(input[[x]])>0){
      try(what2include<-get("what2include",envir=parent.frame(2)),silent=T)
      #what2include=c("Rrs_805","INTTIME")					
      #Save the endTime into a variable
      endTime<-input[[x]]@endTime
      
      #Convert to STIDF (dropping Spectral and Ancillary data, if any)
      if(rowSimplify=="spc.colMeans"){
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
        w2i2[[what2include]] = w2i
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
