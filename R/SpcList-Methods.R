
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
#' Plotting \code{SpcList} object in a grid
#'
#' @description
#' Generating plot of the contents of a \code{SpcList} object in a grid
#'
#' 
#' @usage 
#' spc.plot.grid(x,FUN, nnrow, nncol, mar=c(4,4.5,1,0.5),oma = c(0,0,0,0), lab_cex, ...)
#' @param x	 a \code{SpcList} data 
#' @param FUN a character string giving the name of the ploting function to be used. 
#' Can be either of "spc.plot"
#' @param mar A numeric vector of length 4, which sets the margin sizes in the following order: bottom, left, top, and right. The default is c(4,4.5,1,0.5)
#' @param ... any further arguments of plot
#' @param lab_cex vector of character expansion sizes, used cyclically
#' @param  nrow number of row for grid
#' @param  ncol number of column for grid
#' @param oma oma the "outer margin area" around a figure or figures. The usage of mar and oma is shown when plotting a single figure,
#' 
#' @examples
#' sp <- spc.example_spectra()
#' BL = spc.makeSpcList(sp,"CAST")
#' spc.plot.grid(BL,"spc.plot",3,2)
#' 
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
#' Plotting multiple \code{Spectra} objects inside a \code{SpcList}  
#'
#' @description
#' This function overlays spectra plots of several \code{Spectra} objects inside a 
#' \code{SpcList} object. The first element of the input \code{SpcList} object
#' is plotted with spc.plot() while remaining elements are overlaid with spc.lines().
#' 
#' @usage 
#' spc.plot.overlay(x,lab_cex=1,leg_idx=T, type="l", lty=1,lwd=1, col, ...)
#' @param x	 a \code{SpcList} data 
#' @param lab_cex vector of character expansion sizes, used cyclically
#' @param type character string (length 1 vector) or vector of 1-character strings indicating the type of plot for each column of y, 
#' @param lwd numeric. Vector of line widths. See par().
#' @param ...  any further arguments to the plotting function matplot() or spc.plot()
#' @param lty vector of line types. See par().
#' @param col A specification for the default plotting color. See par().
#' @param leg_idx logical If it is of length 1, it determines whether or not to display the legend.
#' If length(leg_idx) is bigger than 1, then its lengths has to equal length(object). Default is TRUE.
#' @examples
#' sp <- spc.example_spectra()
#' BL = spc.makeSpcList(sp,"CAST")
#' spc.plot.overlay(BL)
#' spc.plot.overlay(BL, xlim=c(400,500),ylim=c(0,0.2),lwd=2)
#' spc.plot.overlay(BL, col=c("red"), leg_idx=FALSE, lty=2)
#' spc.plot.overlay(BL, col=c("red","blue","green","yellow","cyan","black"))
#' 
setGeneric (name= "spc.plot.overlay",
		def=function(object, ...){standardGeneric("spc.plot.overlay")})
setMethod("spc.plot.overlay", "SpcList", function (object, lab_cex=1,leg_idx=T, type="l", lty=1,lwd=1, col, ...){
			if(missing(col))
				col = 1:length(object)
			if(length(col)==1)
			  col = rep(col,length(object))
			if(length(col)>1)
			  stopifnot(length(col)==length(object))
			if(length(leg_idx)>1)
			  stopifnot(length(leg_idx)==length(object))
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
			xlim = range(all_x)
			ylim = range(all_y)
			if(any(grepl("xlim",names(match.call())))){
				xlim = list(...)$xlim #eval(match.call(expand.dots = T)$xlim)
			}
			if(any(grepl("ylim",names(match.call())))){
				ylim = list(...)$ylim #eval(match.call(expand.dots = T)$ylim)
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
				if (0) #(!any(grepl("xlim",names(match.call()))))
					eval_txt = gsub("object\\[\\[I\\]\\],","object\\[\\[I\\]\\],xlim=xlim,",eval_txt)
				if (!any(grepl("ylim",names(match.call()))))
					eval_txt = gsub("object\\[\\[I\\]\\],","object\\[\\[I\\]\\],ylim=ylim,",eval_txt)

				eval(parse(text=eval_txt))				
			}#end for
			if(any(leg_idx)) {
			  if(!all(diff(lty)==0))
			    legend("bottomright",tit[leg_idx],col=col[leg_idx],cex=lab_cex,bty="n",lty=lty[leg_idx],lwd=lwd[leg_idx])	
			  else
			    legend("bottomright",tit[leg_idx],col=col[leg_idx],fill=col[leg_idx],cex=lab_cex,bty="n")	
			}
})

#########################################################################
#' Plotting \code{SpcList} object 
#'
#' @description
#' Generating plot of the contents of a \code{SpcList} object overlay with respect to depth
#'
#' @usage 
#' spc.plot.depth.overlay(object, X, lab_cex, ...)
#' @param object	 a \code{SpcList} data 
#' @param X column name or index 
#' @param lab_cex vector of character expansion sizes, used cyclically
#' @param ...  any further arguments of plot
#' @examples
#' sp <- spc.example_spectra()
#' BL = spc.makeSpcList(sp,"CAST")
#' spc.plot.depth.overlay(BL, "anap_555")
#' 
#' 
setGeneric (name= "spc.plot.depth.overlay",
		def=function(object,X,...){standardGeneric("spc.plot.depth.overlay")})
setMethod("spc.plot.depth.overlay", "SpcList", function (object, X, lab_cex, ...){
			if(missing(lab_cex))
				lab_cex = 1

			all_y = unlist(lapply(object,function(t) t$DEPTH))
			all_x = unlist(lapply(object,function(t) t@Spectra[,X]))
			xlim = range(all_x)
			ylim = rev(range(all_y))
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
					eval_txt = paste("spc.plot.depth","(object[[I]],X,lab_cex=lab_cex,xlim=xlim,ylim=ylim,col=I,...)",sep="")
				else
					eval_txt =  paste("spc.plot.depth","(object[[I]],X,add=T,lab_cex=lab_cex,xlim=xlim,ylim=ylim,col=I,...)",sep="")

				eval(parse(text=eval_txt))
				#title(main=tit,mgp=c(2,1,0))
			}#end for
			legend("bottomright",tit,col=1:I,fill=1:I,cex=lab_cex,bty="n")
			
		})
#########################################################################
# Method : subset
#########################################################################
#' Subsetting for a \code{spcList} and Spectra classes
#' @description
#' Subsetting can be achieved using the implementation of the R function subset() for \code{Spectra} and SpcList classes
#'It is possible to perform a row-wise selection
#'
#' @usage 
#' subset(x,y,select,...)
#' 
#' 
#' @param drop passed on to [ indexing operator. Default is FALSE 
#' @param ... arguments to be passed to or from other methods.
#' @param x A \code{Spectra} object 
#' @param y Subset
#' @param  select Condition selected
#' @examples 
#' fnm = file.path(system.file(package = "geoSpectral"), "test_data","particulate_absorption.csv.gz")
#' abs = read.table(fnm,sep=",",header=TRUE)
#' abs$STATION=factor(abs$STATION)
#' abs[1:2,1:17] #Display only the first 2 rows and first 17 columns if the data frame
#' lbd = as.numeric(gsub("X","",colnames(abs)[14:514]))
#' Units="1/m"
#' colnames(abs)= gsub("X",paste("anap","_",sep=""), colnames(abs))
#' colnames(abs)= gsub("PRES","DEPTH", colnames(abs))
#' abs = abs[,c(14:514,1:13)]
#' tz<-strsplit(as.character(abs$TIME)," ")[[1]][[3]] #Extract the timezone
#' abs$TIME = as.POSIXct(as.character(abs$TIME),tz=tz) 
#' myS<-Spectra(abs,Wavelengths=lbd,Units=Units,ShortName="a_nap")
#' myS
#' head(spc.getwavelengths(myS))
#' spc.setwavelengths(myS) <- 300:800 
#' myS[1:10]
#' myS[,"anap_400"] 
#' myS[,c("anap_400","anap_500")] 
#' myS[1:10,30:50] #Selection of channels by column index
#' lbd = as.numeric(c(412,440,490,555,670))
#' myS[1:10,lbd] #Selection of channels by wavelength
#' myS[1:10,"415::450"] 
#' myS$CAST #Returns Ancillary data
#' myS$anap_400 #Returns spectra as numeric vector
#' head(myS[["anap_400"]]) #Returns spectra as numeric vector
#' head(myS[[c("Snap","Offset")]]) #Returns data.frame
#' subset(myS,DEPTH<=30) #Subsetting rows with respect to the value of Ancillary data
#' subset(myS,anap_440<=0.01) #Subsetting rows with respect to the value of Spectral data
#' subset(myS,subset=DEPTH<=30,select="CAST") #Selecting Ancillary data columns, leaving Spectral columns intact
#' 
#' 
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
#' names of \code{SpcList} object
#'
#' @description
#' Retrieve   names of a \code{SpcList} object
#'
#' @usage 
#' names(x)
#' @param x  A \code{SpcList} object
#' 
#' @return Returns the coulmn names of an object of class \code{SpcList} as a charecter vector.
#'
#' @examples
#' sp <- spc.example_spectra()
#' BL = spc.makeSpcList(sp,"CAST")
#' names(BL)
#' 
setMethod("names", "SpcList", function(x){
			sapply(x, function(mobject) {
			  if (!is.null(x@by) & !is.na(x@by) & x@by!="VariousVariables") {
			    if(x@by %in% names(mobject))
			      mobject[[x@by]][1]
			    else
			      NULL
			  }
			  else {
			    if(class(mobject)=="Spectra") {
			      mobject@ShortName[1]
			    }
			    else{
			      class(mobject)
			    }
			  }
			})
})

#########################################################################
# Method : $
#########################################################################
#' Extract or replace parts of a \code{SpcList} object
#'
#' @description
#' Operators acting on  \code{Spectra} objects  to extract or replace parts
#' 
#' @usage 
#' x[i] 
#' x[i, j] 
#' x[[i]] 
#' x$i
#' 
#' 
#' @param \code{Spectra} object from which to extract element(s) or in which to replace element(s)
#' @param i A numeric (row index) variable
#' @param j A character (column name) or a numeric (column index) variable
#' 
#'
#' @examples
#'   sp<-spc.example_spectra()
#'   BL = spc.makeSpcList(sp,"STATION")
#'   
#'   #Extract station 394 (returns Spectra object)
#'   BL$`394`
#'   
#'   BL@by="CRUISE"
#'   BL[[1]]$CRUISE="Cruise1"
#'   BL[[2]]$CRUISE="Cruise2"
#'   BL[[3]]$CRUISE="Cruise3"
#'   BL[[4]]$CRUISE="Cruise4"
#'   names(BL)
#'   BL$Cruise4
#' 
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
#' Show a \code{SpcList} object
#'
#' @description
#' Display a \code{SpcList} object 
#'
#' @param x a \code{SpcList} object 
#' @return  show returns an invisible \code{NULL}
#'
#'
#' @examples
#' x <- spc.example_spectra()
#' BL = spc.makeSpcList(x,"CAST")
#' show(BL)
setMethod("show", "SpcList", function(object){
			if(length(object)>0)
				sapply(1:length(object), function(x) {
							if(object@by!="VariousVariables") {
								byName = paste(object@by, names(object)[x], ":")								
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
#'  \code{SpcList} class.
#' @description Definition for \code{SpcList}. This class provides
#' a collection of multiple \code{Spectra} objects inside a list.
#' @usage 
#' SpcList(x)
#' @param x a list object
#' @examples 
#' sp=spc.example_spectra()
#' as(list(sp,sp^2), "SpcList")
SpcList = function (x){
	new("SpcList", spclist)
}
#########################################################################
# Method : spc.invalid.detect
#########################################################################
#' Determinate invalid records insade of a \code{spclist} object
#' @description
#' Detect invalid records (rows) inside of a \code{spclist} object and returns logical object
#'
#' @usage 
#' spc.invalid.detect(source1)
#' 
#' @param source1  A  \code{spclist} object 
#' @examples 
#' sp=spc.example_spectra()
#' BL = spc.makeSpcList(sp,"CAST")
#' invalid=spc.getheader(BL)
#' show(invalid)
setMethod("spc.invalid.detect", signature = "list", def=function(source1){
			out = lapply(source1, function(x) {SetInvalidIdx(x)<-spc.invalid.detect(x)})
			return(out)
		})

#########################################################################
# Method : spc.getheader
#########################################################################
#' Extract a field of the @header slot of a \code{spclist} object
#' @description
#' Extracts the value of a field in the header slot of \code{spclist} object
#'
#' @usage 
#' spc.getheader(object,name)
#'
#' 
#' @param object  A  \code{spclist} object 
#' @param name of the header field to be extracted
#' 
#' @examples 
#' sp=spc.example_spectra()
#' BL = spc.makeSpcList(sp,"CAST")
#' BL[[1]]@header
#' spc.getheader(BL,"CAST")
#' 
#' 
setMethod("spc.getheader", signature = "list", def = function (object,name){
			sapply(object, spc.getheader,name)
		})

#########################################################################
# Method : spc.setheader<-
#########################################################################
#' Set a field of the @header slot of a \code{spclist} object
#' @description
#' Function sets or changes the value of a field in the header slot of \code{spclist} object
#'
#'@usage 
#' spc.setheader(x,name,...)<-value
#'
#' @param value Object of class SpcList
#' @param x A \code{SpcList} object 
#' @param name of the header field to be setted
#' @param ... arguments to be passed to or from other methods
#' @examples 
#' sp=spc.example_spectra()
#' BL=spc.makeSpcList(sp,"CAST")
#' a=new("SpcHeader") # create new SpcHeader class
#' a$Longitude=123 
#' spc.setheader(BL[[1]],"Station") <- a
#' h=spc.getheader(BL[[1]])
#' h
#' 
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
#' Update a field of the @header slot of a \code{spclist} object
#' @description
#'  Updates or changes the value of a field in the header slot of \code{spclist} object 
#'
#' @usage 
#' spc.updateheader(object,name,...)<-value
#' @param ... arguments to be passed to or from other methods 
#' @param object A \code{Spectra} objec 
#' @param name of the header field to be updated
#' @examples 
#' sp=spc.example_spectra()
#' BL=spc.makeSpcList(sp,"CAST")
#' BL[[1]]@header
#' spc.updateheader(BL[[1]],"Station")<-11
#' BL[[1]]@header
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
#' Populate fields of header slot using data from data slot 
#' @description
#' Populates a field of @header with a column data from @data slot.
#'
#' @usage 
#' spc.data2header(object,dataname,headerfield,compress,...)
#'
#' 
#' @param dataname A character object specifying the name of @data column to be used
#' @param object \code{spclist} object 
#' @param compress true or false
#' @param headerfield A character object specifying the name of the @header field to be changed
#'  
#' @return object of class \code{spclist}
#' @details 
#' This function extracts data from a column of the @data slot (specified by dataname)  
#' and creates a new @header field with it. If headerfield is not provided, the name 
#' of the new header field will be the same as dataname. 
#' The name of the new header field can be overwritten by providing headerfield.
#' If all the incoming data rows (dataname) are the same, information put into the header 
#' can be compressed by selecting compress=TRUE (default is FALSE). This would take only the first element 
#' from the @data column.
#'  @examples 
#' sp=spc.example_spectra()
#' BL=spc.makeSpcList(sp,"CAST")
#' BL[[1]]@header
#'  BL[[1]]=spc.data2header(BL[[1]],"CAST","ProjectCast")
#' BL[[1]]@header
#' BL[[1]]$CAST=rep(33, nrow( BL[[1]]))
#' BL[[1]]=spc.data2header(BL[[1]],"CAST","ProjectCast", compress=T)
#' BL[[1]]@header

setMethod("spc.data2header", signature = "list", 
		def=function(object,dataname,headerfield,compress=TRUE,...){
			temp = lapply(object, spc.data2header, dataname,headerfield,compress,...)
			object@.Data=temp
			return(object)
		})

#########################################################################
# Method : sort
#########################################################################
#' Sort a SpcList object
#' @description
#' Applies the sort() method for \code{Spectra} class to every element of a \code{SpcList} object. 
#' All the \code{Spectra} objects within the \code{SpcList} object gets sorted according to the
#' specified criteria.
#' @param x A \code{SpcList} object
#' @param which.col A character, defining the name of the column to be used in the sorting
#' @param decreasing Logical. If TRUE, then the rows are sorted in decreasing order. Passed on to the
#' sort.idx() function from the base package. Default is FALSE.
#' @param na.last for controlling the treatment of NAs. Passed on to the
#' sort.idx() function from the base package. Default is NA.
#' @examples
#' sp <- spc.example_spectra()
#' #Create an SpcList object (one separate Spectra object for each unique STATION)
#' spL <- spc.makeSpcList(sp,"STATION")
#' #Sort all Spectra objects with respect to their rows using the CAST column
#' spL.s <- sort(spL,which.col="CAST",decreasing=TRUE)
#' lapply(spL.s, function(x) as.character(x[["CAST"]]))
setMethod("sort", signature="SpcList", definition= function (x, decreasing = FALSE, na.last=NA, which.col, ...){
			newdata = lapply(x, sort, which.col=which.col, decreasing=decreasing, na.last=na.last, ...)
			x@.Data = newdata
			return(x)
		})

#########################################################################
# Method : spc.lapply
#########################################################################
#' Apply a function over a Spclist 
#' @description
#' lapply returns a list of the same length as X, each element of which is the result of applying FUN to the corresponding element of X.
#'
#' @usage 
#' spc.lapply(X, FUN, ...)
#'
#'
#' @param X A \code{spclist} object 
#' @param ...  optional arguments to FUN.
#' @return FUN  function to be applied to each element of X
#' @examples  
#' sp=spc.example_spectra()
#' BL=spc.makeSpcList(sp,"CAST")
#' #Counts rows (returns a list object)
#' spc.lapply(BL,function(x) {nrow(x)})
#' #Perform arithmetic operations on all Spectra elements. Returns a SpcList object.
#' spc.lapply(BL,function(x) {x^2+1})
setGeneric (name= "spc.lapply",
		def=function(X, FUN,...){standardGeneric("spc.lapply")})
setMethod("spc.lapply", signature="SpcList", definition= function (X, FUN, ...) {
			by = X@by
			by_names <- names(X)
			X = lapply(as(X,"list"),FUN,...)
			if (all(sapply(X, class)=="Spectra")) {
			  X = as(X, "SpcList")
			  X@by = by
			} else {
			  names(X) <- by_names
			}
			validObject(X)
			return(X)
		})

h2d = function(object,headerfield,dataname,compress=TRUE,...) {
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

#########################################################################
# Method : spc.header2data
#########################################################################
#' Get header for data
#' @description
#' Get  the header for data of each element  with a column
#'
#' @usage 
#' spc.header2data(object,headerfield,dataname,compress,...)
#'
#' 
#' @param dataname list \code{spclist} object
#' @param object A \code{spclist} object 
#' @param headerfield  data column
#' @return object of class \code{spclist}
#' @details 
#' If header element has length >1, its type is checked. If it is "character",
#' its elements will be pasted using paste(...,collapse="|"). If it is another 
#' type, only the first element will be taken.  
#' @examples 
#' sp <- spc.example_spectra()
#' BL=spc.makeSpcList(sp,"CAST")
#' spc.updateheader(BL[[1]], "Zone")<- "Zone"
#' BL[[1]] <- spc.header2data(BL[[1]], "Zone")
#' BL[[1]]$Zone
#' @name spc.header2data
setMethod("spc.header2data", signature="list", definition=
            h2d)

#' @name spc.header2data
setMethod("spc.header2data", signature="SpcList", 
		definition=function(object,headerfield,dataname,compress=TRUE,...){
			by = object@by	
			X <- h2d(object,headerfield=headerfield,dataname=dataname,compress=compress,...)
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
