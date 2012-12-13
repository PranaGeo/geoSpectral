# TODO: Add comment
# 
# Author: acizmeli
###############################################################################
	
#########################################################################
# Method : Conversion from BiooList to Bioo
#########################################################################
setAs(from="BiooList", to="Bioo", def=function(from){		
	dims = t(sapply(from,dim))[,2]
	if(!all(dims==dims[1]))
		stop("All BiooList elements should have the same number of rows (including Ancillary data for Spectra objects)")
	nms = sapply(from,names)
	if(!all(sapply(1:nrow(nms), function(x) all(nms[x,1]==nms[x,]))))
		stop("All BiooList elements should have the same names (including Ancillary data for Spectra objects)")
	
	if (all(sapply(from,class)=="Spectra") |(all(sapply(from,class)=="Bioo"))) {
		if (all(sapply(from,class)=="Spectra")){
			DF = do.call(rbind, lapply(from,function(x) x@DF))
			DFAnc = do.call(rbind, lapply(from,function(x) x@Ancillary@DF))
			Anc = new("Bioo",DF=DFAnc,Units=from[[1]]@Ancillary@Units)
			output = new("Spectra",DF=DF,Units=from[[1]]@Units,Wavelengths=from[[1]]@Wavelengths,
					ShortName=from[[1]]@ShortName,LongName=from[[1]]@LongName,Ancillary=Anc)
		}
		if (all(sapply(from,class)=="Bioo")){
			DF = do.call(rbind, lapply(from,function(x) x@DF))
			output = new("Bioo",DF=DF,Units=from[[1]]@Ancillary@Units)
		}
	} else {
		stop("All BiooList elements should be of class 'Spectra' of 'Bioo'")
	}
	return(output)
})

#########################################################################
# Method : spc.plot.grid
#########################################################################
setGeneric (name= "spc.plot.grid",
		def=function(x,FUN, nnrow, nncol,...){standardGeneric("spc.plot.grid")})
setMethod("spc.plot.grid", "BiooList", function (x,FUN, nnrow, nncol, mar=c(4,4.5,1,0.5), 
				oma = c(0,0,0,0), lab_cex, ...){
			nb_spc = length(which(sapply(x, inherits, "Bioo")))
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
						#tit = paste(x@by, ":", as.character(bioo.getheader(x[[I]],x@by)))
						tit = paste(as.character(bioo.getheader(x[[I]],x@by)))
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
setMethod("spc.plot.overlay", "BiooList", function (object, lab_cex, ...){
			if(missing(lab_cex))
				lab_cex = 1
			all_x = unlist(lapply(object,function(t) t@Wavelengths))
			all_y = unlist(lapply(object,function(t) t@DF))
			browser()

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
					#tit[I] = paste(object@by, ":", as.character(bioo.getheader(object[[I]],object@by)))
					tit[I] = paste(as.character(bioo.getheader(object[[I]],object@by)))
				}
				else{
					if(all(!nms))
						tit[I]=names(object)[I]					
					else
						tit[I]=as.character(I)#paste(object[[I]]@ShortName)
				}
				if(I==1)
					eval_txt = paste("spc.plot", "(object[[I]],lab_cex=lab_cex,col=I,...)",sep="")
				else
					eval_txt = paste("spc.lines", "(object[[I]],lab_cex=lab_cex,col=I,...)",sep="")
				if (!any(grepl("xlim",names(match.call()))))
					eval_txt = gsub("object\\[\\[I\\]\\],","object\\[\\[I\\]\\],xlim=xlim,",eval_txt)
				if (!any(grepl("ylim",names(match.call()))))
					eval_txt = gsub("object\\[\\[I\\]\\],","object\\[\\[I\\]\\],ylim=ylim,",eval_txt)
				
				eval(parse(text=eval_txt))				
				#title(main=tit,mgp=c(2,1,0))
			}#end for
			legend("bottomright",tit,col=1:I,fill=1:I)			
		})

#########################################################################
setGeneric (name= "spc.plot.depth.overlay",
		def=function(object,X,...){standardGeneric("spc.plot.depth.overlay")})
setMethod("spc.plot.depth.overlay", "BiooList", function (object, X, lab_cex, ...){
			if(missing(lab_cex))
				lab_cex = 1
#			browser()
			all_y = unlist(lapply(object,function(t) t$DEPTH))
			all_x = unlist(lapply(object,function(t) t@DF[X]))
			xlim = range(all_x)
			ylim = range(all_y)
			tit=""
			for (I in 1:length(object)) {
				if(object@by!="VariousVariables"){
					#tit[I] = paste(object@by, ":", as.character(bioo.getheader(object[[I]],object@by)))
					tit[I] = paste(as.character(bioo.getheader(object[[I]],object@by)))
				}
				else{
					tit[I]=as.character(I)#paste(object[[I]]@ShortName)
				}
				if(I==1)
					eval_txt = paste("plot.depth","(object[[I]],X,lab_cex=lab_cex,xlim=xlim,col=I,...)",sep="")
				else
					eval_txt =  paste("plot.depth","(object[[I]],X,add=T,lab_cex=lab_cex,xlim=xlim,col=I,...)",sep="")
				eval(parse(text=eval_txt))				
				#title(main=tit,mgp=c(2,1,0))
			}#end for
			legend("bottomright",tit,col=1:I,fill=1:I)
			
		})
#########################################################################
# Method : subset
#########################################################################
#The argument "select" is not implemented yet. Use "[]"
setMethod("subset",  signature="BiooList",
		definition=function(x, subset, select, drop = FALSE, ...) {                   
			
			if(class(subset)=="list") { 
				if(length(subset)>1 & length(subset)!=length(x))
					stop('The argument "subset" should be a list of length one or the same length of the BiooList object')          
				
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
#setMethod("Arith",signature(e1 = "BiooList", e2 = "BiooList"),function (e1, e2) {
#			browser()
#			if(length(e1)!=length(e2))
#				stop("Lengths of input BiooList object should match")
#			
#			result <- callGeneric(e1[[1]], e2@DF[[1]])
#			output <- new("Spectra",DF=result,Wavelengths=e1@Wavelengths,Units=e1@Units,
#					ShortName = "Arith", LongName="Arith")			
#			return(output)
#		})
#
#lapply(1:length(e1), function(x){
#			browser()
#			callGeneric(e1[[x]]@DF, e2[[x]]@DF)
#		})

#########################################################################
# Method : names
#########################################################################
setMethod("names", "BiooList", function(x){
			sapply(x, function(mobject) {
						if(class(mobject)=="Bioo") "Bioo"	else mobject@ShortName[1]
					})
		})

#########################################################################
# Method : $
#########################################################################
setMethod("$", signature = "BiooList", 
		function(x, name) {
			myn = names(x)
			if(any(grepl(name,myn)))
				x[[grep(name,myn)[1]]]
			else stop("Could not match any object name")
		})

#########################################################################
# Method : show
#########################################################################
setMethod("show", "BiooList", function(object){
			if(length(object)>0)
				sapply(1:length(object), function(x) {
							cat(paste("Element", x, ":"))
							show(object[[x]])
						})
			else cat("Empty BiooList\n")
		})

#########################################################################
# Constructor function : BiooList()
#########################################################################
BiooList = function (spclist){
	new("BiooList", spclist)
}
#########################################################################
# Method : bioo.invalid.detect
#########################################################################
setMethod("bioo.invalid.detect", signature = "BiooList", def=function(source1){
			out = lapply(source1, function(x) {SetInvalidIdx(x)<-biooInvalidDetect(x)})
			return(out)
		})

#########################################################################
# Method : bioo.getheader
#########################################################################
setMethod("bioo.getheader", signature = "BiooList", 
		def = function (object,name){
			sapply(object, bioo.getheader,name)
		})

#########################################################################
# Method : bioo.setheader<-
#########################################################################
setReplaceMethod(f="bioo.setheader", signature="BiooList",
		definition=function(object,value,...){
			if(inherits(value,"Bioo"))
				stop("It is forbidden to place in a BiooHeader object that inherit from the Bioo class")
			
			if(length(value)==1)
				value = rep(value,length(object))
			
			a=sapply(1:length(object), function(x) {
						object[[x]] = bioo.setheader(object[[x]],value[x])
					})
			
			validObject(object)
			return(object)
		})

#########################################################################
# Method : bioo.data2header
#########################################################################
setMethod("bioo.data2header", signature = "BiooList", 
		def=function(object,headerfield,dataname,compress=TRUE,...){
			temp = lapply(object, bioo.data2header, headerfield,dataname,compress,...)
			object@.Data=temp
			return(object)
		})
#########################################################################
# Method : sort
#########################################################################
setMethod("sort", signature="BiooList", definition= function (x, which.col, decreasing = FALSE, ...){
		newdata = lapply(x, sort, which.col=which.col, decreasing=decreasing, ...)
		x@.Data = newdata
		return(x)
		})

#########################################################################
# Method : lapply
#########################################################################
setGeneric (name= "spc.lapply",
		def=function(X, FUN,...){standardGeneric("spc.lapply")})
setMethod("spc.lapply", signature="BiooList", definition= function (X, FUN, ...) {
			by = X@by
			X = lapply(as(X,"list"),FUN,...)
			X = as(X, "BiooList")
			X@by = by
			validObject(X)
			return(X)
		})
			
	
#########################################################################
# Method : subset
#########################################################################
#setMethod("subset",  signature="BiooList",
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
#			x = as(x, "BiooList")
#			x@by = myby
#		})