# TODO: names(), colnams()
# 
# Author: acizmeli
###############################################################################

#########################################################################
# Method : Conversions from and to data.frame
#########################################################################
setAs(from="Spectra", to="Bioo", def=function(from){
			if(ncol(from@Ancillary)>0){ 
				output = from@Ancillary
				output@DF = cbind(from@DF, output@DF)
				output@Units = c(from@Units, output@Units)
				output@LongName = c(from@LongName, output@LongName)
			} else {
				output = new("Bioo",DF=from@DF,LongName=from@LongName,Units=from@Units)
			}
			output@header = from@header
			validObject(output)
			return(output)
		})

#########################################################################
# Method : Conversions from and to data.frame
#########################################################################
setAs(from="Spectra", to="data.frame", def=function(from){
			if(ncol(from@Ancillary)>0)
				output = cbind(from@DF,from@Ancillary@DF)
			
			attr(output,"ShortName") = from@ShortName
			attr(output,"LongName") = from@LongName
			attr(output,"Wavelengths") = from@Wavelengths
			attr(output,"Units") = from@Units
			return(output)
		})
setAs(from="data.frame", to="Spectra", def=function(from){
			if(any(grepl("Wavelengths", names(attributes(from)))) & 
					any(grepl("Units", names(attributes(from)))) &
					any(grepl("ShortName", names(attributes(from))))){
				
				Wavelengths= attr(from, "Wavelengths") 
				Units=attr(from,"Units") 
				ShortName = attr(from, "ShortName")
				
				if (any(grepl("LongName", names(attributes(from))))){
					LongName = attr(from, "LongName")
				} else {
					LongName = ShortName
				}
				
				if (ncol(from)>length(Wavelengths)) {
					myidx = (length(Wavelengths)+1):ncol(from)
					Ancillary = from[myidx,drop=F]
					Ancillary = new("Bioo", DF=Ancillary, Units=Units[myidx])
				} else {
					Ancillary = new("Bioo") #data.frame()
				}
				outS = new("Spectra",
						DF=from[,1:length(Wavelengths)],
						Ancillary=Ancillary,
						Wavelengths=Wavelengths, Units=Units[1:length(Wavelengths)], 
						LongName = LongName, ShortName = ShortName)
			} else {
				stop("One of the required attributes are not found : Wavelengths, Units or ShortName")
			}
			validObject(outS)
			return(outS)
		})

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
			
			cat("\n", paste(object@ShortName[1], ' : An object of class "Spectra"\n', 
							length(object@Wavelengths),"spectral channels in columns and", nrow(object@DF), 
							"observations in rows"), "\n",
					"LongName : ", LongName, "\n",
					"Wavelengths : ", length(object@Wavelengths), "channels", LbdStr, head(object@Wavelengths)," ...\n",
					"Units : ", Units, "\n",
					"Columns : ", head(colnames(object@DF)), "...\n",
					"Ancillary : ", head(names(object@Ancillary)),"...\n")
		})		

#########################################################################
# Method : names
#########################################################################
setMethod("names", signature = "Spectra", 
		def = function (x){ return(names(x@DF)) })
setReplaceMethod("names", signature = "Spectra", def = function (x,value){
			colnames(x@DF) = value
			validObject(x)
			return(x) 
		})
#########################################################################
# Method : dim
#########################################################################
setMethod("dim", signature = "Spectra", 
		def = function (x){  return(dim(x@DF))  })
#########################################################################
# Method : ncol
#########################################################################
setMethod("ncol", signature = "Spectra", 
		def = function (x){  return(ncol(x@DF))  })
########################################################################
# Method : nrow
#########################################################################
setMethod("nrow", signature = "Spectra", 
		def = function (x){  return(nrow(x@DF))  })
#########################################################################
# Method : head
#########################################################################
setMethod("head", signature = "Spectra", 
		def = function (x){  return(head(x@DF)) })

#########################################################################
# Method : spc.rbind
#########################################################################
setMethod("spc.rbind", signature = "Spectra", def = function (...){
			DFL=sapply(list(...),function(x) names(x),simplify=F)
			#Check that column names match
			if(!all(sapply(1:length(DFL),function(x) all(DFL[[x]]==DFL[[1]]))))
				stop("Names of all columns should be the same")

			DFL=sapply(list(...),ncol)
			#Check that the number of columns match 
			if(!all(sapply(1:length(DFL),function(x) all(DFL[[x]]==DFL[[1]]))))
				stop("All input Spectra objects should have the same number of columns")
			
			DFL=sapply(list(...),spc.getwavelengths)
			#Check that all Wavelengths are equal
			if(!all(apply(DFL,1,diff)==0))
				stop("Wavelengths of all input Spectra objects should be the same")

			outt = ..1
			outt@DF = rbind(..1@DF,..2@DF)
			#TO BE USED LATER : match.call(expand.dots = F)$...
			
			outt@Ancillary = spc.rbind(..1@Ancillary,..2@Ancillary)
			outt@SelectedIdx = logical()
			outt@InvalidIdx = logical()
			validObject(outt)
			return(outt) 
		})

#########################################################################
# Method : Arith
#########################################################################
setMethod("Arith", signature(e1 = "Spectra", e2 = "Spectra"),function (e1, e2) {
			result <- callGeneric(e1@DF, e2@DF)
			output <- new("Spectra",DF=result,Wavelengths=e1@Wavelengths,Units=e1@Units,
					ShortName = "Arith", LongName="Arith",Ancillary=spc.getancillary(e1))			
			return(output)
		})

#########################################################################
# Method : [[
#########################################################################
setMethod("[[", signature="Spectra",
		function(x, i, j, ...) {
			if (i %in% names(x)){
				Boutput = x@DF[[i]]
			} 
			if (i %in% names(x@Ancillary)){
				Boutput = x@Ancillary@DF[[i]]				
			}
			if(!exists("Boutput"))
				stop("Could not match any Spectral or Ancillary data columns")
			validObject(x)
			return(Boutput)
		})
setReplaceMethod("[[",  signature="Spectra", definition=function(x, i, j, value) {
			matched = 0
			if (length(value)!=nrow(x))
				stop("Replace value must have the same number or rows as the input object")
			if (i %in% names(x@DF)){
				matched = 1
				x@DF[[i]] <- value
			} 
			if (i %in% names(x@Ancillary)) {
				matched = 1
				x@Ancillary@DF[[i]] <- value				
			}
			if(!matched)
				stop("Could not match any Spectral or Ancillary data columns")
			validObject(x)
			return(x)
		})
#########################################################################
# Method : $
#########################################################################
setMethod("$", signature="Spectra",
		function(x, name) {
			if (name %in% names(x)){
				Boutput = x@DF[[name]]
			} 
			if (name %in% names(x@Ancillary)){
				Boutput = x@Ancillary@DF[[name]]				
			}
			if(!exists("Boutput"))
				stop("Could not match any Spectral or Ancillary data columns")
			return(Boutput)
		})
setReplaceMethod("$", signature = "Spectra", 
		function(x, name, value) {
			x[[name]]=value
			#validObject(x) will be called by the [[ method
			return(x)
		})
#########################################################################
# Method : [
#########################################################################
setMethod("[",
		signature(x = "Spectra"),
		function(x, i, j) {
			OUT_ANC = 0
			if(missing(i))
				i =  1:nrow(x@DF)
			if(missing(j))
				j =  1:ncol(x@DF)
			
			if (class(j)=="numeric" | class(j)=="character"){
				if (class(j)=="numeric"){
					j.new = match(j,x@Wavelengths)
				}
				if (class(j)=="character"){
					if (!exists("j.new") & any(match(j, names(x),nomatch=F))) {
						j.new = match(j, names(x))
					}
					if (!exists("j.new") & any(match(j, names(x@Ancillary),nomatch=F))) {
						OUT_ANC = 1
						j.new = match(j, names(x@Ancillary))						
					}
					if (!exists("j.new") && length(j)==1 && grepl("::",j)) {					
						#The requested input is in format lbd1::lbd2
						temp = strsplit(j, "::")
						mylower = as.numeric(temp[[1]][1])
						myupper = as.numeric(temp[[1]][2])					
						j.new = which(x@Wavelengths>=mylower & x@Wavelengths<=myupper)
					}
					if (!exists("j.new"))
						stop("Could not recognize the wavelength selection format. Use the operator :: or provide spectra or ancillary data column indexes or names")
					
				}			
				if (all(is.na(j.new)))
					stop("Could not find matching wavelengths or ancillary data")
				if (any(na.idx <-(is.na(j.new)))) {
					j.new=j.new[!is.na(j.new)]
					warning(paste("Could not match wavelengths or ancillary data :", j[which(na.idx)]))
				}
				if (!all(is.finite(j.new)))
					stop("Could not find matching wavelengths or ancillary data")
				j = j.new
			}
			InvalidIdx = x@InvalidIdx
			if (!OUT_ANC) {				
				x@DF=x@DF[i,j,drop=F]
				if(nrow(x@Ancillary)>0)
					x@Ancillary=x@Ancillary[i,,drop=F]
				x@Wavelengths = x@Wavelengths[j]
				x@LongName= x@LongName[j]
				x@Units= x@Units[j] 
			} else{
				x=x@Ancillary
				x = x[i,j,drop=F]				
			}
			if (length(x@InvalidIdx)>1)
				x@InvalidIdx = x@InvalidIdx[i] 
			
			x@SelectedIdx = logical()			
			return(x)
		})
#########################################################################
# Method : names
#########################################################################
#setMethod("names", signature = "Spectra", 
#		def = function (x){ 
#			if(ncol(x@Ancillary)>1)
#				return(c(names(x@DF),names(x@Ancillary)))
#			else                
#				return(names(x@DF)) 
#		})
#########################################################################
# Method : head
#########################################################################
#setMethod("head", signature = "Spectra", 
#		def = function (x){
#			if(ncol(x@Ancillary)>1)
#				return(head(cbind(x@DF, x@Ancillary@DF)))
#			else
#				return(head(x@DF))
#		})

#########################################################################
# Method : plot
#########################################################################
setMethod("plot", "Spectra", function (x, Y, maxSp, ...){						
			if (length(x@InvalidIdx)==0)
				x@InvalidIdx = rep(FALSE,nrow(x@DF))
			
			if(!missing(maxSp) && ncol(x)>maxSp)
				idx = seq(1,nrow(x),length.out=maxSp	)
			else
				idx = 1:nrow(x)
			
			Xidx = rep(FALSE, nrow(x@DF))
			Xidx[idx] = TRUE
			
			if(any(x@InvalidIdx)){
				Xidx[x@InvalidIdx]=FALSE
			}
			if(any(x@SelectedIdx)){
				mycol = rep("gray", nrow(x@DF))
				mycol[x@SelectedIdx]="red"
			} else
				mycol = 1:6
			x@Units = gsub("\\[\\]","",x@Units)
			x@Units = gsub("\\[ \\]","",x@Units)
			if(any(grepl("col",names(match.call())))) {
				matplot(x@Wavelengths,t(x@DF[Xidx,]),#lab=x@Wavelengths,#xaxt="n",
						ylab= paste(x@LongName[1], " [", x@Units[1], "]",sep=""),
						xlab="Wavelength [nm]", type="l", pch=19,cex=0.3, ...)
			} else {
				matplot(x@Wavelengths,t(x@DF[Xidx,]),#lab=x@Wavelengths,#xaxt="n",
						ylab= paste(x@LongName[1], " [", x@Units[1], "]",sep=""),
						xlab="Wavelength [nm]", type="l", pch=19,cex=0.3, col=mycol, ...)
				
			}
			abline(h=0)
			grid(col="black")
		})
#########################################################################
# Method : lines
#########################################################################
setMethod("lines",signature = "Spectra",
		definition = function(x,...){
			a=sapply(1:nrow(x@DF), function(S) {
						lines(x@Wavelengths, x@DF[S,],...)})
		})

#########################################################################
# Method : spc.getancillary
#########################################################################
setGeneric (name= "spc.getancillary",
		def=function(object, ...){standardGeneric("spc.getancillary")})
setMethod("spc.getancillary", signature = "Spectra", 
		def = function (object, Columns){
			if (missing(Columns))
				return(object@Ancillary)
			else 
				return(object@Ancillary[,Columns])
		})
#########################################################################
# Method : SetAncillary
#########################################################################
setGeneric("spc.setancillary<-",function(object,value)
		{standardGeneric("spc.setancillary<-")})
setReplaceMethod(f="spc.setancillary",	signature="Spectra",
		definition=function(object,value){
			if(class(value)=="data.frame")
				value = as(value,"Bioo")
			object@Ancillary <-value
			validObject(object)
			return (object)
		})

#########################################################################
# Method : Getwavelengths
#########################################################################
setGeneric (name= "spc.getwavelengths",
		def=function(object){standardGeneric("spc.getwavelengths")})
setMethod("spc.getwavelengths", signature = "Spectra", 
		def = function (object){
			return(object@Wavelengths)
		})
#########################################################################
# Method : SetWavelengths
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
			return(paste(value,spc.getwavelengths(object),sep="_"))
		})

#########################################################################
# Method : spc.select Select Spectra with the help of the mouse
#########################################################################
mat_identify <- function(x, y, ...){
	l <- locator(1)
	if(all(x <= l$x) || all(x >= l$x)){
		result=NULL
	} else {
		index <- max(which(x <= l$x))
		f <- (l$x - x[index]) / diff(x[index+(0:1)])
		
		yi <- f * (y[index+1,] - y[index,] ) + y[index,]
		result <- which.min(abs(yi-l$y))
		lines(x, y[,result], lwd=2, col="red")
	}
#  text(l, label=colnames(y)[result])
	return(result)
}
setGeneric (name= "spc.select",
		def=function(object){standardGeneric("spc.select")})
setMethod("spc.select", signature = "Spectra", 
		def = function (object){
			#Extract the existing selection Index
			if(length(object@SelectedIdx)>0)
				ExSel = object@SelectedIdx
			else
				ExSel = rep(FALSE, nrow(object@DF))			
			Sel = rep(FALSE, nrow(object@DF))			
			
			plot(object,col="gray")
			lbd = GetWavelengths(object)
			idx = mat_identify(lbd, t(object@DF))
			print(paste("Selected row",idx))
			oidx = idx
			while(!is.null(idx)){
				idx = mat_identify(lbd, t(object@DF))
				print(paste("Selected row",idx))
				oidx=c(oidx, idx)				
			}
			oidx = oidx[!is.null(oidx)]
			Sel[oidx]=T
			
			TrueIdx = isTRUE(ExSel)
			#Apply the XOR operator to Existing Index ExSel 
			#(selecting again an already selected row unselcts it)
			ExSel = xor(ExSel,Sel)
			
			#Update the slot SelectedIdx 
#			object@SelectedIdx = ExSel
			
			#print(cbind(Sel, ExSel))
			return(ExSel)
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
				try(xidx <- eval(mycall, x@DF, parent.frame()),silent=T)
				try(xidx <- eval(mycall, x@Ancillary@DF, parent.frame()),silent=T)		
				if (!is.logical(xidx)) 
					stop("'subset' must evaluate to logical")				
				xidx <- xidx & !is.na(xidx)
				if (length(x@SelectedIdx)>0)
					x@SelectedIdx = x@SelectedIdx[xidx]
				if (length(x@InvalidIdx)>0)
					x@InvalidIdx = x@InvalidIdx[xidx]
				if (nrow(x@Ancillary)>0)
					x@Ancillary@DF = x@Ancillary@DF[xidx,]				
			}	
			if (missing(select)) 
				vars <- TRUE
			else {
				nl <- as.list(seq_along(x@DF))
				names(nl) <- names(x@DF)

				vars <- eval(substitute(select), nl, parent.frame())
				if(!(vars %in% names(x@DF)))
					stop(paste("The select variables", vars, "is not a spectral column name"))
				
				y_idx = as.integer(nl[vars])
				x@Units = x@Units[y_idx]
				x@LongName = x@LongName[y_idx]
				x@Wavelengths = x@Wavelengths[y_idx]
			}
			x@DF = x@DF[xidx, vars, drop = drop]
			
			validObject(x)
			return(x)
		})

#########################################################################
# Method : bioo.add.column
#########################################################################
setMethod("bioo.add.column", signature="Spectra", definition= function (object, name, value, units,longname) {
			A = bioo.add.column(object@Ancillary,name=name,value=value,units=units,longname=longname)
			object@Ancillary = A 
			validObject(object)
			return(object)
		})

#########################################################################
# Method : spc.add.channel
#########################################################################
setGeneric(name= "spc.add.channel",
		def=function(object, name, value, units,wavelengths){standardGeneric("spc.add.channel")})
setMethod("spc.add.channel", signature="Spectra", definition= function (object, name, value, wavelengths) {
			if(missing(wavelengths))
				stop("Missing the required input variable : wavelengths")
			
			if (name %in% names(object)){
				stop(paste("The channel", name, "already exists. Consider using the methods $, [ or [["))
			} 		  
			if(!is.data.frame(value)){
				value = data.frame(value)
				names(value) = name
			}
			
			if (missing(units) && length(unique(object@Units))==1)
				units = object@Units[1]
			
			if (length(units)!= ncol(value))
				stop(paste('The input variable "units" should have the same lengths as the number of columns of "value"'))
			
			object@DF = cbind(object@DF,value)
			object@Units = c(object@Units, units)
			
			validObject(object)
			return(object)
		})

#########################################################################
# Method : bioo.setheader
#########################################################################
setReplaceMethod(f="bioo.setheader", signature="Spectra",
		definition=function(object,value,...){
			object@header<-value
			validObject(object)
			return(object)
		})