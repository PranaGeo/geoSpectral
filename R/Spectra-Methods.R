# TODO: names(), colnams()
# 
# Author: acizmeli
###############################################################################

#########################################################################
# Method : Conversions from and to data.frame
#########################################################################
setAs(from="Spectra", to="data.frame", def=function(from){
			if(any(grepl("Wavelengths", names(attributes(from)))) & 
					any(grepl("Units", names(attributes(from)))))
				output = cbind(from@DF,from@Ancillary)
			
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
				
				if (any(grepl("LongName", names(attributes(from)))))
					LongName = attr(from, "LongName")
				else
					LongName = ShortName
				
				if(ncol(from)>length(Wavelengths))
					Ancillary = from[(length(Wavelengths)+1):ncol(from),drop=F]
				else
					Ancillary = data.frame()
				
				outS = new("Spectra",
						DF=from[,1:length(Wavelengths)],
						Ancillary=Ancillary,
						Wavelengths=Wavelengths, Units=Units[1:length(Wavelengths)], 
						LongName = LongName, ShortName = ShortName)
			} else {
				stop("One of the required attributes are not found : Wavelengths, Units or ShortName")
			}
			return(outS)
		})

#########################################################################
# Method : show
#########################################################################
setMethod("show", "Spectra", function(object){
			cat("\n", paste(object@ShortName, ' : An object of class "Spectra"\n', 
							length(object@Wavelengths),"spectral channels in columns and", nrow(object@DF), 
							"observations in rows"), "\n",
					"LongName : ",object@LongName, "\n",
					"Wavelengths : ", length(object@Wavelengths), 
					"channels [",min(object@Wavelengths),",",max(object@Wavelengths), "] ->",
					head(object@Wavelengths)," ...\n",
					"Units : ", object@Units[1], "\n",
					"Columns : ", head(colnames(object@DF)), "...\n",
					"Ancillary : ", colnames(object@Ancillary),"...\n")
		})

setMethod("Arith",
		signature(e1 = "Spectra", e2 = "Spectra"),
		function (e1, e2) {
			result <- callGeneric(e1@DF, e2@DF)
			output <- new("Spectra",DF=result,Wavelengths=e1@Wavelengths,Units=e1@Units,
					ShortName = "Arith", LongName="Arith")
			
			return(output)
		}
)
#########################################################################
# Method : names
#########################################################################
setMethod("names", signature = "Spectra", 
		def = function (x){
			return(names(x@DF))
		})
#########################################################################
# Method : dim
#########################################################################
setMethod("dim", signature = "Spectra", 
		def = function (x){
			return(dim(x@DF))
		})
#########################################################################
# Method : ncol
#########################################################################
setMethod("ncol", signature = "Spectra", 
		def = function (x){
			return(ncol(x@DF))
		})
#########################################################################
# Method : nrow
#########################################################################
setMethod("nrow", signature = "Spectra", 
		def = function (x){
			return(nrow(x@DF))
		})

setMethod("[",
		signature(x = "Spectra"),
		function(x, i, j) {
			if(missing(j))
				j =  seq_along(x@Wavelengths)
			
			if (class(j)=="numeric" | class(j)=="character"){				
				if (class(j)=="numeric"){
					j.new = match(j,x@Wavelengths)
				}
				if (class(j)=="character"){
					if(!grepl("::",j))
						stop("Could not recognize the wavelength selectio format. Use the operator ::")
					temp = strsplit(j, "::")
					mylower = as.numeric(temp[[1]][1])
					myupper = as.numeric(temp[[1]][2])					
					j.new = which(x@Wavelengths>=mylower & x@Wavelengths<=myupper)
				}
				
				if (all(is.na(j.new)))
					stop("Could not find matching wavelengths")
				if (any(na.idx <-(is.na(j.new)))) {
					j.new=j.new[!is.na(j.new)]
					warning(paste("Could not match wavelengths :", j[which(na.idx)]))
				}
				if (!all(is.finite(j.new)))
					stop("Could not find matching wavelengths")
				j = j.new
			}
			
			x@DF=x@DF[i,j,drop=F]
			x@Ancillary=x@Ancillary[i,,drop=F]
			x@Wavelengths = x@Wavelengths[j] 
			x@SelectedIdx = logical()
			
			if (length(x@InvalidIdx)>1)
				x@InvalidIdx = x@InvalidIdx[i] 
			return(x)
		})

#########################################################################
# Method : head
#########################################################################
setMethod("head", signature = "Spectra", 
		def = function (x){
			return(head(cbind(x@DF, x@Ancillary)))
		})

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
			
			matplot(x@Wavelengths,t(x@DF[Xidx,]),#lab=x@Wavelengths,#xaxt="n",
					ylab= paste(x@LongName, "[", x@Units[1], "]"),
					xlab="Wavelength [nm]", type="l", pch=19,cex=0.3, col=mycol, ...)
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
# Method : spc.GetAncillary
#########################################################################
setGeneric (name= "spc.GetAncillary",
		def=function(object, ...){standardGeneric("spc.GetAncillary")})
setMethod("spc.GetAncillary", signature = "Spectra", 
		def = function (object, Columns){
			if (missing(Columns))
				return(object@Ancillary)
			else 
				return(object@Ancillary[,Columns])
		})
#########################################################################
# Method : spc.SetAncillary
#########################################################################
setGeneric("spc.SetAncillary<-",function(object,value)
		{standardGeneric("spc.SetAncillary<-")})
setReplaceMethod(
		f="spc.SetAncillary",
		signature="Spectra",
		definition=function(object,value){
			object@Ancillary <-value
			validObject(object)
			return (object)
		})

#########################################################################
# Method : spc.Getwavelengths
#########################################################################
setGeneric (name= "spc.GetWavelengths",
		def=function(object){standardGeneric("spc.GetWavelengths")})
setMethod("spc.GetWavelengths", signature = "Spectra", 
		def = function (object){
			return(object@Wavelengths)
		})
#########################################################################
# Method : spc.SetWavelengths
#########################################################################
setGeneric("spc.SetWavelengths<-",function(object,value)
		{standardGeneric("spc.SetWavelengths<-")})
setReplaceMethod(
		f="spc.SetWavelengths",
		signature="Spectra",
		definition=function(object,value){
			object@Wavelengths <-value
			validObject(object)
			return (object)
		})

#########################################################################
# Method : spc.GetSelectedIdx
#########################################################################
setGeneric (name= "spc.GetSelectedIdx",
		def=function(object){standardGeneric("spc.GetSelectedIdx")})
setMethod("spc.GetSelectedIdx", signature = "Spectra", 
		def = function (object){
			return(object@SelectedIdx)
		})
#########################################################################
# Method : spc.SetSelectedIdx	
#########################################################################
setGeneric("spc.SetSelectedIdx<-",function(object,value)
		{standardGeneric("spc.SetSelectedIdx<-")})
setReplaceMethod(
		f="spc.SetSelectedIdx",
		signature="Spectra",
		definition=function(object,value){
			if(is.numeric(value)){
				idx = spc.GetInvalidIdx(object)
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
# Method : spc.GetinvalidIdx
#########################################################################
setGeneric (name= "spc.GetInvalidIdx",
		def=function(object){standardGeneric("spc.GetInvalidIdx")})
setMethod("spc.GetInvalidIdx", signature = "Spectra", 
		def = function (object){
			return(object@InvalidIdx)
		})
#########################################################################
# Method : spc.SetInvaliddidx	
#########################################################################
setGeneric("spc.SetInvalidIdx<-",function(object,value)
		{standardGeneric("spc.SetInvalidIdx<-")})
setReplaceMethod(
		f="spc.SetInvalidIdx",
		signature="Spectra",
		definition=function(object,value){
			if(is.numeric(value)){
				idx = spc.GetInvalidIdx(object)
				if(length(idx)==0)
					idx = rep(FALSE,nrow(object))
				idx[value]=TRUE
				value=idx
			}
			object@InvalidIdx<-value
			validObject(object)
			return (object)
		})

##########################################################################
## Method : spc.SelectionInvalidDo Make the selected spectra Invalid
##########################################################################
#setGeneric (name= "spc.SelectionInvalidDo",
#		def=function(object){standardGeneric("spc.SelectionInvalidDo")})
#setMethod("spc.SelectionInvalidDo", signature = "Spectra", 
#		def = function (object){
#			object@InvalidIdx <- object@SelectedIdx
#			object@SelectedIdx = logical()
#			return(object)
#		})

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
		#	browser()
		
		yi <- f * (y[index+1,] - y[index,] ) + y[index,]
		result <- which.min(abs(yi-l$y))
		lines(x, y[,result], lwd=2, col="red")
	}
#	browser()
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
			lbd = spc.GetWavelengths(object)
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
# Method : spc.plot.time
#########################################################################
setGeneric (name= "spc.plot.time",
		def=function(object, ...){standardGeneric("spc.plot.time")})
setMethod("spc.plot.time", "Spectra", function (object,Y,maxSp, ...){
			idx = !is.na(object@Wavelengths)
			
			if (length(object@InvalidIdx)==0)
				object@InvalidIdx = rep(FALSE,nrow(object@DF))		

			x = 1:nrow(object)
			xlb = "Observation number"
			ylb = paste(object@LongName, object@Units[1])
			
			if(missing(Y)){
				if(!missing(maxSp) && ncol(object)>maxSp)
					Y = seq(1,ncol(object),length.out=maxSp)
				else
					Y = names(object)
			}
			matplot(x[!object@InvalidIdx], 
					object@DF[!object@InvalidIdx,Y], type="l", pch=19,cex=0.3,
					xlab=xlb,ylab=ylb,...)
			grid(col="black")			
		})

#removeClass("TrajectoriesBis")
#slotNames gives the name of the slots as a vector of type character.
#getSlots gives the name of the slots and their type.
#getClass gives the names of slots and their type, but also heirs and ancestors. 
#showMethods
