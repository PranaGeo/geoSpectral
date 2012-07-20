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
				
				if (any(grepl("LongName", names(attributes(from))))){
					LongName = attr(from, "LongName")
				} else {
					LongName = ShortName
				}
				if (ncol(from)>length(Wavelengths)) {
					myidx = (length(Wavelengths)+1):ncol(from)
					Ancillary = from[myidx,drop=F]
					Ancillary = new("Biooo", DF=Ancillary, Units=Units[myidx])
				} else {
					Ancillary = new("Biooo") #data.frame()
				}
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
			cat("\n", paste(object@ShortName[1], ' : An object of class "Spectra"\n', 
							length(object@Wavelengths),"spectral channels in columns and", nrow(object@DF), 
							"observations in rows"), "\n",
					"LongName : ",object@LongName[1], "\n",
					"Wavelengths : ", length(object@Wavelengths), 
					"channels [",min(object@Wavelengths),",",max(object@Wavelengths), "] ->",
					head(object@Wavelengths)," ...\n",
					"Units : ", object@Units[1], "\n",
					"Columns : ", head(colnames(object@DF)), "...\n",
					"Ancillary : ", head(names(object@Ancillary)),"...\n")
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
#setMethod("names", signature = "Spectra", 
#		def = function (x){
#			return(names(x@DF))
#		})
#########################################################################
# Method : dim
#########################################################################
#setMethod("dim", signature = "Spectra", 
#		def = function (x){
#			return(dim(x@DF))
#		})
#########################################################################
# Method : ncol
#########################################################################
#setMethod("ncol", signature = "Spectra", 
#		def = function (x){
#			return(ncol(x@DF))
#		})
#########################################################################
# Method : nrow
#########################################################################
#setMethod("nrow", signature = "Spectra", 
#		def = function (x){
#			return(nrow(x@DF))
#		})

#########################################################################
# Method : [
#########################################################################
setMethod("[",
		signature(x = "Spectra"),
		function(x, i, j) {
			if(missing(i))
				i =  1:nrow(x@DF)
			if(missing(j))
				j =  1:ncol(x@DF)
			
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
			if(ncol(x@Ancillary)>1)
				return(head(cbind(x@DF, x@Ancillary@DF)))
			else
				return(head(x@DF))
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
					ylab= paste(x@LongName[1], "[", x@Units[1], "]"),
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
# Method : GetAncillary
#########################################################################
setGeneric (name= "GetAncillary",
		def=function(object, ...){standardGeneric("GetAncillary")})
setMethod("GetAncillary", signature = "Spectra", 
		def = function (object, Columns){
			if (missing(Columns))
				return(object@Ancillary)
			else 
				return(object@Ancillary[,Columns])
		})
#########################################################################
# Method : SetAncillary
#########################################################################
setGeneric("SetAncillary<-",function(object,value)
		{standardGeneric("SetAncillary<-")})
setReplaceMethod(
		f="SetAncillary",
		signature="Spectra",
		definition=function(object,value){
			if(class(value)=="data.frame")
				value = as(value,"Biooo")
			object@Ancillary <-value
			validObject(object)
			return (object)
		})

#########################################################################
# Method : Getwavelengths
#########################################################################
setGeneric (name= "GetWavelengths",
		def=function(object){standardGeneric("GetWavelengths")})
setMethod("GetWavelengths", signature = "Spectra", 
		def = function (object){
			return(object@Wavelengths)
		})
#########################################################################
# Method : SetWavelengths
#########################################################################
setGeneric("SetWavelengths<-",function(object,value)
		{standardGeneric("SetWavelengths<-")})
setReplaceMethod(
		f="SetWavelengths",
		signature="Spectra",
		definition=function(object,value){
			object@Wavelengths <-value
			validObject(object)
			return (object)
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