# TODO: names(), colnams()
# 
# Author: acizmeli
###############################################################################

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



setMethod("Arith",
		signature(e1 = "Spectra", e2 = "Spectra"),
		function (e1, e2) {
			result <- callGeneric(e1@DF, e2@DF)
			output <- new("Spectra",DF=result,Wavelengths=e1@Wavelengths,Units=e1@Units,
					ShortName = "Arith", LongName="Arith")			
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
setReplaceMethod("[[",  signature="Spectra",
		definition=function(x, i, j, value) {
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
setReplaceMethod("$", signature = "Bioo", 
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
				x@Ancillary=x@Ancillary[i,,drop=F]
				x@Wavelengths = x@Wavelengths[j]
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
setMethod("names", signature = "Spectra", 
          def = function (x){ 
            if(ncol(x@Ancillary)>1)
              return(c(names(x@DF),names(x@Ancillary)))
            else                
              return(names(x@DF)) 
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
setReplaceMethod(f="SetAncillary",	signature="Spectra",
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
setReplaceMethod(f="SetWavelengths", signature="Spectra",
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
#The argument "select" is not implemented yet. Use "[]"
setMethod("subset",  signature="Spectra",
          definition=function(x, subset, select, drop = FALSE, ...) {
            DF = subset(x@DF,subset,select)
            if(ncol(x@Ancillary)>0)
              Anc = subset(x@Ancillary,subset)
            
            #Save the column names of the original data
            mynames = names(x)
            
            #Perform the changes
            x@DF = DF
            if(exists("Anc") && class(Anc)=="Bioo")
              x@Ancillary = Anc
            
            #Find index of the remaining columns
            if(!missing(select)) { 
              c_idx = match(names(DF), mynames)
              
              #Update units and Wavelengths, LongName
              x@Units = x@Units[c_idx]
              x@LongName = x@LongName[c_idx]
              x@Wavelengths = x@Wavelengths[c_idx]
            }
            validObject(x)
            return(x)
          })