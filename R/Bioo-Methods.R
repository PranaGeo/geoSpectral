# TODO: names(), colnams()
# 
# Author: acizmeli
###############################################################################

#########################################################################
# Method : Conversions from and to data.frame
#########################################################################
setAs(from="Biooo", to="data.frame", def=function(from){
			if(any(grepl("Units", names(attributes(from)))))
				output = from@DF
			
			attr(output,"ShortName") = from@ShortName
			attr(output,"LongName") = from@LongName
			attr(output,"Units") = from@Units
			return(output)
		})
setAs(from="data.frame", to="Biooo", def=function(from){
#			if (any(grepl("Units", names(attributes(from)))) &
#					any(grepl("ShortName", names(attributes(from)))))				
				outS = new("Biooo", DF=from)
				
				if (any(grepl("Units", names(attributes(from)))))
					outS@Units=rep(attr(from,"Units"),ncol(outS)) 
				if (any(grepl("ShortName", names(attributes(from)))))
					outS@ShortName = rep(attr(from, "ShortName"),ncol(outS))				
				if (any(grepl("LongName", names(attributes(from)))))
					outS@LongName = rep(attr(from, "LongName"),ncol(outS))				
#			} else {
#				stop("One or more of the required data.frame attributes are not found : Units or ShortName")
#			}
			return(outS)
		})

#########################################################################
# Method : show
#########################################################################
setMethod("show", "Biooo", function(object){
			cat("\n", paste(object@ShortName[1], ' : An object of class "Biooo"\n', 
							ncol(object@DF),"variables in columns and", nrow(object@DF), 
							"observations in rows"), "\n",
					"LongName : ",head(object@LongName), "\n",					
					"Units : ", head(object@Units), "\n",
					"Columns : ", head(colnames(object@DF)), "...\n")
		})

#########################################################################
# Method : Arith
#########################################################################
setMethod("Arith",
		signature(e1 = "Biooo", e2 = "Biooo"),
		function (e1, e2) {
			result <- callGeneric(e1@DF, e2@DF)
			output <- new("Biooo",DF=result,Units=e1@Units,
					ShortName = "Arith", LongName="Arith")			
		}
)
#########################################################################
# Method : names
#########################################################################
setMethod("names", signature = "Biooo", 
		def = function (x){
			return(names(x@DF))
		})
#########################################################################
# Method : dim
#########################################################################
setMethod("dim", signature = "Biooo", 
		def = function (x){
			return(dim(x@DF))
		})
#########################################################################
# Method : ncol
#########################################################################
setMethod("ncol", signature = "Biooo", 
		def = function (x){
			return(ncol(x@DF))
		})
#########################################################################
# Method : nrow
#########################################################################
setMethod("nrow", signature = "Biooo", 
		def = function (x){
			return(nrow(x@DF))
		})

setMethod("[", signature(x = "Biooo"),
		function(x, i, j) {
			if(missing(i))
				i =  1:nrow(x@DF)
			if(missing(j))
				j =  1:ncol(x@DF)
			
#			x@DF <- callGeneric(x@DF, i, j)
			
			x@DF=x@DF[i,j,drop=F]
			x@SelectedIdx = logical()
			
			if (length(x@InvalidIdx)>1)
				x@InvalidIdx = x@InvalidIdx[i] 
			return(x)
		})

#########################################################################
# Method : head
#########################################################################
setMethod("head", signature = "Biooo", 
		def = function (x){
			return(head(x@DF))
		})

#########################################################################
# Method : GetSelectedIdx
#########################################################################
setGeneric (name= "GetSelectedIdx",
		def=function(object){standardGeneric("GetSelectedIdx")})
setMethod("GetSelectedIdx", signature = "Biooo", 
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
		signature="Biooo",
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
setMethod("GetInvalidIdx", signature = "Biooo", 
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
		signature="Biooo",
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
setMethod("plot.time", signature="Biooo", function (object,Y,maxSp, ...){
			idx = as(1:ncol(object@DF), "logical")
			
			if (length(object@InvalidIdx)==0)
				object@InvalidIdx = rep(FALSE,nrow(object@DF))		

			x = 1:nrow(object)
			xlb = "Observation number"
#			ylb = ""
			
			if(missing(Y)){
				if(!missing(maxSp) && ncol(object)>maxSp)
					Y = seq(1,ncol(object),length.out=maxSp)
				else
					Y = names(object)
			}
			matplot(x[!object@InvalidIdx], 
					object@DF[!object@InvalidIdx,Y], type="l", pch=19,cex=0.3,
					xlab=xlb,...)
			grid(col="black")			
		})