# TODO: Add comment
# 
# Author: acizmeli
###############################################################################

#########################################################################
# Method : plot.grid
#########################################################################
setGeneric (name= "plot.grid",
		def=function(x,FUN, nnrow, nncol,...){standardGeneric("plot.grid")})
setMethod("plot.grid", "BiooList", function (x,FUN, nnrow, nncol, ...){
			nb_spc = length(which(sapply(x, inherits, "Bioo")))
			mypar = par()
			nrow = ceiling(nb_spc/nncol)
			
#			FUN <- match.fun(FUN)
			
			mar = c(4,4,1,0.5)
			oma = c(0,0,0,0)#c(1.5,2,1,1)
			par(mfrow=c(nnrow,nncol), mar=mar, oma=oma)
			
			for (I in 1:length(x)) {
				if(nrow(x[[I]])>1){
					if(x@by!="VariousVariables"){
						tit = paste(x@by, ":", as.character(x[[I]]$STATION[1]))
						eval_txt = paste(FUN, "(x[[I]],title=tit,...)",sep="")
					}
					else{
						eval_txt = paste(FUN, "(x[[I]],...)",sep="")
					}
					eval(parse(text=eval_txt))

					if (par()$mfg[1]==par()$mfg[3] & par()$mfg[2]==par()$mfg[4] & I<length(x)) {
						dev.new()
						par(mfrow=c(nnrow,nncol), mar=mar, oma=oma)
					}				
				}
			}
			par(mfrow=mypar$mfrow,mar=mypar$mar,oma=mypar$oma)
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
# Method : biooInvalidDetect
#########################################################################
setMethod("biooInvalidDetect", signature = "BiooList", def=function(source1){
			out = lapply(source1, function(x) {SetInvalidIdx(x)<-biooInvalidDetect(x)})
			return(out)
})

#########################################################################
# Method : GetBiooHeader
#########################################################################
setMethod("GetBiooHeader", signature = "BiooList", 
          def = function (object,name){
            sapply(object, GetBiooHeader,name)
          })

#########################################################################
# Method : SetBiooHeader
#########################################################################
setReplaceMethod(f="SetBiooHeader", signature="BiooList",
  definition=function(object,value,...){
    if(inherits(value,"Bioo"))
      stop("It is forbidden to place in a BiooHeader object that inherit from the Bioo class")

    if(length(value)==1)
      value = rep(value,length(object))
    
    a=sapply(1:length(object), function(x) {
      object[[x]] = SetBiooHeader(object[[x]],value[x])
      })

    validObject(object)
    return(object)
  })

#########################################################################
# Method : biooDataToHeader
#########################################################################
setMethod("biooDataToHeader", signature = "BiooList", 
          def=function(object,headerfield,dataname,compress=TRUE,...){
            temp = lapply(object, biooDataToHeader, headerfield,dataname,compress,...)
            object@.Data=temp
            return(object)
          })
