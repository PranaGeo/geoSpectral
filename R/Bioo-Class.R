# TODO: Add comment
# 
# Author: acizmeli
###############################################################################
#removeClass("Spectra")
if (1) {
	setClass("Bioo", 
			representation(
					DF="data.frame", 
					header="BiooHeader",
					Units="character",
					LongName="character",
					SelectedIdx="logical", 
					InvalidIdx="logical",
					ProcessLog="list",
					ClassVersion="numeric"), 
			prototype=prototype(
					DF=data.frame(),
					header=new("BiooHeader"),
					Units=character(), 
					LongName=character(), 
					SelectedIdx=logical(),
					InvalidIdx=logical(),
					ProcessLog=list(time),
					ClassVersion=numeric()))
}
setMethod("initialize",
		signature(.Object = "Bioo"),
		function (.Object, DF, Units, LongName, header, ClassVersion,...) 
		{
			if(missing(DF))
				DF = data.frame()
			if(missing(header))
				header = new("BiooHeader")
			if(missing(Units))
				Units <- "[ ]"	
			if (length(Units)==1)
				Units <- rep(Units, ncol(DF))				 							
			if(missing(LongName))
				LongName <- names(DF)				 
			if (length(LongName)==1)
				LongName <- rep(LongName, ncol(DF))				 							
			if (missing(ClassVersion))
				ClassVersion = 1
			
#			cat("---------Bioo::Initialize\n")						
			.Object <- callNextMethod(.Object,DF=DF,LongName=LongName,Units=Units,
					header=header,ClassVersion=ClassVersion)

#			if (length(.Object@Units)==1 & .Object@Units[1]=="[]" & ncol(.Object@DF)>0){
#				.Object@Units = rep("[ ]",ncol(.Object@DF))
  
			#			if (.Object@ShortName[1]=="[]" & ncol(.Object@DF)!=0) {
#				.Object@ShortName = colnames(.Object@DF)
#			}
#			if (.Object@ShortName[1]=="[]") {
#				.Object@ShortName = "Bioo"
#			}
			#			validObject(.Object)
			return(.Object)
		}
)

setValidity("Bioo", function(object){
#			cat("---------Bioo::setValidity\n")
			if(! class(object@DF)=="data.frame"){
				return(" data should be a data.frame object")
			}
#			if (length(object@Units)>0)
			  if(length(object@Units)!= ncol(object@DF))
			    return("Number of Unit elements is not equal the number of columns in slot DF")
			
#			if (length(object@LongName)>0)
			if (length(object@LongName)!=ncol(object@DF))
				if(class(object)=="Bioo") {
					return("The slot LongName should have the same length as the number of columns in slot DF")
				}
			if(length(object@SelectedIdx)!=0){
				if(length(object@SelectedIdx)!=nrow(object@DF)){
					return("The slot SelectedIdx should have the same length as the number of rows in slot DF")
				}
			}
			if(length(object@InvalidIdx)!=0){
				if(length(object@InvalidIdx)!=nrow(object@DF)){
					return("The slot InvalidIdx should have the same length as the number of rows of spectral data")
				}
			}
			return(TRUE)
		})

#########################################################################
# Class : BiooList
#########################################################################
setClass("BiooList",contains="list",
		representation=representation(by="character"), 
		prototype=prototype(by="VariousVariables"), 
		)

setValidity("BiooList", function(object){
#			cat("---------BiooList::setValidity\n")
			if(!all(sapply(object, inherits,"Bioo"))) {
				return("All BiooList elements should inherit from the Bioo class")
			}
			return(TRUE)
		})
