# TODO: Add comment
# 
# Author: acizmeli
###############################################################################
#removeClass("Spectra")
if (1) {
	setClass("Biooo", 
			representation(DF="data.frame", 
					Units="character",
					SelectedIdx="logical", InvalidIdx="logical",
					ShortName="character", LongName="character"), 
			prototype=prototype(DF=data.frame(),
					Units="[|]", ShortName="[|]", LongName="[|]", 
					SelectedIdx=logical(),
					InvalidIdx=logical()))
	
}
setMethod("initialize",
		signature(.Object = "Biooo"),
		function (.Object, ...) 
		{
#			cat("---------Spectra::Initialize\n")						
			.Object <- callNextMethod()
#browser()			
			.Object@Units = rep("[ ]",ncol(.Object@DF))
			
			if (.Object@ShortName[1]=="[|]") {
				.Object@ShortName = colnames(.Object@DF)
			}
			.Object@LongName <- .Object@ShortName				 				
			#			validObject(.Object)
			return(.Object)
		}
)

setValidity("Biooo", function(object){
#			cat("---------Spectra::setValidity\n")
			if(! class(object@DF)=="data.frame"){
				return(" data should be a data.frame object")
			}
			if (object@Units[1]=="[|]") {
			} else if(length(object@Units)!= ncol(object@DF)){
				return("Number of Unit elements is not equal the number of DF columns")
			}
			if (object@ShortName[1]=="[|]") {
			} else if (length(object@ShortName)!=ncol(object@DF)){
				return("The slot ShortName should have the same length as the number of columns in slot DF")
			}
			if (!all(is.character(object@ShortName)) | any(is.na(object@ShortName)) | any(is.null(object@ShortName)) 
					|  any(object@ShortName=="")){
				return("Invalid slot : ShortName!")
			}	
			if (any(grepl(" ", object@ShortName))){
				return("The slot ShortName cannot contain ' '.")
			}
			if (object@LongName[1]=="[|]") {
			} else if (length(object@LongName)!=ncol(object@DF)){
				return("The slot LongName should have the same length as the number of columns in slot DF")
			}
			if(length(object@SelectedIdx)!=0){
				if(length(object@SelectedIdx)!=nrow(object@DF)){
					return("The slot SelectedIdx should have the same length as the number of columns in slot DF")
				}
			}
			if(length(object@InvalidIdx)!=0){
				if(length(object@InvalidIdx)!=nrow(object@DF)){
					return("The slot InvalidIdx should have the same length as the number of rows of spectral data")
				}
			}
			return(TRUE)
		})