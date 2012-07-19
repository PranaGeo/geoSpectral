# TODO: Add comment
# 
# Author: acizmeli
###############################################################################
#removeClass("Spectra")
if (1) {
	setClass("Spectra", 
			representation(
					Wavelengths="numeric", Ancillary="Biooo"), 
			prototype=prototype(DF=data.frame(),
					Wavelengths=numeric(),Units="[ ]", SelectedIdx=logical(),
					InvalidIdx=logical(),ShortName="spvar",LongName="spvar longname",
					Ancillary=data.frame()))
	
}
setMethod("initialize",
		signature(.Object = "Spectra"),
		function (.Object, ...) 
		{
#			cat("---------Spectra::Initialize\n")						
			.Object <- callNextMethod()
			
			if (.Object@ShortName=="spvar") {
				ShortName = colnames(.Object@DF)[1]
				if (!is.na(ShortName)){
					if (grepl("_", ShortName)) {
						ShortName = strsplit(ShortName,"_")[[1]][1]
					}
					.Object@ShortName <- ShortName				 
				}
			}
			if (.Object@LongName=="spvar longname") {
				.Object@LongName <- .Object@ShortName				 				
			}				
			#			validObject(.Object)
			return(.Object)
		}
)

setValidity("Spectra", function(object){
#			cat("---------Spectra::setValidity\n")
			if(!all(sapply(object@DF, class)=="numeric")){
				return("Spectral data should be a data.frame object with numeric columns")
			}
			if(length(object@Wavelengths)!= ncol(object@DF)){
				return("Number of Spectral channels is not equal the number of DF columns")
			}
			if(any(diff(object@Wavelengths)<=0)){
				return("Wavelength should be increasing and without replicates.")
			}
			if(length(object@Units)!= ncol(object@DF)){
				return("Number of Unit elements is not equal the number of DF columns")
			}
			if(length(object@Ancillary)!=0){
				if(nrow(object@Ancillary)!=nrow(object@DF)){
					return("Ancillary data frame should have the same number of rows as spectral data")
				}
			}
			if (!length(object@ShortName)==1){
				return("The length of slot ShortName should be equal to 1!")
			}
			if (!is.character(object@ShortName) | is.na(object@ShortName) | is.null(object@ShortName) 
					|  object@ShortName==""){
				return("Invalid slot : ShortName!")
			}	
			if (grepl(" ", object@ShortName)){
				return("The slot ShortName cannot contain ' '.")
			}
			if(length(object@SelectedIdx)!=0){
				if(length(object@SelectedIdx)!=nrow(object@DF)){
					return("The slot SelectedIdx should have the same length as the number of rows of spectral data")
				}
			}
			if(length(object@InvalidIdx)!=0){
				if(length(object@InvalidIdx)!=nrow(object@DF)){
					return("The slot InvalidIdx should have the same length as the number of rows of spectral data")
				}
			}
			return(TRUE)
		})