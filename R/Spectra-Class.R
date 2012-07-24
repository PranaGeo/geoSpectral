# TODO: Add comment
# 
# Author: acizmeli
###############################################################################
if (1) {
	setClass("Spectra", contains="Bioo",
			representation(
					ShortName="character",
					Wavelengths="numeric", Ancillary="Bioo"), 
			prototype=prototype(DF=data.frame(),
					ShortName="spvar2",Wavelengths=0, Ancillary=new("Bioo")))
	
}
setMethod("initialize",
		signature(.Object = "Spectra"),
		function (.Object, DF, ShortName, LongName, Wavelengths, Units, Ancillary, ...) 
			{
#			cat("---------Spectra::Initialize\n")
			#Set defaults for ShortName
			if(missing(ShortName))
				ShortName <- "spvar"				 
			if (length(ShortName)==1)
				ShortName <- rep(ShortName, ncol(DF))				 							
			#Set defaults for LongName
			if(missing(LongName))
				LongName <- "spvar longname"				 
			if (length(LongName)==1)
				LongName <- rep(LongName, ncol(DF))				 							
			#Set defaults for Units
			if(missing(Units))
				Units <- "[ ]"	
			if(missing(Ancillary))
				Ancillary=new("Bioo")
			if (length(Units)==1)
				Units<- rep(Units, ncol(DF))				 							

			.Object=callNextMethod(.Object, ShortName=ShortName,DF=DF,
					LongName=LongName,Wavelengths=Wavelengths,Units=Units,Ancillary=Ancillary)
			#			.Object <- callNextMethod()
			
#			if (.Object@ShortName=="[]") {
#				ShortName = colnames(.Object@DF)[1]
#				if (!is.na(ShortName)){
#					if (grepl("_", ShortName)) {
#						ShortName = strsplit(ShortName,"_")[[1]][1]
#					}
#				}
#			}			
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
#			if(length(object@Units)!= ncol(object@DF)){
#				browser()
#				return("Number of Unit elements is not equal the number of DF columns")
#			}
			if(nrow(object@Ancillary)!=0){
				if(nrow(object@Ancillary)!=nrow(object@DF)){
					return("Ancillary data frame should have the same number of rows as spectral data")
				}
			}
#			browser()
#			if (!length(object@ShortName)==1){
#				return("The length of the slot ShortName should be equal to 1!")
#			}
#			if (!is.character(object@ShortName) | is.na(object@ShortName) | is.null(object@ShortName) 
#					|  object@ShortName==""){
#				return("Invalid slot : ShortName!")
#			}	
#			if (grepl(" ", object@ShortName)){
#				return("The slot ShortName cannot contain ' '.")
#			}
#			if(length(object@SelectedIdx)!=0){
#				if(length(object@SelectedIdx)!=nrow(object@DF)){
#					return("The slot SelectedIdx should have the same length as the number of rows of spectral data")
#				}
#			}
#			if(length(object@InvalidIdx)!=0){
#				if(length(object@InvalidIdx)!=nrow(object@DF)){
#					return("The slot InvalidIdx should have the same length as the number of rows of spectral data")
#				}
#			}
			return(TRUE)
		})