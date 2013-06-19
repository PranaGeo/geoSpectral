# TODO: Add comment
# 
# Author: acizmeli
###############################################################################
#########################################################################
# Class : Spectra
#########################################################################
setClass("Spectra", contains="STIDF", 
		representation=representation(
				ShortName="character",
				LongName="character",
				Wavelengths="numeric", 
				WavelengthsUnit = "character", 
				Spectra="matrix",
				header="BiooHeader",
				Units="character",
				InvalidIdx="logical",
				SelectedIdx="logical"), 
		prototype=prototype(
				ShortName="spvar2",
				LongName="spvar2 longname",
				Wavelengths=0, 
				WavelengthsUnit = "nm", 
				Spectra=matrix(),
				header=new("BiooHeader"),
				Units="[ ]",
				InvalidIdx=logical(),
				SelectedIdx=logical()))
if (1){
	setMethod("initialize",
			signature(.Object = "Spectra"),
			function (.Object, ShortName, LongName, Wavelengths, WavelengthsUnit,
					Spectra,header,Units, time, ...) 
			{
#			cat("---------Spectra::Initialize\n")
				#Set defaults for ShortName
				if (missing(Spectra))
					Spectra <- matrix()
				if (missing(Wavelengths))
					Wavelengths <- numeric(ncol(Spectra))
				if (missing(ShortName))
					ShortName <- "spvar"				 
				if (length(ShortName)!=1)
					ShortName <- ShortName[1]				 							
				#Set defaults for LongName
				if (missing(LongName))
					LongName <- "spvar longname"				 
				if (length(LongName)==1)
					LongName <- rep(LongName, ncol(Spectra))				 							
				#Set the default for Units
				if (missing(Units))
					Units <- "[ ]"	
				if (length(Units)==1)
					Units<- rep(Units, ncol(Spectra))				 							
				#Set the default header
				if(missing(header))
					header = .Object@header
				if(missing(WavelengthsUnit))
					WavelengthsUnit = "nm"
				#Set the default time object
				if(missing(time))
					.Object@time = xts()
				
				.Object@Wavelengths=Wavelengths
				.Object@Units=Units
				.Object@Spectra=Spectra
				.Object@ShortName=ShortName
				.Object@LongName=LongName
				.Object@header=header
				.Object@WavelengthsUnit = WavelengthsUnit
				#			.Object=callNextMethod(.Object, data=data, ShortName=ShortName,
#					LongName=LongName,Wavelengths=Wavelengths,Units=Units,Ancillary=Ancillary)
				#			.Object <- callNextMethod()
				browser()
				validObject(.Object)
				return(.Object)
			})
}

setValidity("Spectra", function(object){
#			cat("---------Spectra::setValidity\n")
			if(!all(sapply(object@Spectra, class)=="numeric")){
				return("Spectral data should be a data.frame object with numeric columns")
			}
			if(length(object@Wavelengths)!= ncol(object@Spectra)){
				return("Number of Spectral channels is not equal the number of data columns")
			}
			if(!all(is.finite(object@Wavelengths))){
				return("All the wavelengths should be numeric and finite")
			}
			if(any(diff(object@Wavelengths)<=0)){
				return("Wavelength should be increasing and without replicates.")
			}
			if(nrow(object@data)!=0){
				if(nrow(object@data)!=nrow(object@Spectra)){
					return("Ancillary data frame should have the same number of rows as spectral data")
				}
			}
			if(length(object@InvalidIdx)!=0){
				if(length(object@InvalidIdx)!=nrow(object(Spectra)))
					return("Invalid index length should match the number of data rows")
			}
			if(length(object@SelectedIdx)!=0){
				if(length(object@SelectedIdx)!=nrow(object(Spectra)))
					return("Selected index length should match the number of data rows")
			}
			if(length(object@ShortName)!=1)
				return("The slot ShortName should have a length of 1")
			if(length(object@LongName)!=1)
				return("The slot LongName should have a length of 1")
			if(length(object@Units)!=1)
				return("The slot Units should have a length of 1")
			return(TRUE)
		})