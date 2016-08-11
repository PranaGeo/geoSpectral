# TODO: Add comment
# 
# Author: acizmeli
###############################################################################
#########################################################################
# Class : Spectra
#########################################################################
#' Create a spectra class definition
#' @description
#' Create a spectra class definition, specifying the representation (the slots) and/or the classes contained in this one (the superclasses),
#' plus other optional details. As a side effect, the class definition is stored in the specified environment
#'
#' @usage 
#' setClass(class, contains,pepresentation,prototype)
#' 
#' 
#'
#' 
#' @param  contains STIDF 
#' @param class  character string name for the class.
#' @param  pepresentation avoided
#' @param  prototype n object providing the default data for the slots in this class
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
setClass("Spectra", contains="STIDF", 
		representation=representation(
				ShortName="character",
				LongName="character",
				Wavelengths="numeric", 
				WavelengthsUnit = "character", 
				Spectra="matrix",
				header="BiooHeader",
				Units="character",
        UnitsAnc="character",
				ShortNameAnc="character",
				LongNameAnc="character",
				InvalidIdx="logical",
				SelectedIdx="logical",
				ClassVersion="numeric"), 
		prototype=prototype(
				ShortName="spvar2",
				LongName="spvar2 longname",
				Wavelengths=numeric(), 
				WavelengthsUnit = "nm", 
				Spectra=matrix(NA,0,0),
				header=new("BiooHeader"),
				Units="[ ]",
        UnitsAnc=character(0),
				ShortNameAnc=character(0),
				LongNameAnc=character(0),
				InvalidIdx=logical(),
				SelectedIdx=logical(),
				ClassVersion=0.1))
if (0){
	setMethod("initialize",
			signature(.Object = "Spectra"),
			function (.Object, ShortName, LongName, Wavelengths, WavelengthsUnit,
					Spectra,header,Units, time, ...) 
			{
#			cat("---------Spectra::Initialize\n")
				#Set defaults for ShortName
				if (missing(Spectra))
					Spectra <- matrix(NA,0,0)
				if (missing(Wavelengths))
					Wavelengths <- numeric()
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
				return("Spectral data should be a matrix object with numeric columns")
			}
			if(length(object@Wavelengths)!= ncol(object@Spectra)){
				return("Number of Spectral channels is not equal the number of data columns")
			}
			if(!all(is.finite(object@Wavelengths))){
				return("All the wavelengths should be numeric and finite")
			}
			if (anyDuplicated(object@Wavelengths)!=0) {
				return("Wavelength values should be without replicates.")
			}
			if(nrow(object@data)!=0){
				if(nrow(object@data)!=nrow(object@Spectra)){
					return("Ancillary data frame should have the same number of rows as spectral data")
				}
			}
			if(length(object@InvalidIdx)!=0){
				if(length(object@InvalidIdx)!=nrow(object@Spectra))
					return("Invalid index length should match the number of data rows")
			}
			if(length(object@SelectedIdx)!=0){
				if(length(object@SelectedIdx)!=nrow(object@Spectra))
					return("Selected index length should match the number of data rows")
			}
			if(length(object@ShortName)!=1)
				return("The slot ShortName should have a length of 1")
			if(length(object@LongName)!=1)
				return("The slot LongName should have a length of 1")
			if(length(object@Units)!=1)
				return("The slot Units should have a length of 1")
			if(!(length(object@UnitsAnc)==0 | length(object@UnitsAnc)==ncol(object@data)))
			  return("The slot UnitsAnc should have a length of 1 or ncol(object@data)")
			if(!(length(object@LongNameAnc)==0 | length(object@LongNameAnc)==ncol(object@data)))
			  return("The length of slot LongNameAnc should be equal to ncol(object@data)")
			if(!(length(object@ShortNameAnc)==0 | length(object@ShortNameAnc)==ncol(object@data)))
			  return("The length of slot ShortNameAnc should be equal to ncol(object@data)")
			return(TRUE)
})