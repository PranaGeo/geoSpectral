# TODO: Add comment
# 
# Author: acizmeli
###############################################################################
#########################################################################
# Class : Spectra
#########################################################################
setClass("Spectra", contains="STIDF", 
		representation(
				ShortName="character",
				Wavelengths="numeric", 
				WavelengthUnit = "character", 
				Ancillary="data.frame"), 
		prototype=prototype(
				data=data.frame(),
				ShortName="spvar2",
				Wavelengths=numeric(), 
				WavelengthUnit = character(), 
				Ancillary=data.frame()))

setMethod("initialize",
		signature(.Object = "Spectra"),
		function (.Object, data, ShortName, LongName, Wavelengths, Units, Ancillary, header,WavelengthUnit,...) 
		{
#			cat("---------Spectra::Initialize\n")
			#Set defaults for ShortName
			if (missing(data))
				data <- data.frame()
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
				LongName <- rep(LongName, ncol(data))				 							
			#Set the default for Units
			if (missing(Units))
				Units <- "[ ]"	
			if (length(Units)==1)
				Units<- rep(Units, ncol(data))				 							
			#Set the default for Ancillary data
			if (missing(Ancillary))
				Ancillary=new("Bioo")
			#Set the default header
			if(missing(header))
				header = .Object@header
			if(missing(WavelengthUnit))
				WavelengthUnit = "nm"

			.Object@Wavelengths=Wavelengths
			.Object@Ancillary=Ancillary
			.Object@Units=Units
			.Object@data=data
			.Object@ShortName=ShortName
			.Object@LongName=LongName
			.Object@header=header
			.Object@WavelengthUnit = WavelengthUnit
			#			.Object=callNextMethod(.Object, data=data, ShortName=ShortName,
#					LongName=LongName,Wavelengths=Wavelengths,Units=Units,Ancillary=Ancillary)
			#			.Object <- callNextMethod()
			
			return(.Object)
		})

setValidity("Spectra", function(object){
#			cat("---------Spectra::setValidity\n")
			if(!all(sapply(object@data, class)=="numeric")){
				return("Spectral data should be a data.frame object with numeric columns")
			}
			if(length(object@Wavelengths)!= ncol(object@data)){
				return("Number of Spectral channels is not equal the number of data columns")
			}
			if(!all(is.finite(object@Wavelengths))){
				return("All the wavelengths should be numeric and finite")
			}
			if(any(diff(object@Wavelengths)<=0)){
				return("Wavelength should be increasing and without replicates.")
			}
			if(nrow(object@Ancillary)!=0){
				if(nrow(object@Ancillary)!=nrow(object@data)){
					return("Ancillary data frame should have the same number of rows as spectral data")
				}
			}
			return(TRUE)
		})