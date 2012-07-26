# TODO: Add comment
# 
# Author: acizmeli
###############################################################################
#########################################################################
# Class : Spectra
#########################################################################
setClass("Spectra", contains="Bioo",
		representation(
				ShortName="character",
				Wavelengths="numeric", Ancillary="Bioo"), 
		prototype=prototype(DF=data.frame(),
				ShortName="spvar2",Wavelengths=0, Ancillary=new("Bioo")))

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
			if(nrow(object@Ancillary)!=0){
				if(nrow(object@Ancillary)!=nrow(object@DF)){
					return("Ancillary data frame should have the same number of rows as spectral data")
				}
			}
			return(TRUE)
		})

#########################################################################
# Class : SpectraCollection
#########################################################################
setClass("SpectraCollection",representation(header="list"), contains="list")

setValidity("SpectraCollection", function(object){
#			cat("---------SpectraCollection::setValidity\n")
			if(!any(sapply(object, is, "Spectra")) ){
				return("SpectraCollection should contain at least one Spectra object")
			}
			return(TRUE)
		})
