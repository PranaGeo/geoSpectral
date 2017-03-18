#########################################################################
# Class : SpcList
#########################################################################
#' SpcList class definition
#' @description Definition for \code{SpcList}, a class to store multiple \code{Spectra}
#' objects inside a list-like object. See the help of the constructor function \code{\link{SpcList}}.
#' @slot by character, Determines the header field in the Spectra objects
#' within the SpcList that describes how they are different one from the other.
#' @export
setClass("SpcList",contains="list",
		representation=representation(by="character"), 
		prototype=prototype(by="VariousVariables")
)

setValidity("SpcList", function(object){
#			cat("---------SpcList::setValidity\n")
			if(!all(sapply(object, inherits,"Spectra"))) {
#				return("All SpcList elements should inherit from the Spectra class")
			}
			return(TRUE)
		})
