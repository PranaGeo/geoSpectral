Spectral is an R package that provides a data type for R that stores spectral data (radiometric data that were measured at various wavelengths) as well as methods for accessing and manipulating the spectral (and non-spectral) data. 

Spectral was initially created to answer needs encountered in bio-optical oceanography, remote sensing, environmental and earth sciences. However anyone using spatial/temporal/spectral data of any kind can possible make use of tools and methods provided by the package. It provides S4 classes and basic data access and manipulation methods.

It provides S4 classes 
Spectra class (stores spatial/temporal/spectral aspects of data)
SpcHeader class (stores metadata in an R list object)
SpcList class (makes a collection of Spectra objects in an R list)

and basic data access and manipulation methods for :
subsetting
plotting

The main building block of Spectral is the R package spacetime, which, itself, was built on the packages rgdal and xts (built on zoo). Rgdal provides spatial attributes and methods. xts and zoo provide temporal attributes and methods. Spacetime provides the spatio-temporal characteristics into data, standardizing 

is a data type (S4 class) called Spectra which extends the STIDF class of 


TO BE DONE
