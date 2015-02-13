#Spectral

Spectral is an R package providing a new data type for R that stores spectral, temporal and spatial attributes of measurement data as well as methods for accessing and manipulating the spectral (and non-spectral) data. Once spectral data is imported into Spectral, the statistical and data processing power of R is available for various kinds of scientific analyses.

It provides the S4 classes: **_Spectra_** (stores spatial/temporal/spectral aspects of data), **_SpcHeader_** (stores metadata in an R list object) and **_SpcList_** (makes a collection of Spectra objects in an R list) as well as basic data access and manipulation methods for importing, acessing and subsetting, converting into R objects, analyzing, plotting and exporting scientific data.

##License
The package is issued with a [GPLv3](http://www.gnu.org/copyleft/gpl.html) license. Please consult the license documentation if you would like to use **Spectral** in your software projects.

##Requirements
**Spectral** depends on the R packages *rgdal*, *spacetime* and *xlsx*. You need to install them before you can install **Spectral**. If you don't have them already, try :
```
install.packages(c("rgdal","spacetime","xlsx"),dep=T)
```

##Installation
First install the *devtools* package using *install.packages()* and then :
```
require(devtools)
install_github("PranaGeo/Spectral")
```
##Usage
The R code documentation is very incomplete. Sorry. After installing the package, you can try from the R prompt : ```?Spectral``` to consult the brief documentation of the package or ```?Spectra``` to  see the help of the constructor function the main class : *Spectra()*.

You can view the documentation [wiki](https://github.com/PranaGeo/Spectral/wiki) to learn how to use the package. Your comments,suggestions and contributions are very welcome.
