## ----eval=FALSE----------------------------------------------------------
## install.packages(c("rgdal","spacetime"),dep=T)

## ----eval=FALSE----------------------------------------------------------
## install.packages("devtools")

## ----eval=FALSE----------------------------------------------------------
## install.packages("devtools",dependencies=T)

## ----eval=FALSE----------------------------------------------------------
## require(devtools)
## install_github("PranaGeo/geoSpectral")

## ------------------------------------------------------------------------
library('geoSpectral')
showClass("Spectra")

## ------------------------------------------------------------------------
fnm = file.path(base::system.file(package = "geoSpectral"),"test_data","particulate_absorption.csv.gz")
abs = read.table(fnm,sep=",",header=T)
abs$STATION=factor(abs$STATION)
abs[1:2,1:17] #Display only the first 2 rows and first 17 columns if the data frame

## ------------------------------------------------------------------------
lbd = as.numeric(gsub("X","",colnames(abs)[14:514]))
Units="1/m"
colnames(abs)= gsub("X",paste("anap","_",sep=""), colnames(abs))
colnames(abs)= gsub("PRES","DEPTH", colnames(abs))
abs = abs[,c(14:514,1:13)]

## ------------------------------------------------------------------------
tz<-strsplit(as.character(abs$TIME)," ")[[1]][[3]] #Extract the timezone
abs$TIME = as.POSIXct(as.character(abs$TIME),tz=tz)

## ------------------------------------------------------------------------
myS<-Spectra(abs,Wavelengths=lbd,Units=Units,ShortName="a_nap")
myS

## ------------------------------------------------------------------------
dim(myS) 
ncol(myS) 
nrow(myS) 
tail(names(myS))
tail(spc.colnames(myS))