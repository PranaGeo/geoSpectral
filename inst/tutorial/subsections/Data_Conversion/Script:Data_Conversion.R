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

## ------------------------------------------------------------------------
head(spc.getwavelengths(myS))
spc.setwavelengths(myS) <- 300:800 

## ------------------------------------------------------------------------
myS[1:10]

## ------------------------------------------------------------------------
myS[,"anap_400"] 
myS[,c("anap_400","anap_500")] 

## ------------------------------------------------------------------------
myS[1:10,30:50] #Selection of channels by column index
lbd = as.numeric(c(412,440,490,555,670))
myS[1:10,lbd] #Selection of channels by wavelength

## ------------------------------------------------------------------------
myS[1:10,"415::450"] 

## ------------------------------------------------------------------------
myS$CAST #Returns Ancillary data
myS$anap_400 #Returns spectra as numeric vector
head(myS[["anap_400"]]) #Returns spectra as numeric vector
head(myS[[c("Snap","Offset")]]) #Returns data.frame

## ------------------------------------------------------------------------
subset(myS,DEPTH<=30) #Subsetting rows with respect to the value of Ancillary data
subset(myS,DEPTH<=30)$DEPTH
subset(myS,anap_440<=0.01) #Subsetting rows with respect to the value of Spectral data
subset(myS,subset=DEPTH<=30,select="CAST") #Selecting Ancillary data columns, leaving Spectral columns intact
subset(myS,subset=DEPTH<=30,select="anap_440") #Selecting Spectral data columns, leaving Ancillary columns intact

## ----eval=FALSE----------------------------------------------------------
## showMethods(subset,classes="Spectra",includeDefs=T)

## ------------------------------------------------------------------------
idx=rep(FALSE,nrow(myS)); 
idx[1:5]=TRUE
spc.setselected.idx(myS)<-idx 

## ------------------------------------------------------------------------
spc.setinvalid.idx(myS)<-spc.getselected.idx(myS) 

## ----fig.width=5, echo=TRUE, fig.height=5--------------------------------
library(plotly)
spc.plot.plotly(myS)

## ----eval=FALSE----------------------------------------------------------
## spc.setselected.idx(myS)<-spc.select(myS)

## ------------------------------------------------------------------------
spc.getselected.idx(myS)

## ----warning=FALSE-------------------------------------------------------