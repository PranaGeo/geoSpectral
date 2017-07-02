#' Read the NOMAD v2 bio-optical database
#'
#' @description
#' Imports the NOMAD v2 database of the SeaBASS project. More information 
#' about this dataset can be found at \url{https://seabass.gsfc.nasa.gov/wiki/NOMAD}
#'
#' @param skip.all.na.rows \code{logical} whether or not eliminate records where all 
#' channels are NAs 
#'
#' @return Returns an object of class \code{data.frame}.
#'
#' @examples
#' nomad = spc.Read_NOMAD_v2()
#' class(nomad[[1]])
#' spc.plot.plotly(nomad[[4]], plot.max=15)
#' 
#' @export
# @importFrom dplyr "%>%" select
spc.Read_NOMAD_v2 = function(skip.all.na.rows=TRUE) {
  fnm = file.path(system.file(package = "geoSpectral"), "test_data","nomad_seabass_v2.a_2008200.txt.gz")
  #Read data off disk
  print(paste("Reading the NOMAD file", fnm, "off disk."))
  mydata=read.table(fnm, header=T,sep=",", comment.char = "!")
  
  #Date-time conversion
  cc=paste(mydata$year, mydata$month,mydata$day,sep="/")
  bb=paste(mydata$hour, mydata$minute,mydata$second,sep=":")
  a=paste(cc,bb)
  a=strptime(a,"%Y/%m/%d %H:%M:%S")
  mydata = mydata[,-(1:6)] #Remove cols year month day hour minute second
  nms = names(mydata)
  nms = gsub("lat","LAT",nms)
  nms = gsub("lon","LON",nms)
  names(mydata)=nms
  mydata = cbind(mydata, TIME=as.POSIXct(a))
  mydata[mydata==-999]=NA
  
  ShortNames = c("kd","lw","es","ap","ad","ag","a","bb","bbr")
  idx=lapply(ShortNames, function(x) {
    i = regexpr(paste0("^",x,"[[:digit:]][[:digit:]][[:digit:]]$"), names(mydata))
    i = which(i!=-1)
    i
    #grep(glob2rx(paste0(x,"???")), names(mydata))
  })
  #Extract Spectral data
  sp = lapply(idx, function(x) mydata[,x])
  #Cleanup mydata from Spectral data
  mydata = mydata [, -do.call(c, idx)]
  
  #Reorder columns
  #mydata = mydata %>% dplyr::select(TIME, LON, LAT, cruise, flag, everything())
  mydata = mydata %>% dplyr::select("TIME", "LON", "LAT", "cruise", "flag", everything())
  
  out = lapply(1:length(ShortNames), function(x) {
    #Find rows that do not contain NAs
    lbd = as.numeric(gsub(ShortNames[x], "", names(sp[[x]])))
    #out = mydata[complete.cases(mydata[,idx[[x]]]),c(names(mydata)[idx[[x]]],"datetime","latitude","longitude")]    out = Spectra(out, Wavelengths = lbd,ShortName=ShortNames[x],time="TIME",Units="1/m")
    
    out = cbind(sp[[x]], mydata)
    
    if (skip.all.na.rows){
      #Find rows where there at least some data
      na_idx = !apply((apply(sp[[x]], 2, is.na)),1,all)
      out = out[na_idx,]
    }
    out = Spectra(out, Wavelengths = lbd,ShortName=ShortNames[x],time="TIME",Units="1/m")
    spc.colnames(out)<-spc.cname.construct(out)
    return (out)
  })
  names(out) = ShortNames
  out
}

#' Read the ASD Spectra from text file
#'
#' @description
#' Imports ASD spectra from text files prepared by the software provided by ASD inc. 
#' This function imports only one spectra per file.
#'
#' @param filename A \code{string} name of the input text file containing the raw ASD data.
#'
#' @return Returns an object of class \code{Spectra}.
#'
#' @examples
#' filename = file.path(system.file(package = "geoSpectral"),"test_data", "106.064.txt")
#' L = spc.Read_ASD(filename)
#' class(L)
#' spc.plot.plotly(L)
#' 
#' @export
spc.Read_ASD <- function(filename){
  ONECOLUMN = FALSE
  
  #Reading data
  asd=readLines(filename)
  
  # Finding breaking point
  id = grep("Wavelength", asd)
  if(length(id)==0){
    ONECOLUMN = TRUE
    nxx = "Spectrum file is"
    id = grep(nxx, asd)+1
  }
    
  DF = read.table(filename, header = FALSE, skip = id)

  if (ONECOLUMN)
    names(DF)<-"V2"
  
  #creating an spcHeader object
  h=new("SpcHeader")
  
  #rowfilename
  h$RawFilename= as.character(filename)
  
  # Comments ####
  nxx= "The instrument number was"
  id = grep(nxx, asd)
  h$comment=asd[id] #chek
  
  #Time ====
  nxx= "Spectrum saved:"
  id = grep(nxx, asd)
  k=strsplit(as.character(asd[id]),"Spectrum saved:")
  k=k[[1]][2]
  k=strsplit(as.character(k),"at")
  
  #converting 100 to 2000, or 101 to 2001
  k1=k[[1]][1]
  k1 = gsub(" ", "", k1)
  k1=strsplit(as.character(k1),"/")
  if(k1[[1]][3]==100){
    k1[[1]][3]=2000
  }else{
    k1[[1]][3]=2001
  }

  k1=paste0(k1[[1]],collapse = "/")
  k2=k[[1]][2]
  k=paste0(k1[[1]],k2[[1]])
  k = strptime(k, "%m/%d/%Y %H:%M:%S",tz = "UTC")
  ttime=as.POSIXct(k)
  #sp@time=xts(1,k)
  
  # asdSerialNo and CalibrationNo
  nxx= "The instrument number was"
  id = grep(nxx, asd)
  k=strsplit(as.character(asd[id])," ")
  k=k[[1]][length(k[[1]])]
  k=strsplit(as.character(k),"/")
  h$asdSerialNo=as.numeric(k[[1]][1])
  h$CalibrationNo=as.numeric(k[[1]][2])
  
  # InstrumentDescription not added yet
  
  # ProgramVersion  and  FileVersion      
  nxx= "New ASD spectrum file:"
  id = grep(nxx, asd)
  k=strsplit(as.character(asd[id]),":")
  k=k[[1]][2]
  k=strsplit(as.character(k),"file")
  k1=k[[1]][1]
  k2=k[[1]][2]
  k1=strsplit(as.character(k1)," ")
  k2=strsplit(as.character(k2)," ")
  k1=k1[[1]][length(k1[[1]])]
  k2=k2[[1]][length(k2[[1]])]
  h$ProgramVersion=as.numeric(k1)
  h$FileVersion=as.numeric(k2)
    
  #BeginTime
  h$BeginTime = ttime[1]
  
  # IntegrationTime       
  nxx= "Integration time"
  id = grep(nxx, asd)
  k=strsplit(as.character(asd[id]),":")
  k=k[[1]][2]
  h$IntegrationTime= as.numeric(k)

  #BeginWavelength and WavelengthStep
  nxx= "Channel 1"
  id = grep(nxx, asd)
  k=strsplit(as.character(asd[id]),"=")
  kk1=k[[1]][2]
  kk2=k[[1]][3]
  k1=strsplit(kk1,"wavelength")[[1]][1]
  h$BeginWavelength=as.numeric(k1)
  h$WavelengthStep=as.numeric(kk2)
  
  #  SamplesPerDataValue    
  nxx= "sample per data"
  id = grep(nxx, asd)
  if (length(id)==0){
    nxx= "samples per data"
    id = grep(nxx, asd)
  }
  k=asd[id]
  k = strsplit(k," ")
  sample_id = grep("sample",k[[1]])
  k = k[[1]][sample_id-1]
  if (k=="one")
    k=1
  k = as.numeric(k)
  h$SamplesPerDataValue = k

  # xmin and xmax  
  nxx= "xmin"
  id = grep(nxx, asd)
  k=strsplit(as.character(asd[id]),"xmax")
  k1=strsplit(as.character(k[[1]][1]),"=")
  k2=strsplit(as.character(k[[1]][2]),"=")
  h$xmax=as.numeric(k2[[1]][2])
  h$xmin=as.numeric(k1[[1]][2])

  if (ONECOLUMN)
    DF$V1 <- seq(h$xmin, h$xmax, length.out = nrow(DF))
  
  #ymin  and ymax   
  nxx= "ymin"
  id = grep(nxx, asd)
  k=strsplit(as.character(asd[id]),"ymax")
  k1=strsplit(as.character(k[[1]][1]),"=")
  k2=strsplit(as.character(k[[1]][2]),"=")
  h$ymax=as.numeric(k2[[1]][2])
  h$ymin=as.numeric(k1[[1]][2])

  #Digitization
  nxx= "The instrument digitizes spectral values"
  id = grep(nxx, asd)
  k=strsplit(as.character(asd[id]),"to")
  h$Digitization=as.numeric(k[[1]][2])
  
  #DarkCurrentCorrected   
  nxx= "dark signal subtracted"
  nx="dark signal not subtracted"
  h$DarkCurrentCorrected = NA
  if(length(grep(nxx,asd))>0) {
    h$DarkCurrentCorrected="TRUE"
  }
  if(length(grep(nx,asd))>0) {
    h$DarkCurrentCorrected="FALSE"
    h$DarkCurrentNumbers=0
    h$DarkCurrentAcquisitionTime = NA
    h$DccValue=NA
  }

  #DarkCurrentNumbers    and DarkCurrentAcquisition    
  nx = "VNIR dark signal not subtracted"
  nxx = "dark measurements taken"
  id = grep(nxx,asd)
  if(length(id)>0) {
    k=strsplit(as.character(asd[id]),"dark measurements taken")
    h$DarkCurrentNumbers=as.numeric(k[[1]][1])
    h$DarkCurrentAcquisitionTime = as.POSIXct(strptime(k[[1]][2],  " %a %B %d %T %Y"))
    
    # DccValue
    nxx= "DCC value was"
    id = grep(nxx, asd)
    k=strsplit(as.character(asd[id]),"DCC value was")
    h$DccValue=as.numeric(k[[1]][2])
  }

  #WhitePanelReferenced   
  nxx = "Data is compared to a white reference"
  nx  = "Data is not compared to a white reference"
  h$WhitePanelReferenced = NA
  if(length(grep(nxx,asd))>0){
    h$WhitePanelReferenced="TRUE"
  }
  if(length(grep(nx,asd))>0){
    h$WhitePanelReferenced="FALSE"
  }
  
  # ForeOptics     
  nxx= "There was foreoptic attached"
  nx="There was no foreoptic attached"
  h$ForeOptics = NA
  if(length(grep(nxx,asd))>0){
    h$ForeOptics="TRUE"
  }
  if(length(grep(nx,asd))>0){
    h$ForeOptics="FALSE"
  }

  #RadiometricCalibration  
  nxx= "Spectrum file is"
  id = grep(nxx, asd)
  if (grepl("raw data", asd[id]))
    h$RadiometricCalibration=FALSE
  else
    h$RadiometricCalibration=TRUE
  
  #IntegrationTimeCorrected   
  nxx= "Integration Time Corrected"
  h$IntegrationTimeCorrected = NA
  if(length(grep(nxx,asd))>0){
    h$IntegrationTimeCorrected="TRUE"
  }else{
    h$IntegrationTimeCorrected="FALSE"
  }

  #Coordinates
  crd = data.frame(LAT=1,LON=2)
  #coordinates(crd) = ~LON+LAT
  
  #there is no code for the following
  # FileName                 
  # SaturationCorrected      
  
  df = as.data.frame(t(as.matrix(as.numeric(DF$V2))))
  df = cbind(df, crd, ttime)

  #endTime = BeginTime + Integration Time
  endTime = ttime[1]+h$IntegrationTime/1000

  #Predict optical measurement Units
  Units = "DC"
  if (h$RadiometricCalibration)
    warning("Estimating Units from a calibrated reading is not supported yet")
  
  sp <- Spectra(df,Wavelengths=DF$V1, header=h,
          space=c("LON","LAT"), time="ttime",
          Units=Units,ShortName="L", endTime=endTime,
          LongName = basename(h$RawFilename))

  # ColumnNames               
  spc.colnames(sp) <-spc.cname.construct(sp)
  
  #spc.setheader(sp) <- h # header added to spectra
  validObject(sp)
  return(sp)
}
