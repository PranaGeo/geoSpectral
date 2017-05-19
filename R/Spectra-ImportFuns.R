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
#' ap = spc.Read_NOMAD_v2()
#' class(ap)
#' spc.plot.plotly(ap[[4]], plot.max=15)
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
  mydata = mydata %>% dplyr::select_("TIME", "LON", "LAT", "cruise", "flag", everything())
  
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
#' Imports 
#'
#' @param filename A \code{string} name of the input text file containing the raw ASD data.
#'
#' @return Returns an object of class \code{Spectra}.
#'
#' @examples
#' filename = file.path(system.file(package = "geoSpectral"),"test_data", "106.064.txt")
#' L = spc.Read_ASD(filename)
#' class(L)
#' spc.plot.plotly(L, plot.max=15)
#' 
#' @export
spc.Read_ASD <- function(filename){
  #Reading data
  asd=readLines(filename)
  
  # Finding breaking point
  id = grep("Wavelength", asd)
  # for(x in 1:nrow(asd)){
  #   nxx= "Wavelength"
  #   if(length(grep(nxx,asd[x,1]))>0){
  #     id=x
  #   }
  # }
  
  DF = read.table(filename, header = FALSE, skip = id)
  Wavelengths = DF$V1
  
  # deviding data frame for wavelenghts
  #Wavelengths = asd[id+1:length(asd)]
  #Wavelengths= na.omit(Wavelengths)
  #Wavelengths= as.factor(Wavelengths)
  
  # Creating Spectra
  sp <- new("Spectra")
  
  # setting wavelenghts into spectra
  sp@Wavelengths <- Wavelengths
  
  #Spectra slot create
  sp@Spectra = matrix(1,length(Wavelengths))
  sp@Spectra[,1] = DF$V2
  
  #creating spcheader
  h=new("SpcHeader")
  
  #rowfilename
  h$RawFilename= as.character(filename)
  
  #Comments ###########################
  nxx= "The instrument number was"
  id = grep(nxx, asd)
  h$comment=asd[id] #chek

  #Time
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
  k = strptime(k, "%m/%d/%Y %H:%M:%S")
  k=as.POSIXct(k)
  sp@time=xts(1,k)
  
  browser()
UNTIL here

  # asdSerialNo and CalibrationNo
  for(x in 1:id){
    nxx= "The instrument number was"
    if(length(grep(nxx,asd[x,1]))>0){
      indx=x
      k=strsplit(as.character(asd[indx,])," ")
      k=k[[1]][length(k[[1]])]
      k=strsplit(as.character(k),"/")
      h$asdSerialNo=as.numeric(k[[1]][1])
      h$CalibrationNo=as.numeric(k[[1]][2])
    }
  }
  
  
  # InstrumentDescription   not added yet
  
  # ProgramVersion  and  FileVersion      
  for(x in 1:id){
    nxx= "New ASD spectrum file:"
    if(length(grep(nxx,asd[x,1]))>0){
      dx=x
      k=strsplit(as.character(asd[dx,]),":")
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
    }
  }
  
  
  #BeginTime
  for(x in 1:id){
    nxx= "Spectrum saved:"
    if(length(grep(nxx,asd[x,1]))>0){
      ww=x
      k=strsplit(as.character(asd[ww,]),":")
      k=k[[1]][2]
      h$BeginTime= as.factor(k)
    }
  }
  
  
  # IntegrationTime       
  for(x in 1:id){
    nxx= " Integration time"
    if(length(grep(nxx,asd[x,1]))>0){
      ww=x
      k=strsplit(as.character(asd[ww,]),":")
      k=k[[1]][2]
      h$IntegrationTime= as.factor(k)
    }
  }
  
  
  #BeginWavelength and WavelengthStep
  for(x in 1:id){
    nxx= "  Channel 1"
    if(length(grep(nxx,asd[x,1]))>0){
      ww=x
      k=strsplit(as.character(asd[ww,]),"=")
      k=k[[1]][2]
      k=strsplit(as.character(k),"wavelength")
      k1=k[[1]][2]
      k2=k[[1]][3]
      k1=strsplit(as.character(k1)," ")
      k1=k1[[1]][1]
      h$BeginWavelength=as.numeric(k1)
      h$WavelengthStep=as.numeric(k2)
      
    }
  }
  
  #  SamplesPerDataValue    
  for(x in 1:id){
    nxx= "sample per data"
    if(length(grep(nxx,asd[x,1]))>0){
      ww=x
      k=asd[ww,]
      h$SamplesPerDataValue =as.factor(k)
    }
  }
  
  
  # xmin and xmax  
  for(x in 1:id){
    nxx= "xmin"
    if(length(grep(nxx,asd[x,1]))>0){
      ww=x
      k=strsplit(as.character(asd[ww,]),"xmax")
      k1=strsplit(as.character(k[[1]][1]),"=")
      k2=strsplit(as.character(k[[1]][2]),"=")
      h$xmax=k2[[1]][2]
      h$xmin=k1[[1]][2]
    }
  }
  
  
  #ymin  and ymax   
  for(x in 1:id){
    nxx= "ymin"
    if(length(grep(nxx,asd[x,1]))>0){
      ww=x
      k=strsplit(as.character(asd[ww,]),"ymax")
      k1=strsplit(as.character(k[[1]][1]),"=")
      k2=strsplit(as.character(k[[1]][2]),"=")
      h$ymax=k2[[1]][2]
      h$ymin=k1[[1]][2]
      
    }
  }
  
  #Digitization
  for(x in 1:id){
    nxx= "The instrument digitizes spectral values"
    if(length(grep(nxx,asd[x,1]))>0){
      ww=x
      k=strsplit(as.character(asd[ww,]),"to")
      h$Digitization=as.numeric(k[[1]][1])
    }
  }
  
  
  #DarkCurrentCorrected   
  for(x in 1:id){
    nxx= "dark signal subtracted"
    nx="dark signal not subtracted"
    if(length(grep(nxx,asd[x,1]))>0){
      h$DarkCurrentCorrected="TRUE"
    }
    if(length(grep(nx,asd[x,1]))>0){
      h$DarkCurrentCorrected="FALSE"
    }
  }
  
  #DarkCurrentNumbers    and DarkCurrentAcquisition    
  for(x in 1:id){
    nxx= "dark measurements taken"
    if(length(grep(nxx,asd[x,1]))>0){
      ww=x
      k=strsplit(as.character(asd[ww,]),"dark measurements taken")
      h$DarkCurrentNumbers=as.factor(k[[1]][1])
      h$DarkCurrentAcquisition=as.factor(k[[1]][2])
    }
  }
  
  
  # DccValue
  for(x in 1:id){
    nxx= "DCC value was"
    if(length(grep(nxx,asd[x,1]))>0){
      ww=x
      k=strsplit(as.character(asd[ww,]),"DCC value was")
      h$DccValue=k[[1]][2]
    }
  }
  
  
  #WhitePanelReferenced   
  for(x in 1:id){
    nxx= "Data is compared to a white reference"
    nx="Data is not compared to a white reference"
    if(length(grep(nxx,asd[x,1]))>0){
      h$WhitePanelReferenced="TRUE"
    }
    if(length(grep(nx,asd[x,1]))>0){
      h$WhitePanelReferenced="FALSE"
    }
  }
  
  # ForeOptics     
  for(x in 1:id){
    nxx= "There was foreoptic attached"
    nx="There was no foreoptic attached"
    if(length(grep(nxx,asd[x,1]))>0){
      h$ForeOptics="TRUE"
    }
    if(length(grep(nx,asd[x,1]))>0){
      h$ForeOptics="FALSE"
    }
  }
  
  #RadiometricCalibration  
  for(x in 1:id){
    nxx= "Spectrum file is"
    if(length(grep(nxx,asd[x,1]))>0){
      ww=x
      k=strsplit(as.character(asd[ww,]),"is")
      h$RadiometricCalibration=k[[1]][2]
    }
  }
  
  
  #IntegrationTimeCorrected   
  for(x in 1:id){
    nxx= "Integration Time Corrected"
    if(length(grep(nxx,asd[x,1]))>0){
      h$IntegrationTimeCorrected="TRUE"
    }else{
      h$IntegrationTimeCorrected="FALSE"
    }
  }
  
  # Index.SpectralName                                                                              
  h$Index.SpectralName =sp@ShortName                                                                             
  
  # ColumnNames               
  ss=spc.cname.construct(sp)
  spc.colnames<-(ss)  
  #there is not code for rest of row not 
  # Station                                                                                     
  # Series                                                                                            
  # DataType                                                              
  # DataName                                                        
  # GeographicalCoverage                                                                  
  # FileName                 
  # RawFileIndex                                                                          
  # SaturationCorrected      
  
  spc.setheader(sp) <- h # header added to spectra
  validObject(sp)
  return(sp)
}