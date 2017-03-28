#' Constructor function for the class \code{Spectra}.
#'
#' @description
#' \code{Spectra} Creates an instance of class \code{Spectra}.
#'
#' @param inDF a long-format \code{data.frame} containing LAT,LON and TIME columns as well as Ancillary data.
#' See \code{\link{stConstruct}} for more information on long DF format.
#' @param Spectra \code{matrix} containing spectral data. Channels are in columns, observations are in rows.
#' If \code{Spectra} is missing, the first \code{length(Wavelengths)} columns of inDF will be taken
#' as spectral data.
#' @param Wavelengths \code{numeric} vector containing wavelengths of spectral channels.
#' @param Units \code{character} defining the units of the wavelengths.
#' @param space a character or integer holding the 
#' column index in inDF where the spatial coordinates are (if length(space)==2) or where the ID of 
#' the spatial location is (if (length(space)==1). If \code{space} is not provided, inDF columns are
#' searched to match one of the following : LAT,lat,latitude,LATITUDE,LON,LONG,lon,long,longitude,LONGITUDE
#' If LAT & LON are not found, they set the dummy value of 1.
#' @param time \code{character} or \code{integer} indicating the column in inDF containing POSIXct TIME
#' data values. if \code{time} is missing, it is set the dummy integer sequential vector of {1:nrow(Spectra)}.
#' @param endTime \code{character} or \code{integer} indicating the column in inDF containing POSIXct
#' ENDTIME data values. If the temporal measurements are performed over an interval, \code{time} and \code{endtime} 
#' contain the time for the start and end of intervals respectively. If the temporal measurements are performed over 
#' a time-instance, then \code{endTime==TIME}. If \code{endTime} is not provided, inDF columns are searched to match 
#' ENDTIME. If none found, then it is assumed that data are time-instance measurements. For more information, see the
#'  documentation of \pkg{spacetime}.
#' @param header \code{SpcHeader} object containing metadata
#' @param ... other input arguments to be passed to the new() function 
#' 
#' @details
#' This constructor function uses The function \code{Spectra()} calls \code{spacetime::stConstruct()}
#' that is the constructor  of the \code{STIDF} class using an input \code{data.frame} object of long-table format.
#'
#' \code{length{@@Wavelengths}==ncol(@@Spectra)}. The default @@WavelengthsUnit is nm^{-1}.
#' 
#' @return Returns an object of class \code{Spectra}.
#'
#' @examples
#' fnm = file.path(base::system.file(package = "geoSpectral"),
#' "test_data","particulate_absorption.csv.gz")
#' fnm=gsub("\\\\", "/", fnm)
#' abs = read.table(fnm,sep=",",header=TRUE)
#' abs$STATION=factor(abs$STATION)
#' abs[1:2,1:17] #Display only the first 2 rows and first 17 columns if the data frame
#' lbd = as.numeric(gsub("X","",colnames(abs)[14:514]))
#' Units="1/m"
#' colnames(abs)= gsub("X",paste("anap","_",sep=""), colnames(abs))
#' colnames(abs)= gsub("PRES","DEPTH", colnames(abs))
#' abs = abs[,c(14:514,1:13)] #Rearrange so that Spectra columns come first
#' tz<-strsplit(as.character(abs$TIME)," ")[[1]][[3]] #Extract the timezone
#' abs$TIME = as.POSIXct(as.character(abs$TIME),tz=tz) #Compute the time
#' 
#' #Space and time columns are automatically found in the column names of inDF
#' myS<-Spectra(abs,Wavelengths=lbd,Units=Units,ShortName="a_nap")
#'
#' #Space and time columns are explicitly chosen from inDF columns
#' myS<-Spectra(abs,Wavelengths=lbd, space=c("LONG","LAT"), time="TIME",
#' Units=Units,ShortName="a_nap")
#'
#' @export
Spectra = function(inDF,Spectra,Wavelengths,Units,space,time,endTime,header,...){
  longcol="";latcol="";timecol=""
  
  #Extract Wavelengths from data frame columns
  if(missing(Wavelengths)){
    Wavelengths = attr(inDF,"Wavelengths")
    lbd.idx = !is.na(Wavelengths)
    Wavelengths = Wavelengths[lbd.idx]
    
    #Extract Spectra from data frame columns
    if(missing(Spectra)){
      Spectra = as.matrix(inDF[,lbd.idx])  	
    }
  }
  #Extract Spectra from data frame columns
  if(missing(Spectra)){
    Spectra = as.matrix(inDF[,1:length(Wavelengths)])
    inDF = cbind(data.frame(idx=1:nrow(inDF)), inDF[,-(1:length(Wavelengths))])
  }
  
  if(missing(space)){
    if ("LAT" %in% names(inDF))
      latcol = "LAT"
    if ("lat" %in% names(inDF))
      latcol = "lat"
    if ("latitude" %in% names(inDF))
      latcol = "latitude"
    if ("LATITUDE" %in% names(inDF))
      latcol = "LATITUDE"
    if ("LON" %in% names(inDF))
      longcol = "LON"
    if ("LONG" %in% names(inDF))
      longcol = "LONG"
    if ("lon" %in% names(inDF))
      longcol = "lon"
    if ("long" %in% names(inDF))
      longcol = "long"
    if ("longitude" %in% names(inDF))
      longcol = "longitude"
    if ("LONGITUDE" %in% names(inDF))
      longcol = "LONGITUDE"
    
    if (!(longcol %in% names(inDF))) {
      inDF$LONG=1
      longcol="LONG"
      warning("Could not find a longitude column named either of: lon,long,LON,LONG,longitue,LONGITUDE. Assigning LONG=1.0 to all rows")
    } 
    if(!(latcol %in% names(inDF))){
      inDF$LAT=1
      latcol="LAT"
      warning("Could not find a latitude column named either of: lat,LAT,latitude,LATITUDE. Assigning LAT=1.0 to all rows")
    }
    space=c( which(longcol==names(inDF)), which(latcol==names(inDF)))
    names(inDF)[space]<-c("LON","LAT")
  }
  if(missing(time)){
    if ("time" %in% names(inDF))
      timecol = "time"
    if ("TIME" %in% names(inDF))
      timecol = "TIME"
    if (!timecol %in% names(inDF)){
      inDF$TIME=as.POSIXct(1:nrow(inDF),origin = "1970-01-01 00:00:00",tz=base::format(Sys.time(), format="%Z"))
      timecol="TIME"
      warning("Could not find a time column named either of : time or TIME. Assigning TIME=1.0 seconds to all rows")
    }
    time=timecol
  }
  if(missing(endTime)){
    if("ENDTIME" %in% names(inDF)){
      endTime = inDF$ENDTIME
    } else{
      endTime = inDF[,time]
    }
  }
  if(missing(Units)){
    #Extract Units
    Units = attr(inDF,"Units")[1]
  }
  if(missing(header)){
    #Extract Units
    header = new("SpcHeader")
  }

  #First construct a STIDF object using stConstruct()
  out = stConstruct(x=inDF,space=space,time=time,endTime=endTime)
  
  #I think stConstruct does not take endTime into account. Force it again
  out@endTime = endTime
  out = new("Spectra",out, Spectra=Spectra,Wavelengths=Wavelengths,Units=Units,header=header,...)
  validObject(out)
  return(out)
}

#' @title Conversion between \code{Spectra} and data.frame objects
#'
#' @description Converting \code{Spectra} object to data.frame is straightforward 
#' while the conversion in the opposite direction requires a set of attributes
#' to be present in the source data.frame object.
#' These attributes are generally created during the conversion of a
#' \code{Spectra} object into data.frame, they can 
#' also be manually set if they are non-existant (see the example below).
#' @param from The input object
#' @param to Name of the class of output object
#' @aliases as,Spectra
#' @rdname Spectra-coerce
#' @name Spectra-coerce
#' @examples 
#' #Convert a Spectra object to data.frame
#' sp <- spc.example_spectra()
#' df <- as(sp, "data.frame")
#' class(df); dim(df)
#' attributes(df)
#' 
#' #Convert the data.frame back to Spectra
#' sp2 <- as(df, "Spectra")
#' sp2
#' 
#' #Convert a bare data.frame to Spectra with minimal attributes
#' df2 <- data.frame(ch1=c(1,2,3,4), ch2=c(5,6,7,8), TIME=Sys.time()+1:4, LAT=1:4, LON=5:8)
#' attr(df2, "Units") <- "m-1"
#' attr(df2, "Wavelengths") <- c(500, 600)
#' attr(df2, "ShortName") <- "abs"
#' as(df2, "Spectra")
setAs(from="Spectra", to="data.frame", def=function(from){
  if(ncol(from@data)>0)
    output = cbind(as.data.frame(from@Spectra),from@data)
  
  delidx = match(c("LON","LAT","TIME","ENDTIME"),names(output))
  delidx = delidx[-which(is.na(delidx))]
  if(length(delidx)>0)
    output = output[,-delidx[!is.na(delidx)]]
  
  output$LON = from@sp@coords[,1]
  output$LAT = from@sp@coords[,2]
  output$TIME=as.POSIXct(time(from@time))
  output$ENDTIME=from@endTime
  
  attr(output,"ShortName") = from@ShortName
  attr(output,"LongName") = from@LongName
  attr(output,"Wavelengths") = from@Wavelengths
  attr(output,"Units") = from@Units
  attr(output,"header") = as(from@header,"list")
  names(attr(output,"header")) = names(from@header)
  
  return(output)
})

#' @rdname Spectra-coerce
#' @name Spectra-coerce
setAs(from="data.frame", to="Spectra", def=function(from){
  #This function makes use of geoSpectral::Spectra()
  if(!any(grepl("Wavelengths", names(attributes(from))))) 
    stop("The required data.frame attribute was not found : Wavelengths")
  
  if(!any(grepl("Units", names(attributes(from)))))
    stop("The required data.frame attribute was not found : Units")
  
  if(!any(grepl("ShortName", names(attributes(from)))))
    stop("The required data.frame attribute was not found : ShortName")
  
  Wavelengths= attr(from, "Wavelengths") 
  Units=attr(from,"Units") 
  ShortName = attr(from, "ShortName")
  
  if (any(grepl("LongName", names(attributes(from))))){
    LongName = attr(from, "LongName")
  } else {
    LongName = ShortName
  }
  
  #Create Spectra matrix
  Spectra = as.matrix(from[,1:length(Wavelengths)])
  if(prod(dim(Spectra))==0)
    stop("The Spectra matrix is empty. Cannot create a spectra object")
  if(!all(sapply(Spectra,class)=="numeric"))
    stop(paste("Cannot create a numeric matrix from", length(Wavelengths),
               "columns of the input data.frame. Cannot create a spectra object"))
  
  #Create ancillary data.frame
  if (ncol(from)>length(Wavelengths)) {
    myidx = (length(Wavelengths)+1):ncol(from)
    #Suppress warnings for the below operation ("drop" creates warnings)
    myWarn = options()$warn
    options(warn=-1)
    data = from[myidx,drop=F]
    options(warn=myWarn)				
  } else {
    data = data.frame(1:nrow(Spectra))
  }
  
  #Extract the header
  if(!is.null(attr(from,"header")))
    header = as(attr(from,"header"),"SpcHeader")
  else
    header = new("SpcHeader")
  
  if(!xts::is.timeBased(from$TIME))
    stop("The TIME column does not contain time-based data")
  TIME = xts::xts(1:length(from$TIME), from$TIME)
  if(!xts::is.timeBased(from$ENDTIME)){
    endTime = from$TIME
  }else{
    endTime = from$ENDTIME
  }
  outS =geoSpectral::Spectra(data,Spectra,Wavelengths,Units=Units,
                          header=header,ShortName=ShortName,LongName=LongName)
  #			outS = new("Spectra", time = TIME, endTime = endTime,
  #					Spectra=Spectra, data=data,
  #					Wavelengths=Wavelengths, Units=Units[1], 
  #					LongName = LongName, ShortName = ShortName,header=header)
  
  validObject(outS)
  return(outS)
})

#' Dimensions of a \code{Spectra} object.
#'
#' @description
#' Gives number of dimension of a \code{Spectra} object
#'
#' @param x A \code{Spectra} object
#' 
#' @return Returns a numeric vector containing \code{nrow} and \code{ncol} of the \code{Spectra} object.
#'
#' @examples
#' sp<-spc.example_spectra()
#' dim(sp)
#' 
#' @export
setMethod("dim", signature = "Spectra", 
          def = function (x){
            return(dim(x@Spectra))  
          })
#########################################################################
# Method : ncol
#########################################################################
#' The Number of Columns  of a Spectra object
#'
#' @description
#' \code{nrow} and \code{ncol} return the number of rows or columns of a \code{Spectra} object 
#'
#' @param  x A \code{Spectra} object 
#'
#' @examples
#' x <- spc.example_spectra() 
#' ncol(x)  #501 
#' nrow(x)  #26
#' 
#' @export
setMethod("ncol", signature = "Spectra", 
          def = function (x){  return(ncol(x@Spectra))  })

########################################################################
# Method : nrow
#########################################################################
#' The Number of rows  of a \code{Spectra} object
#'
#' @description
#' \code{nrow} and \code{ncol} return the number of rows or columns present in a \code{Spectra} object 
#'  
#' @param  x a \code{Spectra} object 
#'
#' @examples
#' x <- spc.example_spectra() 
#' ncol(x)  #501 
#' nrow(x)  #26
#' 
#' @export
setMethod("nrow", signature = "Spectra", 
          def = function (x){  return(nrow(x@Spectra))  })
#########################################################################
# Method : names
#########################################################################
#' The Names of a \code{Spectra} object
#'
#' @description
#'  Retrieve  the names of \code{Spectra} object 
#' @param  x  a \code{Spectra} object
#' @examples
#' 
#' x <- spc.example_spectra() 
#' names(x)
#' 
#' @export
setMethod("names", signature = "Spectra", 
          def = function (x){ 
            #			if(ncol(x@data)>1)
            return(c(colnames(x@Spectra),names(x@data)))
            #			else                
            #				return(names(x@data)) 
          })

setGeneric("endTime",function(x){standardGeneric("endTime")})
setMethod("endTime", signature = "Spectra", def = function (x){  
  return(x@endTime)
})

#########################################################################
# Method : head
#########################################################################
#' Return the first or last part of a \code{Spectra} object
#'
#' @description
#' Return the first or last parts of a \code{Spectra} object 
#'
#' @param  x a \code{Spectra} object
#' @param  ... arguments to be passed to or from other methods
#'  
#' @return Returns a matrix (\code{Spectra} data)
#'
#' @examples
#' x <- spc.example_spectra()
#' head(x)
#' 
#' @export
setMethod("head", signature = "Spectra", 
          def = function (x, ...){  return(head(x@Spectra, ...)) })
#########################################################################
# Method : show
#########################################################################
#' Show a \code{Spectra} object
#'
#' @description Display a \code{Spectra} object 
#'
#' @param object a \code{Spectra} object 
#' @return  show returns an invisible \code{NULL}
#'
#' @examples
#' x <- spc.example_spectra()
#' show(x)
#' 
#' @export
setMethod("show", "Spectra", function(object){
  if(ncol(object)==0){
    LongName = character()
    Units = character()
    LbdStr = character()
  } else {
    LongName = object@LongName[1]
    Units = object@Units[1]
    LbdStr = paste("[",min(object@Wavelengths),",",max(object@Wavelengths), "] ->",sep=" ")                    
  }
  bbx=bbox(object@sp)
  if (length(object@time)>1){
    period = paste(as.character(xts::periodicity(object@time))[1],
                   as.character(xts::periodicity(object@time))[5])
    timerange = as.character(range(time(object@time)),usetz=F)
    tz = format(time(object@time[1]),format="%Z")
    timestr = paste("Time : periodicity of ", period, " between (", 
                    timerange[1]," - ",timerange[2],"), tz=", tz ,sep="")
  } 
  if (length(object@time)==1) { 
    timestr = paste("Time : ", as.character(time(object@time),usetz=T))
  }
  if (length(object@time)==0) { 
    timestr = paste("Time : NA")
  }
  if(ncol(object)==0)
    Str = c("\n","Empty Spectra object","\n")
  else
    Str = c("\n", paste(object@ShortName[1], ' : An object of class "Spectra"\n', 
                        length(object@Wavelengths),"spectral channels in columns and", nrow(object@data), 
                        "observations in rows"), "\n",
            "LongName: ", LongName, "\t", "Units: ", Units, "\n",
            "Wavelengths : ", length(object@Wavelengths), "channels with units of",object@WavelengthsUnit,  LbdStr, head(object@Wavelengths)," ...\n",
            "Spectra Columns: ", head(colnames(object@Spectra)), "...\n",
            "Ancillary Columns: ", head(names(object@data)),"...\n",
            "Bounding box:", "LON(",format(bbx[1,],digits = 7),") LAT(",format(bbx[2,],digits = 7),")\n",
            timestr, "\n")
  if(length(object@Wavelengths)==1)
    Str = gsub("channels","channel",Str)
  cat(Str)
})		

#########################################################################
# Method : $
#########################################################################
#' Extract or replace parts of a \code{Spectra} object
#'
#' @description
#' Operators acting on \code{Spectra} objects to extract parts
#' 
#' @param x A \code{Spectra} object from which to extract element(s) or in which to replace element(s)
#' @param i A numeric (row index) variable
#' @param j A character (column name) or a numeric (column index) variable
#' @param name A character (column name) or a numeric (column index) variable
#' @param value A vector or matrix or data.frame. Values to be replaced with matched \code{Spectra} column.
#' @examples
#'  sp<-spc.example_spectra()
#'  # spc.colnames() is used to extract column names
#'  head(spc.colnames(sp))
#'  head(sp$anap_300)
#'  sp[,"anap_345"]
#'  sp[,"anap_345"] #returns Spectra object with only one channel (column)
#'  sp[1:3,"anap_345"] #returns Spectra object with first 3 rows and only one channel (column)
#' @export
#' @rdname Spectra-Access
#' @aliases $,Spectra
setMethod("$", signature="Spectra", function(x, name) {
  if (name %in% colnames(x@Spectra)){
    Boutput = x@Spectra[,name]
  } 
  if (name %in% names(x@data)){
    Boutput = x@data[,name]				
  }
  if(!exists("Boutput"))
    stop("Could not match any Spectral or Ancillary (@data) columns")
  return(Boutput)
})

#' Replace parts of a \code{Spectra} object
#'
#' @examples
#'  # spc.colnames() is used to extract column names
#'  head(spc.colnames(sp))
#'  head(sp$anap_300)
#'  sp[,"anap_345"]
#' @rdname Spectra-Access
#' @aliases $<-,Spectra
setReplaceMethod("$", signature = "Spectra",  function(x, name, value) {
  x[[name]]=value
  #validObject(x) will be called by the [[ method
  return(x)
})

#########################################################################
# Method : spc.colnames
#########################################################################
#' Column names of \code{Spectra} object
#'
#' @description
#' Set or retrieve column names of a \code{Spectra} object
#'
#' @param x  A \code{Spectra} object
#' @param value character vector containing new column names to be assigned
#' @return spc.colnames() returns the column  names of an object of class \code{Spectra} 
#' as a character vector. spc.colnames()<- returns a \code{Spectra} object.
#'
#' @examples
#' x <- spc.example_spectra()
#' head(spc.colnames(x))
#' # or 
#' spc.colnames(x) <- spc.cname.construct(x)
#' spc.colnames(x)
#' @seealso \code{\link{spc.cname.construct}}
#' @rdname spc.colnames
#' @export
setGeneric("spc.colnames",function(x){standardGeneric("spc.colnames")})

#' @rdname spc.colnames
setMethod("spc.colnames", signature = "Spectra", 
          def = function (x){ return(colnames(x@Spectra)) })

#' Set column names of a \code{Spectra} object
#' @rdname spc.colnames
#' @export
setGeneric("spc.colnames<-",function(x,value){standardGeneric("spc.colnames<-")})

#' @rdname spc.colnames
setReplaceMethod("spc.colnames", signature = "Spectra", def = function (x,value){
  colnames(x@Spectra) = value
  validObject(x)
  return(x) 
})


#########################################################################
# Method : spc.plot
#########################################################################
#'  Plotting \code{Spectra} object
#'
#' @description
#' Generating plot of the intensity of a measurement inside a \code{Spectra} object with respect to the wavelength.
#'
#' @usage 
#' spc.plot(x, Y, maxSp, lab_cex,xlab,ylab,type,pch,lwd,cex,...)
#' @param x and Y	 a \code{Spectra} data 
#' @param Y fskjldsk
#' @param xlab title for x  axix, as in plot().
#' @param ylab title for y axis, as in plot().
#' @param pch character string or vector of 1-characters or integers for plotting characters.
#' See help of \code{\link{par}}.
#' @param ...  any further arguments to be passed to matplot
#' @param lab_cex vector of character expansion sizes, used cyclically
#' @param lwd vector of line widths. See help of \code{\link{par}}. 
#' @param maxSp maximum number of \code{Spectra} to plot
#' @param cex A numerical value giving the amount by which plotting text and symbols should 
#' be magnified relative to the default. See help of \code{\link{par}}.
#' @param type character string (length 1 vector) or vector of 1-character strings indicating 
#' the type of plot for each column of y. See help of matplot() or plot().
#' 
#' @seealso \code{\link{spc.lines}}, \code{\link{par}}
#' @examples
#' x <- spc.example_spectra()
#' spc.plot(x)
#' 
#' @rdname spc.plot
#' @export
setGeneric("spc.plot",function(x,Y,maxSp,lab_cex,xlab,ylab,type="l",pch=19,lwd=2,cex=0.3,...){standardGeneric("spc.plot")})

#' @rdname spc.plot
setMethod("spc.plot", "Spectra", function (x, Y, maxSp, lab_cex,xlab,ylab,type="l",pch=19,lwd=2,cex=0.3,...){						
  if (length(x@InvalidIdx)==0)
    x@InvalidIdx = rep(FALSE,nrow(x@Spectra))
  
  if(!missing(maxSp) && ncol(x)>maxSp)
    idx = seq(1,nrow(x),length.out=maxSp	)
  else
    idx = 1:nrow(x)

  Xidx = rep(FALSE, nrow(x@Spectra))
  Xidx[idx] = TRUE
  
  #if(any(x@InvalidIdx)){
  #  Xidx[x@InvalidIdx]=FALSE
  #}
  #			if(any(x@SelectedIdx)){
  #				mycol = rep("gray", nrow(x@Spectra))
  #				mycol[x@SelectedIdx]="red"
  #			} else
  
  #			if(missing(col)) 
  #				col = 1:10
  
  x@Units = gsub("\\[\\]","",x@Units)
  x@Units = gsub("\\[ \\]","",x@Units)
  
  if(missing(lab_cex))
    lab_cex = 1
  
  YY = x@Spectra[Xidx,]
  if(class(YY)=="matrix" && nrow(YY)!=length(x@Wavelengths))
    YY = t(YY)
  
  inargs_in <- list(...)
  inargs_out <- c(list(x=x@Wavelengths,y=YY), inargs_in)

  if(! ("xlim" %in% names(inargs_in))){
    xlim = range(x@Wavelengths)
    if (x@WavelengthsUnit=="cm-1")
      xlim = rev(xlim)
    inargs_out$xlim <- xlim
  }
  
  # if("ylim" %in% names(inargs_in))
  #   inargs_out <- c(list(ylim=inargs_in$ylim), inargs_out)
  inargs_out <- c(list(xlab=""), inargs_out)
  inargs_out <- c(list(ylab=""), inargs_out)
  if(! ("type" %in% names(inargs_in)))
    inargs_out <- c(list(type=type), inargs_out)
  if(! ("pch" %in% names(inargs_in)))
    inargs_out <- c(list(pch=pch), inargs_out)
  if(! ("cex" %in% names(inargs_in)))
    inargs_out <- c(list(cex=cex), inargs_out)
  if(! ("cex.axis" %in% names(inargs_in)))
    inargs_out <- c(list(cex.axis=lab_cex), inargs_out)
  if(! ("lwd" %in% names(inargs_in)))
    inargs_out <- c(list(lwd=lwd), inargs_out)

  do.call(matplot, inargs_out)
  # matplot(x@Wavelengths,YY,#lab=x@Wavelengths,#xaxt="n",
  #         ylab= "",xlab="",type=type,xlim=xlim,pch=pch,cex=cex,cex.axis=lab_cex,lwd=lwd,...)

  if((!("ylab" %in% names(inargs_in))))
    ylab = bquote(.(x@ShortName)*", ["*.(x@Units[1])*"]")
  else
    ylab=inargs_in$ylab
  
  if((!("xlab" %in% names(inargs_in))))
    xlab=bquote("Wavelength ["*.(x@WavelengthsUnit)*"]")
  else
    xlab=inargs_in$xlab
  
  mtext(xlab,side=1,line=2,cex=lab_cex)			
  mtext(ylab,side=2,line=2,cex=lab_cex)
  
  abline(h=0)
  grid(col="black")
})

#########################################################################
# Method : spc.lines
#########################################################################
#'  Add spectra to an existing plot
#'
#' @description
#' Adds spectra to an existing plot created by spc.plot() using lines()
#'
#' 
#' @usage 
#' spc.lines(x,...)
#' @param x	 An object of class \code{Spectra}
#' @param ... Additional input arguments to be passed to lines()
#' 
#' @seealso \code{\link{spc.plot}}
#' @examples 
#' sp = spc.example_spectra()
#' spc.plot(sp[2,])
#' spc.lines(sp[3,],col="red")
#' 
#' @rdname spc.lines
#' @export
setGeneric("spc.lines",function(x,...){standardGeneric("spc.lines")})

#' @rdname spc.lines
setMethod("spc.lines",signature = "Spectra",definition = function(x,...){
  a=sapply(1:nrow(x@Spectra), function(S) {
    lines(x@Wavelengths, x@Spectra[S,],...)})
})

#########################################################################
# Method : spc.rbind
#########################################################################
#' Combine \code{Spectra} Objects by Rows
#'
#' @description
#' Take a \code{Spectra} objects and combine by rows
#'
#' @param ... \code{Spectra} object
#' @param compressHeader Compress the header (make multiple all-equal header elements as ONE, default value is TRUE	
#' @return  \code{Spectra} object 
#' @examples
#' x <- spc.example_spectra()
#' nrow(x)  #[1] 26
#' x2=spc.rbind(x,x)
#' nrow(x2)  #[1] 52
#' 
#' @rdname spc.rbind
#' @export
setGeneric (name= "spc.rbind",def=function(...){standardGeneric("spc.rbind")})

#' @rdname spc.rbind
setMethod("spc.rbind", signature = "Spectra", def = function (...,compressHeader=TRUE){
  #Check that column names match
  DFL=sapply(list(...),function(x) names(x@data),simplify=F)
  if(!all(sapply(1:length(DFL),function(x) all(DFL[[x]]==DFL[[1]]))))
    stop("Names of all Ancillary data columns should be the same")
  
  #Check that column names match
  DFL=sapply(list(...),function(x) colnames(x@Spectra),simplify=F)
  if(!all(sapply(1:length(DFL),function(x) all(DFL[[x]]==DFL[[1]]))))
    stop("Names of all Spectral data columns should be the same")
  
  #Check that the number of columns match 
  DFL=sapply(list(...), function(x) ncol(x@Spectra),simplify=F)
  if(!all(sapply(1:length(DFL),function(x) all(DFL[[x]]==DFL[[1]]))))
    stop("All Spectra arrays should have the same number of columns")
  DFL=sapply(list(...), function(x) ncol(x@data),simplify=F)
  if(!all(sapply(1:length(DFL),function(x) all(DFL[[x]]==DFL[[1]]))))
    stop("All Ancillary arrays should have the same number of columns")
  
  DFL=sapply(list(...),spc.getwavelengths)
  #Check that all Wavelengths are equal
  if(!all(apply(DFL,1,diff)==0))
    stop("Wavelengths of all input Spectra objects should be the same")
  #Create the output variable
  outt = ..1
  
  #Error if does not contain SpatialPoints
  if(class(outt@sp)!="SpatialPoints")
    stop("Only support ST* inherited object based on SpatialPoints")
  
  #Get a list of all input arguments
  allinargs = aa=match.call(expand.dots = F)$...
  
  if(length(allinargs)>1){
    #For all input arguments
    for(I in 2:length(allinargs)){
      if(!inherits(eval((allinargs[[I]])),"STI"))
        stop("The input argument should inherit from class STI")
      #Get the slot Names
      sltn = slotNames(..1)
      #Slots to omit in the rbind process 
      sltn = sltn[sltn!="ShortName"]
      sltn = sltn[sltn!="LongName"]
      sltn = sltn[sltn!="Wavelengths"]
      sltn = sltn[sltn!="WavelengthsUnit"]
      sltn = sltn[sltn!="Units"]
      #For all slots
      for(J in 1:length(sltn)){
        myslot = slot(eval((allinargs[[I]])),sltn[J])
        if(class(myslot)[1]=="SpcHeader"){
          aa=rbind(as.data.frame(slot(outt,sltn[J]),stringsAsFactors=F), as.data.frame(myslot,,stringsAsFactors=F))
          rownames(aa)=NULL
          bb = as.list(aa)
          bb = lapply(bb,function(x){names(x)<-NULL;x})
          outt@header = as(bb,"SpcHeader")
        }
        #					if (length(myslot)==0)
        #						myslot=NA
        if(class(myslot)[1]=="matrix"|class(myslot)[1]=="data.frame")
          slot(outt,sltn[J])<- rbind(slot(outt,sltn[J]),myslot)
        if(class(myslot)[1]=="logical"|class(myslot)[1]=="numeric"|
             class(myslot)[1]=="character"|class(myslot)[1]=="POSIXct")
          if(class(myslot)[1]=="POSIXct"){
            mytz = attr(outt@endTime,"tzone")
            slot(outt,sltn[J])<-as.POSIXct(as.POSIXlt(c(slot(outt,sltn[J]),myslot),tz=mytz))
          }
        if(class(myslot)[1]=="xts"){
          slot(outt,sltn[J])<-c(slot(outt,sltn[J]),myslot)
          slot(outt,sltn[J])<-xts::xts(1:length(slot(outt,sltn[J])),time(slot(outt,sltn[J])))
        }	
        if(class(myslot)[1]=="SpatialPoints"){
          prj = slot(outt,sltn[J])@proj4string
          if (!identical(prj@projargs,myslot@proj4string@projargs))
            stop("proj4strings do not match!")
          #rbind the coordinates
          coords = rbind(coordinates(slot(outt,sltn[J])),coordinates(myslot))
          #Create a SpatialPoints object
          slot(outt,sltn[J])<-SpatialPoints(coords,proj4string=prj)
        }
      } #end for all slots
    } #end for all input arguments
  } #end for if(length(allinargs)>1)
  #Compress the header (make multiple all-equal header elements as ONE)
  if(compressHeader){
    for(J in names(outt@header)){
      if(length(outt@header[[J]])>1){
        myO = sapply(2:length(outt@header[[J]]),function(x){
          outt@header[[J]][x]==outt@header[[J]][1]})
        try(if(all(myO)) outt@header[[J]]=outt@header[[J]][1],silent=T)
        if(all(is.na(outt@header[[J]])))
          outt@header[[J]]=outt@header[[J]][1]
      }
    }
  }
  validObject(outt)
  return(outt) 
})

#########################################################################
# Method : spc.rbind
#########################################################################
#' Combine \code{STIDF} objects by Rows
#'
#' @description
#' Take a \code{STIDF} objects and combine by rows
#'
#' @param ... \code{STIDF} object
#'
#' @examples
#' x <- spc.example_spectra()
#' nrow(x)  #[1] 26
#' x2 <- spc.rbind(as(x, "STIDF"),as(x, "STIDF"))
#' nrow(x2)  #[1] 52
#' 
#' @export
setMethod("spc.rbind", signature = "STIDF", def = function (...){
  #Create the output variable
  outt = ..1
  
  #Get a list of all input arguments
  allinargs = aa=match.call(expand.dots = F)$...
  
  #For all input arguments
  for(I in 2:length(allinargs)){
    #Get the slot Names
    sltn = slotNames(outt)
    
    #Error if does not inherit from STI or contain SpatialPoints 
    if(class(eval(allinargs[[I]])@sp)!="SpatialPoints")
      stop("Only support ST* inherited object based on SpatialPoints")				
    if(!inherits(eval((allinargs[[I]])),"STI"))
      stop("The input argument should inherit from class STI")
    #For all slots
    for(J in 1:length(sltn)){
      myslot = slot(eval((allinargs[[I]])),sltn[J])
      if(class(myslot)[1]=="matrix"|class(myslot)[1]=="data.frame")
        slot(outt,sltn[J])<- rbind(slot(outt,sltn[J]),myslot)
      if(class(myslot)[1]=="logical"|class(myslot)[1]=="numeric"|
           class(myslot)[1]=="character"|class(myslot)[1]=="POSIXct")
        if(class(myslot)[1]=="POSIXct"){
          mytz = attr(outt@endTime,"tzone")
          slot(outt,sltn[J])<-as.POSIXct(as.POSIXlt(c(slot(outt,sltn[J]),myslot),tz=mytz))
        }
      if(class(myslot)[1]=="xts"){
        slot(outt,sltn[J])<-c(slot(outt,sltn[J]),myslot)
        slot(outt,sltn[J])<-xts::xts(1:length(slot(outt,sltn[J])),time(slot(outt,sltn[J])))
      }
      if(class(myslot)[1]=="SpatialPoints"){
        prj = slot(outt,sltn[J])@proj4string
        if (!identical(prj@projargs,myslot@proj4string@projargs))
          stop("proj4strings do not match!")
        #rbind the coordinates
        coords = rbind(coordinates(slot(outt,sltn[J])),coordinates(myslot))
        #Create a SpatialPoints object
        slot(outt,sltn[J])<-SpatialPoints(coords,proj4string=prj)
      }
    } #end for all slots
  } #end for all input arguments			
  validObject(outt)
  return(outt) 
})

#########################################################################
# Method : spc.getwavelengths
#########################################################################
#' Extract wave lenghts of a \code{Spectra} object
#'
#' @description
#' Get wave lenghts inside of  a \code{Spectra} object
#'
#' @usage 
#' spc.getwavelengths(object)
#'
#' @param object A \code{Spectra} object
#' 
#' @return numeric vector of  wave lengths
#' @seealso \code{\link{spc.setwavelengths<-}}
#' @examples
#'  x <- spc.example_spectra()
#'  spc.getwavelengths(x)
#'  
#' @rdname spc.getwavelengths
#' @export
setGeneric (name= "spc.getwavelengths",def=function(object){standardGeneric("spc.getwavelengths")})

#' @rdname spc.getwavelengths
setMethod("spc.getwavelengths", signature = "Spectra", def = function (object){
  return(object@Wavelengths)
})

#########################################################################
# Method : spc.setwavelengths
#########################################################################
#' Setting wavelengths  in a \code{Spectra} object
#'
#' @description
#' Function  to change or set wavelengths  inside  of  a \code{Spectra} object
#'
#' @param object A \code{Spectra} object
#' @param value Numeric 
#' 
#' 
#' @seealso \code{\link{spc.getwavelengths}}
#' 
#'
#' @examples
#'  x <- spc.example_spectra()
#'  show(x)
#'  spc.setwavelengths(x) <- 300:800
#'  show(x)
#' @rdname spc.setwavelengths
#' @export
setGeneric("spc.setwavelengths<-",function(object,value){standardGeneric("spc.setwavelengths<-")})

#' @rdname spc.setwavelengths
setReplaceMethod(f="spc.setwavelengths", signature="Spectra",definition=function(object,value){
  object@Wavelengths <-value
  validObject(object)
  return (object)
})

#########################################################################
# Method : spc.cname.construct
#########################################################################
#' Generating column names for a \code{Spectra} object
#' @description
#'Function for a \code{Spectra} object that generates column names made of a 
#'combination of @shortName and @Wavelenght slots. If \code{value} is 
#'omitted, the @ShortName slot is used.
#'
#' @usage 
#' spc.cname.construct(object, value)
#'
#' @param value A character object
#' @param object A variable of class \code{Spectra}
#' 
#' @return vector of characters
#' @examples 
#' sp <- spc.example_spectra()
#' spc.cname.construct(sp)
#' spc.cname.construct(sp,"Newvar")
#' 
#' @rdname spc.cname.construct
#' @export
setGeneric("spc.cname.construct",function(object,value){standardGeneric("spc.cname.construct")})

#' @rdname spc.cname.construct
setMethod(f="spc.cname.construct", signature="Spectra",definition=function(object,value){
            if(missing(value))
              value = object@ShortName
            return(paste(value,round(spc.getwavelengths(object)),sep="_"))
          })

##########################################################################
#spc.timeMatch
##########################################################################
#' Match two time sequences
#' @description
#' Match two time sequences for a \code{Spectra} object, where each can be intervals or instances.
#'
#' @usage 
#' spc.timeMatch(master,searched,returnList,method,limits,report)
#'
#' @param master ordered sequence of variable of class \code{Spectra}
#' @param searched A variable of class \code{Spectra}which is searched
#' @param returnList Boolean; should a list be returned with all matches (TRUE), or a vector with single matches (FALSE)?
#' @param method Method used in time-based matching. See the details section.
#' @param limits the interval limits
#' @param report return character string which has information about searching results, default is False
#' @details 
#' spc.timeMatch is similar to spacetime::timeMatch(), only adding some more matching methods.
#' When method is "over", the same technique used by spacetime::timeMatch() is used. Useful when
#' matched timestamps of both master and searched are exactly equal.
#' When method is "nearest", the nearest measurement will be found, 
#' matching only one data for ALL elements of master.
#' When method is "within", measurements that are within the interval limits=c(upper,lower) (in seconds) 
#' will be found.
#' @examples 
#' #Read the Nomad database inside a SpcList object.
#' dat = SpcList(spc.Read_NOMAD_v2())
#' 
#' #Different list elements containt different parameters
#' names(dat)
#' 
#' #We would like to find elements of Es that match time-wise rows of Kd.
#' nrow(dat$kd); nrow(dat$es)
#' 
#' #Use spc.timeMatch() to get row indexes of Es that would match those of Kd time-wise
#' t_idx=spc.timeMatch((dat$kd), (dat$es))
#' #Verification
#' all(time(dat$es)[t_idx]==time(dat$kd))
#' 
#' 
#' @export
spc.timeMatch = function(master,searched,returnList=FALSE,method="over",limits,report=FALSE) {
  if(!is.timeBased(master))
    if(!(inherits(master,"ST")) & is.timeBased(master))
      stop("Input argument 'master' needs to either inherit from spacetime::ST class or be a timeBased variable")
  stopifnot(inherits(master,"ST"))
  if(!is.timeBased(searched))
    if(!(inherits(searched,"ST")) & is.timeBased(master))
      stop("Input argument 'searched' needs to either inherit from spacetime::ST class or be a timeBased variable")
  stopifnot(inherits(searched,"ST"))
  if(method=="over")
    browser()
    out = spacetime::timeMatch(time(master),time(searched),returnList=returnList)
  if(method=="nearest"){
    out = sapply(time(master),function(x){mymin = which.min(abs(time(searched)-x))})
    if(returnList)
      out = lapply(out,function(x)x)
  }
  if(method=="within"){
    if(missing(limits))
      stop(simpleError("The input argument 'limits' is required if method=='within'"))
    if(length(limits)==0 || length(limits)>2)
      stop(simpleError("The input argument 'limits' needs to have a length of 1 or 2"))
    if(length(limits)==1)
      limits = c(limits,limits)
    out = which(time(searched)>time(master)[1]-limits[1] & 
                  time(searched)<master@endTime[length(master)]+limits[2])
  }
  if(report){
    print(paste(time(master)[1],master@endTime[length(master)],paste(out,collapse=" ")))
  }
  return(out)
}
#' Report the space and time distance of each row of an STI-inherited object
#' @description
#' Function that reports the space and time distance of each 
#' row of the STI-inherited object \code{searched} to the corresponding row of the 
#' STI-inherited object \code{master}
#'
#' @param master  An STI-inherited object
#' @param searched An STI-inherited object
#' @param report Logical. Default value is FALSE
#' 
#' @details 
#' Reports the space and time distance of each row of the STI-inherited object
#' \code{searched} to the corresponding row of the STI-inherited object \code{master}. 
#' 
#' @return Outputs a data.frame, with two columns : time2master ("difftime", in seconds) and 
#' distance2master ("numeric", in meters) 
#' 
#' @export
spc.STI.stdistance = function(master,searched,report=F){
  stopifnot(length(master)==length(searched))
  
  if(inherits(master,"STI"))
    mastertime = time(master)
  if(is.timeBased(master))
    mastertime = master	
  if(inherits(searched,"STI"))
    searchedtime = time(searched)
  if(is.timeBased(searched))
    searchedtime = searched
  output =  difftime(searchedtime,mastertime,units="secs")
  output = data.frame(time2master = as.numeric(output))
  
  if(inherits(master,"STI") && inherits(searched,"STI"))
    distn = sapply(1:length(master), function(x) {
      spDistsN1(t(as.matrix(coordinates(master)[x,])),t(as.matrix(coordinates(searched)[x,])))*1000
    })
  output = cbind(output,data.frame(distance2master=distn))
  
  if(report){
    a=hist(distn,breaks=50);a$breaks
    plot(master@sp)
    lines(spc.bbox2lines(master@sp))
  }	
  return(output)
}


#########################################################################
# Method : Arith
#########################################################################
#' Apply arithmetic operations on and between \code{Spectra} objects.
#' @description
#' Methods defining Arithmetic and Math operations between two \code{Spectra} objects e1 and e2 or one
#' \code{Spectra} object e1 and a numeric value.
#'
#' @param e1 spectra object 
#' @param e2 spectra object or other
#' @param x spectra object 
#' @details 
#' These methods allow performing arithmetic operations involving \code{Spectra} objects.
#' @seealso \code{\link{Arith}}
#' @rdname Spectra-Arith
setMethod("Arith", signature(e1 = "Spectra", e2 = "Spectra"),function (e1, e2) {
  result <- methods::callGeneric(e1@Spectra, e2@Spectra)
  output = e1
  output@Spectra = result
  validObject(output)
  return(output)
})

#' @rdname Spectra-Arith
setMethod("Arith", signature(e1 = "Spectra", e2 = "numeric"),function (e1, e2) {
  result <- callGeneric(e1@Spectra, e2)
  output = e1
  output@Spectra = result
  validObject(output)
  return(output)
})

#' @rdname Spectra-Arith
setMethod("Math", signature("Spectra"),function (x) {
  x@Spectra <- callGeneric(x@Spectra)
  validObject(x)
  return(x)
})
#############################################################
#spc.colMeans
#############################################################
#' Computes the mean along the rows of a \code{Spectra} object
#' @description
#' Computes the mean along the rows of a \code{Spectra} object. The method finds the measurement 
#' closest in time to the mean time and keeps the spatial/time attributes as well as Ancillary
#' data table (@data) associated to that measurement as that of the mean spectra
#' @usage 
#' spc.colMeans(object)
#'
#' @param object a \code{Spectra} object 
#' @examples 
#' sp=spc.example_spectra()
#' spc.colMeans(sp)
#' @rdname spc.colMeans
#' @export
setGeneric (name= "spc.colMeans",def=function(object){standardGeneric("spc.colMeans")})

#' @rdname spc.colMeans
setMethod("spc.colMeans", signature("Spectra"),function (object) {
  object@Spectra <- t(as.matrix(colMeans(object@Spectra)))
  #			object@data <- as.data.frame(t(callGeneric(object@data)))
  #Find the mean time
  meantime <- xts::xts(1,mean(time(object@time)),tzone=attr(object@time,"tzone"))
  #Find the row index closer in time to meantime
  min.idx = which.min(abs(as.numeric(time(meantime)-time(object@time))))
  object@sp <- object@sp[min.idx]
  object@time <-object@time[min.idx]
  object@data <- object@data[min.idx,,F]
  object@endTime <- mean(object@endTime)
  object@InvalidIdx <- logical()
  object@SelectedIdx <- logical()
  validObject(object)
  return(object)
})
#######################################################################
#spc.bbox2lines
######################################################################
#'  Constructs a rectangle with a \code{Spectra} object
#' @description
#' Constructs a rectangle of sp::Lines using the bounding box of a \code{Spectra} object.
#' @usage 
#' spc.bbox2lines(object)
#'
#' @param object spectra object t 
#' 
#' @examples 
#' sp=spc.example_spectra()
#' spc.bbox2lines(sp)
#' 
#' @rdname spc.bbox2lines
#' @export
setGeneric (name= "spc.bbox2lines",def=function(object){standardGeneric("spc.bbox2lines")})

#' @rdname spc.bbox2lines
setMethod("spc.bbox2lines",signature="Spatial",definition=function(object){
  bb = bbox(object)
  pt = bb[,1]
  pt = rbind(pt, c(bb[1,1],bb[2,2]))
  pt = rbind(pt, c(bb[1,2],bb[2,2]))
  pt = rbind(pt, c(bb[1,2],bb[2,1]))
  #				pt = rbind(pt, bb[,1])
  row.names(pt)<-NULL
  out = Lines(list(Line(pt[1:2,]),Line(pt[2:3,]),
                   Line(pt[3:4,]), Line(pt[c(4,1),])),ID="spc.bbox2lines")
  return(out)
})

#' @rdname spc.bbox2lines
setMethod("spc.bbox2lines",signature="STI",definition=function(object){
  return(callGeneric(object@sp))
})
#' @rdname spc.bbox2lines
setMethod("spc.bbox2lines",signature="Spectra",definition=function(object){
  return(callGeneric(object@sp))
})

#########################################################################
# Method : spc.invalid.detect
#########################################################################
#' Determinate invalid rows of a \code{Spectra} object
#' @description
#' Determine invalid rows (records) of a \code{Spectra} \code{SpcList} object
#'
#' @usage 
#' spc.invalid.detect(source1)
#' @return logical. TRUE for invalid rows
#' @param source1 A  \code{Spectra} object 
#' @examples 
#' sp=spc.example_spectra()
#' nrow(sp)
#' invalid=spc.invalid.detect(sp)
#' show(invalid); length(invalid)
#' 
#' BL = spc.makeSpcList(sp,"CAST")
#' invalid=spc.invalid.detect(BL)
#' show(invalid)
#' 
#' @rdname spc.invalid.detect
#' @export
setGeneric(name= "spc.invalid.detect",
           def=function(source1){standardGeneric("spc.invalid.detect")})

#' @rdname spc.invalid.detect
setMethod("spc.invalid.detect", signature = "Spectra", def=function(source1){
  out = apply(source1@Spectra, 2,is.na)
  if(is.null(dim(out))& nrow(source1@Spectra)==1)
    dim(out)<-c(1,ncol(source1@Spectra))
  out = apply(out,1,all)
})

#########################################################################
# Method : spc.getheader
#########################################################################
#' Extract a field of the @header slot of a \code{Spectra} object
#' @description
#' Extracts the value of a field in the header slot of \code{Spectra} object
#'
#' @usage 
#' spc.getheader(object,name)
#'
#' @seealso \code{\link{spc.setheader<-}}
#' 
#' @param object  A  \code{Spectra} object 
#' @param name of the header field to be extracted
#' 
#' @examples 
#' sp=spc.example_spectra()
#' sp@header
#' spc.getheader(sp,"Latitude")
#' @rdname spc.getheader
#' @export
setGeneric (name= "spc.getheader", def=function(object,name){standardGeneric("spc.getheader")})

#' @rdname spc.getheader
setMethod("spc.getheader", signature = "Spectra", def = function (object,name){
  if(missing(name)){
    out = object@header
  }else {
    if(is.null(object@header[[name]])){
      out = NA
    }else{
      out = object@header[[name]]
    }
    return(out)
  }
})
#########################################################################
# Method : spc.setheader
#########################################################################
#' Set a field of the @header slot of a \code{Spectra} object
#' @description
#' Function sets or changes the value of a field in the header slot of \code{Spectra} object
#'
#' @seealso \code{\link{spc.getheader}}
#' @param value Object of class SpcHeader
#' @param object A \code{Spectra} object 
#' @examples 
#' sp=spc.example_spectra()
#' a=new("SpcHeader") # create new SpcHeader class
#' a$Longitude=123 
#' spc.setheader(sp) <- a
#' sp@header
#' 
#' @rdname spc.setheader
#' @export
setGeneric (name="spc.setheader<-",def=function(object,value){standardGeneric("spc.setheader<-")})

#' @rdname spc.setheader
setReplaceMethod(f="spc.setheader", signature="Spectra",
                 definition=function(object,value){
                   stopifnot(class(value)=="SpcHeader")
                   object@header<-value 
                   validObject(object)
                   return(object)
                 })

#########################################################################
# Method : spc.updateheader
#########################################################################
#' Update a field of the @header slot of a \code{Spectra} object
#' @description
#'  Updates or changes the value of a field in the header slot of \code{Spectra} object 
#'
#' @usage spc.updateheader(object,Name,value,...)
#' @param object A \code{Spectra} object
#' @param Name of the header field to be updated
#' @param value to update header with
#' @param ... arguments to be passed to or from other methods
#' @examples 
#' sp=spc.example_spectra()
#' sp@header
#' sp <- spc.updateheader(sp,"Station", 11)
#' sp@header
#' 
#' #SpcList example
#' sp=spc.example_spectra()
#' BL=spc.makeSpcList(sp,"CAST")
#' BL[[1]]@header
#' BL[[1]] <- spc.updateheader(BL[[1]],"Station", 11)
#' BL[[1]]@header
#' 
#' @rdname spc.updateheader
#' 
#' @export
setGeneric(name="spc.updateheader",
            def=function(object,Name,value,...){standardGeneric("spc.updateheader")})

#' @rdname spc.updateheader
setMethod("spc.updateheader", signature="Spectra", definition=function(object,Name,value,...){
  hdr=spc.getheader(object)
  hdr[[Name]]=value
  spc.setheader(object)<-hdr
  validObject(object)
  return(object)
})

#########################################################################
# Method : spc.getselected.idx
#########################################################################
#' Extract index inside of a \code{Spectra} object
#' @description
#' Extracts index of rows marked as selected
#' 
#' @seealso \code{\link{spc.setselected.idx<-}}
#' 
#' @param object A \code{Spectra} object 
#' @return \code{Spectra} object
#' @examples 
#' x <- spc.example_spectra()
#' idx=rep(FALSE,nrow(x)); 
#' idx[1:5]=TRUE
#' spc.setselected.idx(x)<-idx 
#' spc.getselected.idx(x)
#' 
#' @rdname spc.getselected.idx
#' @export
setGeneric (name= "spc.getselected.idx", def=function(object){standardGeneric("spc.getselected.idx")})

#' @rdname spc.getselected.idx
setMethod("spc.getselected.idx", signature = "Spectra", def = function (object){
  return(object@SelectedIdx)
})

#########################################################################
# Method : spc.setselected.idx	
#########################################################################
#' Set index to a \code{Spectra} object
#' @description
#' Set or change selected  row index of a \code{Spectra} object 
#'
#' @param object A \code{Spectra} object 
#' @param value index for a \code{Spectra} object
#' @seealso \code{\link{spc.getselected.idx}}
#' @examples 
#' x <- spc.example_spectra()
#' idx=rep(FALSE,nrow(x)); 
#' idx[1:5]=TRUE
#' spc.setselected.idx(x)<-idx 
#' spc.plot(x)
#' @return A \code{Spectra} object
#' 
#' @rdname spc.setselected.idx
#' @export 
setGeneric("spc.setselected.idx<-",function(object,value){standardGeneric("spc.setselected.idx<-")})

#' @rdname spc.setselected.idx
setReplaceMethod(f="spc.setselected.idx", signature="Spectra",
                 definition=function(object,value){
                   if(is.numeric(value)){
                     idx = spc.getinvalid.idx(object)
                     if(length(idx)==0)
                       idx = rep(FALSE,nrow(object))
                     idx[value]=TRUE
                     value=idx
                   }
                   object@SelectedIdx<-value
                   validObject(object)
                   return (object)
                 })

#########################################################################
# Method : spc.getinvalid.idx
#########################################################################
#' Get index of \code{Spectra} rows marked as invalid
#' @description
#' Extract the row indexes stored as invalid 
#'
#' @usage 
#' spc.getinvalid.idx(object)
#'
#' @param object A \code{Spectra} object 
#' @return Logical vector 
#' @examples 
#' sp= spc.example_spectra()
#' spc.getinvalid.idx(sp) #No invalid rows
#' 
#' @rdname spc.getinvalid.idx
#' @export
setGeneric (name= "spc.getinvalid.idx",
            def=function(object){standardGeneric("spc.getinvalid.idx")})

#' @rdname spc.getinvalid.idx
setMethod("spc.getinvalid.idx", signature = "Spectra", def = function (object){
  return(object@InvalidIdx)
})
#########################################################################
# Method : spc.setinvalid.idx
#########################################################################
#' Set rows of \code{Spectra} as invalid
#' @description Stores the row indexes to be stored as invalid.
#'
#' @param object A \code{Spectra} object 
#' @param value Logical vector 
#' 
#' @seealso \code{\link{spc.setselected.idx<-}}
#' @examples  
#' sp = spc.example_spectra()
#' spc.getinvalid.idx(sp) #No invalid rows
#' vld = rep(TRUE,26)
#' vld[1:5]<-FALSE
#' spc.setinvalid.idx(sp)<-vld #Mark the first 5 rows as invalid
#' spc.getinvalid.idx(sp)
#'
#' @rdname spc.setinvalid.idx
#' @export
setGeneric("spc.setinvalid.idx<-",function(object,value){standardGeneric("spc.setinvalid.idx<-")})

#' @rdname spc.setinvalid.idx
setReplaceMethod(f="spc.setinvalid.idx", signature="Spectra", definition=function(object,value){
  if(is.numeric(value)){
    idx = spc.getinvalid.idx(object)
    if(length(idx)==0)
      idx = rep(FALSE,nrow(object))
    idx[value]=TRUE
    value=idx
  }
  object@InvalidIdx<-value
  validObject(object)
  return (object)
})

#########################################################################
# Method : spc.data2header
#########################################################################
#' Populate fields of header slot using data from data slot 
#' @description
#' Populates a field of @header with a column data from @data slot.
#'
#' @usage 
#' spc.data2header(object,dataname,headerfield,compress,...)
#'
#' @param dataname A character object specifying the name of @data column to be used.
#' @param object A \code{Spectra} object.
#' @param compress logical. Whether or not to compress data put into the header. 
#' See the description section.
#' @param headerfield A character object specifying the name of the @header field to be changed
#' @param ... arguments to be passed to or from other methods
#' @return object of class \code{Spectra}
#' @details 
#' This function extracts data from a column of the @data slot (specified by dataname)  
#' and creates a new @header field with it. Ifa header field is not provided, the name 
#' of the new header field will be the same as dataname. 
#' 
#' The name of the new header field can be overwritten by providing header field.
#' If all the incoming data rows (dataname) are the same, information put into the header 
#' can be compressed by selecting compress=TRUE (default is FALSE). This would take only 
#' the first element from the @data column.
#' 
#' @examples 
#' sp=spc.example_spectra()
#' sp=spc.data2header(sp,"CAST")
#' sp@header
#' sp=spc.data2header(sp,"CAST","ProjectCast")
#' sp@header
#' sp$CAST=rep(33, nrow(sp))
#' sp=spc.data2header(sp,"CAST","ProjectCast", compress=TRUE)
#' sp@header
#' 
#' @rdname spc.data2header
#' @export
setGeneric(name= "spc.data2header",
           def=function(object,dataname,headerfield,compress=FALSE,...){standardGeneric("spc.data2header")})

#' @rdname spc.data2header
setMethod("spc.data2header", signature = "Spectra", 
          def=function(object,dataname, headerfield,compress,...){
            if(missing(headerfield))
              headerfield = dataname
            object@header[[headerfield]]=object[[dataname]]
            if(compress )
              object@header[[headerfield]]=object[[dataname]][1]
            
            return(object)
          })

#########################################################################
# Method : spc.header2data
#########################################################################
#' Copy header data into the @data slot
#' @description
#' Get the header metadata and place it inside the @data slot
#'
#' @param object A \code{Spectra} object 
#' @param headerfield character. Field name of the header to be copied.
#' @param dataname character. Column name of @data slot to copy the incoming data.
#' @param compress logical. Whether or not to compress data put into the header. 
#' See help of \code{\link{spc.data2header}}.
#' @return object of class \code{Spectra} or \code{SpcList}
#' @param ... arguments to be passed to or from other methods
#' @details 
#' If header element has length >1, its type is checked. If it is "character",
#' its elements will be pasted using paste(...,collapse="|"). If it is another 
#' type, only the first element will be taken. For list and SpcList objects, the same 
#' procedure is repeated for all elements of the list containing \code{Spectra} objects.
#' If \code{dataname} is missing, then it will be taken equal to \code{headerfield}.
#' @examples 
#' sp <- spc.example_spectra()
#' sp <- spc.updateheader(sp,"Zone", "ZoneA")
#' sp <- spc.header2data(sp, "Zone")
#' sp$Zone
#' 
#' @rdname spc.header2data
#' @export
setGeneric(name= "spc.header2data",
           def=function(object,headerfield,dataname,compress,...){standardGeneric("spc.header2data")})

#' @rdname spc.header2data
setMethod("spc.header2data", signature = "Spectra", def=function(object,headerfield,dataname,compress=TRUE,...){
  if(missing(dataname))
    dataname = headerfield
  if (headerfield %in% names(object@header)){
    if(class(object@header[[headerfield]])=="character")
      object[[dataname]] = object@header[[headerfield]][1]
    else
      object[[dataname]] = paste(object@header[[headerfield]],collapse="|")
  }
  else
    stop(simpleError("Could not match a header field"))
  
  return(object)
  #			if(compress )
  #				object[[dataname]]=object@header[[headerfield]][1]
})

#########################################################################
# Method : [
#########################################################################
#' Extract or replace parts of a \code{Spectra} object
#' @description
#' Operators acting on \code{Spectra} object and \code{Spectra} lists to extract or replace parts.
#'
#' @details 
#' These operators are generic. You can write methods to handle indexing of specific classes of objects
#' 
#' @examples 
#' sp=spc.example_spectra()
#' sp #501 spectral channels in columns and 26 observations in rows 
#' sp[1] #returns Spectra object, 501 spectral channels in columns and 1 observations in rows
#' names(sp)
#' sp[["CAST"]] #returns the CAST data column
#' sp[[4]] #returns the CAST data column
#' sp[["CAST"]]=12 #Modify the CAST column
#' sp[["CAST"]] #returns the CAST data column
#' 
#' @rdname Spectra-Access
setMethod("[", signature(x = "Spectra"), function(x, i, j) {
  OUT_ANC = 0
  if(missing(i))
    i <-  1:nrow(x@Spectra)

  if(missing(j))
    j =  1:ncol(x@Spectra)
  
  if (class(j)=="numeric" | class(j)=="character"){
    if (class(j)=="numeric"){
      j.new = match(j,x@Wavelengths)
    }
    if (class(j)=="character"){
      if (!exists("j.new") & any(match(j, colnames(x@Spectra),nomatch=F))) {
        j.new = match(j, names(x))
      }
      if (!exists("j.new") & any(match(j, names(x@data),nomatch=F))) {
        OUT_ANC = 1
        j.new = match(j, names(x@data))						
      }
      if (!exists("j.new") && length(j)==1 && grepl("::",j)) {					
        #The requested input is in format lbd1::lbd2
        temp = strsplit(j, "::")
        mylower = as.numeric(temp[[1]][1])
        myupper = as.numeric(temp[[1]][2])					
        j.new = which(x@Wavelengths>=mylower & x@Wavelengths<=myupper)
      }
      if (!exists("j.new"))
        stop("Could not recognize the wavelength selection format. Use the operator :: or provide spectra or inDF data column indexes or names")
      
    }			
    if (all(is.na(j.new)))
      stop("Could not find matching wavelengths or inDF data")
    if (any(na.idx <-(is.na(j.new)))) {
      j.new=j.new[!is.na(j.new)]
      #					warning(paste("Could not match wavelengths or inDF data :", j[which(na.idx)]))
    }
    if (!all(is.finite(j.new)))
      stop("Could not find matching wavelengths or inDF data")
    j = j.new
  }
  InvalidIdx = x@InvalidIdx
  if (!OUT_ANC) {
    x@Spectra=x@Spectra[i,j,drop=F]
    if(nrow(x@data)>0)
      x@data=x@data[i,,drop=F]
    x@Wavelengths = x@Wavelengths[j]
  } else{
    x@data = x@data[i,j,drop=F]				
  }
  x@sp = x@sp[i]
  x@time = x@time[i,]
  x@endTime = x@endTime[i]
  
  if (length(x@InvalidIdx)>1)
    x@InvalidIdx = x@InvalidIdx[i] 
  
  x@SelectedIdx = logical()
  validObject(x)
  return(x)
})


#########################################################################
# Method : [[
#########################################################################
#' @rdname Spectra-Access
setMethod("[[", signature=c("Spectra","character","missing"), function(x, i, j) {
  Boutput = list()
  for (II in 1:length(i)){		
    if (i[II] %in% colnames(x@Spectra)){
      #					idx = which(i[II]==colnames(x@Spectra))
      Boutput[[II]] = x@Spectra[,i[II]]
      names(Boutput[[II]])<-i[II]
    }
    if (i[II] %in% names(x@data)){
      #					idx = which(i[II]==names(x@data))
      Boutput[[II]] = x@data[[i[II]]]				
      names(Boutput[[II]])<-i[II]
    }
  }
  if(length(Boutput)==0)
    stop("Could not match any Spectral or ancillary data columns")
  
  names(Boutput)<-i
  if(length(i)>1){
    Boutput = as.data.frame(Boutput)
    row.names(Boutput)<-NULL
  } else {
    Boutput = Boutput[[1]]
    names(Boutput)<-NULL				
  }
  validObject(Boutput)            
  return(Boutput)
})

#' @rdname Spectra-Access
setReplaceMethod("[[",  signature=c("Spectra","character","missing"), definition=function(x, i, j, value) {
  #			matched = 0
  if(class(value)=="data.frame")
    stop("The input variable 'value' cannot be a data.frame")
  if (i %in% colnames(x@Spectra))
    stop(simpleError("Matched a Spectra column. Use spc.add.channel() to add a spectral channel"))
  #			if (i %in% names(x@data)){
  #				matched = 1
  x@data[[i]] <- value				
  #			}
  #			if(!matched)
  #				stop("Could not match any Spectral or ancillary data columns")
  validObject(x)
  return(x)
})

#########################################################################
# Method : rep
#########################################################################
#' Replicate rows of \code{Spectra} object
#' @description
#' Operators 
#'
#' @param x A \code{Spectra} object whose rows are to be replicated.
#' @param times A integer vector giving the (non-negative) number of times to repeat each row.
#' See help of \code{\link{rep}}.
#' @param ... further arguments to be passed to or from other methods. 
#' See help of \code{\link{rep}}.
#'  
#' @details Replicates rows of \code{x}, making \code{times} copies of each row. 
#' Replicates \code{Spectra}, \code{data}, \code{sp}, \code{time}, \code{endTime}, 
#' \code{InvalidIdx} slots. Resets the \code{SelectedIdx} slot.
#' 
#' @return A \code{Spectra} object
#' @examples 
#' sp=spc.example_spectra()
#' dim(sp)
#' sp2 = rep(sp, 5)
#' dim(sp2)
#' 
#' @export
setMethod("rep", signature(x = "Spectra"), function(x, times, ...) {
  SP = sapply(1:ncol(x), function(y) rep(x@Spectra[,y], times))
  
  if(prod(dim(x@data))!=0){
    DT = as.data.frame(matrix(rep(matrix(NA,nrow(x@data),ncol(x@data)), times), ncol = ncol(x@data)))
    for (I in 1:ncol(DT))
      DT[,I] = rep(x@data[,I],times, ...)
    names(DT)<-names(x@data)
  }
  
  if (length(x@InvalidIdx)>1)
    x@InvalidIdx = rep(x@InvalidIdx,times)
  
  crds = matrix(rep(x@sp@coords,times),ncol=ncol(x@sp@coords),byrow=T)
  colnames(crds)<-c("LON","LAT")
  x@time = xts::xts(rep(x@time,times),rep(time(x@time),times))
  x@endTime = rep(x@endTime,times)
  x@sp@coords <- crds
  if(prod(dim(x@data))!=0)
    x@data = DT 
  x@Spectra = SP	
  x@InvalidIdx = rep(x@InvalidIdx, times)
  x@SelectedIdx = logical()
  validObject(x)
  return(x)
})

#########################################################################
# Method : spc.interp.spectral
#########################################################################
#'  Interpolate spectral values 
#' @description
#' Estimate spectral data at a new set of wavelengths through interpolation
#' using approx().
#'
#' @usage 
#' spc.interp.spectral(source1,target_lbd,show.plot, ...)
#' 
#' @param source1  A \code{Spectra} object 
#' @param  target_lbd numeric vector giving desired wavelengths  
#' @param show.plot logical TRUE if a graphical representation is required 
#' @param ... further arguments to pass on to approx(). 
#' @examples 
#' sp=spc.example_spectra()
#' lbd = as.numeric(c(412,440,490,555,670))
#' sp2 = spc.interp.spectral(sp[,lbd],c(430,450,500))
#' spc.plot.overlay(SpcList(list(sp,sp2)))
#' 
#' #Quick Plot only the first row
#' spc.interp.spectral(sp[,lbd],c(430,450,500),show.plot=TRUE)
#' 
#' @rdname spc.interp.spectral
#' @export
setGeneric (name= "spc.interp.spectral",
            def=function(source1,target_lbd,show.plot=FALSE,...){standardGeneric("spc.interp.spectral")})

#' @rdname spc.interp.spectral
setMethod("spc.interp.spectral", signature = "Spectra", def = function (source1,target_lbd,show.plot=FALSE){
  if(missing(target_lbd))
    stop("The input argument 'target_lbd' is missing")
  
  out = source1
  lbd_source1 = spc.getwavelengths(source1)
  DF = matrix(nrow=nrow(source1),ncol=length(target_lbd))
  my = list()
  for(x in 1:nrow(DF)) {
    my[[x]] = approx(lbd_source1, source1@Spectra[x,],xout=target_lbd,rule=2)
    DF[x,] = t(my[[x]]$y)
  }
  if(show.plot){
    plot(lbd_source1, source1@Spectra[1,],type="b",ylab=source1@LongName,xlab="Wavelength",pch="o")
    points(my[[x]]$x,my[[1]]$y,col="red",cex=1)
    grid(col="black")
  }
  out@Spectra = DF
  out@Wavelengths = target_lbd
  spc.colnames(out) <- spc.cname.construct(out)
  validObject(out)
  return(out)
})
#########################################################################
# Method : spc.export.text
#########################################################################
#' Exporting into text format
#' @description
#' Save the \code{Spectra} and \code{SpcHeader} objects on disk in text format and read back in.
#'
#' @seealso \code{\link{spc.import.text}}
#' @param input  A \code{Spectra} object 
#' @param  filename Name of the output text file
#' @param ... arguments to be passed to or from other methods
#' @param sep character. the field separator string
#' @param append logical. Only relevant if file is a character string. Default is  TRUE
#' @param writeheader either a logical value indicating whether the header names  are to be written        
#' @examples 
#' x=spc.example_spectra()
#' spc.export.text(x,filename="anap.txt")
#' aa=spc.import.text("anap.txt")
#' dev.new()
#' spc.plot(aa)
#' 
#' #Export the SpcHeader object
#' spc.export.text(x@header,filename="anap_header.txt")
#' hdr=spc.import.text("anap_header.txt")
#' class(hdr)
#' 
#' @rdname spc.export.text
#' @export
setGeneric(name="spc.export.text",
           def=function(input,filename,sep=";",append=FALSE,writeheader=TRUE, ...) {standardGeneric("spc.export.text")})

#' @rdname spc.export.text
setMethod("spc.export.text", signature="Spectra", definition=function(input,filename,sep,append,writeheader,...){
  data = as(input,"data.frame")
  idx.idx = which(colnames(data) == "idx")
  if(length(idx.idx)>0){
    data = data[,-idx.idx]
  }
  data = cbind(data.frame(idx=1:nrow(data)),data)
  clmnnames = colnames(data)
  data$TIME = as.character(data$TIME,usetz=TRUE)
  data$ENDTIME = as.character(data$ENDTIME,usetz=TRUE)
  
  written=0
  if(writeheader){
    spc.export.text(input@header,filename,append=F)
    written=length(input@header)
  }
  slotInfos = .spc.slot.infos(input,sep)
  for(I in 1:length(slotInfos)){
    if(length(slotInfos[[I]])==1)
      mysl=paste(names(slotInfos)[I],slotInfos[[I]],sep=sep)
    else
      mysl = paste(names(slotInfos)[I],paste(slotInfos[[I]],collapse=sep),sep=sep)
    if(written==0)
      write.table(mysl,filename,row.names=F,col.names=F,append=F,quote=F)
    else
      write.table(mysl,filename,row.names=F,col.names=F,append=T,quote=F)
    written = written+1
  }
  
  #Write column names
  write.table(paste(clmnnames,collapse=sep), filename, row.names=F, col.names=F,append=T, quote=F,eol="\n")
  #Write Spectra+Ancillary data
  write.table(data, filename, sep=sep, row.names=F, col.names=F,append=T,quote=F)
  print(paste("Wrote", filename ))			
})

.spc.slot.infos = function(input,sep){
  out=list('Spectra|ShortName'=input@ShortName,
           'Spectra|LongName'=input@LongName,
           'Spectra|Units'=input@Units,'Spectra|proj4string'=input@sp@proj4string@projargs,
           'Spectra|WavelengthsUnit'=input@WavelengthsUnit,
           'Spectra|Wavelengths'=spc.getwavelengths(input))
  return(out)
}

#' @rdname spc.export.text
setMethod("spc.export.text", signature="SpcHeader", definition=function(input,filename,sep=";",append=F,...){
  nms = names(input)
  nms = paste0("Spectra|header",sep,nms)

    out1 = lapply(1:length(input),function(x){
    myfield <- input[[x]]
    if(class(myfield) %in%  c("logical","numeric","character","factor")) {
      #If the separator character exists in the header, then eliminate it 
      if(class(myfield)=="character")
        myfield <-gsub(sep,"",input[[x]])
      
      #If vector (more than one value) then collapse it into one line
      if(length(myfield)>1)
        myfield<-paste(myfield,collapse=sep)
      #Convert it to character
      myfield<-as.character(myfield)
    } else {
      #If it is a complex type, then serialize it
      nms[[x]] <<- paste0(nms[[x]], "|Serialized")

      myfield = rawToChar(serialize(myfield,connection = NULL,ascii = T))
      myfield = gsub('\n','_a_',myfield)
      }
    myfield
  })
  out1 = cbind(nms,out1)
  write.table(out1,filename,row.names=F,col.names=F,append=append,quote=F,sep=sep)
})

#########################################################################
# Method : spc.import.text
#########################################################################
#' @rdname spc.export.text
#' @export
spc.import.text = function(filename,sep=";",...){
  myT = readLines(con=filename)
  
  #Extract the header
  header.idx = grep("Spectra\\|header",myT)
  if(length(header.idx)>0){
    hdr = strsplit(myT[header.idx],sep)
    
    nms = sapply(hdr,function(x)x[2])
    header = sapply(hdr,function(x){
      if(length(x)>2)
        x[3:length(x)]
      else
        ""
    })
    names(header)<- nms
    
    #Extract Serialized fields, if any and unserialized them
    header.idx.ser = grep("\\|Serialized",myT)
    header = spc.header.infos(header) 
    if (length(header.idx.ser)>0) {
      for (JJ in header.idx.ser){
        header[[JJ]] = unserialize(charToRaw(gsub('_a_','\n',header[[JJ]])))
        names(header)[JJ] <- gsub("\\|Serialized","",names(header)[JJ])
      }
    }
    
    if(any(grepl("StationType",nms)))
      if(is.logical(header$StationType))
        header$StationType = "T"
    header = as(header,"SpcHeader")
    myT = myT[-header.idx]
    
  } else {
    header = new("SpcHeader")
  }
  #Extract the Spectra slots
  Slots.idx = grep("Spectra\\|",myT)
  if(length(Slots.idx)>0){
    Slots = strsplit(myT[Slots.idx],sep)
    idx = grep("LongName",Slots)
    LongName = Slots[[idx]][2]
    idx = grep("ShortName",Slots)
    ShortName = Slots[[idx]][2]
    idx = grep("Units",Slots)
    Units= Slots[[idx]][2]
    idx = grep("proj4string",Slots)
    proj4string=Slots[[idx]][2]
    if(grepl("NA",proj4string))
      proj4string = NA
    idx = grep("WavelengthsUnit",Slots)
    WavelengthsUnit=Slots[[idx]][2]
    idx = which("Spectra|Wavelengths"==sapply(Slots,function(x)x[1]))
    try(Wavelengths<-as.numeric(Slots[[idx]][2:length(Slots[[idx]])]),silent=T)
    if(!exists("Wavelengths"))
      stop(simpleError("Could not find Wavelength information"))
    myT = myT[-Slots.idx]
    con = textConnection(myT)
    Spec = read.table(con,header=T,sep=sep)
    close(con)
    
    #Eliminate the first (idx) column
    idx = which(names(Spec)=="idx")
    if(length(idx)>0){
      Spec = Spec[,-idx]
    }
  #browser()
    Spec$TIME<-as.character(Spec$TIME)
    tz = strsplit(Spec$TIME[1]," ")[[1]][3]
    Spec$TIME<-as.POSIXct(strptime(Spec$TIME,"%Y-%m-%d %H:%M:%S",tz=tz))
    Spec$ENDTIME<-as.character(Spec$ENDTIME)
    Spec$ENDTIME<-as.POSIXct(strptime(Spec$TIME,"%Y-%m-%d %H:%M:%S",tz=tz))
    Spec = Spectra(Spec,ShortName=ShortName,Wavelengths=Wavelengths,Units=Units,
                   LongName=LongName,header=header)
  } else {
    Spec <- header
  }
  return(Spec)
}
##########################################
#spc.header.infos
#########################################
#' Getting as input the \code{Spectra} heade
#' @description
#' This internal function takes as input the \code{Spectral} header as a list and 
#' converts its elements to numbers (when possible)
#' evals its elements in case the text contains some R code
#' 
#' @usage 
#' spc.header.infos(header)
#' 
#' @param header A \code{Spectra} header
#' 
#' @examples 
#' sp=spc.example_spectra()
#' spc.header.infos(sp@header)
#' 
#' @export
#This internal function takes as input the Spectra header as a list and 
#1)converts its elements to numbers (when possible)
#2)evals its elements in case the text contains some R code
spc.header.infos = function(header){ 
  #Suppress warnings for the below operation (as.numeric creates warnings)
  myWarn = options()$warn
  options(warn=-1)
  header = lapply(header,function(x) {
    try(y<-as.numeric(x),silent=T)
    if(!is.na(y))
      x<-y
    return(x)
  })
  #	header = lapply(1:length(header),function(x) {
  ##				if(names(header)[x]=="Rsky750")
  #					try(y<-eval(parse(text=header[[x]])),silent=T)
  #				if(exists("y"))
  #					header[[x]]<-y
  #				return(header[[x]])
  #			})
  options(warn=myWarn)
  return(header)
}

#' Exports a \code{Spectra} object into Excel format.
#'
#' @description
#' Exports  a \code{Spectra} object into Excel format.
#' @param input  A \code{Spectra} object 
#' @param  filename Name of the output xlsx file
#' @param sheetName The \code{Spectra} object to be output.
#' @param writeheader A boolean, indicating whether or not the metadata (contents of the 
#' slot \code{header}) is to be included in the excel file. Default : TRUE
#' @param append A boolean, indicating whether or not to append the contents of the \code{Spectra} object
#' into the existing file. Default : FALSE (overwrites the existing Excel file if it exists.)
#' @param sep Not used.
#' @param ... Not used.
#' 
#' @details
#' \code{spc.export.xlsx()} calls functions from package \code{xlsx} to write the contents of 
#' a \code{Spectra} object into an Excel file. For this function to work, make sure the 
#' package \code{xlsx} is installed.
#' 
#' @return None. Simply creates an Excel file on disk.
#'
#' @examples
#' \dontrun{
#'   sp=spc.example_spectra()
#'   if("xlsx" %in% installed.packages())
#'      spc.export.xlsx(sp,"test.xlsx")
#' }
#' 
#' @rdname spc.export.xlsx
#' @export
setGeneric(name="spc.export.xlsx",
           def=function(input,filename,sheetName,writeheader=TRUE,append=F,sep=";",...) {standardGeneric("spc.export.xlsx")})

#' @rdname spc.export.xlsx
setMethod("spc.export.xlsx", signature="Spectra", definition=function(input,filename,sheetName,writeheader,append,sep,...){
  if (!requireNamespace("xlsx", quietly = TRUE)) {
    print("xlsx needed for this function to work. Please install it.")
  } else {
    if(missing(sheetName))
      sheetName = input@ShortName
    data = as(input,"data.frame")
    data$TIME = as.character(data$TIME,usetz=TRUE)
    data$ENDTIME = as.character(data$ENDTIME,usetz=TRUE)
    data = cbind(data.frame(idx=1:nrow(data)),data)
    
    slotInfos = .spc.slot.infos(input,sep)
    if(!append){
      #Create an empty excel workbook and start writing into it
      wb <- xlsx::createWorkbook()
    }else{
      #Create an empty excel workbook and start writing into it
      wb <- xlsx::loadWorkbook(file=filename)
    }
    sheet <- xlsx::createSheet(wb, sheetName=sheetName)
    if(writeheader){
      for(I in 1:length(input@header)){					
        if(length(input@header[[I]])==1)
          myH=cbind("Spectra|header",names(input@header)[I],input@header[[I]])
        else		
          myH = cbind("Spectra|header",names(input@header)[I],t(input@header[[I]]))
        xlsx::addDataFrame(myH, sheet,row.names=F,col.names=F,,startRow=I,startColumn=1)
      }
    }
    written = length(input@header)
    for(I in 1:length(slotInfos)){
      if(length(slotInfos[[I]])==1)
        mysl=cbind(names(slotInfos)[I],slotInfos[[I]])
      else
        mysl = cbind(names(slotInfos)[I],t(slotInfos[[I]]))
      xlsx::addDataFrame(mysl,sheet,row.names=F,col.names=F,startRow=written+1,startColumn=1)
      written = written+1
    }
    xlsx::addDataFrame(data, sheet,row.names=F,startRow=written+1,startColumn=1)
    xlsx::saveWorkbook(wb, filename)
    print(paste("Wrote sheet", sheetName, "to", filename))
  }
})

#########################################################################
# Method : subset
#########################################################################
#' Subsetting for a \code{Spectra} and spcList classes
#' @description
#' Subsetting can be achieved using the implementation of the R function subset() for \code{Spectra} and SpcList classes
#'It is possible to perform a row-wise selection
#'
#' @param drop passed on to [ indexing operator. Default is FALSE 
#' @param ... arguments to be passed to or from other methods.
#' @param x A \code{Spectra} object 
#' @param subset logical expression indicating elements or rows to keep: 
#' missing values are taken as false.
#' @param select Condition selected
#' @examples 
#' fnm = file.path(system.file(package = "geoSpectral"), "test_data","particulate_absorption.csv.gz")
#' abs = read.table(fnm,sep=",",header=TRUE)
#' abs$STATION=factor(abs$STATION)
#' abs[1:2,1:17] #Display only the first 2 rows and first 17 columns if the data frame
#' lbd = as.numeric(gsub("X","",colnames(abs)[14:514]))
#' Units="1/m"
#' colnames(abs)= gsub("X",paste("anap","_",sep=""), colnames(abs))
#' colnames(abs)= gsub("PRES","DEPTH", colnames(abs))
#' abs = abs[,c(14:514,1:13)]
#' tz<-strsplit(as.character(abs$TIME)," ")[[1]][[3]] #Extract the timezone
#' abs$TIME = as.POSIXct(as.character(abs$TIME),tz=tz) 
#' myS<-Spectra(abs,Wavelengths=lbd,Units=Units,ShortName="a_nap")
#' myS
#' head(spc.getwavelengths(myS))
#' spc.setwavelengths(myS) <- 300:800 
#' myS[1:10]
#' myS[,"anap_400"] 
#' myS[,c("anap_400","anap_500")] 
#' myS[1:10,30:50] #Selection of channels by column index
#' lbd = as.numeric(c(412,440,490,555,670))
#' myS[1:10,lbd] #Selection of channels by wavelength
#' myS[1:10,"415::450"] 
#' myS$CAST #Returns Ancillary data
#' myS$anap_400 #Returns spectra as numeric vector
#' head(myS[["anap_400"]]) #Returns spectra as numeric vector
#' head(myS[[c("Snap","Offset")]]) #Returns data.frame
#' #Subsetting rows with respect to the value of Ancillary data
#' subset(myS,DEPTH<=30)
#' #Subsetting rows with respect to the value of Spectral data
#' subset(myS,anap_440<=0.01)
#' #Selecting Ancillary data columns, leaving Spectral columns intact
#' subset(myS,subset=DEPTH<=30,select="CAST") 
#' 
#' @export
setMethod("subset",  signature="Spectra",definition=function(x, subset, select, drop = FALSE, ...) {
  if (missing(subset)) 
    mycall <- TRUE
  else {
    mycall <- substitute(subset)
    if(any(sapply(as.character(mycall),function(y) {y %in% colnames(x@Spectra)})))
      try(xidx <- eval(mycall, as.data.frame(x@Spectra), parent.frame()),silent=T)
    if(any(sapply(as.character(mycall),function(y) {y %in% names(x@data)})))
      try(xidx <- eval(mycall, x@data, parent.frame()),silent=T)		
    if (!exists("xidx") || !is.logical(xidx)) 
      simpleError(stop("'subset' must evaluate to logical"))				
    xidx <- xidx & !is.na(xidx)
    if (length(x@SelectedIdx)>0)
      x@SelectedIdx = x@SelectedIdx[xidx]
    if (length(x@InvalidIdx)>0)
      x@InvalidIdx = x@InvalidIdx[xidx]
    if (nrow(x@data)>0)
      x@data = x@data[xidx,,drop=drop]
    x@Spectra = x@Spectra[xidx,]
    x@sp = x@sp[xidx,]
    x@time= x@time[xidx,]
    x@endTime= x@endTime[xidx]
  }	
  
  if (missing(select)) 
    vars <- TRUE
  else {				
    nl <- as.list(1:ncol(x@Spectra))
    names(nl) <- colnames(x@Spectra)					
    vars <- eval(substitute(select), nl, parent.frame())				
    if(vars %in% colnames(x@Spectra)){
      y_idx = as.integer(nl[vars])
      
      x@Wavelengths = x@Wavelengths[y_idx]
      x@Spectra = x@Spectra[,y_idx,drop=F]
    }					
    
    nl <- as.list(1:ncol(x@data))
    names(nl) <- colnames(x@data)
    vars <- eval(substitute(select), nl, parent.frame())
    if(vars %in% colnames(x@data)){
      y_idx = as.integer(nl[vars])	
      x@data = x@data[,y_idx,drop=F]
    }
  }
  
  validObject(x)
  return(x)
})

#########################################################################
# Method : spc.select Select Spectra with the help of the mouse
#########################################################################
mat_identify <- function(x, y, ...){
  l <- locator(1)
  if(all(x <= l$x) || all(x >= l$x)){
    result=NULL
  } else {
    index <- max(which(x <= l$x))
    f <- (l$x - x[index]) / diff(x[index+(0:1)])
    
    yi <- f * (y[index+1,] - y[index,] ) + y[index,]
    result <- which.min(abs(yi-l$y))
    lines(x, y[,result], lwd=2, col="red")
  }
  #  text(l, label=colnames(y)[result])
  return(result)
}
setGeneric (name= "spc.select",
            def=function(object){standardGeneric("spc.select")})
setMethod("spc.select", signature = "Spectra", 
          def = function (object){
            print("Click on graph to select Spectra, click Esc to quit ")
            
             #Extract the existing selection Index
            if(length(object@SelectedIdx)>0)
              ExSel = object@SelectedIdx
            else
              ExSel = rep(FALSE, nrow(object@Spectra))			
            Sel = rep(FALSE, nrow(object@Spectra))			
            
            lbd = spc.getwavelengths(object)
            idx = mat_identify(lbd, t(object@Spectra))
            print(paste("Selected row",idx))
            oidx = idx
            while(!is.null(idx)){
              idx = mat_identify(lbd, t(object@Spectra))
              print(paste("Selected row",idx))
              oidx=c(oidx, idx)				
            }
            oidx = oidx[!is.null(oidx)]
            Sel[oidx]=T
            
            TrueIdx = isTRUE(ExSel)
            #Apply the XOR operator to Existing Index ExSel 
            #(selecting again an already selected row unselcts it)
            ExSel = xor(ExSel,Sel)
            
            #Update the slot SelectedIdx 
            #			object@SelectedIdx = ExSel
            
            #print(cbind(Sel, ExSel))
            return(ExSel)
          })

#########################################################################
# spc.makeSpcList
#########################################################################
#'  Conversion from \code{Spectra} to \code{Spclist}
#' @description
#'Conversion from \code{Spectra} to \code{Spclist} using a data field
#' @usage 
#'  spc.makeSpcList(myobj, name)
#' @param myobj a \code{Spectra} object
#' @param name  name of station of a \code{Spectra} object
#' @examples 
#' sp <- spc.example_spectra()
#' BL = spc.makeSpcList(sp,"CAST")
#' show(BL)
#' 
#' @export
#Method : Conversion from Spectra to SpcList using a data field (factor)
#Later add the functionality with FUN (i.e. taking means)
spc.makeSpcList = function(myobj, name){
  if(length(name)!=1)
    simpleError(stop("Argument 'name' should have a length of 1"))
  #Get the indexes of each DF row :
  idx = lapply(unique(myobj[[name]]),function(x) {which(x==myobj[[name]])})
  #For each row index in the list, subset the DF, return a list
  output = lapply(idx,function(x) {
    out = myobj[x,]
    out <- spc.updateheader(out, name, as.character(out[[name]][1]))
    out
  })
  output = SpcList(output)
  output@by = name
  return(output)
}

#########################################################################
# Method : spc.plot.time
#########################################################################
#'  Plotting \code{Spectra} object
#'
#' @description
#' Generating plot of the contents of a \code{Spectra} object with respect to time.
#' If xdata is 'time', data is plotted with respect to the 'TIME' column. If xdata 
#' is 'observations', data is plotted with respect to an integer index equal to 1:nrow(object).
#'
#' @param object A \code{Spectra} object.
#' @param Y character. Name of the columns of the \code{Spectra} object to be plotted.
#' @param maxSp numeric. Maximum number of \code{Spectra} to plot.
#' @param xdata character. Type of time-series data. Can be 'time' or 'observations'.
#' @param lab_cex vector of character expansion sizes, used cyclically.
#' @param lwd vector of line widths
#' @param ... any further arguments of plot
#' @seealso \code{\link{spc.plot.depth}}
#' @examples
#' x <- spc.example_spectra()
#' spc.plot.time(x)
#' 
#' @rdname spc.plot.time
#' @export
setGeneric(name= "spc.plot.time",def=function(object, ...){standardGeneric("spc.plot.time")})

#' @rdname spc.plot.time
setMethod("spc.plot.time", signature="Spectra", function (object,Y,maxSp=50,xdata="time",lab_cex,lwd=2, ...){
  stopifnot((xdata=="time") | (xdata == "observations"))
  idx = as(1:ncol(object@data), "logical")
  
  if (length(object@InvalidIdx)==0)
    object@InvalidIdx = rep(FALSE,nrow(object@data))		

  if(missing(Y)){
    Y = spc.colnames(object)
  }
  if(ncol(object)>maxSp)
    Y = Y[seq(1,ncol(object),length.out=maxSp)]
  
  tsdata = object[[Y]] #[!object@InvalidIdx,]
  
  if(missing(lab_cex))
    lab_cex = 1
  
  tsCol = rainbow(ncol(tsdata))
  
  if(xdata=="time") {
    x = time(object)
    x = x[!object@InvalidIdx]
    xlb = "Time"
    XX = xts::xts(tsdata,time(object@time))
    plot.new()
    #xts::plot.xts(XX,screens=1) #,xlab="",ylab="",lwd=lwd,col=tsCol, ...)
    #xtsExtra::plot.xts(XX,screens=1, xlab="",ylab="",lwd=lwd,col=tsCol, ...)#Problem: does not plot inside the function
    zoo::plot.zoo(XX,screens=1,xlab="",ylab="",lwd=lwd,col=tsCol, ...)
  }
  if (xdata == "observations") {
    x = 1:nrow(object)
    xlb = "Observation number"
    x = x[!object@InvalidIdx]
    matplot(x,tsdata, type="l", pch=19,cex=0.3,xlab="",ylab="",lwd=lwd,col=tsCol,...)        
  }
  
  # 			df$Date <- as.Date( df$Date, '%m/%d/%Y')
  # 			require(ggplot2)
  # 			ggplot( data = df, aes( Date, Visits )) + geom_line() 
  
  grid(col="black")
  
  #Draw the legend
  if(length(Y)>1&length(Y)<=10) {
    legend("bottomright",Y,col=1:length(Y),fill=1:length(Y),bty="n",cex=lab_cex)
    ylb = bquote(.(object@LongName[1])*", ["*.(object@Units[1])*"]")	
  }
  else{
    if(length(Y)==1)
      ylb = Y
    else
      if(object@LongName=="spvar2 longname")
        ylb = bquote(.(object@ShortName)*", ["*.(object@Units[1])*"]")  
      else
        ylb = bquote(.(object@LongName[1])*", ["*.(object@Units[1])*"]")  
  }
  mtext(xlb,side=1,line=2,cex=lab_cex)
  mtext(ylb,side=2,line=2,cex=lab_cex)
  
  #Draw the legend
  if(length(Y)>1 & length(Y)<=10)
    legend("bottomright",Y,col=1:length(Y),lty=1:length(Y),bty="n",lwd=2,cex=lab_cex)
})

#########################################################################
# Method : spc.plot.depth
#########################################################################
#'  Plotting \code{Spectra} object
#'
#' @description
#' Generating plot of the contents of a \code{Spectra} object with respect to depth
#' 
#' @param object a \code{Spectra} data.
#' @param X character. Column names of the a \code{Spectra} object to be plotted.
#' @param maxSp numeric. Maximum number of \code{Spectra} to plot.
#' @param lab_cex vector of character expansion sizes, used cyclically.
#' @param title a chracter string, title for plot
#' @param add logical. If TRUE, plots are added to current one,
#' @param xlab,ylab titles for x and y axes, as in plot.
#' @param ylim,xlim ranges of x and y axes, as in plot.
#' @param lwd numeric vector of line widths
#' @param ... any further arguments of plot
#' @seealso \code{\link{spc.plot}}
#' @examples
#' x <- spc.example_spectra()
#' spc.plot.depth(x)
#' 
#' @rdname spc.plot.depth
#' @export
setGeneric (name= "spc.plot.depth",def=function(object, ...){standardGeneric("spc.plot.depth")})

#' @rdname spc.plot.depth
setMethod("spc.plot.depth", signature="Spectra", function (object,X,maxSp=10,lab_cex, title, add=FALSE, xlab=NULL, ylab=NULL, ylim=NULL,xlim=NULL,lwd=2,...){
  idx = as(1:ncol(object@Spectra), "logical")
  depth=object$DEPTH
  if(length(is.finite(depth))<1)
    stop("Could not find the column DEPTH")
  
  if (length(object@InvalidIdx)==0)
    object@InvalidIdx = rep(FALSE,nrow(object))		
  
  if(missing(X)){
    if(ncol(object)>maxSp)
      X = round(seq(1,ncol(object),length.out=maxSp))
    else
      X = names(object)
  }
  if (is.numeric(X))
    X = names(object)[X]
  
  if(missing(ylab))
    ylab = "Depth [m]"
  
  if(missing(xlab)) {
    if (class(object)=="Spectra") {
      if(object@LongName=="spvar2 longname")
        xlab = bquote(.(object@ShortName)*", ["*.(object@Units[1])*"]")  
      else
        xlab = bquote(.(object@LongName[1])*", ["*.(object@Units[1])*"]")          
    } else {
      if(length(X)==1)
        xlab =  bquote(.(X)*", ["*.(object@Units[1])*"]")	
    }
  }
  if(missing(ylim)){
    #ylim = rev(range(pretty(depth[!object@InvalidIdx],n=10)))
    ylim = rev(range(pretty(depth,n=10)))
    ylim[2]=-0.1	
  }
  #If any, do not draw these parameters
  X = gsub("DEPTH","",X,fixed=T)
  X = gsub("VOLTAGE","",X,fixed=T)
  X = gsub("TIME","",X,fixed=T)
  X=X[X!=""]
  
  mynames = spc.colnames(object)[match(X,names(object))]
  u_units = object@Units 
  colidx = match(X,spc.colnames(object))
  if(any(is.na(colidx)))
    u_units = c(u_units, "unknown") #For now, we can only add unknown units XXX
  my_sides = rep(c(1,3), ceiling(length(u_units)/2))
  
  #Extract the data to plot
  #			myX = object[!object@InvalidIdx,X,drop=F]
  #			myY = depth[!object@InvalidIdx]
  myX = object[[X]]
  myY = depth
  #Sort with respect to depth
  d_idx = sort.int(myY,index.return = TRUE)
  myY = d_idx$x
  
  if(class(myX)=="data.frame"){
    myX = myX[d_idx$ix,,drop=F]
    #Eliminate rows full with zeros
    idx = which(!apply(myX==0,1,all))
    myY = myY[idx]
    myX = myX[idx,,drop=F]
    #Eliminate NAs in depth
    idx = !is.na(myY)
    myY = myY[idx]
    myX = myX[idx,,drop=F]
  }
  else{
    myX = myX[d_idx$ix]
    #Eliminate rows full with zeros
    idx = myX!=0
    myY = myY[idx]
    myX = myX[idx]
    #Eliminate NAs in depth
    idx = !is.na(myY)
    myY = myY[idx]
    myX = myX[idx]
  }
  
  if(missing(lab_cex))
    lab_cex=1
  
  depth_diff = TRUE
  if(length(myY)>1)
    depth_diff = !all(diff(myY)==0) 
  
  mytype = "l"
  if(length(myY)==1)
    mytype = "p"
  
  if (depth_diff & !(length(myY)<1)) {
    if(length(u_units)==1){	
      #All columns to be plotted have the same unit 
      if(add)
        matlines(myX,myY,type=mytype,xlab="",ylab="",ylim=ylim,...)
      else{
        if (all(is.finite(xlim)))
          matplot(myX,myY,type=mytype,pch=19,cex.axis=lab_cex,xlab="",ylab="",ylim=ylim,xlim=xlim,lwd=lwd,...)
        else
          matplot(myX,myY,type=mytype,pch=19,cex.axis=lab_cex,xlab="",ylab="",ylim=ylim,lwd=lwd,...)						
      }
      matpoints(myX,myY,xlab="",ylab="",pch=19,cex=0.4,ylim=ylim,...)					
      
      mtext(ylab,side=2,line=2,cex=lab_cex)
      mtext(xlab,side=1,line=2,cex=lab_cex)
      grid(col="black")		
      #Draw the legend
      if(length(X)>1 & !add & length(X)<=10)
        legend("bottomright",X,col=1:length(X),lty=1:length(X),bty="n",lwd=2,cex=lab_cex)
      
    }else{
      #All columns to be plotted have different units 
      #					for (I in 1:length(u_units)){
      for (I in 1:2){
        if (I!=1)
          par(new=T)
        #						col_idx = which(u_units[I]==myunits)
        xlab = bquote(.(object@LongName[1])*", ["*.(object@Units[1])*"]")
        
        matplot(object[[X[[I]]]],myY,type="l", axes=F,pch=19,cex=0.3, ylim=ylim,col=I,xlab="",ylab="",lwd=lwd,...)
        mtext(my_sides[I],text=xlab,line=2,cex=lab_cex)
        axis(my_sides[I], col=I, pretty(range(object[[X[[I]]]]),10))
        if (I==1){
          box(); 	
          cols = rep(1, length(X))
        }
        else {
          #							cols[col_idx] = I
          cols[I] = I
        }
      }
      grid(col="black")
      axis(2,pretty(range(myY),10))
      mtext(2, text=ylab,line=2)
      #Draw the legend
      if(length(X)>1 & !add & length(X)<=10)
        legend("bottomright",X,col=cols,lty=cols,bty="n",lwd=2,cex=lab_cex)
      
    }
    #Draw the title if provided from the call
    if(!missing(title))
      title(title)
  } else{
    return(0)
  }
})
#################################################
#spc.example_spectra
################################################
#' Create example of Spectral object 
#' @description
#' Example of Spectral object is created by the function
#'
#' 
#' @usage 
#' spc.example_spectra()
#' @examples 
#' sp = spc.example_spectra()
#' class(sp)
#' show(sp)
#' 
#' @export
spc.example_spectra <- function(){
  #Search in the package installation directory
  fnm = file.path(base::system.file(package = "geoSpectral"),"test_data","particulate_absorption.csv.gz")
  #If the previous search fails, search the file in the source code directory
  if(!file.exists(fnm))
    fnm = file.path(base::system.file(package = "geoSpectral"),"inst","test_data","particulate_absorption.csv.gz")
  
  abs = read.table(fnm,sep=",",header=TRUE)
  abs$STATION=factor(abs$STATION)
  abs[1:2,1:17] #Display only the first 2 rows and first 17 columns if the data frame
  lbd = as.numeric(gsub("X","",colnames(abs)[14:514]))
  Units="1/m"
  colnames(abs)= gsub("X",paste("anap","_",sep=""), colnames(abs))
  colnames(abs)= gsub("PRES","DEPTH", colnames(abs))
  abs = abs[,c(14:514,1:13)] #Rearrange so that Spectra columns come first
  tz<-strsplit(as.character(abs$TIME)," ")[[1]][[3]] #Extract the timezone
  abs$TIME = as.POSIXct(as.character(abs$TIME),tz=tz) #Compute the time
  
  #Space and time columns are automatically found in the column names of inDF
  myS<-Spectra(abs,Wavelengths=lbd,Units=Units,ShortName="a_nap",
               LongName="Absorption coefficient by non-algal particles")
  myS
}

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
#' Plot a Spectra object data 
#' @description
#' Plot a \code{Spectra} object with plotly engine 
#' @param sp A \code{Spectra} object
#' @param plot.max numeric value for a maximum number of data in plot. Default is 10.
#' @param showlegend logical, to display legend or not, default is FALSE 
#' @param legend_field character. Gives the name of the column to be used in the legend.
#' @param hoverinfo a chracter, info about  \code{Spectra} object to be used  in hover box.
#' @param title a chracter string, title for plot.
#' @examples 
#'sp = spc.example_spectra()
#'spc.plot.plotly(sp)
#'spc.plot.plotly(sp,legend_field = "Spectra")
#'spc.plot.plotly(sp,legend_field = "CAST")
#'spc.plot.plotly(sp,legend_field = "NISKIN")
#'spc.plot.plotly(sp,legend_field = "STATION")
#'spc.plot.plotly(sp,legend_field = "anap_440")
#' 
#' @rdname spc.plot.plotly
#' @export
setGeneric (name= "spc.plot.plotly",
            def=function(sp, plot.max=10,showlegend=FALSE,legend_field="row",hoverinfo="title",title=sp@LongName){standardGeneric("spc.plot.plotly")})
#' @rdname spc.plot.plotly
setMethod("spc.plot.plotly", signature="Spectra", function (sp, plot.max=10,showlegend = FALSE,legend_field,hoverinfo,title) {
  #library(reshape2)
  # lbd = spc.getwavelengths(sp)
  # kk = data.frame(Wavelength=lbd,t(sp@Spectra))
  # kk=melt(kk,id.vars=1)
  # p <- plotly::plot_ly(kk, x=~Wavelength, y=~value, type="scatter", mode="lines",color = ~variable,
  #              colors="Spectral", opacity=0.5, line=list(width = 1)) #,evaluate = FALSE) #, colors=pal,line = list(opacity=0.1))
  # require(plotly)
  if (plot.max > nrow(sp))
    plot.max = nrow(sp)

  idx = floor(seq(1, nrow(sp), length.out = plot.max))
  if (legend_field %in% names(sp)) {
    legend_field = paste(legend_field, sp[[legend_field]])
  }
  else
    legend_field = paste(legend_field, 1:nrow(sp))
  
  ylab = paste(sp@ShortName, " [", sp@Units, "]", sep="")
  xlab = paste("Wavelength [", sp@WavelengthsUnit, "]", sep="")
  p <- plotly::plot_ly()
  for(I in 1:length(idx)) {  
    p <- plotly::add_trace(p, x=sp@Wavelengths, y=sp@Spectra[idx[I],],type = "scatter", mode="lines",
                   name=legend_field[idx[I]], hoverinfo=hoverinfo
                   #,marker=list(color=line[['color']])
                   )
  }
  p = plotly::layout(p,
             title = title,
             hovermode = "closest",
             xaxis = list(title = xlab), #rangeslider = list(type = "linear")),
             yaxis = list(title = ylab),
             showlegend=showlegend
             )
  p
})

#' Plot a Spectra object data with respect to time
#' @description
#' Plot a \code{Spectra} object with respect to time
#' @param sp A \code{Spectra} object
#' @param column Number or name , defoult value is 10 if a number or name has not been entered
#' @param plot.max numeric value for a maximum number of data in plot
#' @param showlegend logical, to display legend or not, default is FALSE 
#' @param hoverinfo  a chracter, info about  \code{Spectra} object to be used  in hover box
#' @param title a chracter string, title for plot
#' @examples 
#' \dontrun{
#' sp = spc.example_spectra()
#' spc.plot.time.plotly(sp)
#' spc.plot.time.plotly(sp, plot.max = 3)
#' spc.plot.time.plotly(sp, c("anap_450","anap_550","anap_650"))
#' }
#' 
#' @rdname spc.plot.time.plotly
#' @export
setGeneric (name= "spc.plot.time.plotly",
            def=function(sp, column, plot.max=10,showlegend=FALSE,hoverinfo="name",title=sp@LongName){standardGeneric("spc.plot.time.plotly")})

#' @rdname spc.plot.time.plotly
setMethod("spc.plot.time.plotly", signature="Spectra", function (sp, column, plot.max=10,showlegend,hoverinfo,title) {
  if(missing("column")){
    if(ncol(sp)<10)
      idx = 1:ncol(sp)
    else
      idx = round(seq(1, ncol(sp), length.out = plot.max))
    
    column = colnames(sp@Spectra)[idx]
  }
  ylab = paste(sp@ShortName, " [", sp@Units, "]", sep="")
  myTime = time(sp@time)
  
  p=plotly::plot_ly(x = myTime , y = sp[[column[1]]], type="scatter", mode = "lines + markers",name=column[1])
  if(length(column)>1)
    for(I in 2:length(column))
      p=plotly::add_trace(p, x = myTime , y = sp[[column[I]]], 
                  type="scatter", mode = "lines + markers", 
                  name=column[I], hoverinfo=hoverinfo) 
  p = plotly::layout(p,
             title = title,
             hovermode = "closest",
             xaxis = list(title = "Time",
                          rangeslider = list(type = "date")),
             yaxis = list(title = ylab),
             showlegend=showlegend
             )
  p
})

#########################################################################
#spc.plot.depth.plotly
#########################################################################
#' Display a Spectra object
#' @description
#' Plot a \code{Spectra} object with respect to depth
#' @examples 
#' sp = spc.example_spectra()
#' BL = spc.makeSpcList(sp,"CAST")
#' p1<-spc.plot.depth.plotly(BL[[5]])
#' #p1<-layout(p1,title=paste("CAST =", BL[[5]]$CAST[1]))
#' p2<-spc.plot.depth.plotly(BL[[4]])
#' #p2<-plotly::layout(p2,title=paste("CAST =", BL[[4]]$CAST[1]))
#' p <- plotly::subplot(p1, p2,  margin = 0.05, shareY=TRUE,shareX=TRUE,titleX=TRUE,titleY=TRUE)
#' p <- plotly::layout(p, showlegend = TRUE, 
#' annotations = list(
#' list(x = 0.2 , y = 1.05, text = BL[[5]]$CAST[1], showarrow = FALSE, xref='paper', yref='paper'),
#' list(x = 0.8 , y = 1.05, text = BL[[4]]$CAST[1], showarrow = FALSE, xref='paper', yref='paper')))
#' p
#' @param sp A \code{Spectra} object
#' @param column Number or name , default  value is 10 if a number or name has not been entered
#' @param plot.max numeric value for a maximum number of data in plot
#' @param showlegend logical, to display legend or not, default is FALSE 
#' @param hoverinfo  a chracter, info about  \code{Spectra} object to be used  in hover box
#' @param title a chracter string, title for plot
#' @rdname spc.plot.depth.plotly
#' @export
setGeneric (name= "spc.plot.depth.plotly",
            def=function(sp, column, plot.max=10,showlegend=FALSE,hoverinfo="name",title=sp@LongName){standardGeneric("spc.plot.depth.plotly")})

#' @rdname spc.plot.depth.plotly
setMethod("spc.plot.depth.plotly", signature="Spectra", function (sp, column, plot.max=10,showlegend,hoverinfo,title) {
  if(missing("column")){
    if(ncol(sp)<10)
      idx = 1:ncol(sp)
    else
      idx = round(seq(1,ncol(sp), length.out = plot.max))
    
    column = colnames(sp@Spectra)[idx]
  }
  xlab = paste(sp@ShortName, " [", sp@Units, "]", sep="")
  
  p=plotly::plot_ly(x = sp[[column[1]]] , y = sp$DEPTH, type="scatter", mode = "lines + markers",name=column[1])
  if(length(column)>1)
    for(I in 2:length(column))
      p=plotly::add_trace(p, x = sp[[column[I]]] , y =sp$DEPTH, type="scatter", mode = "lines + markers", 
                  name=column[I], hoverinfo=hoverinfo) 
  # layout(yaxis = list(autorange = "reversed"))
  p = plotly::layout(p,
             title = title,
             hovermode = "closest",
             xaxis = list(title = xlab),
             yaxis = list(title = "Depth [ m ]", 
                          rangeslider = list(type = "linear"),
                          autorange = "reversed"),
             showlegend=showlegend
             )
  p 
})

#########################################################################
#spc.plot.map.plotly
#########################################################################
#' Display a Spectra object
#' @description
#' Create a point map with ploty engine using \code{Spectra} rows 
#' @examples 
#' sp <- spc.example_spectra()
#' spc.plot.map.plotly(sp)
#' 
#' @param sp A \code{Spectra} object
#' @param hover_field A character, column  names of sp object to be used  in hover box
#' @param opacity The opacity transparency of the glyph 
#' between 0 (transparent) and 1 (opaque)
#' @param color Determine color of points
#' 
#' @rdname spc.plot.map.plotly
#' @export
setGeneric (name= "spc.plot.map.plotly",
            def=function(sp,hover_field="row", color="#FF0000", opacity=1){standardGeneric("spc.plot.map.plotly")})

#' @rdname spc.plot.map.plotly
setMethod("spc.plot.map.plotly", signature="Spectra", function (sp, hover_field, color, opacity) {
  # require(plotly)
  bbx = sp@sp@bbox
  bbx[,2] =  bbx[,2] + (0.04 * abs(bbx[,2]))
  if(bbx[2,2]>90)
    bbx[2,2]<-89
  bbx[,1] =  bbx[,1] - (0.04 * abs(bbx[,1]))
  if(bbx[2,1]< -90)
    bbx[2,1]<- -89
  
  g <- list(
    #scope = 'north america',
    showland = TRUE,
    landcolor = plotly::toRGB("grey83"),
    subunitcolor = plotly::toRGB("white"),
    countrycolor = plotly::toRGB("white"),
    showlakes = TRUE,
    lakecolor = plotly::toRGB("blue"),
    showrivers = TRUE,
    showsubunits = TRUE,
    showcountries = TRUE,
    resolution = 50,
    projection = list(
      type = 'conic conformal',
      rotation = list(
        lon = -100
      )
    ),
    lonaxis = list(
      showgrid = TRUE,
      gridwidth = 0.5,
      range = c(bbx[1,1], bbx[1,2]),
      dtick = 5
    ),
    lataxis = list(
      showgrid = TRUE,
      gridwidth = 0.5,
      range = c(bbx[2,1],bbx[2,2]),
      dtick = 5
    )
  )
    
  if(length(color==1))
    color = rep(color, nrow(sp))
  p <- plotly::plot_ly(lat = sp@sp@coords[,"LAT"], lon = sp@sp@coords[,"LON"], 
               #text = hover, color = Globvalue,marker = m
               type = 'scattergeo', color=color, opacity=opacity
  ) 
  p <- plotly::layout(p, geo = g, showlegend=FALSE)
  p
})

###########################################################
# spc.plot.map.leaflet
###########################################################
#' Display a Spectra object
#' @description
#' Create a point map with leaflet engine using \code{Spectra} rows 
#' @param sp \code{Spectra} object
#' @param color Determine color of points
#' @param hover_field A character  or vector of strings giving column 
#' names of \code{Spectra} object. This information will be displayed when 
#' hovering over the glyph
#' @param opacity The opacity transparency of the glyph 
#' between 0 (transparent) and 1 (opaque)
#' @param weight Stroke width in pixels
#' @examples 
#' sp=spc.example_spectra()
#' spc.plot.map.leaflet(sp)
#' 
#' @rdname spc.plot.map.leaflet
#' @export
 setGeneric (name= "spc.plot.map.leaflet",
            def=function(sp,hover_field = "row",color = "#FF0000",opacity = 1,  weight=5){standardGeneric("spc.plot.map.leaflet")})
 
#' @rdname spc.plot.map.leaflet
setMethod("spc.plot.map.leaflet", signature="Spectra", function (sp,hover_field = "row",color = "#FF0000",opacity = 1,  weight=5) {

   hover_field = paste0(hover_field, 1:nrow(sp))
  
  m = leaflet() %>% 
    addCircles(lng = sp@sp@coords[,"LON"], lat=sp@sp@coords[,"LAT"], 
               opacity=opacity,color=color, weight=5) %>% 
    addTiles()# %>%
  #addPopups(lng = sp@sp@coords[,"LON"], lat=sp@sp@coords[,"LAT"], 
  # popup = legend_field  )
  #addLegend(pal = qpal, values = , opacity = 1)
  m
  

 })

###########################################################
# spc.plot.map.rbokeh
###########################################################
#' Display a Spectra object
#' @description
#' Create a point map with rbokeh engine using \code{Spectra} rows 
#' @param sp \code{Spectra} object
#' @param color Determine color of points
#' @param legend not implemented  yet
#' @param hover String or vector of strings giving column 
#' names of \code{Spectra} object. This information will be displayed when 
#' hovering over the glyph
#' @param opacity The opacity transparency of the glyph 
#' between 0 (transparent) and 1 (opaque)
#' @param glyph Value(s) or field name of the glyph to
#'  use \code{\link{point_types}}
#' @examples 
#' \dontrun{
#'   sp=spc.example_spectra()
#'   spc.plot.map.rbokeh(sp, hover = "Snap")
#'   spc.plot.map.rbokeh(sp)
#' }
#' 
#' @rdname spc.plot.map.rbokeh
#' @export
setGeneric (name= "spc.plot.map.rbokeh",
            def=function(sp,glyph = 2,color = "#FF0000", legend=NULL,hover="row",opacity =1){standardGeneric("spc.plot.map.rbokeh")})

#' @rdname spc.plot.map.rbokeh
setMethod("spc.plot.map.rbokeh", signature="Spectra", function (sp,glyph,color, legend,hover,opacity ) {
  # require(rbokeh)
  # require(maps)
  #a=sp$Snap
  df = data.frame(LON = sp@sp@coords[,"LON"])
  df$LAT = sp@sp@coords[,"LAT"]
  df$color = color
  df$opacity  = opacity
  df$row = 1:nrow(sp)
  for (I in 1:length(hover))
  if (hover[I] %in% names(sp))
    df[hover[I]] = sp[[hover[I]]]
 
   bbx = sp@sp@bbox
  bbx[,2] =  bbx[,2] + (0.04 * abs(bbx[,2]))
  if(bbx[2,2]>90)
    bbx[2,2]<-89
  bbx[,1] =  bbx[,1] - (0.04 * abs(bbx[,1]))
  if(bbx[2,1]< -90)
    bbx[2,1]<- -89
  
  figure(xlim=c(bbx[1,1],bbx[1,2]),ylim=c(bbx[2,1],bbx[2,2]),padding_factor = 0) %>%
    #gmap(lat = mean(sp@sp@bbox[2,]), lng = mean(sp@sp@bbox[1,]),zoom = 12, width = 680, height = 600)
    ly_map("world", color = "gray") %>%
    #ly_points(x=sp@sp@coords[,"LON"], y=sp@sp@coords[,"LAT"],legend=legend,hover=hover )
    ly_points(x="LON", y="LAT", data=df, color=color,fill_alpha=opacity,
              line_alpha=opacity,hover=names(df)[5:length(names(df))] )
})

#' Sort a Spectra object
#' @description
#' Sort a \code{Spectra} object with respect to its rows with respect to values of one 
#' given column (specified by which.col). Sorting with respect to multiple columns is not implemented yet.
#' @param x A \code{Spectra} object
#' @param which.col A character, defining the name of the column to be used in the sorting
#' @param decreasing Logical. If TRUE, then the rows are sorted in decreasing order. Passed on to the
#' sort.idx() function from the base package. Default is FALSE.
#' @param na.last for controlling the treatment of NAs. Passed on to the
#' sort.idx() function from the base package. Default is NA.
#' @param ...	arguments to be passed to or from methods. See help of \code{\link{sort}}.
#' @examples 
#' sp <- spc.example_spectra()
#' sp2 <- sort(sp, which.col="Offset")
#' sp2$Offset
#' sp2 <- sort(sp, which.col="CAST", decreasing=TRUE)
#' sp2$CAST
#' 
#' @export
setMethod("sort", signature="Spectra", definition= function (x, decreasing = FALSE, na.last=NA, which.col, ...){
  srt <- sort.int(x[[which.col]], decreasing=decreasing, index.return = TRUE, na.last=na.last, ...)
  x<- x[srt$ix]
  validObject(x)
  return(x)
})