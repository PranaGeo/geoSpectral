#########################################################################
# Method : spc.ScanFileName  
#########################################################################
#' Scan File Name
#' @description Scan name of file
#'
#' @usage 
#' spc.ScanFileName (template,in.listnames,Scan)
#' 
#' @param object of class SpcHeader
#' 
#' @examples 
#' sp=spc.example_spectra()
#' 
#' 
#Uses FileNameScanTemplate
#If there is Skip in the field name, it will be skipped
#Scan : A string, can be either of "DirName" or "FileName".
spc.ScanFileName = function(template, in.listnames,Scan) {
  #Take the first instrument
  out = SpcHeaderAdd(template, "InBaseName", basename(in.listnames))
  out = SpcHeaderAdd(out, "InDirName", dirname(in.listnames))

  mytemplate = switch(Scan,
         DirName = "DirNameScanTemplate",
         FileName = "FileNameScanTemplate")
  
  if (is.character(template[[mytemplate]])){		
    for (I in 1:length(template[[mytemplate]])){
      
      if(!grepl(template[[mytemplate]][I],"Skip", ignore.case=T)) {
        if (class(out)=="SpcHeader")
          EXT = out$extension
        if (class(out)=="SpcHeaderList")
          EXT = out[[1]]$extension
        #Choose the directory name in the hierarchy that contains the tags
        try(tags<- ExtractTagFromFilename(in.listnames, REV_TAGNO=template$ReverseDirTagNo, SEP="/"),silent=T)
        if (!exists("tags") || is.null(tags))
          tags = ExtractTagFromFilename(in.listnames, REV_TAGNO=1, SEP="/")

        #Determine the tags in the input directory name
        tags = ExtractTagFromFilename(tags, TAGNO=I, SEP="_",  EXT)
        
        #If it is the Date field, use DatesFromFilename() to convert it to an xts object
        if(template[[mytemplate]][I]=="Date"){
          temp = DatesFromFilename(tags, "Date", SORT=FALSE)
          tags = temp$StartDate
        }
        tags = gsub(template[[mytemplate]][I],"",tags)
        out = SpcHeaderAdd(out, template[[mytemplate]][I], tags)
        rm(tags)
      }
    }
  }
  return(out)
}

#########################################################################
# Method : SpcHeaderAdd	
#########################################################################
#'  Set a field of the @header slot of a \code{SpcHeader} class object
#' @description Function add the value of a field in the header slot of \code{SpcHeader} class object
#'
#' @usage 
#' SpcHeaderAdd (object,Name,Value)
#' 
#' @param object of class SpcHeader
#' @param Name a character variable  
#' @param Value a numeric variable
#' 
#' @examples 
#' sp=spc.example_spectra()
#' sp@header
#' sp@ShortName
#' sp@header=SpcHeaderAdd(sp@header,sp@ShortName,10
#' 
#' )
#' 
setGeneric("SpcHeaderAdd",function(object,Name,Value,...)
		{standardGeneric("SpcHeaderAdd")})
setMethod("SpcHeaderAdd", signature="SpcHeader", function(object,Name,Value){	
			templist = list()
			if (length(Value)>1){
				#We will return SpcHeaderList instead of SpcHeader
				out = 	lapply(Value, function(x) {
							templist[Name]=x
							as(modifyList(object,templist),"SpcHeader")})
				out = as(out, "SpcHeaderList")
			} else{
				templist[[Name]]=Value
				out = as(modifyList(object,templist),"SpcHeader")
			}
			return(out)
		})

#########################################################################
# Method : show	
#########################################################################
#' Show a SpcHeader object
#' @description Display a SpcHeader object
#'
#' @usage 
#' show(object)
#' 
#' @param object of class SpcHeader
#' @seealso \code{\link{show}}
#' 
#' @examples 
#' x=spc.example_spectra()
#' show(x@header)
#' 
setMethod("show", signature="SpcHeader", function(object){
			fieldnames = names(object)
			sapply(1:length(fieldnames), function(x) cat(paste(fieldnames[x], " : ", 
										object[fieldnames[x]], "\n")))
		})
setGeneric(name="spc.export.text",
           def=function(input,filename,writeheader=TRUE,sep=";",...) {standardGeneric("spc.export.text")})
setMethod("spc.export.text", signature="SpcHeader", definition=function(input,filename,append=F,sep=";",...){
  nms = names(input)
  nms = paste("Spectra|header",sep,nms,sep="")
  browser()
  out1 = lapply(input,function(x){
    #If the separator character exists in the header, then eliminate it 
    x<-gsub(sep,"",x)
    if(length(x)>1)
      x<-paste(x,collapse=sep)
    else
      x<-as.character(x)
  })
  out1 = cbind(nms,out1)
  write.table(out1,filename,row.names=F,col.names=F,append=append,quote=F,sep=sep)
})
