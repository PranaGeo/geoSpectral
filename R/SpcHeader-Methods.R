#########################################################################
# Method : spc.ScanFileName  
#########################################################################
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