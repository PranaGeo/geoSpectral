#########################################################################
# Method : biooScanFileName  
#########################################################################
#Uses FileNameScanTemplate
#If there is Skip in the field name, it will be skipped
#Scan : A string, can be either of "DirName" or "FileName".
biooScanFileName = function(template, in.listnames,Scan) {
  #Take the first instrument
  out = biooHeaderAdd(template, "InBaseName", basename(in.listnames))
  out = biooHeaderAdd(out, "InDirName", dirname(in.listnames))

  mytemplate = switch(Scan,
         DirName = "DirNameScanTemplate",
         FileName = "FileNameScanTemplate")
  
  if (is.character(template[[mytemplate]])){		
    for (I in 1:length(template[[mytemplate]])){
      
      if(!grepl(template[[mytemplate]][I],"Skip", ignore.case=T)) {
        if (class(out)=="BiooHeader")
          EXT = out$extension
        if (class(out)=="BiooHeaderList")
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
        out = biooHeaderAdd(out, template[[mytemplate]][I], tags)
        rm(tags)
      }
    }
  }
  return(out)
}

#########################################################################
# Method : biooHeaderAdd	
#########################################################################
setGeneric("biooHeaderAdd",function(object,Name,Value,...)
		{standardGeneric("biooHeaderAdd")})
setMethod("biooHeaderAdd", signature="BiooHeader", function(object,Name,Value){	
			templist = list()
			if (length(Value)>1){
				#We will return BiooHeaderList instead of BiooHeader
				out = 	lapply(Value, function(x) {
							templist[Name]=x
							as(modifyList(object,templist),"BiooHeader")})
				out = as(out, "BiooHeaderList")
			} else{
				templist[[Name]]=Value
				out = as(modifyList(object,templist),"BiooHeader")
			}
			return(out)
		})

#########################################################################
# Method : show	
#########################################################################
#' Show a spectra object
#' @description
#' Display the object, by printing, plotting 
#'
#' @usage 
#' show(object)
#' 
#' 
#'
#' 
#' @param  object of spectra 
#' @seealso http://127.0.0.1:41556/help/library/methods/help/showMethods
#' 
#' @examples 
#' x=spc.example_spectra()
#' show(x)
#' 
#' 
setMethod("show", signature="BiooHeader", function(object){
			fieldnames = names(object)
			sapply(1:length(fieldnames), function(x) cat(paste(fieldnames[x], " : ", 
										object[fieldnames[x]], "\n")))
		})