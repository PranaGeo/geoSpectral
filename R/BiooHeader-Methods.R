# TODO: names(), colnams()
# 
# Author: acizmeli
###############################################################################

#########################################################################
# Method : biooScanDirName	
#########################################################################
#If there is Skip in the field name, it will be skipped
biooScanDirName = function(template, in.listnames) {
	#Take the first instrument
	out = biooHeaderAdd(template, "InBaseName", basename(in.listnames))
	out = biooHeaderAdd(out, "InDirName", dirname(in.listnames))
	
	if (is.character(template$DirNameTemplate)){		
		for (I in 1:length(template$DirNameTemplate)){
			
			if(!grepl(template$DirNameTemplate[I],"Skip", ignore.case=T)) {
				tags = ExtractTagFromFilename(basename(in.listnames), TAGNO=I, SEP="_",  out[[1]]$extension)
				if(template$DirNameTemplate[I]=="Date"){
					temp = DatesFromFilename(tags, "Date", SORT=FALSE)
					tags = temp$StartDate
				}
				tags = gsub(template$DirNameTemplate[I],"",tags)
				out = biooHeaderAdd(out, template$DirNameTemplate[I], tags)
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
setMethod("show", signature="BiooHeader", function(object){
			fieldnames = names(object)
			sapply(1:length(fieldnames), function(x) cat(paste(fieldnames[x], " : ", 
										object[fieldnames[x]], "\n")))
		})