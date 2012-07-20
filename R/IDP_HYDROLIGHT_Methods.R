#########################################################################
# Method : spc.save.HL.ac9
#########################################################################
setGeneric (name= "spc.save.HL.ac9",
            def=function(a_ac9, c_ac9, ...){standardGeneric("spc.save.HL.ac9")})
setMethod("spc.save.HL.ac9", signature = "Spectra", 
          definition = function (a_ac9, c_ac9, file, headertext){
            NB_HEADERS = 10
            
            if (missing(headertext)) {
              headertext = c("File created by spc.save.HL.ac9()",
                             as.character(2:NB_HEADERS))
            }
            
            if (length(headertext)>NB_HEADERS) {
              error(paste("The header text should not contain more than", NB_HEADERS, "lines"))
            }
            if (length(headertext)<NB_HEADERS) {
              headertext = as.character((length(headertext)+1):(NB_HEADERS))
              print(headertext)
            }
            headertext = paste("#", headertext)

            if(class(a_ac9)!="Spectra" | class(c_ac9)!="Spectra")
              stop('Input variables a_ac9 and c_ac9 should be of Class "Spectra"')
            
            DEPTH=data.frame()
            if(any(grepl("DEPTH", colnames(a_ac9@Ancillary)))) {
              idx = grep("DEPTH", colnames(a_ac9@Ancillary))
              DEPTH=a_ac9@Ancillary[,idx]
            } else {
              stop("Could not find DEPTH column in slot Ancillary")              
            }
            
            if (!all.equal(dim(a_ac9), dim(c_ac9)))
              stop("Input variables a_ac9 and c_ac9 should have the same dimension lengths")
            
            output = cbind(DEPTH, a_ac9@DF, c_ac9@DF)
            output = rbind(output, -1)
            
            write.table(headertext, file=file,row.names=F,col.names=F, append=F,quote=F)  
            write.table(c(length(a_ac9@Wavelengths), a_ac9@Wavelengths), 
                        file=file, row.names=F,col.names=F, append=T,eol=" ")
            write.table("", file=file, row.names=F,col.names=F, append=T,quote=F)
            write.table(output, file=file, row.names=F,col.names=F, append=T)
          })

#########################################################################
# Method : spc.save.HL.HS6
#########################################################################
setGeneric (name= "spc.save.HL.HS6",
            def=function(bbp, ...){standardGeneric("spc.save.HL.HS6")})
setMethod("spc.save.HL.HS6", signature = "Spectra", 
          definition = function (bbp, file, headertext){
            NB_HEADERS = 10
            
            if (missing(headertext)) {
              headertext = c("File created by spc.save.HL.HS6()",
                             as.character(2:NB_HEADERS))
            }
            
            if (length(headertext)>NB_HEADERS) {
              error(paste("The header text should not contain more than", NB_HEADERS, "lines"))
            }
            if (length(headertext)<NB_HEADERS) {
              headertext = as.character((length(headertext)+1):(NB_HEADERS))
              print(headertext)
            }
            headertext = paste("#", headertext)
            
            if(class(bbp)!="Spectra")
              stop('The Input variable bbp should be of Class "Spectra"')
            
            DEPTH=data.frame()
            if(any(grepl("DEPTH", colnames(bbp@Ancillary)))) {
              idx = grep("DEPTH", colnames(bbp@Ancillary))
              DEPTH=bbp@Ancillary[,idx]
            } else {
              stop("Could not find DEPTH column in slot Ancillary")              
            }
            
            output = cbind(DEPTH, bbp@DF)
            output = rbind(output, -1)

            write.table(headertext, file=file,row.names=F,col.names=F, append=F,quote=F)
            write.table(c(length(bbp@Wavelengths), bbp@Wavelengths), 
                        file=file, row.names=F,col.names=F, append=T,eol=" ")
            write.table("", file=file, row.names=F,col.names=F, append=T,quote=F)
            write.table(output, file=file, row.names=F,col.names=F, append=T)
          })

HL_wavelengths = function(wavelengths, bandwidths){
  wavelengths = c(412, 440, 488, 510, 532, 555, 630, 676, 715)
  bandwidths = 5
  output = rep(NA, length(wavelengths*2))
  
  output = (t(rbind(wavelengths-bandwidths/2,wavelengths+bandwidths/2)))
  
  
}