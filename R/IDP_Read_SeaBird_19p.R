# TODO: Add comment
# 
# Author: acizmeli
###############################################################################

#The CTD data file needs to be corrected for incomplete lines
#No need to delete the lines containing the <Executed/> tag. The
#function takes care of this tag.
#Do not remove the Start: or End: tags from the files. These information are being 
#used by the function in the computation of the TIME column. 

#From SeaBird 19p manual:
#OutputFormat=x
#x=0: Output raw frequencies and voltages
#in hexadecimal. Must use this format for
#acquiring and viewing real-time data in
#Seasave. When using Seaterm232’s
#Upload menu, Seaterm232 always uploads
#data from memory in raw hex (compatible
#				with SBE Data Processing), regardless of
#user-programmed OutputFormat=.

#x=1: Output converted (engineering units)
#data in hexadecimal.

#x=2: Output raw frequencies and voltages
#in decimal.

#x=3: Output converted (engineering units)
#data in decimal.

#x=4: Output pressure and scan number
#only, in hexadecimal. Typically used only
#for interfacing with Auto Fire Module
#(AFM) and SBE 32 Carousel Water
#Sampler or with SBE 55 ECO Water
#Sampler (for autonomous water sampling).

#x=5: Output converted (engineering units)
#data in decimal, in XML.

#x=6 Same as x=3 but with OutputSal=Y et OutputUCSD=Y (P. 54 of the Manual)

IDP_Read_SeaBird_19p = function(infile, output_format=6,return.data.frame=T) {
	if (!file.exists(infile)) {
		e <- simpleError(paste("File ", infile, " does not exist!", sep=""))
		stop(e)
	}
	print(paste("Reading ", infile, sep=""))
	
	if (!(output_format==2|output_format==3|output_format==6)) {
		e <- simpleError("Not implemented yet!")
		stop(e)
	}
	
	out = readLines(infile)	
	out = sub("<Executed/>","", out)
	
	start_idx = grep("Start",out)
	if (length(start_idx)>0) { 
		start_str = out[start_idx]
		start_str = sub("Start:","",start_str)
		start_str = strptime(start_str,"%m/%d/%Y %H:%M:%S")
		out = out[-start_idx]
	}
	end_idx = grep("End",out)
	if (length(end_idx)>0) { 
		end_str = out[end_idx]
		end_str = sub("End:","",end_str)
		end_str = strptime(end_str,"%m/%d/%Y %H:%M:%S")
		out = out[-end_idx]
	}
	
	tc=textConnection(out)
	if  (output_format==6) {
		out2 = read.table(tc,as.is=T,sep=",")
		myTIME = strptime(out2$V5,"%d %b %Y %H:%M:%S")
		out2 = out2[,-5]
		colnames(out2) = c("W_TEMPERATURE", "CONDUCTIVITY","PRESSURE",
				"SALINITY","DENSITY","VOLTAGE")
		}
	else {
		out2 = read.table(tc,colClasses="numeric",sep=",")
		colnames(out2) = c("W_TEMPERATURE", "CONDUCTIVITY","PRESSURE","PRESSURE_TEMP_COMPENSATION")
		timerange = paste(as.character(start_str),"/",as.character(end_str),sep="")
		myTIME=seq(from=start_str,to=end_str,length.out=nrow(out2))
	}
	close(tc)
	
	#myTIME = timeBasedSeq(timerange,length=nrow(out2))
	out2$TIME=myTIME
	attr(out2,"Units") = c("degC","S/m","dbar","PSU","kg/m3","V","TIME")

	if (!return.data.frame){
		out2 = as(out2,"Biooo")
	}
	
	return(out2)
} 
#fnm = file.path(system.file(package = "BiOpticaR"), "test_data","CTD002.TXT.gz")
#a= Read_SeaBird_19p(fnm, output_format=2)