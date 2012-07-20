# TODO: Add comment
# 
# Author: acizmeli
###############################################################################

IDP_GetStationName = function(in.listnames, format=1) {
	if (format==1) {
		#This format is for directory structure : Station179, Station175
		#or the structure : 20110727_Station179, 20110728_Station175 etc. returns c("179","175")
		Stations = unlist(strsplit(in.listnames, "Station"))
		Stations = Stations[seq(2,length(Stations),by=2)]
	}
	return(Stations)
} #End function