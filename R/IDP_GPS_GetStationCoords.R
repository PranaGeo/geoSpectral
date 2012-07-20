# TODO: Add comment
# 
# Author: acizmeli
###############################################################################

IDP_GetStationCoordsFromGPS = function(input, ingps) {
	
	#Concatenate all GPS data from all stations into one big data.frame
	gps <- do.call(rbind, ingps)
	
	#Find the GPS record that is closest in time to the first measurement 
	#of each instrument data. Then store the coordinates as the data.frame
	#attributes "LON" and "LAT" 
	for (ZZ in 1:length(input)) {
		df = input[[ZZ]]
		if (nrow(df)>1) {
			mintime=sort(as.numeric(abs(df$TIME[1]-gps$TIME)),index.return=T)
			attr(df, "LAT") = gps[mintime$ix[1],"Latitude"]
			attr(df, "LON") = gps[mintime$ix[1],"Longitude"]
			input[[ZZ]] = df
		}
	}
	return(input)
}