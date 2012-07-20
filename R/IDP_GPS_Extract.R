# TODO: Add comment
# 
# Author: acizmeli
###############################################################################


IDP_Gps_Extract = function(gps, what2extract){
	if(class(gps)=="data.frame") {
		#dimensions of the gps boundingbox (in degrees)
		bounddims = apply(gps[,c("Longitude","Latitude")], 2, max)-apply(gps[,c("Longitude","Latitude")], 2, min)
	}	
	if(class(gps)=="SpatialPoints") {
		#dimensions of the gps boundingbox (in degrees)
		bounddims = apply(bbox(gps),1, diff)
	}
	#For large gps datasets, if the entire gps is used to seek station coordinates/times
	#it may be computationally very inefficient. As a workaround, incrementally increase 
	#the search radius (to be used by spdep::knearneigh)
	
	#Ship stations : Extract the Date/Time information from the ship gps data
	mindist = 1 #in meters
	#maximum available distance (the diagonal of the rectangle)
	maxdist = sqrt(sum(bounddims^2))* 1111200 #Converted to meters 
	
	if (maxdist/mindist>1000) {
		#Take logarithmic intervals
		myintervals = seq(log10(mindist), log10(maxdist))
		myintervals = seq(log10(mindist), log10(maxdist),  length.out=100)
		#Take incremental distances (100m) 		
	}
	J=1
	tolerance = (10^myintervals[J])/1111200
	gps_idx = gps$Latitude<(what2extract[2]+tolerance)&gps$Latitude>(what2extract[2]-tolerance) & 
			gps$Longitude<(what2extract[1]+tolerance)&gps$Longitude>(what2extract[1]-tolerance)
	
	while(length(which(gps_idx))<15 & J<=length(myintervals)){
		tolerance = 10^myintervals[J]/1111200
		gps_idx = gps$Latitude<(what2extract[2]+tolerance)&gps$Latitude>(what2extract[2]-tolerance) & 
				gps$Longitude<(what2extract[1]+tolerance)&gps$Longitude>(what2extract[1]-tolerance)
		if (length(which(gps_idx))>=15)
			print(paste("Tolerance: ", tolerance*1111200, "m. Found", length(which(gps_idx)), "records"))
		J=J+1
	}
	#in_gps = gps[gps_idx,]
	
	coords=rbind(what2extract, gps[which(gps_idx),c("Longitude","Latitude")])

	knn <- spdep::knearneigh(as.matrix(coords), k=1, longlat=T)
	knn_idx = knn$nn[1,]-1
		
	mydistance = spDists(t(matrix(what2extract)), gps[which(gps_idx)[knn_idx],c("Longitude","Latitude")],longlat=T)
	if (0) {
		plot(coords[,1], coords[,2],type="p")
		points(what2extract[1],what2extract[2], col="red")
		points(gps$Longitude[which(gps_idx)[knn_idx]], gps$Latitude[which(gps_idx)[knn_idx]], col="green",pch=2)
	}
	
	return(cbind(which(gps_idx)[knn_idx], mydistance))
}
