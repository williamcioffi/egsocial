###
# some silly distance functions
# ~wrc

torad <- function(ang) {
	radians <- ang*pi/180
	return(radians)
}

# haversine
latlond <- function(lat1, lon1, lat2, lon2) {
	R <- 6371

	dlat <- torad(lat2) - torad(lat1)
	dlon <- torad(lon2) - torad(lon1)

	a = sin(dlat/2) * sin(dlat/2) + cos(torad(lat1)) * cos(torad(lat2)) * sin(dlon/2) * sin(dlon/2)
	c = 2 * atan2(sqrt(a), sqrt(1-a))

	d = R * c

	return(d)
}


# lat1, lat1 in degrees
# bearing in degrees
# distance in meters
# returns final lat lon in degrees
lldistbear <- function(lat1, lon1, bearing1, distance) {
	R <- 6371*1000 #radius of earth in meters
	
	#convert to radians
	lat <- torad(lat1)
	lon <- torad(lon1)
	
	bearing <- torad(bearing1)
	
	a <- sin(lat)*cos(distance/R)
	b <- cos(lat)*sin(distance/R)*cos(bearing)
	
	lat2 <- asin(a + b)
	
	c <- sin(bearing)*sin(distance/R)*cos(lat)
	d <- cos(distance/R) - sin(lat)*sin(lat2)
	
	lon2 <- lon + atan2(c,d)
	
	#convert back to degrees
	lat3 = 360*(lat2/(2*pi))
	lon3 = 360*(lon2/(2*pi))
	
	return(data.frame(lat = lat3, lon = lon3))
}
