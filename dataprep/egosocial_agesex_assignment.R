###
# data prep for right whale social
#
# ~wrc 20171201

dat  <- read.table("../data/3-Khan-data-calf-errors-fixed.csv"       , header = TRUE, sep = ',')
cdat <- read.table("../data/2017-11-06-Khan-data-request-calving.csv", header = TRUE, sep = ',')
###
# rules


# calves are excluded
# calf until December 1st of birth year and after juv
# considered adults if:
# 	1. nine years or more have ellapsed since known birth year
#	2. eight years or more have ellapsed from date of frist sighting (when birth year is unknown)
#	3. Jan 1st of the year prior to giving birth to a calf
# whales without a known birth year seen for less than eight years were excluded (unknown age class)
# lactating:
# 	1. females are considered lactating from their first sighting with a dependent calf. until dec 1st of the calving year
# 	2. if a calf is lost; mother was considered lactating until last sighting of calf, and non-lactating after.

# dat[, 'ageclass'] <- NA
# dat[, 'lactating'] <- NA


### make dates

# these records do not appear to have a real time associated with them
missingdatetime <- which(dat$Time == 0 | dat$Day == 0 | dat$Month == 0)

# format for hhmm and split into hh and mm
time_char <- sprintf("%04d", dat$Time)
time_hrs <- substring(time_char, 1, 2)
time_min <- substring(time_char, 3, 4)

# paste together and convert to POSIX
date <- paste0(dat$Year, "-", sprintf("%02d", dat$Month), "-", sprintf("%02d", dat$Day), " ", time_hrs, ":", time_min, ":", "00 UTC")

# remove entries with missing data
date[missingdatetime] <- NA
date_posix <- as.POSIXct(date, tz = "UTC")

# put records in temporal order
oo <- order(date)
dat <- dat[oo, ]
date <- date[oo]
date_posix <- date_posix[oo]

# apply the age class rules
ageclass_tmp <- 1:nrow(dat) * NA
lactatin_tmp <- 1:nrow(dat) * NA

years_since_birth <- dat$Year - dat$BirthYear
years_since_sight <- dat$Year - dat$FirstYearSighted
years_since_1calf <- dat$Year - dat$FirstCalvingYear

# make unknowns
ageclass_tmp[which(years_since_sight <= 8 & is.na(dat$BirthYear))] <- "U"

# apply adult rules
ageclass_tmp[which(years_since_birth >=  9)] <- "A"
ageclass_tmp[which(years_since_sight >=  8 & is.na(years_since_birth))]  <- "A"
ageclass_tmp[which(years_since_1calf >= -1)] <- "A"

# calf behaviors
calfbehs <- c("CALFW/MOM", "CALFOFUNPHMOM", "CALF", "CALFW/OTHERS(S)", "CALFW/UNPH")
mombehs  <- c("W/CALF", "W/CALFUNPH")

allbehs  			<- strsplit(as.character(dat$Behaviors), "\\.")
calfs_bybehavior    <- sapply(allbehs, function(l) any(l %in% calfbehs))
calfs 				<- calfs_bybehavior | (dat$BirthYear >= dat$Year)
moms	 			<- sapply(allbehs, function(l) any(l %in% mombehs))

# make moms and calves
ageclass_tmp[calfs] <- "calf"
# lactatin_tmp[moms]  <- "moms"

# make calves older than december 1st of their "birthyear" juvs
calfjuvcutoff <- paste0(dat$BirthYear, "-12-01 00:00:00 UTC")
calfjuvcutoff[is.na(dat$BirthYear)] <- NA
calfjuvcutoff <- as.POSIXct(calfjuvcutoff, tz = "UTC")
ageclass_tmp[which(calfs & date >= calfjuvcutoff)] <- "J"

# make the rest of the juveniles (at this point this is everything that is unassigned)
ageclass_tmp[is.na(ageclass_tmp)] <- "J"

# go mom-calf pair by pair
# look for first sighting
# make lactating from first sighting until dec 1st or calf dies

for(m in 1:nrow(cdat)) {
	calfyear <- cdat$CalvingYear[m]
	calfyear_early <- calfyear - 1
	mom <- cdat$EGNo[m]
	
	dese <- which(dat$EGNo == mom & (dat$Year == calfyear | dat$Year == calfyear_early))
	nodates <- which(is.na(date[dese]))
	
	if(length(nodates) != 0)
		dese <- dese[-nodates]
	
	if(length(dese) != 0) {
		lactating_st <- min(which(moms[dese]))
		if(lactating_st != Inf) {
			calfjuvcutofflate <- paste0(calfyear, "-12-01 00:00:00 UTC")
			calfjuvcutofflate <- as.POSIXct(calfjuvcutofflate, tz = "UTC")
			
			lactating_en <- max(which(date[dese] < calfjuvcutofflate))
			
			lostcalf <- grep("LOST", allbehs[dese])
			if(length(lostcalf) != 0) {
				lactating_en <- min(lostcalf) - 1
			}
			lactatin_tmp[dese][lactating_st:lactating_en] <- "L"
		}
	}
}

dat[, 'lactating'] <- lactatin_tmp
dat[, 'ageclass'] <- ageclass_tmp