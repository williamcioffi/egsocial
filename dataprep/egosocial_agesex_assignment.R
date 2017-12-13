###
# data prep for right whale social
#
# ~wrc 20171201


### load files

dat  <- read.table("../data/4-Khan-data-calf-errors-fixed-nodups.csv"       , header = TRUE, sep = ',')
cdat <- read.table("../data/2017-11-06-Khan-data-request-calving.csv", header = TRUE, sep = ',')


### make dates

# these records do not appear to have a real time associated with them
missingdatetime <- which(dat$Time == 0 | dat$Day == 0 | dat$Month == 0 | dat$Year == 0)
missingjustdate <- which(dat$Day == 0 | dat$Month == 0 | dat$Year == 0)

# format for hhmm and split into hh and mm
time_char <- sprintf("%04d", dat$Time)
time_hrs <- substring(time_char, 1, 2)
time_min <- substring(time_char, 3, 4)

# paste together and convert to POSIX
datetmp <- paste0(dat$Year, "-", sprintf("%02d", dat$Month), "-", sprintf("%02d", dat$Day), " ", time_hrs, ":", time_min, ":", "00 UTC")

# remove entries with missing data
datetmp[missingdatetime] <- NA
date_posix <- as.POSIXct(datetmp, tz = "UTC")

date <- paste0(dat$Year, "-", sprintf("%02d", dat$Month), "-", sprintf("%02d", dat$Day))
date[missingjustdate] <- NA
date <- as.Date(date)

# put records in temporal order including missing dates
oo1 <- order(dat$Time)
oo2 <- order(dat$Day[oo1])
oo3 <- order(dat$Month[oo1][oo2])
oo4 <- order(dat$Year[oo1][oo2][oo3])
oo <- oo1[oo2][oo3][oo4]

dat <- dat[oo, ]
date <- date[oo]
date_posix <- date_posix[oo]


### apply rules
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

# apply the age class rules
ageclass_tmp <- 1:nrow(dat) * NA
lactating_tmp <- 1:nrow(dat) * NA

years_since_birth <- dat$Year - dat$BirthYear
years_since_sight <- dat$Year - dat$FirstYearSighted
years_since_1calf <- dat$Year - dat$FirstCalvingYear

# make unknowns
ageclass_tmp[which(years_since_sight <= 8 & is.na(dat$BirthYear))] <- "U"

# apply adult rules
ageclass_tmp[which(years_since_birth >=  9)] <- "A"
ageclass_tmp[which(years_since_sight >=  8 & is.na(years_since_birth))]  <- "A"
ageclass_tmp[which(years_since_1calf >= -1)] <- "A"

# calf and mom behaviors
calfbehs <- c("CALFW/MOM", "CALFOFUNPHMOM", "CALF", "CALFW/OTHERS(S)", "CALFW/UNPH")
mombehs  <- c("W/CALF", "W/CALFUNPH")

# set up provisional mom and calf catagories based on the behavior column
# these will be adjusted and filled in later
allbehs  			<- strsplit(as.character(dat$Behaviors), "\\.")
calfs_bybehavior    <- sapply(allbehs, function(l) any(l %in% calfbehs))
calfs 				<- calfs_bybehavior | (dat$BirthYear >= dat$Year)
moms	 			<- sapply(allbehs, function(l) any(l %in% mombehs))

# make moms and calves
ageclass_tmp[calfs] <- "calf"
lactating_tmp[moms]  <- "moms"

# make calves older than december 1st of their "birthyear" juvs
calfjuvcutoff <- paste0(dat$BirthYear, "-12-01 00:00:00 UTC")
calfjuvcutoff[is.na(dat$BirthYear)] <- NA
calfjuvcutoff <- as.POSIXct(calfjuvcutoff, tz = "UTC")
ageclass_tmp[which(calfs & date_posix >= calfjuvcutoff)] <- "J"

# make the rest of the juveniles (at this point this is everything that is unassigned)
ageclass_tmp[is.na(ageclass_tmp)] <- "J"

# go mom-calf pair by pair
# look for first sighting
# make lactating from first sighting until dec 1st or calf dies

for(m in 1:nrow(cdat)) {
	calfyear <- cdat$CalvingYear[m]
	calfyear_early <- calfyear - 1
	mom <- cdat$EGNo[m]
	
	# find that mom and that calfyear (and the year before)
	dese <- (dat$EGNo == mom & (dat$Year == calfyear | dat$Year == calfyear_early))
	
	# get rid off entries that have no year, month, or day
	# Ok if no time, because we only need day resolution
	dese <- dese & !is.na(date)
	
	lactating_st <- min(which(lactating_tmp[dese] == "moms"))
	if(lactating_st != Inf) {
		calfjuvcutofflate <- paste0(calfyear, "-12-01")
		calfjuvcutofflate <- as.Date(calfjuvcutofflate)
		
		lactating_en <- max(which(date[dese] < calfjuvcutofflate))
		
		lostcalf <- grep("LOST", allbehs[dese])
		if(length(lostcalf) != 0) {
			lostcalf_index <- max(lostcalf)
			lostcalf_date <- as.Date(date[dese][lostcalf_index]) + 1
			lactating_en <- max(which(date[dese] < lostcalf_date))
		}
		lactating_tmp[dese][lactating_st:lactating_en] <- "L"
	}
}

dat[, 'lactating'] <- lactating_tmp
dat[, 'ageclass'] <- ageclass_tmp