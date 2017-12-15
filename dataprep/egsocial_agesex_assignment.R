#######################################
##                                   ##
##   Right Whale Social Analysis     ##
##                                   ##
##          -data prep-              ##
##                                   ##
##   Prep the data recieved from     ## 
##   North Atlantic Right Whale      ##
##   Consortium and get it ready     ##
##   for analysis by tidying it up   ##
##   and assigning agesex classes    ##
##                                   ##
##  Will Cioffi + Christin Khan      ##
##                                   ##
##        December 14, 2017          ##
#######################################


### prep Excel file received from Phil Hamilton
# open data and Save As in Excel file with a new name
# delete 'Calving Data' worksheet
# delete 'Code Explanation' worksheet
# delete top row header that says 'All sightings of identified whales from 1980 through 2016'
# highlight all and remove shading and borders   
# highlight top row and replace all spaces with nothing
# highlight all and replace all spaces with a period
# highlight all and replace all commas with a period
# highlight all and replace all double periods with a single period
# highlight all and replace all blanks with 'NA' values
# delete column "Association Id"
# delete column "Association Type"
# delete column "Singleton"
# delete column "Observer"
# delete column "Area Code"
# delete column "Letter"
# delete any whales (EGNo) of unknown gender 'X'
# moved "Latitude" and "Longitude" columns over to the left, and formatted to 5 decimal places
# insert a RID column for row names
# save
# save as a .txt file

# later we deleted some rows of bad CALF codes when it wasn't a calf + duplicate rows of data


### load files wrc
dat  <- read.table("../data/5-Khan-data-cleaned-up.csv", header = TRUE, sep = ',')
cdat <- read.table("../data/2017-11-06-Khan-data-request-calving.csv", header = TRUE, sep = ',')

### load files from Christin's computer
# setwd("C:/Users/christin.khan/Documents/Projects/Social-Behavior/2-Data")
# dat  <- read.table("5-Khan-data-cleaned-up.csv", header = TRUE, sep = ',')
# cdat <- read.table("2017-11-06-Khan-data-request-calving.csv", header = TRUE, sep = ',')


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
#	2. eight years or more have ellapsed from date of first sighting (when birth year is unknown)
#	3. Jan 1st of the year prior to giving birth to a calf
# whales without a known birth year seen for less than eight years were excluded (unknown age class)
# lactating:
# 	1. females are considered lactating from their first sighting with a dependent calf. until dec 1st of the calving year
# 	2. if a calf is lost; mother was considered lactating until last sighting of calf, and non-lactating after.

# apply the age class rules
ageclass_tmp <- 1:nrow(dat) * NA
lactating_tmp <- 1:nrow(dat) * NA

# create new columns to help double check agesex classes
years_since_birth <- dat$Year - dat$BirthYear
years_since_sight <- dat$Year - dat$FirstYearSighted
years_since_1calf <- dat$Year - dat$FirstCalvingYear

dat[, 'yearminusbirthyear']  	<- years_since_birth
dat[, 'yearminusfirstsight'] 	<- years_since_sight
dat[, 'yearminusfirstcalving'] 	<- years_since_1calf

# make unknowns
ageclass_tmp[which(years_since_sight <= 8 & is.na(dat$BirthYear))] <- "U"

# apply adult rules
ageclass_tmp[which(years_since_birth >=  9)] <- "A"
ageclass_tmp[which(years_since_sight >=  8 & is.na(years_since_birth))]  <- "A"
ageclass_tmp[which(years_since_1calf >= -1)] <- "A"

# calf and mom behaviors
calfbehs <- c("CALFW/MOM", "CALFOFUNPHMOM", "CALF", "CALFW/OTHERS(S)", "CALFW/UNPH")
mombehs  <- c("W/CALF", "W/CALFUNPH")

# set up provisional lactating (moms) and calf categories based on the behavior column
# these will be adjusted and filled in later
allbehs  			<- strsplit(as.character(dat$Behaviors), "\\.")
calfs_bybehavior    <- sapply(allbehs, function(l) any(l %in% calfbehs))
calfs 				<- calfs_bybehavior | (dat$BirthYear >= dat$Year)
moms	 			<- sapply(allbehs, function(l) any(l %in% mombehs))

# make moms and calves
ageclass_tmp[calfs] <- "calf"
lactating_tmp[moms]  <- "potentiallact"

# make calves older than december 1st of their "birthyear" juvs
calfjuvcutoff <- paste0(dat$BirthYear, "-12-01")
calfjuvcutoff[is.na(dat$BirthYear)] <- NA
calfjuvcutoff <- as.Date(calfjuvcutoff)
ageclass_tmp[which(calfs & date >= calfjuvcutoff)] <- "J"

# make the rest of the juveniles (at this point this is everything that is unassigned)
ageclass_tmp[is.na(ageclass_tmp)] <- "J"

# go mom-calf pair by pair
# look for first sighting
# make lactating from first sighting until dec 1st or calf dies

# while I'm doing this make an improved cdat which has each mom-calf pair and the first day seen and last day of lactation
# should either be December 1st or the day of last calf sighting before inferred death of calf.
# i'll need this for later when i'm making the day by day availability matrix

first_lactday_tmp <- vector(mode = "character", length = nrow(cdat))
last_lactday_tmp  <- vector(mode = "character", length = nrow(cdat))

for(m in 1:nrow(cdat)) {
	calfyear <- cdat$CalvingYear[m]
	calfyear_early <- calfyear - 1
	mom <- cdat$EGNo[m]
	
	# find that mom and that calfyear (and the year before)
	dese <- (dat$EGNo == mom & (dat$Year == calfyear | dat$Year == calfyear_early))
	
	# get rid off entries that have no year, month, or day
	# Ok if no time, because we only need day resolution
	dese <- dese & !is.na(date)
	
	lactating_st <- min(which(lactating_tmp[dese] == "potentiallact"))
	
	if(lactating_st != Inf) {
		calfjuvcutofflate <- paste0(calfyear, "-12-01")
		calfjuvcutofflate <- as.Date(calfjuvcutofflate)
		
		lactating_en <- max(which(date[dese] < calfjuvcutofflate))
		
		# save these for later to incorporate into cdat
		# start off with december 1 as last lactday, but if calf dies update to earlier
		first_lactday_tmp[m] <- as.character(date[dese][lactating_st])
		last_lactday_tmp[m]  <- as.character(max(date[dese]))
		
		lostcalf <- grep("LOST", allbehs[dese])
		if(length(lostcalf) != 0) {
			lostcalf_index <- max(lostcalf)
			lostcalf_date <- as.Date(date[dese][lostcalf_index]) + 1
			lactating_en <- max(which(date[dese] < lostcalf_date))
			
			# update last_lactday
			last_lactday_tmp[m] <- as.character(date[dese][lostcalf_index])
		}
		lactating_tmp[dese][lactating_st:lactating_en] <- "L"
	}
}

# add in additional information to cdat
cdat[, 'first_lactday'] <- first_lactday_tmp
cdat[, 'last_lactday'] <- last_lactday_tmp

# remove "potentiallact" temporary designation
lactating_tmp[lactating_tmp == "potentiallact"] <- NA

dat[, 'lactating'] <- lactating_tmp
dat[, 'ageclass'] <- ageclass_tmp


### make id based on agesex

agesexid_tmp <- paste0(ageclass_tmp, dat$Gender)
agesexid_tmp[agesexid_tmp == "AF" & lactating_tmp == "L"] <- "LF"
agesexid_tmp[agesexid_tmp == "AF"] <- "NF"


dat[, 'agesexid'] <- paste0(dat$EGNo, agesexid_tmp)


### make a table of birthdays, firstsightings, and deaths
# check to make sure that there are no typos in BithYear or FirstYearSighted
# there should be only one unique value for each animal for both of values
# watch the multinegative logic...
dat_list <- split(dat, dat$EGNo)

stopifnot(!any(sapply(dat_list, function(l) length(unique(l$BirthYear))) != 1))
stopifnot(!any(sapply(dat_list, function(l) length(unique(l$FirstYearSighted))) != 1))

# make births and first
uids <- sort(unique(dat$EGNo))
nids <- length(uids)

births <- sapply(dat_list, function(l) unique(l$BirthYear))
firsts <- sapply(dat_list, function(l) unique(l$FirstYearSighted))

# look for deaths
deadcodes <- grep("DEAD", allbehs)
dat[, 'date'] <- date
dat_deads <- dat[deadcodes, ]
dat_deads_list <- split(dat_deads, dat_deads$EGNo)
deaths_tmp <- do.call('c', lapply(dat_deads_list, function(l) min(l$date)))

deaths <- vector(mode = "character", length = nids)
deaths[1:length(deaths)] <- NA
deaths[match(names(deaths_tmp), uids)] <- as.character(deaths_tmp)

# calculate deaths using the 6 year rule
lasts <- sapply(dat_list, function(l) max(l$Year, na.rm = TRUE))
lastsdeath <- lasts + 6
lastsdeath[!is.na(lastsdeath)] <- paste0(lastsdeath[!is.na(lastsdeath)], "-01-01")

# kill if we know a real death date
lastsdeath[!is.na(deaths)] <- NA

# comebine lastsdeath and deaths
deathdates <- lastsdeath
deathdates[is.na(lastsdeath)] <- deaths[is.na(lastsdeath)]

birthdeath <- data.frame(EGNo = uids, birthyear = births, firstyearsighted = firsts, lastyearsighted = lasts, lastsighteddeath = lastsdeath, knowndeathdate = deaths, deathdates = deathdates, stringsAsFactors = FALSE)
write.table(birthdeath, "../data/birthdata.csv", row.names = FALSE, sep = ',')


### remove calfs
dat <- dat[dat$ageclass != "calf", ]
