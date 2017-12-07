###
# birthdeath.r
# apply right whale specific rules about age sex class
# ~wrc 20171003

### plotting function
naxlook <- function(nax, nx = nids, ny = nyears, ux = uids, uy = uyears, xlabs = FALSE, ...) {
	par(mar = rep(0, 4), oma = c(4.1, 4.1, 0, 0))
	image(nax, axes = FALSE, col = c("white", "black", "orange", "green", "red")[1:(max(nax) + 1)], ...)
	plotdims <- par()$usr
	yseq <- seq(plotdims[3], plotdims[4], len = ny + 1)
	yinc <- yseq[2] - yseq[1]
	yats <- yseq[1: ny] + yinc/2
	
	abline(h = yseq, col = "purple")
	axis(2, at = yats, labels = uy, las = 1, tick = FALSE, cex.axis = 0.75)
	
	if(xlabs) {
		xseq <- seq(plotdims[1], plotdims[2], len = nx + 1)
		xinc <- xseq[2] - xseq[1]
		xats <- xseq[1:nx] + xinc/2
		axis(1, at = xats, labels = ux, las = 2, tick = FALSE, cex.axis = 0.75)	
	}
}

### rules

# this is the raw nax which will be modified throughout to apply birth death rules as follows:

# AM = alive each year between first year seen and either date of known death or 'presumed 
# dead' date(details below)

# NF = alive each year between first year and either date of known death or 'presumed dead' date 
# (details below) minus years they were LF

# LF = alive only in the years they were lactating and seen

# JF = alive each year between first year and last year seen. If the JF was seen as a LF or NF later 
# then each year between last year seen as JF and first year seen as LF or NF are also marked as 
# alive for JF up to age 9.

# JM = alive each year between first year and last year seen. If the JM was seen as an AM later on 
# then each year between last year seen as JM and first year seen as AM are also marked as alive 
# for JM up to age 9.

# A whale becomes presumed dead in its sixth year without sightings, on January 1st (a whale last 
# seen any time during the year 1992, is presumed dead on January 1, 1998 and any date after 
# that).

# note: I don't do anything with the JU UM UF or UU.
# note: I don't add ids to the dataset that were never observed. e.g., if a female was only observed as a LF and never as an NF.
# note: when an adult female is marked as alive for a year when it was not actually observed it is listed as an NF. This is not strictly true, since it might have been an LF and unobserved.

birthdeath <- function() {
### constants
ADULT_THRESHOLD <- 8 # years (Hamilton et al. 1998) this is based on 1st calving ages for females
MAX_YEARS_TILL_DEATH <- 5 # i.e., declared dead in their 6th year
KILLVALUE <- 2 # anything set to this will be killed from the matrix at the end of the loop (set to 0)
ADDVALUE <- 3 # anything set to this will be added as a 1  in the matrix at the end of the loop

ww <- read.table("Master_DATA_primary.csv", header = TRUE, sep = ',')
ww$xx <- -ww$xx # correct lon values

dates.ct <- as.POSIXct(ww$Date, format = "%m/%d/%y %H:%M", tz = "UTC")
dates.lt <- as.POSIXlt(dates.ct)
ww$year  <- dates.lt$year + 1900

birthdeath <- read.table("birthdeath.csv", header = TRUE, sep = ',')
kill <- -which(birthdeath$DeathYear > 2009 | birthdeath$BirthYear > 2009 | birthdeath$FirstYearSighted > 2009) 
birthdeath <- birthdeath[kill,]

birth <- birthdeath[!is.na(birthdeath$BirthYear), ]
death <- birthdeath[!is.na(birthdeath$DeathYear), ]

years 	<- ww$year
uyears <- sort(unique(ww$year)) # this is sorted.
nyears 	<- length(uyears)

ids 	<- ww$ID
uids 	<- sort(unique(ww$ID))
nids 	<- length(unique(ww$ID))

nax 			<- matrix(0, nids, nyears)
rownames(nax) 	<- uids
colnames(nax) 	<- uyears

shortid <- substring(uids, 1, 4)
agesex  <- substring(uids, 5, 6)
age 	<- substring(agesex, 1, 1)
sex 	<- substring(agesex, 2, 2)

ww_byid_list <- split(ww, ww$ID) # split sorts so these are now in uid order
years_byid <- lapply(ww_byid_list, function(l) unique(l$year))
years_byid_index <- lapply(years_byid, function(l) match(l, uyears))

# initially simple set everytime an animal was sighted as 1
for(i in 1:length(ww_byid_list)) {
	curyears <- years_byid[[i]]
	nax[i, match(curyears, uyears)] <- 1
}
	
pb <- txtProgressBar(style = 3)
for(i in 1:nids) {
setTxtProgressBar(pb, i/nids)
	curyears <- years_byid_index[[i]]

### LACTATING FEMALE
	if(agesex[i] == "LF") {
		# this should already be correctly coded
		

### NON-LACTATING FEMALE
	} else if(agesex[i] == "NF") {
		nax[i, min(curyears):max(curyears)] <- ADDVALUE
		lfmatch <- match(paste0(shortid[i], "LF"), uids) 
		
		# is there a corresponding LF?		
		if(!is.na(lfmatch)) {
			lfyears <- years_byid_index[[lfmatch]]
			
			#for death
			curyears <- c(curyears, lfyears)
		}
		
		#deal with death
		deathyear <- death[match(shortid[i], death$EGNo), 'DeathYear']
		deathyear <- deathyear - min(uyears) + 1
		
		st <- max(curyears) + 1
		en <- NA
					
		if(!is.na(deathyear)) {
			if(deathyear > max(curyears)) {
				en <- deathyear
				curyears <- c(curyears, en)
			}			
		} else {
			st <- max(curyears) + 1
			en <- st + MAX_YEARS_TILL_DEATH
		}
		
		if(!is.na(en)) {
			if(en > nyears)
				en <- nyears
			
			if(st <= nyears) {
				nax[i, st:en] <- ADDVALUE
			}
		}
		
		# go back and get rid of lfyears
		if(!is.na(lfmatch)) {
			nax[i, lfyears] <- KILLVALUE
		}
				
### JUVENILE FEMALE
	} else if(agesex[i] == "JF") {
		nfmatch <- match(paste0(shortid[i], "NF"), uids)
		lfmatch <- match(paste0(shortid[i], "LF"), uids)
		
		afmatch <- c(nfmatch, lfmatch)
		if(any(is.na(afmatch)) & !all(is.na(afmatch))) {
			afmatch <- na.omit(afmatch)
		} else if(all(is.na(afmatch))) {
			afmatch <- NULL
		}		

		
		birthyear <- birth[match(shortid[i], birth$EGNo), 'BirthYear']
		maxjuvyears <- ADULT_THRESHOLD
		
		# define start year
		st <- min(curyears)
		
		if(!is.na(birthyear)) {
			if(min(uyears[curyears]) == birthyear) {
				maxjuvyears <- ADULT_THRESHOLD + 1
			} else {
				st <- birthyear - min(uyears) + 1 + 1
			}
		}
		
		# define end year
		en <- st + maxjuvyears - 1
		
		if(!is.null(afmatch))
			en <- min(unlist(years_byid_index[afmatch])) - 1
		
		if(length(st:en) > maxjuvyears)
			en <- st + maxjuvyears - 1
		
		if(!is.na(nfmatch))
			nax[nfmatch, (en + 1):min(years_byid_index[[nfmatch]])] <- ADDVALUE
		
		# deal with death if there is no nf or lf match
		if(is.null(afmatch)) {
			deathyear <- death[match(shortid[i], death$EGNo), 'DeathYear']
			deathyear <- deathyear - min(uyears) + 1
			
			if(!is.na(deathyear)) {
				if(deathyear < en)
					en <- deathyear
			}
		}
		
		# touch up start and end
		if(st < 1) st <- 1
		if(en > length(uyears)) en <- length(uyears)
		
		
		nax[i, st:en] <- ADDVALUE
		
		
### JUVENILE MALE
	} else if(agesex[i] == "JM") {
		ammatch <- match(paste0(shortid[i], "AM"), uids)
		birthyear <- birth[match(shortid[i], birth$EGNo), 'BirthYear']
		maxjuvyears <- ADULT_THRESHOLD
		
		# define start year
		st <- min(curyears)
		
		if(!is.na(birthyear)) {
			if(min(uyears[curyears]) == birthyear) {
				maxjuvyears <- ADULT_THRESHOLD + 1
			} else {
				st <- birthyear - min(uyears) + 1 + 1
			}
		}
		
		# define end year
		en <- st + maxjuvyears - 1
		
		if(!is.na(ammatch))
			en <- min(years_byid_index[[ammatch]]) - 1
		
		
		if(length(st:en) > maxjuvyears)
			en <- st + maxjuvyears - 1
		
		if(!is.na(ammatch))
			nax[ammatch, (en + 1):min(years_byid_index[[ammatch]])] <- ADDVALUE
			
		
		# deal with death if there is no am
		if(is.na(ammatch)) {
			deathyear <- death[match(shortid[i], death$EGNo), 'DeathYear']
			deathyear <- deathyear - min(uyears) + 1
			
			if(!is.na(deathyear)) {
				if(deathyear < en)
					en <- deathyear
			}
		}
		
		# touch up start and end
		if(st < 1) 
			st <- 1
			
		if(en > length(uyears)) 
			en <- length(uyears)
		
		nax[i, st:en] <- ADDVALUE
		
		
### ADULT MALE
	} else if(agesex[i] == "AM") {
		nax[i, min(curyears):max(curyears)] <- ADDVALUE
		
		#deal with death
		deathyear <- death[match(shortid[i], death$EGNo), 'DeathYear']
		deathyear <- deathyear - min(uyears) + 1
		
		st <- max(curyears) + 1
		en <- NA
					
		if(!is.na(deathyear)) {
			if(deathyear > max(curyears)) {
				en <- deathyear
				curyears <- c(curyears, en)
			}			
		} else {
			st <- max(curyears) + 1
			en <- st + MAX_YEARS_TILL_DEATH
		}
		
		if(!is.na(en)) {
			if(en > nyears)
				en <- nyears
							
			if(st <= nyears) {
				nax[i, st:en] <- ADDVALUE
			}
		}
				
	}
}

nax[which(nax == ADDVALUE)] <- 1
nax[which(nax == KILLVALUE)] <- 0

nax
}