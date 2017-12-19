### NON FUNCTIONAL CODE STUBS
### WORK IN PROGRESS

###
# birthdeath_byday.r
# create an availability matrix for egs sensitive to agesex class
# calculate for each sampling day instead of by year (birthdeath.r)
# this code ignores unknown age animals UM, UF. UUs, JUs, and AUs have been removed already.
# ~wrc 20171211

### plotting function
naxlook <- function(nax, nids = nids, ny = nyears, uids = uids, uy = uyears, ylabs = FALSE, ...) {
	par(mar = rep(0, 4), oma = c(4.1, 4.1, 0, 0))
	image(t(nax), axes = FALSE, col = c("white", "black", "orange", "green", "red")[1:(max(nax) + 1)], ...)
	plotdims <- par()$usr
	
	xseq <- seq(plotdims[1], plotdims[2], len = ndates)[seq(1, ndates, by = 365)]
	xinc <- xseq[2] - xseq[1]
	xats <- xseq[1: ny] + xinc/2
	
	abline(v = xseq, col = "purple")
	axis(1, at = xats, labels = uy, las = 2, tick = FALSE, cex.axis = 0.75)
	
	if(ylabs) {
		yseq <- seq(plotdims[3], plotdims[4], len = nrow(nax) + 1)
		yinc <- yseq[2] - yseq[1]
		yats <- yseq[1:nrow(nax)] + yinc/2
		abline(h = yseq, col = "lightgrey")
		axis(2, at = yats, labels = rownames(nax), las = 2, tick = FALSE, cex.axis = 0.75)	
	}
}

### assign each id an agesex for each sampling (survey) day
source("../dataprep/egsocial_agesex_assignment.R")

### constants
KILLVALUE <- 2 # anything set to this will be killed from the matrix at the end of the loop (set to 0)
ADDVALUE <- 3 # anything set to this will be added as a 1  in the matrix at the end of the loop

stdate <- as.POSIXct(paste0(format(min(date, na.rm = TRUE), "%Y"), "-01-01 00:00:00"), tz = "UTC")
endate <- as.POSIXct(paste(max(date, na.rm = TRUE), "00:00:00"), tz = "UTC")

udates <- as.Date(seq.POSIXt(stdate, endate, by = "day"))
ndates <- length(udates)

uyears <- unique(format(udates, "%Y"))
nyears <- length(uyears)

ids 		<- dat$agesexid
uids 	<- sort(unique(ids))
nids 	<- length(uids)

agesex  <- substring(uids, 5, 6)
age 	<- substring(agesex, 1, 1)
sex 	<- substring(agesex, 2, 2)

nax 				<- matrix(0, nids, ndates)
rownames(nax) 	<- uids
colnames(nax) 	<- as.character(udates)

shortid  <- substring(uids, 1, 4)
ushortid <- unique(shortid)
nshortid <- length(ushortid)

### juvs
js <- which(age == "J")
nj <- length(js)

for(i in 1:nj) {
	sid <- shortid[js][i]
	birthyear <- birthdeath[match(shortid[js][i], birthdeath$EGNo), 'birthyear']
	firstyear <- birthdeath[match(shortid[js][i], birthdeath$EGNo), 'firstyearsighted']
	deathdate <- birthdeath[match(shortid[js][i], birthdeath$EGNo), 'deathdates']
	
	if(is.na(birthyear)) {
		stdate <- as.Date(paste0(firstyear, "-01-01"))
		endate <- as.Date(paste0(firstyear + 7, "-12-31"))
	} else {
		stdate <- as.Date(paste0(birthyear, "-12-01"))
		endate <- as.Date(paste0(birthyear + 8, "-12-31"))
	}
	
	if(sex[js][i] == "F") {
		firstcalving <- min(cdat[match(shortid[js][i], cdat$EGNo), 'CalvingYear'])
		if(!is.na(firstcalving)) {
			adultcalving <- as.Date(paste0(firstcalving - 1, "-01-01"))
			if(endate > adultcalving) {
				endate <- adultcalving
			}
		}
	}

	st <- min(which(udates >= stdate), na.rm = TRUE)
	en <- max(which(udates <= endate), na.rm = TRUE)
	
	deathdate <- max(which(udates < deathdate))
	
	if(en > deathdate) {
		en <- deathdate - 1
	}
	
	nax[js[i], st:en] <- ADDVALUE
}

### adult males
ms <- which(age == "A")
nm <- length(ms)

for(i in 1:nm) {
	sid <- shortid[ms][i]
	birthyear <- birthdeath[match(shortid[ms][i], birthdeath$EGNo), 'birthyear']
	firstyear <- birthdeath[match(shortid[ms][i], birthdeath$EGNo), 'firstyearsighted']
	deathdate <- birthdeath[match(shortid[ms][i], birthdeath$EGNo), 'deathdates']
	
	if(is.na(birthyear)) {
		stdate <- as.Date(paste0(firstyear + 8, "-01-01"))
	} else {
		stdate <- as.Date(paste0(birthyear + 9, "-01-01"))
	}
	
	st <- min(which(udates >= stdate), na.rm = TRUE)
	en <- max(which(udates < deathdate), na.rm = TRUE)
	
	if(en > st) {
		nax[ms[i], st:en] <- ADDVALUE
	}
}

### non-lactating females
# this one is slightly different than the previous
# because we'll take our cue from the JF for where to start if one exists
ns <- which(age == "N")
nn <- length(ns)

for(i in 1:nn) {
	sid <- shortid[ns][i]
	birthyear <- birthdeath[match(shortid[ns][i], birthdeath$EGNo), 'birthyear']
	firstyear <- birthdeath[match(shortid[ns][i], birthdeath$EGNo), 'firstyearsighted']
	deathdate <- birthdeath[match(shortid[ns][i], birthdeath$EGNo), 'deathdates']
		
	jfmatch <- match(paste0(sid, "JF"), uids)
	if(!is.na(jfmatch)) {
		st <- max(which(nax[jfmatch, ] != 0)) + 1
	} else {
		if(is.na(birthyear)) {
			stdate <- as.Date(paste0(firstyear + 8, "-01-01"))
		} else {
			stdate <- as.Date(paste0(birthyear + 9, "-01-01"))
		}
		
		firstcalving <- min(cdat[match(shortid[ns][i], cdat$EGNo), 'CalvingYear'])
		if(!is.na(firstcalving)) {
			adultcalving <- as.Date(paste0(firstcalving - 1, "-01-01"))
			if(stdate > adultcalving) {
				stdate <- adultcalving
			}
		}
		
		st <- min(which(udates >= stdate), na.rm = TRUE)
	}
	
	en <- max(which(udates < deathdate), na.rm = TRUE)
	
	if(en > st) {
		nax[ns[i], st:en] <- ADDVALUE
	}
}

### lactating females
# go back and delete these instances also from the matching non-lactating female if it exists
lf <- which(age == "L")
ln <- length(lf)

for(i in 1:ln) {
	sid <- shortid[lf][i]
	dismom <- cdat[cdat$EGNo == sid, ]
	
	lactating <- vector()
	for(d in 1:nrow(dismom)) {
		st <- which(udates == dismom[d, 'first_lactday'])
		
		if(!dismom[d, 'lost_lactday']) {
			momlast <- as.Date(paste0(dismom[d, 'CalvingYear'], "-11-30"))
			en <- max(which(udates == momlast))
		} else {
			en <- which(udates == dismom[d, 'last_lactday'])
		}
		
		lactating <- c(lactating, st:en)
	}
	
	nax[lf[i], lactating] <- ADDVALUE
	
	# kill these days for the nf if it exists
	nfmatch <- match(paste0(sid, "NF"), uids)
	if(!is.na(nfmatch)) {
		nax[nfmatch, lactating] <- KILLVALUE
	}
}