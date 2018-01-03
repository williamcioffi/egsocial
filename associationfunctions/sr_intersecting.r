###
# calculate sr index
# using a max association distance for right whales
# only using denominator values for whales that were "alive" in same year
# ~wrc 20170102

source("../associationfunctions/distance.R")

### these functions for outer

	pairdistances <- function(n, p) {
		lat1 <- ww[n, 'yy']
		lon1 <- ww[n, 'xx']
		lat2 <- ww[p, 'yy']
		lon2 <- ww[p, 'xx']
					
		dists <- latlond(lat1, lon1, lat2, lon2)
		
		return(dists)
	}
	
	timedifferences <- function(n, p) {
		timedif <- dateswithtimes[p] - dateswithtimes[n]
		timedif.hours <- abs(as.numeric(timedif, units = "hours"))
		return(timedif.hours)
	}


max_assoc_distance <- 10 #km
numsampcutoff <- 35
overlapdaycutoff <- 365
relative_speed_km_per_hour <- 3.1 * 2

dates <- as.Date(ww$Date, tz = "UTC")
dateswithtimes <- as.POSIXct(ww$Date, tz = "UTC")
udates <- unique(dates)
# uyears <- as.POSIXlt(udates)$year + 1900

uids <- sort(unique(ww$ID))
nids <- length(uids)
nsamp <- length(udates)

assoc <- matrix(0, nids, nids)
rownames(assoc) <- uids
colnames(assoc) <- uids

nax <- matrix(0, nids, nsamp)
rownames(nax) <- uids
colnames(nax) <- as.character(udates)

starts <- Sys.time()
pb <- txtProgressBar(style = 3)
for(i in 1:nsamp) {
setTxtProgressBar(pb, i/nsamp)
	samp <- which(dates == udates[i])
	
	curids <- ww[samp,]$ID
	nax[match(curids, uids), i] <- 1

	dists <- outer(samp,samp, FUN = pairdistances)
	diag(dists) <- NA

	timediffs <- outer(samp, samp, FUN = timedifferences)
	diag(timediffs) <- NA

	new_dists <- dists + (relative_speed_km_per_hour * timediffs)
	close_enough <- which(new_dists <= max_assoc_distance, arr.ind = TRUE)

	if(nrow(close_enough) > 0) {
		a <- match(ww$ID[samp[close_enough[,1]]], uids)
		b <- match(ww$ID[samp[close_enough[,2]]], uids)
		kill <- which(a == b)
		if(length(kill) > 0) {
			a <- a[-1*kill]
			b <- b[-1*kill]
		}
		goods <- unique(c((b-1)*nids + a))
		assoc[goods] <- assoc[goods] + 1
		}
}
close(pb)
Sys.time() - starts

overlapping <- which(overlap >= overlapdaycutoff, arr.ind = TRUE)

nmatrix <- matrix(0, nids, nids)
yab_prime <- matrix(0, nids, nids)

numsamp_a <- matrix(0, nids, nids)
numsamp_b <- matrix(0, nids, nids)

starts <- Sys.time()
pb <- txtProgressBar(style = 3)
for(i in 1:nrow(overlapping)) {
		a <- overlapping[i, 1]
		b <- overlapping[i, 2]
		
setTxtProgressBar(pb, i/nrow(overlapping))
		nax_tmp <- nax[c(a, b), ]
		nax_avail_tmp <- nax_avail_nou[c(a, b), ]
		samps_tmp <- colnames(nax_avail_tmp)[colSums(nax_avail_tmp) == 2]
		samps_tmp <- intersect(samps_tmp, colnames(nax))
		nax_tmp <- nax[c(a, b), samps_tmp]
		
		numsamp_a[c(a, b), c(a, b)] <- sum(nax_tmp[1, ])
		numsamp_b[c(a, b), c(a, b)] <- sum(nax_tmp[2, ])
		
		yab_prime_tmp <- tcrossprod(nax_tmp)
		yab_prime[c(a, b), c(a, b)] <- yab_prime_tmp
		
		nsight_tmp <- rowSums(nax_tmp)
		nmatrix_tmp <- outer(nsight_tmp, nsight_tmp, '+')
		nmatrix[c(a, b), c(a, b)] <- nmatrix_tmp
}
close(pb)
Sys.time() - starts

yab <- yab_prime - assoc
sr <- assoc / (nmatrix - assoc - yab)
sr[which(assoc == 0)] <- 0

diag(sr) <- NA

neverseentogether <- which(overlap <= overlapdaycutoff)
sr[neverseentogether] <- NA

# get rid of individuals seen less than numsampcutoff times
diag(numsamp_a) <- NA
diag(numsamp_b) <- NA

notenoughsightings <- which(numsamp_a < numsampcutoff | numsamp_b < numsampcutoff)
sr_filtered <- sr
sr_filtered[notenoughsightings] <- NA

isallna <- apply(sr_filtered, 2, function(r) all(is.na(r)))

sr_filtered <- sr_filtered[!isallna, !isallna]
