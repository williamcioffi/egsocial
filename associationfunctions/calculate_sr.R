###
# calculate sr index 
# using a max associaton distance for right whales
# ~wrc 20171003

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

source("birthdeath.r")
source("distance.R")

max_assoc_distance <- 20 #km
numsampcutoff <- 35
relative_speed_km_per_hour <- 3.1 * 2

ww <- read.table("Master_DATA_primary.csv", header = TRUE, sep = ',')
ww[, 'xx'] <- ww[, 'xx']*-1 # correct the lons

dates <- as.Date(ww$Date, format = "%m/%d/%y %H:%M", tz = "UTC")
dateswithtimes <- as.POSIXct(ww$Date, format = "%m/%d/%y %H:%M", tz = "UTC")
udates <- unique(dates)


uids <- sort(unique(ww$ID))
nids <- length(uids)
nsamp <- length(udates)

assoc <- matrix(0, nids, nids)
rownames(assoc) <- uids
colnames(assoc) <- uids

nax <- matrix(0, nids, nsamp)
rownames(nax) <- uids

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

idcount <- data.frame(rowSums(nax))
nmatrix <- outer(idcount[,1], idcount[,1], FUN = "+")

diag(nmatrix) <- 0
diag(assoc) <- 0

yab_prime <- tcrossprod(nax) #equivalent to nax %*% t(nax)
yab <- yab_prime - assoc

sr <- assoc / (nmatrix - assoc - yab)
diag(sr) <- 1

### find out which ids were never seen together
# and remove them from the nmatrix

nax_birthdeath <- birthdeath()
togethers <- tcrossprod(nax_birthdeath)
neverseentogether <- which(togethers == 0)
nmatrix[neverseentogether] <- NA

### filter by number of days
numsamp <- apply(nax, 1, sum)
numrec <- table(ww$ID)

keep <- which(numsamp >= numsampcutoff)
assoc_filtered <- assoc[keep, keep]
nmatrix_filtered <- nmatrix[keep, keep]

### output sr tables
yab_filtered <- yab[keep, keep]

sr_filtered <- assoc_filtered / (nmatrix_filtered - assoc_filtered - yab_filtered)
diag(sr_filtered) <- 1

# fname <- paste0("names_", max_assoc_distance, "km.ge", numsampcutoff, "numsamp_nodead.csv")
# write.table(rownames(assrate_filtered), fname, row.names = FALSE, sep = ',', col.names = FALSE)

# fname <- paste0("sr_", max_assoc_distance, "km.ge", numsampcutoff, "numsamp_nodead.csv")
# write.table(sr_filtered, fname, sep = ',', col.names = NA)


### output to .mat
# library(R.matlab)

# fname <- paste0("sr_", max_assoc_distance, "km.ge", numsampcutoff, "numsamp_nodead.csv")
# writeMat(fname, sr = sr_filtered)
# fname <- paste0("moremats_", max_assoc_distance, "km.ge", numsampcutoff, "numsamp_nodead.csv")
# writeMat(fname, yab = yab_filtered, assoc = assoc_filtered, nmatrix = nmatrix_filtered, sr = sr_filtered)


### calculate agesex rates

diag(sr_filtered) <- NA

meanassocrates <- as.data.frame(apply(sr_filtered, 1, mean, na.rm = TRUE))
# write.table(meanassocrates, "mean-adj-assoc-rates.csv", col.names = TRUE, row.names = TRUE, sep = ',')

agesex <- substring(rownames(sr_filtered), 5,6)

jf <- which(agesex == "JF")
nf <- which(agesex == "NF")
uf <- which(agesex == "UF")
lf <- which(agesex == "LF")

am <- which(agesex == "AM")
um <- which(agesex == "UM")
jm <- which(agesex == "JM")

au <- which(agesex == "AU")
ju <- which(agesex == "JU")
uu <- which(agesex == "UU")


###count


focal <- jf
jfpaired_count <- c(
length(which(!is.na(sr_filtered[focal,jf]))),
length(which(!is.na(sr_filtered[focal,jm]))),
length(which(!is.na(sr_filtered[focal,lf]))),
length(which(!is.na(sr_filtered[focal,nf]))),
length(which(!is.na(sr_filtered[focal,am])))
)

focal <- jm
jmpaired_count <- c(
length(which(!is.na(sr_filtered[focal,jf]))),
length(which(!is.na(sr_filtered[focal,jm]))),
length(which(!is.na(sr_filtered[focal,lf]))),
length(which(!is.na(sr_filtered[focal,nf]))),
length(which(!is.na(sr_filtered[focal,am])))
)

focal <- lf
lfpaired_count <- c(
length(which(!is.na(sr_filtered[focal,jf]))),
length(which(!is.na(sr_filtered[focal,jm]))),
length(which(!is.na(sr_filtered[focal,lf]))),
length(which(!is.na(sr_filtered[focal,nf]))),
length(which(!is.na(sr_filtered[focal,am])))
)

focal <- nf
nfpaired_count <- c(
length(which(!is.na(sr_filtered[focal,jf]))),
length(which(!is.na(sr_filtered[focal,jm]))),
length(which(!is.na(sr_filtered[focal,lf]))),
length(which(!is.na(sr_filtered[focal,nf]))),
length(which(!is.na(sr_filtered[focal,am])))
)

focal <- am
ampaired_count <- c(
length(which(!is.na(sr_filtered[focal,jf]))),
length(which(!is.na(sr_filtered[focal,jm]))),
length(which(!is.na(sr_filtered[focal,lf]))),
length(which(!is.na(sr_filtered[focal,nf]))),
length(which(!is.na(sr_filtered[focal,am])))
)


### mean

focal <- jf
jfpaired <- c(
mean(sr_filtered[focal,jf], na.rm = TRUE),
mean(sr_filtered[focal,jm], na.rm = TRUE),
mean(sr_filtered[focal,lf], na.rm = TRUE),
mean(sr_filtered[focal,nf], na.rm = TRUE),
mean(sr_filtered[focal,am], na.rm = TRUE)
)

focal <- jm
jmpaired <- c(
mean(sr_filtered[focal,jf], na.rm = TRUE),
mean(sr_filtered[focal,jm], na.rm = TRUE),
mean(sr_filtered[focal,lf], na.rm = TRUE),
mean(sr_filtered[focal,nf], na.rm = TRUE),
mean(sr_filtered[focal,am], na.rm = TRUE)
)

focal <- lf
lfpaired <- c(
mean(sr_filtered[focal,jf], na.rm = TRUE),
mean(sr_filtered[focal,jm], na.rm = TRUE),
mean(sr_filtered[focal,lf], na.rm = TRUE),
mean(sr_filtered[focal,nf], na.rm = TRUE),
mean(sr_filtered[focal,am], na.rm = TRUE)
)

focal <- nf
nfpaired <- c(
mean(sr_filtered[focal,jf], na.rm = TRUE),
mean(sr_filtered[focal,jm], na.rm = TRUE),
mean(sr_filtered[focal,lf], na.rm = TRUE),
mean(sr_filtered[focal,nf], na.rm = TRUE),
mean(sr_filtered[focal,am], na.rm = TRUE)
)

focal <- am
ampaired <- c(
mean(sr_filtered[focal,jf], na.rm = TRUE),
mean(sr_filtered[focal,jm], na.rm = TRUE),
mean(sr_filtered[focal,lf], na.rm = TRUE),
mean(sr_filtered[focal,nf], na.rm = TRUE),
mean(sr_filtered[focal,am], na.rm = TRUE)
)

### max

focal <- jf
jfpaired_max <- c(
max(sr_filtered[focal,jf], na.rm = TRUE),
max(sr_filtered[focal,jm], na.rm = TRUE),
max(sr_filtered[focal,lf], na.rm = TRUE),
max(sr_filtered[focal,nf], na.rm = TRUE),
max(sr_filtered[focal,am], na.rm = TRUE)
)

focal <- jm
jmpaired_max <- c(
max(sr_filtered[focal,jf], na.rm = TRUE),
max(sr_filtered[focal,jm], na.rm = TRUE),
max(sr_filtered[focal,lf], na.rm = TRUE),
max(sr_filtered[focal,nf], na.rm = TRUE),
max(sr_filtered[focal,am], na.rm = TRUE)
)

focal <- lf
lfpaired_max <- c(
max(sr_filtered[focal,jf], na.rm = TRUE),
max(sr_filtered[focal,jm], na.rm = TRUE),
max(sr_filtered[focal,lf], na.rm = TRUE),
max(sr_filtered[focal,nf], na.rm = TRUE),
max(sr_filtered[focal,am], na.rm = TRUE)
)

focal <- nf
nfpaired_max <- c(
max(sr_filtered[focal,jf], na.rm = TRUE),
max(sr_filtered[focal,jm], na.rm = TRUE),
max(sr_filtered[focal,lf], na.rm = TRUE),
max(sr_filtered[focal,nf], na.rm = TRUE),
max(sr_filtered[focal,am], na.rm = TRUE)
)

focal <- am
ampaired_max <- c(
max(sr_filtered[focal,jf], na.rm = TRUE),
max(sr_filtered[focal,jm], na.rm = TRUE),
max(sr_filtered[focal,lf], na.rm = TRUE),
max(sr_filtered[focal,nf], na.rm = TRUE),
max(sr_filtered[focal,am], na.rm = TRUE)
)


### sd

focal <- jf
jfpaired_sd <- c(
sd(sr_filtered[focal,jf], na.rm = TRUE),
sd(sr_filtered[focal,jm], na.rm = TRUE),
sd(sr_filtered[focal,lf], na.rm = TRUE),
sd(sr_filtered[focal,nf], na.rm = TRUE),
sd(sr_filtered[focal,am], na.rm = TRUE)
)

focal <- jm
jmpaired_sd <- c(
sd(sr_filtered[focal,jf], na.rm = TRUE),
sd(sr_filtered[focal,jm], na.rm = TRUE),
sd(sr_filtered[focal,lf], na.rm = TRUE),
sd(sr_filtered[focal,nf], na.rm = TRUE),
sd(sr_filtered[focal,am], na.rm = TRUE)
)

focal <- lf
lfpaired_sd <- c(
sd(sr_filtered[focal,jf], na.rm = TRUE),
sd(sr_filtered[focal,jm], na.rm = TRUE),
sd(sr_filtered[focal,lf], na.rm = TRUE),
sd(sr_filtered[focal,nf], na.rm = TRUE),
sd(sr_filtered[focal,am], na.rm = TRUE)
)

focal <- nf
nfpaired_sd <- c(
sd(sr_filtered[focal,jf], na.rm = TRUE),
sd(sr_filtered[focal,jm], na.rm = TRUE),
sd(sr_filtered[focal,lf], na.rm = TRUE),
sd(sr_filtered[focal,nf], na.rm = TRUE),
sd(sr_filtered[focal,am], na.rm = TRUE)
)

focal <- am
ampaired_sd <- c(
sd(sr_filtered[focal,jf], na.rm = TRUE),
sd(sr_filtered[focal,jm], na.rm = TRUE),
sd(sr_filtered[focal,lf], na.rm = TRUE),
sd(sr_filtered[focal,nf], na.rm = TRUE),
sd(sr_filtered[focal,am], na.rm = TRUE)
)

comparemat <- matrix(c(jfpaired, jmpaired, lfpaired, nfpaired, ampaired), byrow = TRUE, 5,5)
comparemat_max <- matrix(c(jfpaired_max, jmpaired_max, lfpaired_max, nfpaired_max, ampaired_max), byrow = TRUE, 5,5)
comparemat_sd <- matrix(c(jfpaired_sd, jmpaired_sd, lfpaired_sd, nfpaired_sd, ampaired_sd), byrow = TRUE, 5,5)
comparemat_count <- matrix(c(jfpaired_count, jmpaired_count, lfpaired_count, nfpaired_count, ampaired_count), byrow = TRUE, 5, 5)

rownames(comparemat) <- c("jf", "jm", "lf", "nf", "am")
colnames(comparemat) <- c("jf", "jm", "lf", "nf", "am")

rownames(comparemat) <- c("jf", "jm", "lf", "nf", "am")
colnames(comparemat) <- c("jf", "jm", "lf", "nf", "am")

rownames(comparemat_max) <- c("jf", "jm", "lf", "nf", "am")
colnames(comparemat_max) <- c("jf", "jm", "lf", "nf", "am")

rownames(comparemat_sd) <- c("jf", "jm", "lf", "nf", "am")
colnames(comparemat_sd) <- c("jf", "jm", "lf", "nf", "am")

comparemat_paste <- paste(round(comparemat, 4), " (",round(comparemat_sd,4), ")", sep = "")
comparemat_paste_max <- paste(round(comparemat_max, 4), " (",round(comparemat_sd,4), ")", sep = "")

matkey <- matrix(c(1:25), 5, 5)
compare.df <- data.frame(rep("a", 5), rep("b", 5), rep("c", 5), rep("d", 5), rep("e", 5), stringsAsFactors = FALSE)
colnames(compare.df) <- c("jf", "jm", "lf", "nf", "am")
rownames(compare.df) <- c("jf", "jm", "lf", "nf", "am") 

for(i in 1:25) {
	cc <- which(matkey == i, arr.ind = TRUE)
	
	compare.df[cc[1], cc[2]] <- comparemat_paste[i]
}

compare.max.df <- data.frame(rep("a", 5), rep("b", 5), rep("c", 5), rep("d", 5), rep("e", 5), stringsAsFactors = FALSE)
colnames(compare.max.df) <- c("jf", "jm", "lf", "nf", "am")
rownames(compare.max.df) <- c("jf", "jm", "lf", "nf", "am")

for(i in 1:25) {
	cc <- which(matkey == i, arr.ind = TRUE)
	
	compare.max.df[cc[1], cc[2]] <- comparemat_paste_max[i]
}

compare.df[upper.tri(compare.df)] <- ""
compare.max.df[upper.tri(compare.max.df)] <- ""

compare.max.df.filename <- paste(max_assoc_distance, "km.ge", numsampcutoff, "numsamp.max.csv", sep = '')
compare.df.filename <- paste(max_assoc_distance, "km.ge", numsampcutoff, "numsamp.csv", sep = '')

# write.table(compare.max.df, compare.max.df.filename, sep = ',', col.names = NA)
# write.table(compare.df, compare.df.filename, sep = ',', col.names = NA)
