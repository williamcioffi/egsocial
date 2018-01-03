########################################
### THIS IS A TEMPORARY TESTING FILE ###
########################################

###
# prepsightdat.r
# assigns age sex classes
# calculates sighting overlap
# final clean up of dat
# ~wrc 20180102

# change this so that the raw data is loaded in this file
# and then the work on it is sourced or done in functions

# assign age sex classes
source("../dataprep/egsocial_agesex_assignment.R")

# calculate sighting overlap
source("../dataprep/calculate_available.R")

# make a cleaned up data frame and make sure nax_avail matches
# Date, yy, xx, ID

ww <- data.frame(Date = as.character(dat$date_posix), yy = dat$Latitude, xx = dat$Longitude, ID = dat$agesexid)
ww <- ww[!is.na(ww$Date), ]
ww <- ww[substring(ww$ID, 5, 5) != "U", ]

uids <- unique(ww$ID)
dese <- which(rownames(nax_avail) %in% uids)
nax_avail_nou <- nax_avail[dese, ]

# calculate overlap and kill diagonal
overlap <- tcrossprod(nax_avail_nou)
diag(overlap) <- NA

# get rid of upper triangle
overlap[upper.tri(overlap)] <- NA