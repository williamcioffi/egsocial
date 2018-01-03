###
# mantel test in R
# ~wrc 20170912

library(vegan)
# load the data
assocm0.03 <- read.table("formantel_assocrate_0.03km.ge35numsamp_nodead.txt")
assocm01    <- read.table("formantel_assocrate_01km.ge35numsamp_nodead.txt")
assocm05    <- read.table("formantel_assocrate_05km.ge35numsamp_nodead.txt")
assocm10   <- read.table("formantel_assocrate_10km.ge35numsamp_nodead.txt")
assocm15   <- read.table("formantel_assocrate_15km.ge35numsamp_nodead.txt")
assocm20   <- read.table("formantel_assocrate_20km.ge35numsamp_nodead.txt")

agesex.tmp <- read.table("agesex.csv", header = TRUE, sep = ',', stringsAsFactors = FALSE)
anames <- read.table("names_ge35numsamp_nodead.csv", stringsAsFactors = FALSE)[, 1]
agesex <- agesex.tmp[which(agesex.tmp[, 1] %in% anames), ]
agesex <- agesex[, 2]

n <- nrow(agesex)
m <- ncol(agesex)

adists <- matrix(NA, n, m)
for(r in 1:n) {
  for(c in 1:m) {
    adists[r, c] <- agesex[r] == agesex[c]
  }
}
adists <- adists*1

mantel(assocm0.03, adists, na.rm = TRUE)
mantel(assocm01, adists, na.rm = TRUE)
mantel(assocm05, adists, na.rm = TRUE)
mantel(assocm10, adists, na.rm = TRUE)
mantel(assocm15, adists, na.rm = TRUE)
mantel(assocm20, adists, na.rm = TRUE)
