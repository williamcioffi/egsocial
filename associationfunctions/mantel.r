###
# mantel test in R
# ~wrc 20180102

library(vegan)

agesex <- substring(rownames(sr_filtered), 5, 6)
adists <- outer(agesex, agesex, '==')
adists <- adists*1

mantel(sr_filtered, adists, na.rm = TRUE)