###
# remove all the duplicated rows from egsocial data file
#
# ~wrc 20171212

dat  <- read.table("../data/3-Khan-data-calf-errors-fixed.csv"       , header = TRUE, sep = ',')
dat.backup <- dat
dat <- dat[!duplicated(dat[, -1]), ]

dups <- dat.backup[duplicated(dat.backup[, -1]), -1]
ndup <- dat[, -1]

dups_paste <- apply(dups, 1, paste0, collapse = "")
ndup_paste <- apply(ndup, 1, paste0, collapse = "")

dups_match <- match(dups_paste, ndup_paste)

dups_table <- dat.backup[duplicated(dat.backup[, -1]), ]
dups_table[, 'matches'] <- dat$RowName[dups_match]

write.table(dat, file = "../data/4-Khan-data-calf-errors-fixed-nodups.csv", row.names = FALSE, sep = ',')
write.table(dups_table, file = "../data/duplicated_rows_20171212.csv", row.names = FALSE, sep = ',')