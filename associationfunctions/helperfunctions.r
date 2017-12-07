 ### new helper
dates.ct <- as.POSIXct(ww$Date, format = "%m/%d/%y %H:%M", tz = "UTC")
dates.lt <- as.POSIXlt(dates.ct)
ww$year  <- dates.lt$year + 1900

years 	<- ww$year
uyears <- sort(unique(ww$year)) # this is sorted.
nyears 	<- length(uyears)

ids 	<- ww$ID
uids 	<- sort(unique(ww$ID))
nids 	<- length(unique(ww$ID))
a <- which(uids == "2029JF")
b <- which(uids == "1123LF")
naxlook(nax_birthdeath_byyear[c(a, b), ], ux = uids[c(a, b)], nx = 2, xlabs = TRUE)





### old helper
naxlook(nax.backup[which(shortid == shortid[464]), ])
naxlook(nax[which(shortid == shortid[464]), ])

naxlook(nax.backup[which(agesex == "JF"), ])
naxlook(nax[which(agesex == "NF"), ])
naxlook(nax[which(agesex == "JF"), ] + nax.backup[which(agesex == "JF"), ])

apply(nax[which(agesex == "JF"), ], 1, function(x) {
	x[which(x > 1)] <- 1
	sum(x)
}

birth[match("2503", birth$EGNo), ]
years_byid$"2503JF"

naxlook(nax.backup[which(agesex == "NF"), ])
naxlook(nax[which(agesex == "NF"), ])
naxlook(nax[which(agesex == "NF"), ] + nax.backup[which(agesex == "NF"), ])



 naxlook(nax[which(agesex %in% c("LF", "NF"))[21:40], ])
 ux <- uids[which(agesex %in% c("LF", "NF"))[21:40]]
 nx <- length(which(agesex %in% c("LF", "NF"))[21:40])
 
naxlook(nax[1:10, ])
ux <- uids[1:10]
nx <- length(ux)

plotdims <- par()$usr
xseq <- seq(plotdims[1], plotdims[2], len = nx + 1)
xinc <- xseq[2] - xseq[1]
xats <- xseq[1:nx] + xinc/2
axis(1, at = xats, labels = ux, las = 2, tick = FALSE, cex.axis = 0.75)	

 
 