dat <- read.csv('./crossref.csv')

png('./plot.png')
plot(dat$year, dat$hits, log = "y", xlab ="Year", ylab="Publications", yaxt="n", type='l')
axis(2, at = c(500, 1000, 5000, 10000, 50000, 100000, 500000, 1000000, 5000000), labels = c("500", "1000", "5k", "10k", "50k", "100k", "500k", "1M", "5M"), las=2)
#dev.off()
