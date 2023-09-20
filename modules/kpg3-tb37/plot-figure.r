if (!require(ggplot2)) install.packages('ggplot2')
if (!require(plyr)) install.packages('plyr')
if (!require(patchwork)) install.packages('patchwork')
if (!require(scales)) install.packages('scales')

dat <- read.csv('https://www.researchequals.com/api/modules/main/pzgw-a32k')
dat <- dat[dat$include == 1, ]
dat <- dat[dat$scope != 'n/a', ]

names(dat)[names(dat) == "publisher"] <- "Publisher"
names(dat)[names(dat) == "tCo2e"] <- "mtC02e"
names(dat)[names(dat) == "year"] <- "Year"
names(dat)[names(dat) == "scope"] <- "Scope"

dat$Scope[dat$Scope == 1] <- "Scope 1"
dat$Scope[dat$Scope == 2] <- "Scope 2"
dat$Scope[dat$Scope == 3] <- "Scope 3"

dat$mtC02e[dat$mtC02e == "n/a"] <- NA
dat$mtC02e <- gsub(pattern = ',', replacement = '', x = dat$mtC02e)
dat$mtC02e <- as.numeric(dat$mtC02e)

plotData <- ddply(dat, .(Scope, Publisher, Year), summarise, mtC02e=sum(mtC02e, na.rm = TRUE))

# Panel 1 - fixed scales for comparison
p1 <- ggplot(data = plotData, aes(x = Year, y = mtC02e, color = Publisher)) + 
geom_line() + facet_grid(vars(Scope)) + ggtitle("(A) Yearly GHG emissions (fixed y-scale)") + scale_y_continuous(labels = scales::comma) + theme(legend.position = 'none')

# Panel 2 - free scales for relative change
p2 <- ggplot(data = plotData, aes(x = Year, y = mtC02e, color = Publisher)) + 
geom_line() + facet_grid(vars(Scope), scales="free") + ggtitle("(B) Yearly GHG emissions (variable y-scale)") + scale_y_continuous(labels = scales::comma)

# Panel 3 - Scope comparison across Publishers
plotData2020 <- plotData[plotData$Year == 2020, ]
p3 <- ggplot(data = plotData2020, aes(x = Publisher, y = mtC02e)) + 
geom_col(position = "dodge", aes(fill = Scope)) + ggtitle("(C) GHG emissions for 2020") + scale_y_continuous(labels = scales::comma)

pdf('plot.pdf', height = 10, width = 10)
(p1 | p2) / p3
dev.off()

png('plot.png', height = 1000, width = 1000)
(p1 | p2) / p3
dev.off()
