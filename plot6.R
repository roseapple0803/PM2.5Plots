library(plyr)
library(dplyr)
library(ggplot2)


## go retrieve these two files on the remote site if they don't exist
drawPlot6 <- function()
{
	if (!file.exists("summarySCC_PM25.rds") | !file.exists("Source_Classification_Code.rds"))
	{
		fileUrl <- "http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
		download.file(fileUrl, "source.zip")
		unzip("source.zip", exdir=".")
	}

	## the two files exist already
	summaryFile <- "summarySCC_PM25.rds"
	sourceFile <- "Source_Classification_Code.rds"
	
	NEI <- readRDS(summaryFile)
	SCC <- readRDS(sourceFile)

	
	criteria <- NEI$type=="ON-ROAD" & (NEI$fips == "24510" | NEI$fips == "06037")
	nei.onroad.2cities <- NEI[criteria,]
	pm25.2cities <- ddply(nei.onroad.2cities, c("year", "fips"), summarize, total = sum(Emissions))

	pm25.2cities$fips <- as.factor(pm25.2cities$fips)
	pm25.2cities$year <- as.factor(pm25.2cities$year)
	pm25.2cities$fips <- gsub(pattern="24510", replacement="Baltimore City", pm25.2cities$fips)
	pm25.2cities$fips <- gsub(pattern="06037", replacement="Los Angeles County", pm25.2cities$fips)

	png(file="plot6.png", width=480, height=480) ## call a graphics device and send the plot to the file, plot1.png
	par(lwd=2, mar=c(5.1,5.1,4.1,2.1))
	
	g <- ggplot(pm25.2cities, aes(x=year, y=total)) 
	theplot <- g + geom_bar(width = .5, fill="white", color=rgb(1,0,0,0.5), stat="identity")  + facet_wrap(~fips) +
		xlab("YEAR") + ylab("TOTAL EMISSIONS (TONS)") + ggtitle("PM2.5 emissions from \nmotor vehicle sources in two cities")
	print(theplot)


	dev.off() ## close the device
}


