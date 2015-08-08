library(plyr)
library(dplyr)
library(ggplot2)


## go retrieve these two files on the remote site if they don't exist
drawPlot5 <- function()
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

	nei.onroad <- filter(NEI, type=="ON-ROAD" & fips == "24510")
	by_year <- group_by(nei.onroad, year)
	pm25BaltimoreOnroad <- summarize(by_year, total=sum(Emissions))
	

	neiBaltimore <- NEI[NEI$fips == "24510",]
	pm25BaltimoreType <- ddply(neiBaltimore, c("year", "type"), summarize, total = sum(Emissions))

	png(file="plot5.png", width=480, height=480) ## call a graphics device and send the plot to the file, plot1.png
	par(lwd=2, mar=c(5.1,5.1,4.1,2.1))
	
	g <- ggplot(pm25BaltimoreOnroad, aes(x=factor(year), y=total))
	theplot <- g + geom_bar(width = .5, stat="identity", fill=rgb(1,0,0,0.5)) + xlab("YEAR") + 
		ylab("TOTAL EMISSIONS (TONS)") + ggtitle("PM2.5 emissions from \nmotor vehicle sources in Baltimore City")
	print(theplot)


	dev.off() ## close the device
}


