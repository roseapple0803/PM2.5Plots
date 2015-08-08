library(plyr)
library(dplyr)
library(ggplot2)


## go retrieve these two files on the remote site if they don't exist
drawPlot3 <- function()
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

	neiBaltimore <- NEI[NEI$fips == "24510",]
	pm25BaltimoreType <- ddply(neiBaltimore, c("year", "type"), summarize, total = sum(Emissions))

	png(file="plot3.png", width=480, height=480) ## call a graphics device and send the plot to the file, plot1.png
	par(lwd=2, mar=c(5.1,5.1,4.1,2.1))
	
	g <- qplot(year, total, data=pm25BaltimoreType, fill=type, col=type, geom="path", main="Four types of emission sources \nin Baltimore City")
	theplot <- g + xlab("YEAR") + ylab("TOTAL EMISSIONS (TONS)")
	print(theplot)
	## consider using
	## ggtitle(expression("Emissions of" ~ PM[2.5] ~ " in Baltimore City"))

#####################################################################################################
#  <- ggplot(aggregatedTotalByYearAndType, aes(year, Emissions, color = type))
# g <- g + geom_line(size=1) +
#  xlab("year") +
#  ylab(expression('Total PM'[2.5]*" Emissions")) +
#  ggtitle('Total Emissions in Baltimore City, Maryland (fips == "24510") from 1999 to 2008')
#####################################################################################################


	dev.off() ## close the device
}


