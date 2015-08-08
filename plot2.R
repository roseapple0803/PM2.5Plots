library(plyr)
library(dplyr)


## go retrieve these two files on the remote site if they don't exist
drawPlot2 <- function()
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
	pm25Baltimore <- ddply(neiBaltimore, c("year"), summarize, total = sum(Emissions))
	
	png(file="plot2.png", width=480, height=480) ## call a graphics device and send the plot to the file, plot1.png
	par(mar=c(5.1,5.1,4.1,2.1))
	with(pm25Baltimore, plot(year, total, col=rgb(1,0,0,0.5), pch=19, type="b", xlab="YEAR", ylab="",  main="PM2.5 Emissions in Baltimore", xaxt="n", yaxt="n"))
	mtext("TOTAL EMISSIONS (TONS)",side=2,line=4)
	axis(1, at = seq(1999, 2008, by = 3))
	axis(2, at = seq(1000, 4000, by = 100), las=1)	

	dev.off() ## close the device
}


