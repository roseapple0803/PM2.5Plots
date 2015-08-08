library(plyr)
library(dplyr)
library(ggplot2)


## go retrieve these two files on the remote site if they don't exist
drawPlot4 <- function()
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


	sccCombCoal <- filter(SCC, grepl("Fuel Comb", EI.Sector, ignore.case=TRUE))
	sccCombCoal <- filter(sccCombCoal, grepl("Coal", EI.Sector, ignore.case=TRUE))
	sccCombCoal <- sccCombCoal[,c("SCC", "EI.Sector")]
	combcoal <- merge(sccCombCoal, NEI, by='SCC')

	by_year <- group_by(combcoal, year)
	pm25CombCoal <- summarize(by_year, total=sum(Emissions))


	png(file="plot4.png", width=480, height=480) ## call a graphics device and send the plot to the file, plot1.png
	par(mar=c(5.1,6.1,4.1,2.1))
	
	with(pm25CombCoal, plot(year, total, col=rgb(1,0,0,0.5), pch=19, type="b", xlab="YEAR", ylab="",  main="PM2.5 emissions from \ncoal combustion-related sources", xaxt="n", yaxt="n"))
	mtext("TOTAL EMISSIONS (TONS)",side=2,line=4)
	axis(1, at = seq(1999, 2008, by = 3))
	axis(2, at = seq(300000, 600000, by = 50000), las=1)


	dev.off() ## close the device
}


