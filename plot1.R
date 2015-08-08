library(plyr)
library(dplyr)


## go retrieve these two files on the remote site if they don't exist
drawPlot1 <- function()
{

	##Search and download packages if not present
	if("ggplot2" %in% rownames(installed.packages()) == FALSE) 
	{
		install.packages("ggplot2")
	}
		library(ggplot2)




	if (!file.exists("summarySCC_PM25.rds") | !file.exists("Source_Classification_Code.rds"))
	{
		fileUrl <- "http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
		download.file(fileUrl, "source.zip")
		unzip("source.zip", exdir=".")
	}

	## the two files exist already
	summaryFile <- "summarySCC_PM25.rds"
	sourceFile <- "Source_Classification_Code.rds"
	
	## read in these two files
	NEI <- readRDS(summaryFile)
	SCC <- readRDS(sourceFile)

	## see the total pm2.5 from all sources for each of the years 1999, 2002, 2005, and 2008
	by_year <- group_by(NEI, year)
	pm25 <- summarize(by_year, total=sum(Emissions))

	png(file="plot1.png", width=480, height=480) ## call a graphics device and send the plot to the file, plot1.png
	par(mar=c(5.1,5.1,4.1,2.1))
	with(pm25, plot(year,total, col=rgb(1,0,0,0.5), pch=19, type="b", xlab="YEAR", ylab="", main="PM2.5 emissions in USA", xaxt="n", yaxt="n"))
	mtext("TOTAL EMISSIONS (TONS)",side=2,line=4) ## adjust the text on y-axis
	axis(1, at = seq(1999, 2008, by = 3))
	axis(2, at = seq(3e+6, 8e+6, by = 1e+6), las=1)

	## consider using:
	## xlab="Years", ylab=expression("Total" ~ PM[2.5] ~ "Emission, tons"),
     	## main=expression("Total emissions of" ~ PM[2.5] ~ "in the US"))

	## or
	## barplot(
  	## (Totalperyear$Emissions)/10^6,
  	## names.arg=Totalperyear$year,
  	## xlab="Year", ylab="PM2.5 Emissions (10^6 Tons)",
  	## main="PM2.5 Emissions From All Sources in US",
  	##col = "wheat"
)

	dev.off() ## close the device
}


