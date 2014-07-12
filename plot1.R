plot1 <- function() {
	
	#download file & unzip
	fileURL<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
	
 	download.file(fileURL, destfile = "electricpowerconsumption.zip", method = "curl")
 	
 	unzip("electricpowerconsumption.zip")

	# read dataset into data table variable hholdData
	hholdData<-read.table("./household_power_consumption.txt", sep = ";", header = TRUE)
	
	#extract only dates we are interested in
	firstDate<-subset(hholdData,hholdData[,"Date"]=="1/2/2007")
	secondDate<-subset(hholdData,hholdData[,"Date"]=="2/2/2007")
	
	current<-rbind(firstDate,secondDate)
	
	#change column type to numeric
	current[,3]<-as.numeric(current[,3])
	
	#construct a histogram
	hist(current[,3]/500,col="red", main = "Global Active Power", xlab="Global Active Power (kilowatts)")
	
	#create PNG file 'plot1.png'
	dev.copy(png, file = "plot1.png")
	dev.off()
	
}
	
	
	