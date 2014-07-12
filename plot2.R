## function to create a line plot that depicts the change in global active power
## in kilowatts over time for the 2 day period

plot2 <- function() {
	
	
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
	
	#add a column that combines date and time then transform into date-time format
	current$Date_time<-do.call(paste,c(current[c("Date","Time")]))
	current$Date_time<-strptime(current$Date_time, format='%d/%m/%Y%H:%M:%S')
	
	#create a line plot with time as x-axis, global active power as y-axis
	plot(current$Date_time,current$Global_active_power/500, type='l', ylab="Global Active Power (kilowatts)", xlab = "")
	
	#save plot in png format
	dev.copy(png, file = "plot2.png")
	dev.off()
	
	
}