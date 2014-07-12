# function to create plot4 for the assignment - four separate plots

plot4 <- function() {
	
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
	
	#change Global_active_power column type to numeric
	current[,3]<-as.numeric(current[,3])
	
	#add a column that combines date and time then transform into date-time format
	current$Date_time<-do.call(paste,c(current[c("Date","Time")]))
	current$Date_time<-strptime(current$Date_time, format='%d/%m/%Y%H:%M:%S')
	
	#change Sub_metering_1 & Sub_metering_2 types to numeric, Sub_metering_3 is already numeric type
	
	current[,"Sub_metering_1"]<-as.numeric(current[,"Sub_metering_1"])
	current[,"Sub_metering_2"]<-as.numeric(current[,"Sub_metering_2"])
	
	#change Global_reactive_power, voltage and Global_intensity to numeric
	
	current[,"Global_reactive_power"]<-as.numeric(current[,"Global_reactive_power"])
	current[,"Voltage"]<-as.numeric(current[,"Voltage"])
	current[,"Global_intensity"]<-as.numeric(current[,"Global_intensity"])
	
	#define layout as 2-by-2
	par(mfrow = c(2,2))
	#specify each plot
	with(current,{
		#top left plot
		plot(Date_time, (Global_active_power*2)/1000, type="l", ylab="Global Active Power", xlab="")
	
		#top right plot
		plot(Date_time, Voltage, type="l", ylab="Voltage", xlab="datetime", yaxt="n") #change yticks
		axis(2,at=c(900,1100,1300,1500,1700,1900,2100), labels=c("234","","238","","242","","246"))
	
		#bottom left plot
		plot(Date_time,Sub_metering_1, type="l", ylim=c(0,35),yaxt="n", ylab="Energy sub metering", xlab="")
		axis(2, at=c(0,10,20,30))
		lines(Date_time,Sub_metering_2, col="red")
		lines(Date_time,Sub_metering_3, col="blue")
		legend("topright", pch="_", col=c("black","red","blue"), legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), bty="n")
	
		#bottom right plot
		plot(Date_time, Global_reactive_power/500, type="l", ylim=c(0,0.5), ylab="Global_reactive_power", xlab="datetime")
	
	})
	
	#save plot in png format
	dev.copy(png, file = "plot4.png")
	dev.off()
		
	
}