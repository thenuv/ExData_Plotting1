## JHU - Datascience : Exploratory Analysis 
## Assignment 1 : Read data and Plot graphs to PNG

# Version : 1.0
# Created on : 2018.07.12
# Author : thenuv

plot1 <- function() {

library(data.table)

#Download & unzip the source file
fileurl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
filename <- "household_power_consumption.zip"

if (!file.exists(filename)) {
        download.file(fileurl, filename)
}
if (file.exists(filename) & !file.exists("household_power_consumption.txt")){
        unzip(filename)
}

#Read specific rows with date Feb 1, 2, 2007
df <- fread("household_power_consumption.txt", sep = ";", header= TRUE) [Date=="1/2/2007" | Date=="2/2/2007"]
df$Global_active_power <- as.numeric(df$Global_active_power)
df$Global_reactive_power <- as.numeric(df$Global_reactive_power)
df$Voltage <- as.numeric(df$Voltage)
df$Sub_metering_1 <- as.numeric(df$Sub_metering_1)
df$Sub_metering_2 <- as.numeric(df$Sub_metering_2)
df$Sub_metering_3 <- as.numeric(df$Sub_metering_3)

dt <- paste(df$Date, df$Time)
dt <- as.POSIXct(strptime(dt, "%d/%m/%Y %H:%M:%S"))
df <- cbind(df, dt)

#Plot histogram to PNG
png(filename = "plot1.png", width=480, height = 480)
hist(df$Global_active_power, main="Global Active Power", xlab = "Global Active Power (killowatts)", col = "red" )
dev.off()

}