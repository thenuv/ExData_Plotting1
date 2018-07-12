## JHU - Datascience : Exploratory Analysis 
## Assignment 1 : Read data and Plot graphs to PNG (Multiple Graphs)

# Version : 1.0
# Created on : 2018.07.12
# Author : thenuv

plot4 <- function() {
        
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
        
        #Plot Line graph with multiple graphs to PNG
        png(filename = "plot4.png", width=480, height = 480)
        
        par(mfcol= c(2,2))
        with (df, 
              {
                      plot(df$dt, df$Global_active_power, type="n", xlab = "Day of the week", ylab = "Global Active Power (killowatts)")
                      lines(df$dt, df$Global_active_power, type="l")
                      
                      plot(dt, Sub_metering_1, type="n", ylab = "Energy Sub Metring", xlab = "Day of the week") 
                      lines(df$dt, df$Sub_metering_1, type="l")
                      lines(df$dt, df$Sub_metering_2, col="red")
                      lines(df$dt, df$Sub_metering_3, col="blue")	
                      legend("topright", lty=c(1, 1, 1), col= c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
                      
                      plot(df$dt, df$Voltage, type="n", xlab = "Day of the week", ylab = "Voltage")
                      lines(df$dt, df$Voltage, type="l")
                      
                      plot(df$dt, df$Global_reactive_power, type="n", xlab = "Day of the week", ylab = "Global Reactive Power")
                      lines(df$dt, df$Global_reactive_power, type="l")

              } 
        )
        
        dev.off()
}