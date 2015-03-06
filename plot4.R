plot4 <- function (){
  setClass('myDate')
  setClass('myTime')

  setAs("character","myDate", function(from) as.Date(from, format="%d/%m/%Y") )
  setAs("character","myTime", function(from) strptime(from, format="%H:%M:%S", tz="") )
  d=read.table("household_power_consumption.txt", na.strings=c("?"), stringsAsFactors = FALSE, header=TRUE, sep=";", colClasses=c('myDate', 'character', rep("numeric", 7)))
  selectDates = c('2007-02-01', '2007-02-02')
  d=subset(d, !is.na(d$Global_active_power) & d$Date %in% as.Date(selectDates, "%Y-%m-%d"), select=c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

  d[, 'Time'] = paste(d[,'Date'], " ", d[, 'Time'])
  d[, 'Time'] = as.POSIXct(d[, 'Time'], format="%Y-%m-%d %H:%M:%S", tz="")  
 
  with (d, {
    png(file='plot4.png', width = 480, height = 480, units = "px")
    par (mfrow=c(2,2))
    
    plot(Time, Global_active_power, type="l", xlab="", ylab="Global Active Power(kilowatts)", lwd=2)    
    plot(Time, Voltage, type="l", xlab="datetime", ylab="Voltage", lwd=2)
    
    with (d, plot(Time, Sub_metering_1, type="n", xlab="", ylab="Energy sub metering"))
    with (d, lines(Time, Sub_metering_1, type="l", col="black", lwd=2))
    with (d, lines(Time, Sub_metering_2, type="l", col="red", lwd=2))
    with (d, lines(Time, Sub_metering_3, type="l", col="blue", lwd=2))
    legend("topright", lty = 1, col=c("black", "red", "blue"), legend=c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'), lwd=2, bty="n")
    
    plot(Time, Global_reactive_power, type="l", xlab="datetime", ylab="Global_reactive_power", col="black", lwd=2)
    
    dev.off()
  })
  
  d
}