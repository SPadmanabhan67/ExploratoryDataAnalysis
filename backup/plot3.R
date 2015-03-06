plot3 <- function (){
  setClass('myDate')
  setClass('myTime')
  
  setAs("character","myDate", function(from) as.Date(from, format="%d/%m/%Y") )
  setAs("character","myTime", function(from) strptime(from, format="%H:%M:%S", tz="") )
  d=read.table("household_power_consumption.txt", na.strings=c("?"), stringsAsFactors = FALSE, header=TRUE, sep=";", colClasses=c('myDate', 'character', rep("numeric", 7)))
  selectDates = c('2007-02-01', '2007-02-02')
  d=subset(d, !is.na(d$Global_active_power) & d$Date %in% as.Date(selectDates, "%Y-%m-%d"), select=c("Date", "Time", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

  d[, 'Time'] = paste(d[,'Date'], " ", d[, 'Time'])
  d[, 'Time'] = as.POSIXct(d[, 'Time'], format="%Y-%m-%d %H:%M:%S", tz="")
  #return (d)
  
  png(file="plot3.png", width = 480, height = 480, units = "px")
  with (d, plot(Time, Sub_metering_1, type="n", xlab="Weekday", ylab="Energy Sub metering"))
  with (d, lines(Time, Sub_metering_1, type="l", col="black", lwd=2))
  with (d, lines(Time, Sub_metering_2, type="l", col="red", lwd=2))
  with (d, lines(Time, Sub_metering_3, type="l", col="blue", lwd=2))
  legend("topright", lty = 1, col=c("black", "red", "blue"), legend=c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'))
  dev.off()
}