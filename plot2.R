plot2 <- function (){
  setClass('myDate')
  setClass('myTime')

  setAs("character","myDate", function(from) as.Date(from, format="%d/%m/%Y") )
  setAs("character","myTime", function(from) strptime(from, format="%H:%M:%S", tz="") )
  d=read.table("household_power_consumption.txt", na.strings=c("?"), stringsAsFactors = FALSE, header=TRUE, sep=";", colClasses=c('myDate', 'character', rep("numeric", 7)))
  selectDates = c('2007-02-01', '2007-02-02')
  d=subset(d, !is.na(d$Global_active_power) & d$Date %in% as.Date(selectDates, "%Y-%m-%d"), select=c('Date', 'Time', 'Global_active_power'))

  d[, 'Time'] = paste(d[,'Date'], " ", d[, 'Time'])
  d[, 'Time'] = as.POSIXct(d[, 'Time'], format="%Y-%m-%d %H:%M:%S", tz="")
  #return (d)
  png(file= 'plot2.png', width = 480, height = 480, units = "px")
  h = plot(d$Time, d$Global_active_power, type="l", xlab="Weekday", ylab="Global Active Power(kilowatts)", main="Global Active Power", lwd=3)
  dev.off()
  h
}