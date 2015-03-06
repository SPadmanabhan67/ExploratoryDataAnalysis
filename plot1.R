plot1 <- function (){
    setClass('myDate')
    setClass('myTime')

    setAs("character","myDate", function(from) as.Date(from, format="%d/%m/%Y") )
    setAs("character","myTime", function(from) strptime(from, format="%H:%M:%S", tz="") )
    d=read.table("household_power_consumption.txt", na.strings=c("?"), stringsAsFactors = FALSE, header=TRUE, sep=";", colClasses=c('myDate', 'myTime', rep("numeric", 7)))
    h = hist(d$Global_active_power, plot=FALSE)
    h$counts = h$counts/1000
    png(file='plot1.png', width = 480, height = 480, units = "px")
    plot(h, col="red", xlab="Global Active Power (kilowatts)", ylab="Frequency", main="Global Active Power")
    dev.off()
    h
}