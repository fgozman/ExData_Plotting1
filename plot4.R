plot4 <- function(fPath){
    
    ## set conversions for columns
    setClass("myDate")
    setAs("character","myDate", function(from) as.Date(from, format="%d/%m/%Y") )
    setClass("myPOSIXlt")
    setAs("character","myPOSIXlt", function(from) strptime(from, format='%H:%M:%S'))
    columnClasses <- c("myDate",## Date
                    "myPOSIXlt",## Time
                    "numeric",## Global_active_power
                    "numeric",## Global_reactive_power
                    "numeric",## Voltage
                    "numeric",## Global_intensity
                    "numeric",## Sub_metering_1
                    "numeric",## Sub_metering_2
                    "numeric" ## Sub_metering_3
                    )
    dataFrame <- read.csv(file=fPath,
                       header = TRUE,
                       na.strings = "?",
                       sep=";",    
                       colClasses=columnClasses
                       )
    ## filter data between 2007-02-01 and 2007-02-02
    fromDate <- as.Date("2007-02-01",format="%Y-%m-%d")
    toDate <- as.Date("2007-02-02",format="%Y-%m-%d")
    dataFrame <- dataFrame[dataFrame$Date>=fromDate & dataFrame$Date<=toDate,]
    
    ## extract datetime from Date and Time
    dataFrameDateTime <- as.POSIXlt(
        paste(format(dataFrame$Date,"%Y-%m-%d"), 
              format(dataFrame$Time, "%H:%M:%S"),
              sep=" ", format="%Y-%m-%d %H:%M:%S"))
    ## export as png
    png(filename="plot4.png",width = 480, height = 480, units = "px")
    
    ## draw
    par(mfrow=c(2,2))
    plot(dataFrameDateTime,
         dataFrame$Global_active_power,
         ylab="Global Active Power",
         xlab="", type="l", col="black")
    plot(dataFrameDateTime,
         dataFrame$Voltage,
         ylab="Voltage",
         xlab="datetime", type="l", col="black")
    plot(dataFrameDateTime,
         dataFrame$Sub_metering_1,
         ylab="Energy sub metering",
         xlab="", type="l", col="black")
    points(dataFrameDateTime,
         dataFrame$Sub_metering_2,
         type="l", col="red")
    points(dataFrameDateTime,
         dataFrame$Sub_metering_3,
         type="l", col="blue")
    legend("topright",
           legend=c("Sub_metering_1",
                    "Sub_metering_2",
                    "Sub_metering_3"),
           lty=c(1,1,1), ## use lines
           col=c("black","blue","red"),
           bty = "n", ## remove box
           cex=0.8,## zoom out 
           xpd = TRUE ## plotting is clipped to the figure region
           )
    plot(dataFrameDateTime,
         dataFrame$Global_reactive_power,
         ylab="Global_reactive_power",
         xlab="datetime", type="l", col="black")
    dev.off()
}