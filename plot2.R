plot2 <- function(fPath){
    
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
    png(filename="plot2.png",width = 480, height = 480, units = "px")
    
    ## extract datetime from Date and Time
    dataFrameDateTime <- as.POSIXlt(
        paste(format(dataFrame$Date,"%Y-%m-%d"), 
              format(dataFrame$Time, "%H:%M:%S"),
              sep=" ", format="%Y-%m-%d %H:%M:%S"))
    ## draw
    plot(dataFrameDateTime,
         dataFrame$Global_active_power,
         ylab="Global Active Power (kilowatts)",
         xlab="", type="l")
    dev.off()
}