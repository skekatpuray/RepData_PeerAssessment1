processMissingInput <- function (){
  
  dt <- read.csv("activity.csv")
  
  dtAvgIntervals <- filter(dt, !is.na(steps))   
  dtAvgIntervals <- aggregate(dtAvgIntervals$steps, by=list(dtAvgIntervals$interval), FUN=mean, rm.na = T)  
  colnames(dtAvgIntervals) <- c("Interval", "AverageSteps")  
  mergeData <- merge(dt, dtAvgIntervals, by.x = "interval", by.y="Interval", all=T)  
  mergeData[is.na(mergeData$steps),]$steps = mergeData[is.na(mergeData$steps),]$AverageSteps  
  mergeData$AverageSteps = NULL
  correctedDtNonZero <- filter(mergeData, steps != 0)  
  
  correctedDtNonZero$Date = strptime(correctedDtNonZero$date, "%Y-%m-%d")
  correctedDtNonZero$Day = weekdays(correctedDtNonZero$Date)
  correctedDtNonZero$DayType = sapply(correctedDtNonZero$Day, function(d) if (d == "Sunday" | d == "Sunday") "Weekend" else "Weekday")
  correctedDtNonZero$Day = NULL
  correctedDtNonZero$Date = NULL
  correctedDtNonZeroAvg <- aggregate(correctedDtNonZero$steps, by=list(correctedDtNonZero$interval, correctedDtNonZero$DayType), FUN=mean, rm.na = T)  
  colnames(correctedDtNonZeroAvg) <- c("Interval","DayType","Steps")
  qplot(Interval, Steps, data = correctedDtNonZeroAvg, 
        method="lm", geom="line",
        facets =.~DayType)
  
}