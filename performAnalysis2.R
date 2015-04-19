performAnalysis2 <- function() {  
  dt <- read.csv("activity.csv") 
  
  dt <- filter(dt, steps != 0 & !is.na(steps))  
  dtAvgIntervals <- aggregate(dt$steps, by=list(dt$interval), FUN=mean, rm.na = T)
  colnames(dtAvgIntervals) <- c("Interval", "Average")  
  
  with (dtAvgIntervals, plot(Interval, 
                             Average, 
                             type="l", 
                             xlab="Interval time series", 
                             ylab="Averages", 
                             main="Daily average by intervals"))
  
  
  dtAvgIntervals <- dtAvgIntervals[order(dtAvgIntervals$Average),]
  
  maxActivity <- dtAvgIntervals[nrow(dtAvgIntervals),]
}