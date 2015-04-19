performAnalysis <- function() {  
  dt <- read.csv("activity.csv") 
    
  dtByDate = aggregate(dt$steps, by=list(dt$date), FUN=sum, rm.na = T)    
  colnames(dtByDate) <- c("Date", "Sum")  
  dtByDate$Date <- strptime(dtByDate$Date, "%Y-%m-%d")  
  dtByDate$Month <- months(dtByDate$Date)
  dtByDate$Day <- as.numeric(format(dtByDate$Date , "%d"))
  plotSum <- ggplot(dtByDate, aes(Day, weight= Sum)) + geom_bar(aes(fill = Month), position = "dodge", binwidth=.4) +
    scale_x_discrete(limit = c(1:31)) + xlab("Day of the month") + ylab("Sum (steps)") +
    ggtitle("Sum of the total steps across two months")
  
  ##Filter out zero 
  dtNonZero <- filter(dt, steps != 0)  
    
  dtMedian = aggregate(dtNonZero$steps, by=list(dtNonZero$date), FUN=median)      
  colnames(dtMedian) <- c("Date", "Median")
  dtMedian$Date <- strptime(dtMedian$Date, "%Y-%m-%d")  
  dtMedian$Month <- months(dtMedian$Date)
  dtMedian$Day <- as.numeric(format(dtMedian$Date , "%d"))  
  plotMedian <- ggplot(dtMedian, aes(Day, weight= Median)) + geom_bar(aes(fill = Month), position = "dodge", binwidth=.4) +
    scale_x_discrete(limit = c(1:31)) + xlab("Day of the month") + ylab("Median (steps)") +
    ggtitle("Median of the total steps across two months")
    
  dtMean = aggregate(dtNonZero$steps, by=list(dtNonZero$date), FUN=mean)      
  colnames(dtMean) <- c("Date", "Mean")
  dtMean$Date <- strptime(dtMean$Date, "%Y-%m-%d")  
  dtMean$Month <- months(dtMean$Date)
  dtMean$Day <- as.numeric(format(dtMean$Date , "%d"))  
  plotMean <- ggplot(dtMean, aes(Day, weight= Mean)) + geom_bar(aes(fill = Month), position = "dodge", binwidth=.4) +
    scale_x_discrete(limit = c(1:31)) + xlab("Day of the month") + ylab("Mean (steps)") + 
    ggtitle("Mean of the total steps across two months")
  
  print (plotSum)
  
}