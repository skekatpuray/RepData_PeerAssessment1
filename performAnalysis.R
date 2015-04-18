performAnalysis <- function() {  
  dt <- read.csv("activity.csv") 
  dtByDate = aggregate(dt$steps, by=list(dt$date), FUN=sum, rm.na = T)    
  colnames(dtByDate) <- c("Date", "TotalSteps")  
  dtByDate$Date <- strptime(dtByDate$Date, "%Y-%m-%d")  
  dtByDate$Month <- months(dtByDate$Date)
  dtByDate$Day <- as.numeric(format(dtByDate$Date , "%d"))
  
  x <- ggplot(dtByDate, aes(Day, weight= TotalSteps)) + geom_bar(aes(fill = Month), position = "dodge", binwidth=.4) +
    scale_x_discrete(limit = c(1:31))
  
  print (x)
}