genTabl = function(){
  
  x <- c("2012-05-05", "2012-05-06")
  
  d <- strptime(x, "%Y-%m-%d")
  
  weekdays(d)
}
