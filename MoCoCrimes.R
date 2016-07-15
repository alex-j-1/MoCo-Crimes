
###--------------------------------------------
  
  
  ####Plot 1: Hour Distribution of all crime in Montogomery County, MD.
  
  
  #read the csv and place the date into a separate column
  crime <- read.csv("Crime.csv")
  
  times <- crime$Dispatch.Date...Time
  
  #Convert to military time to avoid ambiguity when doing hourly analysis
  z <-as.POSIXct(times, format="%m/%d/%Y %I:%M:%S %p") 
  
  #load r package for date handling
  install.packages("lubridate")
  library(lubridate)
  
  #get only the time...ignoring date.
  z2 <- hour(z) + min(z)/60
  
  #plot the data
  hist(z2, col="lightblue", xaxt='n', ylim=c(0,8000), main="Time Distribution of Crime in Montgomery County, MD", xlab="Hour", ylab="Crimes")
  axis(side=1, at=seq(0,24))
  
  ###--------------------------------------------
  
  ####Plot 2: line graph of homicide data, by quarter.
  
  #These class codes are the codes for homicide in the data set. 
  #They correspond to, respectively: HOMICIDE-FIREARM, HOMICIDE-SHARP INST, HOMICIDE-BLUNT INST, HOMICIDE-FIST HANDS, & HOMICIDE-OTHER.
  keep <- c("0111","0112","0113","0114","0115")
  
  #get only the crimes which are homicides.
  homicides <- crime[crime$Class %in% keep, ]
  
  #We only care about the time format
  times2 <- homicides$Dispatch.Date...Time
  
  #format the dates into a Posix DateTime format, for convenient date manipulation later
  y <- as.POSIXct(times2, format="%m/%d/%Y %I:%M:%S %p")
  
  #assign each date into a quarter "bin" 
  y2 <- quarter(y, with_year = TRUE)
  
  #put the quarterly counts into a table, to facilitate plotting later
  quarter_homicide_table <- table(y2)
  
  #plot the data as a bar plot
  barplot(quarter_homicide_table, ylab="# of Homicides", xlab="Quarter", main="Homicides by Quarter in Montgomery County, MD", col="lightgreen")
  