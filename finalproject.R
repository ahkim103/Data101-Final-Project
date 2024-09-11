//Renaming Graphs For Convinience
2016.csv -> flights2016.csv
2018.csv -> flights2018.csv

//Data From The Federal Bureau of Transportation Statistics Graphed

barplot(percentage,names.arg=years,xlab="Year",ylab="% of flights that were delayed", col = rainbow(length(percentage)),
+ main="Percentage of Domestic Flights That Were Delayed From 2013-2022",border="black",ylim=c(0,100))
years <- c("2013","2014","2015","2016","2017","2018","2019","2020","2021","2022")
percentage <- c(20.06,21.72,19.06,17.44,19.26,19.01,19.28,9.21,16.39,20.62)

//Removes the one row in the datasets that were words and not numbers
flights2016 <- flights2016[-1, ]
flights2018 <- flights2018[-1, ]
jantojun2020 <- jantojun2020[-1, ]

//Extracts all flights from january of that year
flights2016$Month <- substring(flights2016$V1,6,7)
flights2016$Month <- as.numeric(flights2016$Month,na.rm=TRUE)
jan.2016 <- subset(flights2016, Month == 1)
flights2018$Month <- substring(flights2018$V1,6,7)
flights2018$Month <- as.numeric(flights2018$Month,na.rm=TRUE)
jan.2018 <- subset(flights2018, Month == 1)
jantojun2020$V3 <- as.numeric(jantojun2020$V3,na.rm=TRUE)
jan.2020 <- subset(jantojun2020, V3 == 1)

//Extracts all flights from june of that year
june.2016 <- subset(flights2016, Month == 6)
june.2018 <- subset(flights2018, Month == 6)
june.2020 <- subset(jantojun2020, V3 == 6)

//Changes the column of depature delays from characters to numbers
jan.2016$V8 <- as.numeric(jan.2016$V8,na.rm=TRUE)
jan.2018$V8 <- as.numeric(jan.2018$V8,na.rm=TRUE)
jan.2020$V20 <- as.numeric(jan.2020$V20,na.rm=TRUE)
june.2016$V8 <- as.numeric(june.2016$V8,na.rm=TRUE)
june.2018$V8 <- as.numeric(june.2018$V8,na.rm=TRUE)
june.2020$V20 <- as.numeric(june.2020$V20,na.rm=TRUE)



// Hypothesis 1: Longer distance flights are likely to have longer departure delays than shorter distance flights.
Null Hypothesis: Longer distance flights are likely to have similar departure delays as shorter distance flights.

jan.2016$V22 <- as.numeric(jan.2016$V22,na.rm=TRUE)
jan.2018$V22 <- as.numeric(jan.2018$V22,na.rm=TRUE)
jan.2020$V41 <- as.numeric(jan.2020$V41,na.rm=TRUE)

//Distances under the mean are considered short and above are considered long
distance.mean.2016 <- mean(jan.2016$V22, na.rm=TRUE)
distance.mean.2018 <- mean(jan.2018$V22, na.rm=TRUE)
distance.mean.2020 <- mean(jan.2020$V41, na.rm=TRUE)

jan.2016$Length <- jan.2016$V22
jan.2016$Length[jan.2016$V22 < distance.mean.2016] <- "shorter"
jan.2016$Length[jan.2016$V22 >= distance.mean.2016] <- "longer"
jan.2018$Length <- jan.2018$V22
jan.2018$Length[jan.2018$V22 < distance.mean.2018] <- "shorter"
jan.2018$Length[jan.2018$V22 >= distance.mean.2018] <- "longer"
jan.2020$Length <- jan.2020$V41
jan.2020$Length[jan.2020$V41 < distance.mean.2020] <- "shorter"
jan.2020$Length[jan.2020$V41 >= distance.mean.2020] <- "longer"

/Standard Deviation/
sd.short.2016 <- sd(jan.2016[jan.2016$Length == "shorter",]$V8,na.rm=TRUE)
sd.long.2016 <- sd(jan.2016[jan.2016$Length == "longer",]$V8,na.rm=TRUE)
sd.short.2018 <- sd(jan.2018[jan.2018$Length == "shorter",]$V8,na.rm=TRUE)
sd.long.2018 <- sd(jan.2018[jan.2018$Length == "longer",]$V8,na.rm=TRUE)
sd.short.2020 <- sd(jan.2020[jan.2020$Length == "shorter",]$V20,na.rm=TRUE)
sd.long.2020 <- sd(jan.2020[jan.2020$Length == "longer",]$V20,na.rm=TRUE)

/Mean/
mean.short.2016 <- mean(jan.2016[jan.2016$Length == "shorter",]$V8,na.rm=TRUE)
mean.long.2016 <- mean(jan.2016[jan.2016$Length == "longer",]$V8,na.rm=TRUE)
mean.short.2018 <- mean(jan.2018[jan.2018$Length == "shorter",]$V8,na.rm=TRUE)
mean.long.2018 <- mean(jan.2018[jan.2018$Length == "longer",]$V8,na.rm=TRUE)
mean.short.2020 <- mean(jan.2020[jan.2020$Length == "shorter",]$V20,na.rm=TRUE)
mean.long.2020 <- mean(jan.2020[jan.2020$Length == "longer",]$V20,na.rm=TRUE)

/Number/
num.short.2016 <- length(jan.2016[jan.2016$Length == "shorter",]$V8)
num.long.2016 <- length(jan.2016[jan.2016$Length == "longer",]$V8)
num.short.2018 <- length(jan.2018[jan.2018$Length == "shorter",]$V8)
num.long.2018 <- length(jan.2018[jan.2018$Length == "longer",]$V8)
num.short.2020 <- length(jan.2020[jan.2020$Length == "shorter",]$V20)
num.long.2020 <- length(jan.2020[jan.2020$Length == "longer",]$V20)

/Z-Score &-P Value/
z.score.2016 <- (mean.long.2016 - mean.short.2016)/sqrt((sd.long.2016^2/num.long.2016)+(sd.short.2016^2/num.short.2016))
z.score.2018 <- (mean.long.2018 - mean.short.2018)/sqrt((sd.long.2018^2/num.long.2018)+(sd.short.2018^2/num.short.2018))
z.score.2020 <- (mean.long.2020 - mean.short.2020)/sqrt((sd.long.2020^2/num.long.2020)+(sd.short.2020^2/num.short.2020))
p.value.2016 <- 1-pnorm(z.score.2016)
p.value.2018 <- 1-pnorm(z.score.2018)
p.value.2020 <- 1-pnorm(z.score.2020)



// Hypothesis 2: Weekend flights are likely to have longer departure delays than Weekday flights.
Null Hypothesis: Weekend flights are likely to have similar departure delays as Weekday flights.

/2016/
jan.2016$Day <- substring(jan.2016$V1,9,10)
jan.2016$Day <- as.numeric(jan.2016$Day,na.rm=TRUE)

jan.2016$Day[jan.2016$Day == 2] <- "weekend"
jan.2016$Day[jan.2016$Day == 3] <- "weekend"
jan.2016$Day[jan.2016$Day == 9] <- "weekend"
jan.2016$Day[jan.2016$Day == 10] <- "weekend"
jan.2016$Day[jan.2016$Day == 16] <- "weekend"           
jan.2016$Day[jan.2016$Day == 17] <- "weekend"
jan.2016$Day[jan.2016$Day == 23] <- "weekend"           
jan.2016$Day[jan.2016$Day == 24] <- "weekend"
jan.2016$Day[jan.2016$Day == 30] <- "weekend"           
jan.2016$Day[jan.2016$Day == 31] <- "weekend"

jan.2016$Day[jan.2016$Day == 1] <- "weekday"
jan.2016$Day[jan.2016$Day == 4] <- "weekday"
jan.2016$Day[jan.2016$Day == 5] <- "weekday"
jan.2016$Day[jan.2016$Day == 6] <- "weekday"           
jan.2016$Day[jan.2016$Day == 7] <- "weekday"
jan.2016$Day[jan.2016$Day == 8] <- "weekday"
jan.2016$Day[jan.2016$Day == 11] <- "weekday"
jan.2016$Day[jan.2016$Day == 12] <- "weekday"
jan.2016$Day[jan.2016$Day == 13] <- "weekday"
jan.2016$Day[jan.2016$Day == 14] <- "weekday"           
jan.2016$Day[jan.2016$Day == 15] <- "weekday"
jan.2016$Day[jan.2016$Day == 18] <- "weekday"
jan.2016$Day[jan.2016$Day == 19] <- "weekday"
jan.2016$Day[jan.2016$Day == 20] <- "weekday"
jan.2016$Day[jan.2016$Day == 21] <- "weekday"           
jan.2016$Day[jan.2016$Day == 22] <- "weekday"
jan.2016$Day[jan.2016$Day == 25] <- "weekday"
jan.2016$Day[jan.2016$Day == 26] <- "weekday"
jan.2016$Day[jan.2016$Day == 27] <- "weekday"
jan.2016$Day[jan.2016$Day == 28] <- "weekday"           
jan.2016$Day[jan.2016$Day == 29] <- "weekday"

sd.day.2016 <- sd(jan.2016[jan.2016$Day == "weekday",]$V8,na.rm=TRUE)
sd.end.2016 <- sd(jan.2016[jan.2016$Day == "weekend",]$V8,na.rm=TRUE)
mean.day.2016 <- mean(jan.2016[jan.2016$Day == "weekday",]$V8,na.rm=TRUE)
mean.end.2016 <- mean(jan.2016[jan.2016$Day == "weekend",]$V8,na.rm=TRUE)
num.day.2016 <- length(jan.2016[jan.2016$Day == "weekday",]$V8)
num.end.2016 <- length(jan.2016[jan.2016$Day == 'weekend',]$V8)
z.score.2016 <- (mean.end.2016 - mean.day.2016)/sqrt((sd.day.2016^2/num.day.2016)+(sd.end.2016^2/num.end.2016))
p.value.2016 <- 1-pnorm(z.score.2016)


/2018/
jan.2018$Day <- substring(jan.2018$V1,9,10)
jan.2018$Day <- as.numeric(jan.2018$Day,na.rm=TRUE)

jan.2018$Day[jan.2018$Day == 6] <- "weekend"
jan.2018$Day[jan.2018$Day == 7] <- "weekend"
jan.2018$Day[jan.2018$Day == 13] <- "weekend"
jan.2018$Day[jan.2018$Day == 14] <- "weekend"           
jan.2018$Day[jan.2018$Day == 20] <- "weekend"
jan.2018$Day[jan.2018$Day == 21] <- "weekend"           
jan.2018$Day[jan.2018$Day == 27] <- "weekend"
jan.2018$Day[jan.2018$Day == 28] <- "weekend"           

jan.2018$Day[jan.2018$Day == 1] <- "weekday"
jan.2018$Day[jan.2018$Day == 2] <- "weekday"
jan.2018$Day[jan.2018$Day == 3] <- "weekday"
jan.2018$Day[jan.2018$Day == 4] <- "weekday"           
jan.2018$Day[jan.2018$Day == 5] <- "weekday"
jan.2018$Day[jan.2018$Day == 8] <- "weekday"
jan.2018$Day[jan.2018$Day == 9] <- "weekday"
jan.2018$Day[jan.2018$Day == 10] <- "weekday"
jan.2018$Day[jan.2018$Day == 11] <- "weekday"
jan.2018$Day[jan.2018$Day == 12] <- "weekday"           
jan.2018$Day[jan.2018$Day == 15] <- "weekday"
jan.2018$Day[jan.2018$Day == 16] <- "weekday"
jan.2018$Day[jan.2018$Day == 17] <- "weekday"
jan.2018$Day[jan.2018$Day == 18] <- "weekday"
jan.2018$Day[jan.2018$Day == 19] <- "weekday"           
jan.2018$Day[jan.2018$Day == 22] <- "weekday"
jan.2018$Day[jan.2018$Day == 23] <- "weekday"
jan.2018$Day[jan.2018$Day == 24] <- "weekday"
jan.2018$Day[jan.2018$Day == 25] <- "weekday"
jan.2018$Day[jan.2018$Day == 26] <- "weekday"           
jan.2018$Day[jan.2018$Day == 29] <- "weekday"
jan.2018$Day[jan.2018$Day == 30] <- "weekday"
jan.2018$Day[jan.2018$Day == 31] <- "weekday"

sd.day.2018 <- sd(jan.2018[jan.2018$Day == "weekday",]$V8,na.rm=TRUE)
sd.end.2018 <- sd(jan.2018[jan.2018$Day == "weekend",]$V8,na.rm=TRUE)
mean.day.2018 <- mean(jan.2018[jan.2018$Day == "weekday",]$V8,na.rm=TRUE)
mean.end.2018 <- mean(jan.2018[jan.2018$Day == "weekend",]$V8,na.rm=TRUE)
num.day.2018 <- length(jan.2018[jan.2018$Day == "weekday",]$V8)
num.end.2018 <- length(jan.2018[jan.2018$Day == 'weekend',]$V8)
z.score.2018 <- (mean.end.2018 - mean.day.2018)/sqrt((sd.day.2018^2/num.day.2018)+(sd.end.2018^2/num.end.2018))
p.value.2018 <- 1-pnorm(z.score.2018)


/2020/
jan.2020$Day <- as.numeric(jan.2020$V5,na.rm=TRUE)
jan.2020$Day[jan.2020$V5 == 1] <- "weekday"
jan.2020$Day[jan.2020$V5 == 2] <- "weekday"
jan.2020$Day[jan.2020$V5 == 3] <- "weekday"
jan.2020$Day[jan.2020$V5 == 4] <- "weekday"
jan.2020$Day[jan.2020$V5 == 5] <- "weekday"
jan.2020$Day[jan.2020$V5 == 6] <- "weekend"
jan.2020$Day[jan.2020$V5 == 7] <- "weekend"

sd.day.2020 <- sd(jan.2020[jan.2020$Day == "weekday",]$V20,na.rm=TRUE)
sd.end.2020 <- sd(jan.2020[jan.2020$Day == "weekend",]$V20,na.rm=TRUE)
mean.day.2020 <- mean(jan.2020[jan.2020$Day == "weekday",]$V20,na.rm=TRUE)
mean.end.2020 <- mean(jan.2020[jan.2020$Day == "weekend",]$V20,na.rm=TRUE)
num.day.2020 <- length(jan.2020[jan.2020$Day == "weekday",]$V20)
num.end.2020 <- length(jan.2020[jan.2020$Day == 'weekend',]$V20)
z.score.2020 <- (mean.end.2020 - mean.day.2020)/sqrt((sd.day.2020^2/num.day.2020)+(sd.end.2020^2/num.end.2020))
p.value.2020 <- 1-pnorm(z.score.2020)



// Hypothesis 3: Flights in the summer are more likely to have departure delays than flights in the winter.
Null Hypothesis: Flights in the summer are just as likely to have departure delays as flights in the winter. 

/Standard Deviation/
sd.june.2016 <- sd(june.2016$V8,na.rm=TRUE)
sd.jan.2016 <- sd(jan.2016$V8,na.rm=TRUE)
sd.june.2018 <- sd(june.2018$V8,na.rm=TRUE)
sd.jan.2018 <- sd(jan.2018$V8,na.rm=TRUE)
sd.june.2020 <- sd(june.2020$V20,na.rm=TRUE)
sd.jan.2020 <- sd(jan.2020$V20,na.rm=TRUE)

/Mean/
mean.june.2016 <- mean(june.2016$V8,na.rm=TRUE)
mean.jan.2016 <- mean(jan.2016$V8,na.rm=TRUE)
mean.june.2018 <- mean(june.2018$V8,na.rm=TRUE)
mean.jan.2018 <- mean(jan.2018$V8,na.rm=TRUE)
mean.june.2020 <- mean(june.2020$V20,na.rm=TRUE)
mean.jan.2020 <- mean(jan.2020$V20,na.rm=TRUE)

/Number/
num.june.2016 <- length(june.2016$V8)
num.jan.2016 <- length(jan.2016$V8)
num.june.2018 <- length(june.2018$V8)
num.jan.2018 <- length(jan.2018$V8)
num.june.2020 <- length(june.2020$V20)
num.jan.2020 <- length(jan.2020$V20)

/Z-Score &-P Value/
z.score.2016 <- (mean.june.2016 - mean.jan.2016)/sqrt((sd.june.2016^2/num.june.2016)+(sd.jan.2016^2/num.jan.2016))
p.value.2016 <- 1-pnorm(z.score.2016)
z.score.2018 <- (mean.june.2018 - mean.jan.2018)/sqrt((sd.june.2018^2/num.june.2018)+(sd.jan.2018^2/num.jan.2018))
p.value.2018 <- 1-pnorm(z.score.2018)
z.score.2020 <- (mean.june.2020 - mean.jan.2020)/sqrt((sd.june.2020^2/num.june.2020)+(sd.jan.2020^2/num.jan.2020))
p.value.2020 <- 1-pnorm(z.score.2020)


//Graphs for Data Visualization
install.packages("RColorBrewer")

jan.2016$Delay <- jan.2016$V8
jan.2016$Delay[jan.2016$V8 <= 0] <- "no"
jan.2016$Delay[jan.2016$V8 >= 0] <- "yes"
june.2016$Delay <- june.2016$V8
june.2016$Delay[june.2016$V8 <= 0] <- "no"
june.2016$Delay[june.2016$V8 >= 0] <- "yes"
jan.2018$Delay <- jan.2016$V8
jan.2018$Delay[jan.2018$V8 <= 0] <- "no"
jan.2018$Delay[jan.2018$V8 >= 0] <- "yes"
june.2018$Delay <- june.2018$V8
june.2018$Delay[june.2018$V8 <= 0] <- "no"
june.2018$Delay[june.2018$V8 >= 0] <- "yes"
jan.2020$Delay <- jan.2020$V20
jan.2020$Delay[jan.2020$V20 <= 0] <- "no"
jan.2020$Delay[jan.2020$V20 >= 0] <- "yes"
june.2020$Delay <- june.2020$V20
june.2020$Delay[june.2020$V20 <= 0] <- "no"
june.2020$Delay[june.2020$V20 >= 0] <- "yes"


/Hypothesis 1/
plot(table(jan.2016$Length,jan.2016$Delay),main="Departure Delays vs. Length of Flights in January 20166", xlab="Longer 
or Shorter Distance Than The Mean?",ylab = "Is The Flight Delayed?",col=brewer.pal(n = 4, name = "RdBu"))
plot(table(jan.2018$Length,jan.2018$Delay),main="Departure Delays vs. Length of Flights in January 20166", xlab="Longer 
or Shorter Distance Than The Mean?",ylab = "Is The Flight Delayed?",col=brewer.pal(n = 4, name = "BuPu"))
plot(table(jan.2020$Length,jan.2020$Delay),main="Departure Delays vs. Length of Flights in January 20166", xlab="Longer 
or Shorter Distance Than The Mean?",ylab = "Is The Flight Delayed?",col=brewer.pal(n = 4, name = "GnBu"))

/Hypothesis 2/
plot(table(jan.2016$Day,jan.2016$Delay),main="Departure Delays vs. Weekend/Weekday in January 2016", xlab="Is The Flight
On A Weekday or Weekend?",ylab = "Is The Flight Delayed?",col=brewer.pal(n = 4, name = "RdBu"))
plot(table(jan.2018$Day,jan.2018$Delay),main="Departure Delays vs. Weekend/Weekday in January 2018", xlab="Is The Flight
On A Weekday or Weekend?",ylab = "Is The Flight Delayed?",col=brewer.pal(n = 4, name = "BuPu"))
plot(table(jan.2020$Day,jan.2020$Delay),main="Departure Delays vs. Weekend/Weekday in January 2020", xlab="Is The Flight
On A Weekday or Weekend?",ylab = "Is The Flight Delayed?",col=brewer.pal(n = 4, name = "GnBu"))

/Hypothesis 3/
total.2016 = 215139+149141 = 364280
jan.2016.pct = 149141/364280*100 = 40.94
june.2016.pct = 215139/364280*100 = 59.06
total.2018 = 183723+269894 = 453617
jan.2018.pct = 183723/453617*100 = 40.50
june.2018.pct = 269894/453617*100 =59.50
total.2020 = 181463+47780 = 229243
jan.2020.pct = 181463/229243*100 = 79.16
june.2020.pct = 47780/229243*100 = 20.84

years <- c("Jan 2016","June 2016","Jan 2018","June 2018","Jan 2020","June 2020")
percentage <- c(40.94,59.06,40.50,59.50,79.16,20.84)
barplot(percentage,names.arg=years,xlab="Month+Year",ylab="% of flights that were delayed", 
col = rainbow(length(percentage)),main="Domestic Flights That Were Delayed vs January & June",border="black"
,ylim=c(0,100))

