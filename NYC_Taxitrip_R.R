#################### Taxi tip analysis ####################
library(dplyr)
library(funModeling)
library(tidyverse)

df <- taxi_data
status(df)
# variable q_zeros      p_zeros  q_na       p_na q_inf p_inf      type unique
# 1               VendorID       0 0.0000000000 19513 0.08198981     0     0   integer      2
# 2   tpep_pickup_datetime       0 0.0000000000     0 0.00000000     0     0 character 218054
# 3  tpep_dropoff_datetime       0 0.0000000000     0 0.00000000     0     0 character 218182
# 4        passenger_count    6582 0.0276562756 19513 0.08198981     0     0   integer      8
# 5          trip_distance    6223 0.0261478279     0 0.00000000     0     0   numeric   2952
# 6             RatecodeID       0 0.0000000000 19513 0.08198981     0     0   integer      7
# 7     store_and_fwd_flag       0 0.0000000000     0 0.00000000     0     0 character      3
# 8           PULocationID       0 0.0000000000     0 0.00000000     0     0   integer    250
# 9           DOLocationID       0 0.0000000000     0 0.00000000     0     0   integer    259
# 10          payment_type       0 0.0000000000 19513 0.08198981     0     0   integer      4
# 11           fare_amount     167 0.0007017013     0 0.00000000     0     0   numeric   5111
# 12                 extra  112233 0.4715810969     0 0.00000000     0     0   numeric     18
# 13               mta_tax    3644 0.0153113747     0 0.00000000     0     0   numeric      4
# 14            tip_amount  107447 0.4514712618     0 0.00000000     0     0   numeric   1445
# 15          tolls_amount  230271 0.9675536675     0 0.00000000     0     0   numeric    102
# 16 improvement_surcharge     329 0.0013823936     0 0.00000000     0     0   numeric      3
# 17          total_amount     140 0.0005882526     0 0.00000000     0     0   numeric   6673
# 18  congestion_surcharge   52687 0.2213804608     0 0.00000000     0     0   numeric      3

df$pickup_date <- as.Date(df$tpep_pickup_datetime)
df$dropoff_date <- as.Date(df$tpep_dropoff_datetime)

### as we can see there are some records with zero passenger and zero distance which could not be possible 
## so eliminating thous records 

df1 <- na.omit(df)
status(df1)
df1 <- df1 %>%
    filter(
      passenger_count > 0,
      trip_distance > 0
    )

barplot(table(df1$VendorID), main = 'Verndor', xlab = 'Vendor Id', ylab = 'Count')
histogram(df1$fare_amount)
histogram(df1$tip_amount)
histogram(df1$trip_distance)
histogram(df1$passenger_count)
histogram(df1$total_amount)
barplot(table(df1$pickup_date))
barplot(table(df1$dropoff_date))
library(ggplot2)

toptenpickdates <- freq(df1$pickup_date)
toptendropdates <- freq(df1$dropoff_date)
topfareamnt <- df1[order(df1$fare_amount),]
str(topfareamnt)
topfareamnt <- topfareamnt[1:100,]
topfareamnt <- freq(topfareamnt$fare_amount)

toppickpoints <- freq(df1$PULocationID)
head(toppickpoints)
str(toppickpoints)
lucarativepoint <- toppickpoints[1:10,1]
lucarativepoint
mydata <- df1 %>% 
  filter(df1$PULocationID %in% lucarativepoint)
str(mydata)
unique(mydata$PULocationID)
head(mydata)
test3 <-  mydata[,c(8,14)]
lucarativepoint1 <- as.numeric(lucarativepoint)
lucarativepoint1
rm(mydata2)
mydata2 <- data.frame()
names(mydata2) <- c('location','tip')
str(mydata2)
mydata2$PULocationID <- 0
mydata2$tip_amount <- 0

for (i in lucarativepoint1) {
  localdata <- test3 %>% filter(PULocationID == i)
  localtip <- sum(localdata$tip_amount)
  mydata2 <- rbind(mydata2,c(i,localtip))
  localdata <- NULL
  localtip <- NULL
}

mydata2
