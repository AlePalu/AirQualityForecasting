library(ggplot2)
library(forcats)
library(data.table)
library(chron)
library(fasttime)
library(svglite)
library(raster)
library(ggmap)

#---------- Preparing the datasets for graphic exploration --------
# we should consider having all numerical values under a single column "value" 
# and having another Factor variable which describes the
# type of measure (temperature, pm10, pm2p5)
# since it would maybe be easier to plot with ggplot?

# reading the database (fast) dropping a duplicated column (!?)
wiseairdata=fread("data/rawdata.csv", drop = 16)

# converting dates and time to POSIXct class (very fast)

wiseairdata$created_at = fastPOSIXct(wiseairdata$created_at)

# converting pot_id variable into a factor  (fast)
wiseairdata$pot_id = as.factor(wiseairdata$pot_id)


# aggregating (fast) data to create a pot database (computing the number of measure, first date of measure, mean frequency of measure,)
potdatabase=wiseairdata[,list(max_cfm=max(consecutive_failed_measures),
                              nr_measures=.N, 
                              firstdate=min(created_at), 
                              meanfreq=mean(seconds_until_next_measure), 
                              maxfreq=max(seconds_until_next_measure),
                              minfreq=min(seconds_until_next_measure)  ),by=list(pot_id)]


summary(wiseairdata)
summary(potdatabase)

#---------- drawing some plots ---------------


# some plots of info about pots
ggplot(potdatabase,aes(x=pot_id,y=firstdate)) + geom_point() + scale_y_datetime(date_breaks = "2 days") + coord_flip() + theme(axis.text.x = element_text(angle = 90))
ggplot(potdatabase,aes(x=pot_id,y=meanfreq)) + geom_point() + coord_flip()
ggplot(potdatabase,aes(x=pot_id,y=maxfreq)) + geom_point() + coord_flip()
ggplot(potdatabase,aes(x=pot_id,y=minfreq)) + geom_point() + coord_flip()

# plotting pm10 values for 2 pots since 2020-10-01 
ggplot(wiseairdata[(pot_id==1103 | pot_id==1109) & created_at>"2020-09-01",],aes(x=created_at,y=pm10,color=pot_id)) +
  geom_line() +
  scale_x_datetime(date_breaks = "2 days") +
  theme(axis.text.x = element_text(angle = 90)) 

#plotting pm2p5 in September of some pots
ggplot(wiseairdata[(pot_id %in% c(1103,1109,1116,1094,1095)) & created_at>"2020-09-01" & created_at<"2020-10-01" & pm2p5<50],aes(x=created_at,y=pm2p5,color=pot_id)) +
  geom_line() +
  scale_x_datetime(date_breaks = "2 days") +
  theme(axis.text.x = element_text(angle = 90)) 


#plotting pm2p5 a week of September Monday 21 - Sunday 27 of some pots
ggplot(wiseairdata[(pot_id %in% c(1103,1109,1116,1094,1095)) & created_at>="2020-09-21" & created_at<="2020-09-27" & pm2p5<50],aes(x=created_at,y=pm2p5,color=pot_id)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 day")

#plotting pm2p5 a day of september (24) of one pot (1103)
ggplot(wiseairdata[pot_id == 1103 & created_at>="2020-09-24" & created_at<"2020-09-25" & pm2p5<50],aes(x=created_at,y=pm2p5,color=pot_id)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 hours") +
  theme(axis.text.x = element_text(angle = 90)) 


#plot of pm10 of some plots in 3 days, removing outliers (>30)
ggplot(wiseairdata[(pot_id %in% c(1103,1109,1116,1094,1095)) & created_at>"2020-10-05" & created_at<="2020-10-08" & pm10< 30 ,],aes(x=created_at,y=pm10,color=pot_id)) +
  geom_point() +
  geom_smooth(span = 0.25) +
  scale_x_datetime(date_breaks = "2 hours") +
  theme(axis.text.x = element_text(angle = 90)) 

#plot of pm10 of some plots in 3 days, removing outliers (>30)
ggplot(wiseairdata[(pot_id %in% c(1103,1109,1116,1094,1095)) & created_at>"2020-10-05" & created_at<="2020-10-08" & pm10< 30 ,],aes(x=created_at,y=pm10,color=pot_id)) +
  geom_smooth() +
  scale_x_datetime(date_breaks = "2 hours") +
  theme(axis.text.x = element_text(angle = 90)) 



#plotting pm2p5 a day of september (24) of some pots
ggplot(wiseairdata[(pot_id %in% c(1103,1109,1116,1094,1095)) & created_at>="2020-09-24" & created_at<"2020-09-25" & pm2p5<50],aes(x=created_at,y=pm2p5,color=pot_id)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 hours") +
  theme(axis.text.x = element_text(angle = 90)) 



ggplot(wiseairdata[(pot_id==1103 | pot_id==1109) & created_at>"2020-09-01",],aes(x=created_at,y=pm2p5,color=pot_id)) +
  geom_point() +
  scale_x_datetime(date_breaks = "2 days") +
  theme(axis.text.x = element_text(angle = 90)) 

ggplot(wiseairdata[(pot_id %in% c(1103,1109,1116,1094,1095)) & created_at>"2020-10-01" & created_at<"2020-10-05",],aes(x=created_at,y=temperature_sht,color=pot_id)) + 
  geom_point(shape = "cross") + 
  geom_point(aes(x=created_at,y=pm10,col=pot_id)) + 
  scale_x_datetime(date_breaks = "6 hours") +
  theme(axis.text.x = element_text(angle = 90)) 








