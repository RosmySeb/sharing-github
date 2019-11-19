## R script
library(readr)
library(RSQLite)
library(ggplot2)
library(dplyr)
library(data.table)
library(ggridges)

conn <- dbConnect(SQLite(), "DNA2.sqlite9")
dbListTables(conn)
dbListFields(conn,"daily_activity")
df <- dbGetQuery(conn, "SELECT * FROM daily_activity limit 10")
df <- dbGetQuery(conn, statement = read_file("DNA2.sqlite9"))

```{sql, connection=conn, output.var = "mydataframe"}
SELECT "game_id", "player_id" FROM ("daily_activity")
```
## Sql write tables as csv files.
sqlite3 DNA2.sqlite9
sqlite>.tables
# daily_activity  user_profile
sqlite> SELECT COUNT(*) FROM daily_activity; #1085878
sqlite> SELECT COUNT(*) FROM user_profile; #301000
sqlite> .headers on
sqlite> .mode csv
sqlite> .output data.csv
sqlite> SELECT *
   ...>   FROM daily_activity;
sqlite> .quit

## R script
daily_activity <-fread("daily_activity.csv")
table(daily_activity$game_id,useNA = "ifany") 
#Game A Game B Game C 
#461447 320874 303557 
summary(daily_activity$time_spent)

user_profile<-fread("user_profile.csv") ## read user profile
nrow(user_profile) #301000
profile1<-user_profile[!duplicated(user_profile)]## remove duplicates
nrow(profile1) #300000
daily_activity <-fread("daily_activity.csv") ## read daily activity
nrow(daily_activity) #1085878
daily_activity[!duplicated(daily_activity)] ## no duplicates

daily_activity_new <-merge(profile1,daily_activity,by=c("game_id","player_id"),all = TRUE)
nrow(daily_activity_new) #1085878
retention <-daily_activity_new[,c(1:3,6:7,12:15)]
retention$day_0<-retention$event_date-retention$registration_date
retention_day <- subset(retention,day_0==0|day_0==1) #subset the data for day of registration and the day after registration.

table(subset(retention_day,day_0==28)$game_id,useNA = "ifany")/100000
#Game A Game B Game C 
#135189 126399 133764 
#Game A  Game B  Game C # 1day retention
#0.35189 0.26399 0.33764 
##Game A  Game B  Game C # 7day retention
#0.11544 0.06849 0.06683
##Game A  Game B  Game C # 28day retention
#0.03783 0.02221 0.01631

table(subset(daily_activity_new, iap_revenue>0)$game_id) # the number of IAP revenues made. 

#Game A Game B Game C 
 # 4179   3276    215 
 
 data_A <- subset(daily_activity_new, iap_revenue>0) %>%
  #select(game_id,minutes_spend) %>%
  group_by(game_id,origin_category) %>%
  summarise(iap_revenue = sum(iap_revenue))
  
  
summary(subset(retention_day,day_0==0 & game_id == "Game C")$time_spent)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 #1.00    4.00   12.00   20.16   26.00  520.00    1880 
summary(subset(retention_day,day_0==0 & game_id == "Game A")$time_spent)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.00    5.00   15.00   30.29   40.00  758.00   21363 
summary(subset(retention_day,day_0==0 & game_id == "Game B")$time_spent)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1.00    5.00   13.00   27.33   33.00 1193.00    2084 

for (i in 1:10){
  retention_day[day_0==0 & game_id == "Game A" & time_spent==i, minutes_spend := i]
  retention_day[day_0==0 & game_id == "Game B" & time_spent==i, minutes_spend := i]
  retention_day[day_0==0 & game_id == "Game C" & time_spent==i, minutes_spend := i]
}

data_0 <- subset(retention_day,day_0==0& !is.na(time_spent)) %>%
  #select(game_id,minutes_spend) %>%
  group_by(game_id,minutes_spend) %>%
  summarise(n = n())
  
day_0<-subset(retention_day,day_0==0 & !is.na(time_spent)) #300000
day_1<-subset(retention_day,day_0==1 & !is.na(time_spent))

nrow(day_0)#274673
nrow(day_1) #89543
retention<-merge(day_1,day_0,by=c("game_id","player_id","registration_date"))
nrow(retention) #86617

x<-retention$player_id
y<-day_1$player_id
length(y)
comparison <- setdiff(y,x)
length(comparison)
head(comparison)
subset(day_1,player_id==179460635)
   game_id player_id registration_date origin_category event_date iap_revenue num_purchases
#1:  Game A 179460635             17722         Organic      17723           0             0
#   session_count time_spent day_0 minutes_spend
#1:             2          9     1            NA
subset(retention_day,player_id==179460635)
#   game_id player_id registration_date origin_category event_date iap_revenue num_purchases
#1:  Game A 179460635             17722         Organic      17722           0             0
#2:  Game A 179460635             17722         Organic      17723           0             0
#   session_count time_spent day_0 minutes_spend
#1:             1         NA     0            NA
#2:             2          9     1            NA

nrow(subset(day_0, game_id=="Game A"))#78637
nrow(subset(day_0, game_id=="Game B"))#97916
nrow(subset(day_0, game_id=="Game C"))#98120

names(retention)
retention_data<-retention[,c(1:9,13:17,19)]
setnames(retention_data,"minutes_spend.y","minutes_spend")
data_1 <- retention_data %>%
  #select(game_id,minutes_spend) %>%
  group_by(game_id,minutes_spend) %>%
  summarise(n = n())
  
names(data_0) #"game_id"       "minutes_spend" "n"
#[1] "game_id"       "minutes_spend" "n"            
 setnames(data_0,"n","total_num_day0") #"game_id"       "minutes_spend" "n"
names(data_1) #"game_id"         "minutes_spend.y" "n"
#[1] "game_id"       "minutes_spend" "n"            
setnames(data_1,c("n"),c("total_num_day1")) #"game_id"       "minutes_spend" "n"
data_dna <- merge(data_0,data_1,by=c("game_id","minutes_spend"), all = T)
nrow(data_dna)

data_dna$retention <-(data_dna$total_num_day1/data_dna$total_num_day0)*100
##plot the DNA chart
DNA <- ggplot(data_dna, aes(x=minutes_spend, y=retention,colour = game_id)) + 
  geom_line(aes(group = game_id)) +
  ggtitle("DNA chart for games") +
  xlab("Day 0 minutes played") + ylab("Day 7 retention")+
  geom_point()+scale_x_continuous(breaks = c(1:10))+ylim(0,20)
DNA


data1 <- subset(retention_day,day_0==0&time_spent<=10) %>%
  #select(game_id,minutes_spend) %>%
  group_by(game_id,time_spent,origin_category) %>%
  summarise(n = n())  #%>%
  #mutate(freq = n / sum(n))
data0 <- data_0 %>%
  #select(game_id,minutes_spend) %>%
  group_by(game_id,time_spent) %>%
  summarise(total= sum(n))

data<-merge(data1,data0, by =c("game_id","time_spent"))
data$freq<-(data$n/10)

nrow(subset(retention_day,day_0==1 & time_spent==1 & game_id=="Game C"))

p4 <- ggplot() + geom_bar(aes(y = n, x = time_spent, fill = origin_category), data = subset(data,game_id=="Game C"&!(is.na(time_spent))),
                          stat="identity") +
  ggtitle("Orgin category distribution for Game C") +
  xlab("Day 0 minutes played") + ylab("Day 1 retention")+
  geom_point()+scale_x_continuous(breaks = c(1:10))

p4

iap <- subset(daily_activity_new) %>%
  #select(game_id,minutes_spend) %>%
  group_by(game_id,origin_category) %>%
  summarise(total = sum(iap_revenue))
iap
