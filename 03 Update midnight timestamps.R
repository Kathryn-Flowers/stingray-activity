# UPDATE MIDNIGHTS #

# By Katie Flowers
# 21 Nov 2023

# For some reason, there are missing values for midnight timestamps. 
# This code updates all stingray datasets.

##### Upload datasets & rename to match stingrays for HMMs #####

# original Stingray 1 & 2 were tests w/different loggers and removed (not in MS)
# Stingray 3 & 4 are discussed in MS but ultimately removed also

sting1 <- read.csv("Stingray3_avg.csv")
sting2 <- read.csv("Stingray4_avg.csv")
sting5 <- read.csv("GnarlsBarbley_avg.csv")
sting6 <- read.csv("Stingray8_avg.csv")
sting7 <- read.csv("Stingray9_avg.csv")
sting8 <- read.csv("Beverly_avg.csv")
sting9 <- read.csv("Jesus_avg.csv")
sting10 <- read.csv("Clementine_avg.csv")
sting11 <- read.csv("Seamantha_avg.csv")

# Update date time format
sting1$Date_Time <- as.POSIXct(sting1$Date_Time,format="%Y-%m-%d %H:%M:%S",
                               tz="America/Belize")

sting2$Date_Time <- as.POSIXct(sting2$Date_Time,format="%Y-%m-%d %H:%M:%S",
                               tz="America/Belize")

sting5$Date_Time <- as.POSIXct(sting5$Date_Time,format="%Y-%m-%d %H:%M:%S",
                               tz="America/Belize")

sting6$Date_Time <- as.POSIXct(sting6$Date_Time,format="%Y-%m-%d %H:%M:%S",
                               tz="America/Belize")

sting7$Date_Time <- as.POSIXct(sting7$Date_Time,format="%Y-%m-%d %H:%M:%S",
                               tz="America/Belize")

sting8$Date_Time <- as.POSIXct(sting8$Date_Time,format="%Y-%m-%d %H:%M:%S",
                               tz="America/Belize")

sting9$Date_Time <- as.POSIXct(sting9$Date_Time,format="%Y-%m-%d %H:%M:%S",
                               tz="America/Belize")

sting10$Date_Time <- as.POSIXct(sting10$Date_Time,format="%Y-%m-%d %H:%M:%S",
                                tz="America/Belize")

sting11$Date_Time <- as.POSIXct(sting11$Date_Time,format="%Y-%m-%d %H:%M:%S",
                                tz="America/Belize")

##### Stingray 1 #####

# any NAs
anyNA(sting1$Date_Time) #yes
na_positions <- which(is.na(sting1$Date_Time)) #where
print(na_positions) #6421, 92821, 179221

# update ORIGINAL data, so upload again, this is the only way it works
sting1 <- read.csv("Stingray3_avg.csv")

# get more information
extract_rows<-c(6420, 6421, 6422, 92820, 92821, 92822, 179220, 179221, 179222)
selected<-sting1[extract_rows, ]
print(selected) # midnight is the problem

# add midnight back in
sting1$Date_Time[6421] <- "2017-07-09 00:00:00"
sting1$Date_Time[92821] <- "2017-07-10 00:00:00"
sting1$Date_Time[179221] <- "2017-07-11 00:00:00"

# check updates
extract_rows<-c(6420, 6421, 6422, 92820, 92821, 92822, 179220, 179221, 179222)
selected<-sting1[extract_rows, ]
print(selected) # all good
anyNA(sting1) # double checking

# save new data as new stingray name for HMMs
write.csv(sting1,"Sting1.csv", row.names = FALSE)

##### Stingray 2 #####

# any NAs
anyNA(sting2$Date_Time) #yes
na_positions <- which(is.na(sting2$Date_Time)) #where
print(na_positions) #84961, 171361

# update ORIGINAL data, so upload again, this is the only way it works
sting2 <- read.csv("Stingray4_avg.csv")

# get more information
extract_rows<-c(84960,84961,171360,171361)
selected<-sting2[extract_rows, ]
print(selected) # midnight is the problem

# add midnight back in
sting2$Date_Time[84961] <- "2017-07-10 00:00:00"
sting2$Date_Time[171361] <- "2017-07-11 00:00:00"

# check updates
extract_rows<-c(84960,84961,171360,171361)
selected<-sting2[extract_rows, ]
print(selected) # all good
anyNA(sting2) # double checking

# save new data as new stingray name for HMMs
write.csv(sting2,"Sting2.csv", row.names = FALSE)

##### Stingray 5 #####

# any NAs
anyNA(sting5$Date_Time) #yes
na_positions <- which(is.na(sting5$Date_Time)) #where
print(na_positions) #53041, 139441

# update ORIGINAL data, so upload again, this is the only way it works
sting5 <- read.csv("GnarlsBarbley_avg.csv")

# get more information
extract_rows<-c(53041,53042,139441,139442)
selected<-sting5[extract_rows, ]
print(selected) # midnight is the problem

# add midnight back in
sting5$Date_Time[53041] <- "2019-06-09 00:00:00"
sting5$Date_Time[139441] <- "2019-06-10 00:00:00"

# check updates
extract_rows<-c(53041,53042,139441,139442)
selected<-sting5[extract_rows, ]
print(selected) # all good
anyNA(sting5) # double checking

# save new data as new stingray name for HMMs
write.csv(sting5,"Sting5.csv", row.names = FALSE)

##### Stingray 6 #####

# any NAs
anyNA(sting6$Date_Time) #yes
na_positions <- which(is.na(sting6$Date_Time)) #where
print(na_positions) #53521, 139921

# update ORIGINAL data, so upload again, this is the only way it works
sting6 <- read.csv("Stingray8_avg.csv")

# get more information
extract_rows<-c(53521, 139921)
selected<-sting6[extract_rows, ]
print(selected) # midnight is the problem

# add midnight back in
sting6$Date_Time[53521] <- "2019-06-10 00:00:00"
sting6$Date_Time[139921] <- "2019-06-11 00:00:00"

# check updates
extract_rows<-c(53521, 139921)
selected<-sting6[extract_rows, ]
print(selected) # all good
anyNA(sting6) # double checking

# save new data as new stingray name for HMMs
write.csv(sting6,"Sting6.csv", row.names = FALSE)

##### Stingray 7 #####
 
# any NAs
anyNA(sting7$Date_Time) #yes
na_positions <- which(is.na(sting7$Date_Time)) #where
print(na_positions) #54121, 140521

# update ORIGINAL data, so upload again, this is the only way it works
sting7 <- read.csv("Stingray9_avg.csv")

# get more information
extract_rows<-c(54120,54121, 140521,140522)
selected<-sting7[extract_rows, ]
print(selected) # midnight is the problem

# add midnight back in
sting7$Date_Time[54121] <- "2019-06-11 00:00:00"
sting7$Date_Time[140521] <- "2019-06-12 00:00:00"

# check updates
extract_rows<-c(54120,54121, 140521,140522)
selected<-sting7[extract_rows, ]
print(selected) # midnight is the problem
anyNA(sting7) # double checking

# save new data as new stingray name for HMMs
write.csv(sting7,"Sting7.csv", row.names = FALSE)

##### Stingray 8 #####

# any NAs
anyNA(sting8$Date_Time) #yes
na_positions <- which(is.na(sting8$Date_Time)) #where
print(na_positions) #53641,140041

# update ORIGINAL data, so upload again, this is the only way it works
sting8 <- read.csv("Beverly_avg.csv")

# add midnight back in
sting8$Date_Time[53641] <- "2019-06-12 00:00:00"
sting8$Date_Time[140041] <- "2019-06-13 00:00:00"

# check updates
extract_rows<-c(53641,53642,140040,140041)
selected<-sting8[extract_rows, ]
print(selected) # all good

# save new data as new stingray name for HMMs
write.csv(sting8,"Sting8.csv", row.names = FALSE)

##### Stingray 9 #####

# any NAs
anyNA(sting9$Date_Time) #yes
na_positions <- which(is.na(sting9$Date_Time)) #where
print(na_positions) #48601, 135001

# update ORIGINAL data, so upload again, this is the only way it works
sting9 <- read.csv("Jesus_avg.csv")

# add midnight back in
sting9$Date_Time[48601] <- "2019-06-13 00:00:00"
sting9$Date_Time[135001] <- "2019-06-14 00:00:00"

# check updates
extract_rows<-c(48601,48602,135000, 135001)
selected<-sting9[extract_rows, ]
print(selected) # all good

# save new data as new stingray name for HMMs
write.csv(sting9,"Sting9.csv", row.names = FALSE)


##### Stingray 10 #####

# NAs
na_positions <- which(is.na(sting10$Date_Time)) #where
print(na_positions) # 48001 134401

# update ORIGINAL data, so upload again, this is the only way it works
sting10 <- read.csv("Clementine_avg.csv")

# add midnight back in (after reading row date)
sting10$Date_Time[48001] <- "2019-06-15 00:00:00"
sting10$Date_Time[134401] <- "2019-06-16 00:00:00"

# check updates
extract_rows<-c(48001,48002,134400, 134401)
selected<-sting10[extract_rows, ]
print(selected) # all good

# save new data as new stingray name for HMMs
write.csv(sting10,"Sting10.csv", row.names = FALSE)

##### Stingray 11 #####

# NAs
anyNA(sting11$Date_Time) #none b/c deployment didn't cover that time period

# save new data as new stingray name for HMMs
write.csv(sting11,"Sting11.csv", row.names = FALSE)
