# OVERALL DYNAMIC BODY ACCELERATION #

# TechnoSmArt AXY-Depth loggers unless otherwise noted (LL = Little Leonardo)
# Original code authored by Sarah Luongo & Megan Kelley
# Modified, added to, & commented on by Kathryn Flowers w/input from Connor White
# Last modified 20 Nov 2022

##### Install gRumble package #####

#install("devtools")
#install_github("MBayOtolith/gRumble/gRumble")
#citation(package = "gRumble") #get citation for paper

##### Load libraries & data ##### 

library(devtools)
library(gRumble)
library(dplyr)
library(tidyr)

getwd() #get working directory, mine is set to location where R script is saved 

# WARNING: if you delimit columns in Excel prior to importing data into R,
## you will lose many data rows! Use sep = "" when reading csv instead.
### "d" names represent deployment number, "SWC" = South Water Caye, "GRA" =
#### Glover's Reef Atoll.

#d1 Different logger (LL); enviro data do not align w/accel data - excluded
#d2 Different logger (LL); missing times - excluded
d3<- read.csv("08Jul2017_01_SWC.csv", sep = "") #Stingray3, SWC
d4<- read.csv("08Jul2017_02_SWC.csv", sep = "") #Stingray4, SWC
#d5 Paloma, GRA; orange float = swimming problems apparent in data - excluded
d6<- read.csv("12Jun2018_02.csv", sep = "") #Berta, GRA; missing enviro due to 
##pressure sensor failure
d7<- read.csv("08Jun2019_01.csv") #GnarlsBarbley, GRA
d8<- read.csv("09Jun2019_02.csv") #Stingray8, GRA
d9<- read.csv("10Jun2019_03.csv") #Stingray9, GRA
d10<- read.csv("11Jun2019_04.csv") #Beverly, GRA
d11<- read.csv("12Jun2019_05.csv") #Jesus, GRA
d12<- read.csv("14Jun2019_06.csv") #Clementine, GRA
d13<- read.csv("20Jun2019_07.csv") #Seamantha, GRA; early pop-off

##### Trim data #####

# Start time: two hours post stingray release
# End time: right before tag package pop-off (approximation) 
# Round up from start time to nearest second then search in the view data tab
## for that time on the first day to find row number.
### E.g. two hrs post release June 4 08:19:19 then search for 08:20:00 on June 4

#d3: START = 22:13:00 July 8 (1143601) ; END = 03:11:26.960 July 11 (5911275)
Stingray3<- d3[c(1143601:5911275),(2:8)] #combine rows, then columns

#d4: START = 00:24:00 July 9 (1392601) ; END = 06:31:14.960 July 11 (6263475) 
Stingray4<- d4[c(1392601:6263475),(2:8)] 

#d6: START = 10:20:00 June 12 (461201); END = 10:59:00.960 June 14 (4839725)
Berta <- d6[c(461201:4839725), (2:8)] 

#d7: START = 09:16:00 June 8 (402451); END = 13:36:24.960 June 10 (5113075)
GnarlsBarbley<- d7[c(402451:5113075),(2:8)] 

#d8: START = 09:08:00 June 9 (395126) ; END = 13:28:54.960 June 11 (5106500)
Stingray8<- d8[c(395126:5106500), (2:8)]

#d9: START = 08:58:00 June 10 (341151); END = 09:20:54.960 June 12 (4695525)
Stingray9<- d9[c(341151:4695525), (2:8)]

#d10: START = 09:06:00 June 11 (312176); END = 13:01:37.960 June 13 (4985625)
Beverly<- d10[c(312176:4985625),(2:8)] 

#d11: START = 10:30:00 June 12 (433401); END = 13:15:07.960 June 14 (5001100)
Jesus <- d11[c(433401:5001100),(2:8)] 

#d12: START = 10:40:00 June 14 (466501); END = 16:28:36.960 June 16 (5309425) 
Clementine <- d12[c(466501:5309425),(2:8)] 

#d13: START = 09:25:00 June 20 (357901); END = 19:31:55.960 June 20 (1268300)
Seamantha <- d13[c(357901:1268300),(2:7)] 

# Adjust column names for consistency
names(Stingray3)[7] <- "Temp"
names(Stingray4)[7] <- "Temp"
names(Berta)[7] <- "Temp"
names(GnarlsBarbley)[7] <- "Temp"
names(Stingray8)[7] <- "Temp"
names(Stingray9)[7] <- "Temp"
names(Beverly)[7] <- "Temp"
names(Jesus)[7] <- "Temp"
names(Clementine)[7] <- "Temp"
names(Seamantha)[6] <- "Temp"

##### Updating and organizing raw data #####

# not every dataset needs these steps, only 2019 deployments

# fill in NAs with depth & temp data from first data point in the 25 seconds
# automatically fills from top down

Beverly2 <- Beverly %>% fill(Depth, Temp) 
View(Beverly2)

Clementine2 <- Clementine %>% fill(Depth, Temp) 
View(Clementine2)

GnarlsBarbley2 <- GnarlsBarbley %>% fill(Depth, Temp) 
View(GnarlsBarbley2)

Jesus2 <- Jesus %>% fill(Depth, Temp) 
View(Jesus2)

Seamantha2 <- Seamantha %>% fill(Depth, Temp) 
View(Seamantha2)

Stingray8_2 <- Stingray8 %>% fill(Depth, Temp) 
View(Stingray8_2)

Stingray9_2 <- Stingray9 %>% fill(Depth, Temp) 
View(Stingray9_2)

# Interpolate acceleration for rows where X, Y, Z = 0 because that appears to  
## be an artifact of the TechnoSmArt logger recording depth & temp at beginning 
### of each new second, but not acceleration. 

# rows w/missing acceleration readings (0,0,0)
miss1<- seq(1,nrow(Beverly2),by=25) #starting at row 1 then every 25 rows
miss2<- seq(1,nrow(Clementine2),by=25)
miss3<- seq(1,nrow(GnarlsBarbley2),by=25)
miss4<- seq(1,nrow(Jesus2),by=25)
miss5<- seq(1,nrow(Seamantha2),by=25)
miss6<- seq(1,nrow(Stingray8_2),by=25)
miss7<- seq(1,nrow(Stingray9_2),by=25)

# interpolate missing data by taking avg of readings in row before & row after
Beverly2$X[miss1] <- sapply(miss1, function(i) with(Beverly2, 
                                                    mean(c(X[i-1], X[i+1]))))
Beverly2$Y[miss1] <- sapply(miss1, function(i) with(Beverly2,
                                                    mean(c(Y[i-1], Y[i+1]))))
Beverly2$Z[miss1] <- sapply(miss1, function(i) with(Beverly2, 
                                                    mean(c(Z[i-1], Z[i+1]))))

Clementine2$X[miss2] <- sapply(miss2, function(i) with(Clementine2, 
                                                    mean(c(X[i-1], X[i+1]))))
Clementine2$Y[miss2] <- sapply(miss2, function(i) with(Clementine2, 
                                                       mean(c(Y[i-1], Y[i+1]))))
Clementine2$Z[miss2] <- sapply(miss2, function(i) with(Clementine2, 
                                                       mean(c(Z[i-1], Z[i+1]))))

GnarlsBarbley2$X[miss3] <- sapply(miss3, 
                                  function(i) with(GnarlsBarbley2, 
                                                   mean(c(X[i-1], X[i+1]))))
GnarlsBarbley2$Y[miss3] <- sapply(miss3, 
                                  function(i) with(GnarlsBarbley2, 
                                                   mean(c(Y[i-1], Y[i+1]))))
GnarlsBarbley2$Z[miss3] <- sapply(miss3, 
                                  function(i) with(GnarlsBarbley2, 
                                                   mean(c(Z[i-1], Z[i+1]))))

Jesus2$X[miss4] <- sapply(miss4, function(i) with(Jesus2, 
                                                    mean(c(X[i-1], X[i+1]))))
Jesus2$Y[miss4] <- sapply(miss4, function(i) with(Jesus2, 
                                                 mean(c(Y[i-1], Y[i+1]))))
Jesus2$Z[miss4] <- sapply(miss4, function(i) with(Jesus2, 
                                                 mean(c(Z[i-1], Z[i+1]))))

Seamantha2$X[miss5] <- sapply(miss5, function(i) with(Seamantha2, 
                                                  mean(c(X[i-1], X[i+1]))))
Seamantha2$Y[miss5] <- sapply(miss5, function(i) with(Seamantha2, 
                                                      mean(c(Y[i-1], Y[i+1]))))
Seamantha2$Z[miss5] <- sapply(miss5, function(i) with(Seamantha2, 
                                                      mean(c(Z[i-1], Z[i+1]))))

Stingray8_2$X[miss6] <- sapply(miss6, function(i) with(Stingray8_2, 
                                                      mean(c(X[i-1], X[i+1]))))
Stingray8_2$Y[miss6] <- sapply(miss6, function(i) with(Stingray8_2, 
                                                       mean(c(Y[i-1], Y[i+1]))))
Stingray8_2$Z[miss6] <- sapply(miss6, function(i) with(Stingray8_2, 
                                                       mean(c(Z[i-1], Z[i+1]))))

Stingray9_2$X[miss7] <- sapply(miss7, function(i) with(Stingray9_2, 
                                                       mean(c(X[i-1], X[i+1]))))
Stingray9_2$Y[miss7] <- sapply(miss7, function(i) with(Stingray9_2, 
                                                       mean(c(Y[i-1], Y[i+1]))))
Stingray9_2$Z[miss7] <- sapply(miss7, function(i) with(Stingray9_2, 
                                                       mean(c(Z[i-1], Z[i+1]))))

##### Smooth data & calculate ODBA ####

# Use function "Gsep" on accel x, y, and z columns to separate static 
## acceleration then get a new column with ODBA
# Use "filt" to smooth the data
## filt = rep(1,data freq * smoothing window)/(data freq * smoothing window)
# 3 second smoother okay if animal completes a full stroke (tail/wing beat) in 
## 3 s or less (Shepard et al. 2008). 25*3 = 75

#Berta2 <- Gsep(Berta[,c("X","Y","Z")],filt = rep(1,75)/75) #ERROR
#class(Berta$X) #XYZ not numeric, need to update for 2017 & 2018 deployments
Berta$X<- as.numeric(Berta$X)
Berta$Y<- as.numeric(Berta$Y)
Berta$Z<- as.numeric(Berta$Z)
Stingray3$X<- as.numeric(Stingray3$X)
Stingray3$Y<- as.numeric(Stingray3$Y)
Stingray3$Z<- as.numeric(Stingray3$Z)
Stingray4$X<- as.numeric(Stingray4$X)
Stingray4$Y<- as.numeric(Stingray4$Y)
Stingray4$Z<- as.numeric(Stingray4$Z)

Berta2 <- Gsep(Berta[,c("X","Y","Z")],filt = rep(1,75)/75)
Stingray3_2 <- Gsep(Stingray3[,c("X","Y","Z")],filt = rep(1,75)/75)
Stingray4_2<- Gsep(Stingray4[,c("X","Y","Z")],filt = rep(1,75)/75)
Beverly3 <- Gsep(Beverly2[,c("X","Y","Z")],filt = rep(1,75)/75)
Clementine3 <- Gsep(Clementine2[,c("X","Y","Z")],filt = rep(1,75)/75)
GnarlsBarbley3 <- Gsep(GnarlsBarbley2[,c("X","Y","Z")],filt = rep(1,75)/75)
Jesus3 <- Gsep(Jesus2[,c("X","Y","Z")],filt = rep(1,75)/75)
Seamantha3 <- Gsep(Seamantha2[,c("X","Y","Z")],filt = rep(1,75)/75)
Stingray8_3 <- Gsep(Stingray8_2[,c("X","Y","Z")],filt = rep(1,75)/75)
Stingray9_3 <- Gsep(Stingray9_2[,c("X","Y","Z")],filt = rep(1,75)/75)

# Create new data frames
Berta3 <- as.data.frame(Berta2)
Stingray3_3 <- as.data.frame(Stingray3_2)
Stingray4_3 <- as.data.frame(Stingray4_2)
Beverly4 <- as.data.frame(Beverly3)
Clementine4 <- as.data.frame(Clementine3)
GnarlsBarbley4 <- as.data.frame(GnarlsBarbley3)
Jesus4 <- as.data.frame(Jesus3)
Seamantha4 <- as.data.frame(Seamantha3)
Stingray8_4 <- as.data.frame(Stingray8_3)
Stingray9_4 <- as.data.frame(Stingray9_3)

# Add new columns & merge stingrayname w/ stingrayname 3 (2017,2018 data)
## OR stingrayname2 w/ stingrayname4 (2019 data)

Berta$Row <- 1:nrow(Berta) #new column w/ row numbers
Berta3$Row <- 1:nrow(Berta3) #new column w/row numbers
Berta4 <- merge(Berta, Berta3, by = "Row") #merge

Stingray3$Row <- 1:nrow(Stingray3) 
Stingray3_3$Row <- 1:nrow(Stingray3_3) 
Stingray3_4 <- merge(Stingray3, Stingray3_3, by = "Row") 

Stingray4$Row <- 1:nrow(Stingray4) 
Stingray4_3$Row <- 1:nrow(Stingray4_3) 
Stingray4_4 <- merge(Stingray4, Stingray4_3, by = "Row") 

Beverly2$Row <- 1:nrow(Beverly2) 
Beverly4$Row <- 1:nrow(Beverly4) 
Beverly5 <- merge(Beverly2, Beverly4, by = "Row") 

Clementine2$Row <- 1:nrow(Clementine2) 
Clementine4$Row <- 1:nrow(Clementine4) 
Clementine5 <- merge(Clementine2, Clementine4, by = "Row") 

GnarlsBarbley2$Row <- 1:nrow(GnarlsBarbley2) 
GnarlsBarbley4$Row <- 1:nrow(GnarlsBarbley4) 
GnarlsBarbley5 <- merge(GnarlsBarbley2, GnarlsBarbley4, by = "Row") 

Jesus2$Row <- 1:nrow(Jesus2) 
Jesus4$Row <- 1:nrow(Jesus4) 
Jesus5 <- merge(Jesus2, Jesus4, by = "Row") 

Seamantha2$Row <- 1:nrow(Seamantha2) 
Seamantha4$Row <- 1:nrow(Seamantha4) 
Seamantha5 <- merge(Seamantha2, Seamantha4, by = "Row") 

Stingray8_2$Row <- 1:nrow(Stingray8_2) 
Stingray8_4$Row <- 1:nrow(Stingray8_4) 
Stingray8_5 <- merge(Stingray8_2, Stingray8_4, by = "Row") 

Stingray9_2$Row <- 1:nrow(Stingray9_2) 
Stingray9_4$Row <- 1:nrow(Stingray9_4) 
Stingray9_5 <- merge(Stingray9_2, Stingray9_4, by = "Row") 

# Drop row number column
Berta4<- Berta4[-c(1)]
Stingray3_4<- Stingray3_4[-c(1)]
Stingray4_4<- Stingray4_4[-c(1)]
Beverly5 <- Beverly5[-c(1)]
Clementine5 <- Clementine5[-c(1)]
GnarlsBarbley5 <- GnarlsBarbley5[-c(1)]
Jesus5 <- Jesus5[-c(1)]
Seamantha5 <- Seamantha5[-c(1)]
Stingray8_5 <- Stingray8_5[-c(1)]
Stingray9_5 <- Stingray9_5[-c(1)]

# Write final data frames to CSV
write.csv(Berta4,"Berta.csv", row.names = FALSE)
write.csv(Stingray3_4,"Stingray3.csv", row.names = FALSE)
write.csv(Stingray4_4,"Stingray4.csv", row.names = FALSE)
write.csv(Beverly5,"Beverly.csv", row.names = FALSE)
write.csv(Clementine5,"Clementine.csv", row.names = FALSE)
write.csv(GnarlsBarbley5,"GnarlsBarbley.csv", row.names = FALSE)
write.csv(Jesus5,"Jesus.csv", row.names = FALSE)
write.csv(Seamantha5,"Seamantha.csv", row.names = FALSE)
write.csv(Stingray8_5,"Stingray8.csv", row.names = FALSE)
write.csv(Stingray9_5,"Stingray9.csv", row.names = FALSE)
