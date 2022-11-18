# OVERALL DYNAMIC BODY ACCELERATION #

# TechnoSmArt AXY-Depth loggers unless otherwise noted (LL = Little Leonardo)
# Original gRumble (Gsep & filt functions) code from Sarah Luongo & Megan Kelley
# Modified, added to, & commented on by Kathryn Flowers
# Last modified 18 Nov 2022

##### Install gRumble package #####

#install("devtools")
#install_github("MBayOtolith/gRumble/gRumble") 

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
Stingray4<- d4[c(1392601:6263475),(2:8)] #combine rows, then columns

#d6: START = 10:20:00 June 12 (461201); END = 10:59:00.960 June 14 (4839725)
Berta <- d6[c(461201:4839725), (2:8)] #combine rows, then columns

#d7: START = 09:16:00 June 8 (402451); END = 13:36:24.960 June 10 (5113075)
GnarlsBarbley<- d7[c(402451:5113075),(2:8)] #combine rows, then columns

#d8: START = 09:08:00 June 9 (395126) ; END = 13:28:54.960 June 11 (5106500)
Stingray8<- d8[c(395126:5106500), (2:8)]

#d9: START = 08:58:00 June 10 (341151); END = 09:20:54.960 June 12 (4695525)
Stingray9<- d9[c(341151:4695525), (2:8)]

#d10: START = 09:06:00 June 11 (312176); END = 13:01:37.960 June 13 (4985625)
Beverly<- d10[c(312176:4985625),(2:8)] #combine rows, then columns

#d11: START = 10:30:00 June 12 (433401); END = 13:15:07.960 June 14 (5001100)
Jesus <- d11[c(433401:5001100),(2:8)] #combine rows, then columns

#d12: START = 10:40:00 June 14 (466501); END = 16:28:36.960 June 16 (5309425) 
Clementine <- d12[c(466501:5309425),(2:8)] #combine rows, then columns

#d13: START = 09:25:00 June 20 (357901); END = 19:31:55.960 June 20 (1268300)
Seamantha <- d13[c(357901:1268300),(2:7)] #combine rows, then columns

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

# Remove entire rows where x + y + z = 0 because that appears to be an artifact 
## of the TechnoSmArt logger recording depth & temp at beginning of each new 
### second, but not acceleration. Sampling 25 times per second, so delete every 
#### 25th row starting at row 1.

Beverly3<- Beverly2 %>% filter(row_number() %% 25 != 1)
View(Beverly3) #data are now 24 Hz

Clementine3<- Clementine2 %>% filter(row_number() %% 25 != 1)
View(Clementine3) #data are now 24 Hz

GnarlsBarbley3<- GnarlsBarbley2 %>% filter(row_number() %% 25 != 1)
View(GnarlsBarbley3) #data are now 24 Hz

Jesus3<- Jesus2 %>% filter(row_number() %% 25 != 1)
View(Jesus3) #data are now 24 Hz

Seamantha3<- Seamantha2 %>% filter(row_number() %% 25 != 1)
View(Seamantha3) #data are now 24 Hz

Stingray8_3<- Stingray8_2 %>% filter(row_number() %% 25 != 1)
View(Stingray8_3) #data are now 24 Hz

Stingray9_3<- Stingray9_2 %>% filter(row_number() %% 25 != 1)
View(Stingray9_3) #data are now 24 Hz

##### Smooth data & calculate ODBA ####

# Use function "Gsep" on accel x, y, and z columns to separate static & dynamic
## acceleration then get a new column with ODBA
# Use function "filt" to smooth the data
# 3 second smoother okay if animal completes a full stroke (tail/wing beat) in 
## 3 s or less (Shepard et al. 2008)

# 25*3=75 (Berta, Stingray3, Stingray4)

#Berta2 <- Gsep(Berta[,c("X","Y","Z")],filt = rep(1,75)/75) #ERROR
#class(Berta$X) #X, Y, and Z are not numeric, need to update
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

# 24*3=72 (Beverly, Clementine, GnarlsBarbley, Jesus, Seamantha, Stingray8,  
## Stingray9)

Beverly4 <- Gsep(Beverly3[,c("X","Y","Z")],filt = rep(1,72)/72)
Clementine4 <- Gsep(Clementine3[,c("X","Y","Z")],filt = rep(1,72)/72)
GnarlsBarbley4 <- Gsep(GnarlsBarbley3[,c("X","Y","Z")],filt = rep(1,72)/72)
Jesus4 <- Gsep(Jesus3[,c("X","Y","Z")],filt = rep(1,72)/72)
Seamantha4 <- Gsep(Seamantha3[,c("X","Y","Z")],filt = rep(1,72)/72)
Stingray8_4 <- Gsep(Stingray8_3[,c("X","Y","Z")],filt = rep(1,72)/72)
Stingray9_4 <- Gsep(Stingray9_3[,c("X","Y","Z")],filt = rep(1,72)/72)

# Create new data frames
Berta3 <- as.data.frame(Berta2)
Stingray3_3 <- as.data.frame(Stingray3_2)
Stingray4_3 <- as.data.frame(Stingray4_2)
Beverly5 <- as.data.frame(Beverly4)
Clementine5 <- as.data.frame(Clementine4)
GnarlsBarbley5 <- as.data.frame(GnarlsBarbley4)
Jesus5 <- as.data.frame(Jesus4)
Seamantha5 <- as.data.frame(Seamantha4)
Stingray8_5 <- as.data.frame(Stingray8_4)
Stingray9_5 <- as.data.frame(Stingray9_4)

# Add new columns & merge stingrayname w/ stingrayname 3 (25 Hz data)
## OR stingrayname3 w/ stingrayname5 (reduced 24 Hz data)

Berta$Row <- 1:nrow(Berta) #new column w/ row numbers
Berta3$Row <- 1:nrow(Berta3) #new column w/row numbers
Berta4<-merge(Berta, Berta3, by = "Row") #merge

Stingray3$Row <- 1:nrow(Stingray3) #new column w/ row numbers
Stingray3_3$Row <- 1:nrow(Stingray3_3) #new column w/row numbers
Stingray3_4<-merge(Stingray3, Stingray3_3, by = "Row") #merge

Stingray4$Row <- 1:nrow(Stingray4) #new column w/ row numbers
Stingray4_3$Row <- 1:nrow(Stingray4_3) #new column w/row numbers
Stingray4_4<-merge(Stingray4, Stingray4_3, by = "Row") #merge

Beverly3$Row <- 1:nrow(Beverly3) #new column w/ row numbers
Beverly5$Row <- 1:nrow(Beverly5) #new column w/row numbers
Beverly6<-merge(Beverly3, Beverly5, by = "Row") #merge

Clementine3$Row <- 1:nrow(Clementine3) #new column w/ row numbers
Clementine5$Row <- 1:nrow(Clementine5) #new column w/row numbers
Clementine6<-merge(Clementine3, Clementine5, by = "Row") #merge

GnarlsBarbley3$Row <- 1:nrow(GnarlsBarbley3) #new column w/ row numbers
GnarlsBarbley5$Row <- 1:nrow(GnarlsBarbley5) #new column w/row numbers
GnarlsBarbley6<-merge(GnarlsBarbley3, GnarlsBarbley5, by = "Row") #merge

Jesus3$Row <- 1:nrow(Jesus3) #new column w/ row numbers
Jesus5$Row <- 1:nrow(Jesus5) #new column w/row numbers
Jesus6<-merge(Jesus3, Jesus5, by = "Row") #merge

Seamantha3$Row <- 1:nrow(Seamantha3) #new column w/ row numbers
Seamantha5$Row <- 1:nrow(Seamantha5) #new column w/row numbers
Seamantha6<-merge(Seamantha3, Seamantha5, by = "Row") #merge

Stingray8_3$Row <- 1:nrow(Stingray8_3) #new column w/ row numbers
Stingray8_5$Row <- 1:nrow(Stingray8_5) #new column w/row numbers
Stingray8_6<-merge(Stingray8_3, Stingray8_5, by = "Row") #merge

Stingray9_3$Row <- 1:nrow(Stingray9_3) #new column w/ row numbers
Stingray9_5$Row <- 1:nrow(Stingray9_5) #new column w/row numbers
Stingray9_6<-merge(Stingray9_3, Stingray9_5, by = "Row") #merge

# Drop row number column
Berta4<- Berta4[-c(1)]
Stingray3_4<- Stingray3_4[-c(1)]
Stingray4_4<- Stingray4_4[-c(1)]
Beverly6 <- Beverly6[-c(1)]
Clementine6 <- Clementine6[-c(1)]
GnarlsBarbley6 <- GnarlsBarbley6[-c(1)]
Jesus6 <- Jesus6[-c(1)]
Seamantha6 <- Seamantha6[-c(1)]
Stingray8_6 <- Stingray8_6[-c(1)]
Stingray9_6 <- Stingray9_6[-c(1)]

# Write final data frames to CSV
write.csv(Berta4,"Berta.csv", row.names = FALSE)
write.csv(Stingray3_4,"Stingray3.csv", row.names = FALSE)
write.csv(Stingray4_4,"Stingray4.csv", row.names = FALSE)
write.csv(Beverly6,"Beverly.csv", row.names = FALSE)
write.csv(Clementine6,"Clementine.csv", row.names = FALSE)
write.csv(GnarlsBarbley6,"GnarlsBarbley.csv", row.names = FALSE)
write.csv(Jesus6,"Jesus.csv", row.names = FALSE)
write.csv(Seamantha6,"Seamantha.csv", row.names = FALSE)
write.csv(Stingray8_6,"Stingray8.csv", row.names = FALSE)
write.csv(Stingray9_6,"Stingray9.csv", row.names = FALSE)
