# DOWNSAMPLING BY AVERAGING ACCELERATION DATA #

# Created by Kathryn Flowers
# Last modified 10 Nov 2023

##### Load libraries & get working directory #####
library(dplyr)
library(tidyr)

getwd()

##### Data processing #####

# import data
ray3<-read.csv("Stingray3.csv", stringsAsFactors = FALSE)
ray4<-read.csv("Stingray4.csv", stringsAsFactors = FALSE)
ray6<-read.csv("Berta.csv", stringsAsFactors = FALSE)
ray7<-read.csv("GnarlsBarbley.csv", stringsAsFactors = FALSE)
ray8<-read.csv("Stingray8.csv", stringsAsFactors = FALSE)
ray9<-read.csv("Stingray9.csv", stringsAsFactors = FALSE)
ray10<-read.csv("Beverly.csv", stringsAsFactors = FALSE)
ray11<-read.csv("Jesus.csv", stringsAsFactors = FALSE)
ray12<-read.csv("Clementine.csv", stringsAsFactors = FALSE)
ray13<-read.csv("Seamantha.csv", stringsAsFactors = FALSE)

# remove unnecessary columns
ray3<- ray3[,!names(ray3) %in% c("X_Static", "Y_Static", "Z_Static",
                                 "X_Dynamic", "Y_Dynamic", "Z_Dynamic")]
head(ray3) #done!

ray4<- ray4[,!names(ray4) %in% c("X_Static", "Y_Static", "Z_Static",
                                 "X_Dynamic", "Y_Dynamic", "Z_Dynamic")]
head(ray4)

ray6<- ray6[,!names(ray6) %in% c("X_Static", "Y_Static", "Z_Static",
                                 "X_Dynamic", "Y_Dynamic", "Z_Dynamic")]
head(ray6)

ray7<- ray7[,!names(ray7) %in% c("X_Static", "Y_Static", "Z_Static",
                                 "X_Dynamic", "Y_Dynamic", "Z_Dynamic")]
head(ray7)

ray8<- ray8[,!names(ray8) %in% c("X_Static", "Y_Static", "Z_Static",
                                 "X_Dynamic", "Y_Dynamic", "Z_Dynamic")]
head(ray8)

ray9<- ray9[,!names(ray9) %in% c("X_Static", "Y_Static", "Z_Static",
                                 "X_Dynamic", "Y_Dynamic", "Z_Dynamic")]
head(ray9)

ray10<- ray10[,!names(ray10) %in% c("X_Static", "Y_Static", "Z_Static",
                                 "X_Dynamic", "Y_Dynamic", "Z_Dynamic")]
head(ray10)

ray11<- ray11[,!names(ray11) %in% c("X_Static", "Y_Static", "Z_Static",
                                 "X_Dynamic", "Y_Dynamic", "Z_Dynamic")]
head(ray11)

ray12<- ray12[,!names(ray12) %in% c("X_Static", "Y_Static", "Z_Static",
                                 "X_Dynamic", "Y_Dynamic", "Z_Dynamic")]
head(ray12)

ray13<- ray13[,!names(ray13) %in% c("X_Static", "Y_Static", "Z_Static",
                                 "X_Dynamic", "Y_Dynamic", "Z_Dynamic")]
head(ray13)

# combine date & time into one column
ray3$Date_Time <- paste (ray3$Date, ray3$Time, sep=" ")
ray4$Date_Time <- paste (ray4$Date, ray4$Time, sep=" ")
ray6$Date_Time <- paste (ray6$Date, ray6$Time, sep=" ")
ray7$Date_Time <- paste (ray7$Date, ray7$Time, sep=" ")
ray8$Date_Time <- paste (ray8$Date, ray8$Time, sep=" ")
ray9$Date_Time <- paste (ray9$Date, ray9$Time, sep=" ")
ray10$Date_Time <- paste (ray10$Date, ray10$Time, sep=" ")
ray11$Date_Time <- paste (ray11$Date, ray11$Time, sep=" ")
ray12$Date_Time <- paste (ray12$Date, ray12$Time, sep=" ")
ray13$Date_Time <- paste (ray13$Date, ray13$Time, sep=" ")

# remove old date & time columns
ray3<- ray3[,!names(ray3) %in% c("Date", "Time")]
ray4<- ray4[,!names(ray4) %in% c("Date", "Time")]
ray6<- ray6[,!names(ray6) %in% c("Date", "Time")]
ray7<- ray7[,!names(ray7) %in% c("Date", "Time")]
ray8<- ray8[,!names(ray8) %in% c("Date", "Time")]
ray9<- ray9[,!names(ray9) %in% c("Date", "Time")]
ray10<- ray10[,!names(ray10) %in% c("Date", "Time")]
ray11<- ray11[,!names(ray11) %in% c("Date", "Time")]
ray12<- ray12[,!names(ray12) %in% c("Date", "Time")]
ray13<- ray13[,!names(ray13) %in% c("Date", "Time")]

# update time format

#class(ray3$Date_Time) #check date/time class = character
#WARNING: date format differs across data logger version (rays 3-6)
#OlsonNames() #unsure what time zone to type? run this for full list

ray3$Date_Time <- as.POSIXct(ray3$Date_Time,format="%d-%m-%Y %H:%M:%OS",
                               tz="America/Belize") 

ray4$Date_Time <- as.POSIXct(ray4$Date_Time,format="%d-%m-%Y %H:%M:%OS",
                             tz="America/Belize")

ray6$Date_Time <- as.POSIXct(ray6$Date_Time,format="%d-%m-%Y %H:%M:%OS",
                             tz="America/Belize")

ray7$Date_Time <- as.POSIXct(ray7$Date_Time,format="%m-%d-%Y %H:%M:%OS",
                             tz="America/Belize")

ray8$Date_Time <- as.POSIXct(ray8$Date_Time,format="%m-%d-%Y %H:%M:%OS",
                             tz="America/Belize")

ray9$Date_Time <- as.POSIXct(ray9$Date_Time,format="%m-%d-%Y %H:%M:%OS",
                             tz="America/Belize")

ray10$Date_Time <- as.POSIXct(ray10$Date_Time,format="%m-%d-%Y %H:%M:%OS",
                             tz="America/Belize")

ray11$Date_Time <- as.POSIXct(ray11$Date_Time,format="%m-%d-%Y %H:%M:%OS",
                             tz="America/Belize")

ray12$Date_Time <- as.POSIXct(ray12$Date_Time,format="%m-%d-%Y %H:%M:%OS",
                             tz="America/Belize")

ray13$Date_Time <- as.POSIXct(ray13$Date_Time,format="%m-%d-%Y %H:%M:%OS",
                             tz="America/Belize")

#strftime(ray7$Date_Time[1:25], "%OS4")  #view Date_Time w/more digits

##### Downsample by taking 1 second averages #####

require(dplyr)

Stingray3<- ray3 %>%
  group_by(Date_Time = cut(Date_Time, breaks="1 sec")) %>%
  summarize(Depth = mean(Depth), 
            Temp = mean(Temp), 
            ODBA = mean(ODBA))

Stingray4<- ray4 %>%
  group_by(Date_Time = cut(Date_Time, breaks="1 sec")) %>%
  summarize(Depth = mean(Depth), 
            Temp = mean(Temp), 
            ODBA = mean(ODBA))

Stingray6<- ray6 %>%
  group_by(Date_Time = cut(Date_Time, breaks="1 sec")) %>%
  summarize(Depth = mean(Depth), 
            Temp = mean(Temp), 
            ODBA = mean(ODBA))

Stingray7<- ray7 %>%
  group_by(Date_Time = cut(Date_Time, breaks="1 sec")) %>%
  summarize(Depth = mean(Depth), 
            Temp = mean(Temp), 
            ODBA = mean(ODBA))

Stingray8<- ray8 %>%
  group_by(Date_Time = cut(Date_Time, breaks="1 sec")) %>%
  summarize(Depth = mean(Depth), 
            Temp = mean(Temp), 
            ODBA = mean(ODBA))

Stingray9<- ray9 %>%
  group_by(Date_Time = cut(Date_Time, breaks="1 sec")) %>%
  summarize(Depth = mean(Depth), 
            Temp = mean(Temp), 
            ODBA = mean(ODBA))

Stingray10<- ray10 %>%
  group_by(Date_Time = cut(Date_Time, breaks="1 sec")) %>%
  summarize(Depth = mean(Depth), 
            Temp = mean(Temp), 
            ODBA = mean(ODBA))

Stingray11<- ray11 %>%
  group_by(Date_Time = cut(Date_Time, breaks="1 sec")) %>%
  summarize(Depth = mean(Depth), 
            Temp = mean(Temp), 
            ODBA = mean(ODBA))

Stingray12<- ray12 %>%
  group_by(Date_Time = cut(Date_Time, breaks="1 sec")) %>%
  summarize(Depth = mean(Depth), 
            Temp = mean(Temp), 
            ODBA = mean(ODBA))

Stingray13<- ray13 %>%
  group_by(Date_Time = cut(Date_Time, breaks="1 sec")) %>%
  summarize(Depth = mean(Depth), 
            Temp = mean(Temp), 
            ODBA = mean(ODBA))

# write new csv
write.csv(Stingray3,"Stingray3_avg.csv", row.names = FALSE)
write.csv(Stingray4,"Stingray4_avg.csv", row.names = FALSE)
write.csv(Stingray6,"Berta_avg.csv", row.names = FALSE)
write.csv(Stingray7,"GnarlsBarbley_avg.csv", row.names = FALSE)
write.csv(Stingray8,"Stingray8_avg.csv", row.names = FALSE)
write.csv(Stingray9,"Stingray9_avg.csv", row.names = FALSE)
write.csv(Stingray10,"Beverly_avg.csv", row.names = FALSE)
write.csv(Stingray11,"Jesus_avg.csv", row.names = FALSE)
write.csv(Stingray12,"Clementine_avg.csv", row.names = FALSE)
write.csv(Stingray13,"Seamantha_avg.csv", row.names = FALSE)
