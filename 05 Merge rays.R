# MERGE ALL & GRAND MEANS PER HOUR #

# Author: Kathryn Flowers
# Last modified: 21 November 2023

##### Upload & prep data - only needed to merge files in beginning #####

# 2017 data South Water Caye 
sting1 <- read.csv("Sting1.csv")
sting2 <- read.csv("Sting2.csv")

# 2019 data Glover's Reef Atoll
sting5 <- read.csv("Sting5.csv")
sting6 <- read.csv("Sting6.csv")
sting7 <- read.csv("Sting7.csv")
sting8 <- read.csv("Sting8.csv")
sting9 <- read.csv("Sting9.csv")
sting10 <- read.csv("Sting10.csv")
sting11 <- read.csv("Sting11.csv")

# add column for stingray ID
sting1$ID <- "s1"
sting2$ID <- "s2"
sting5$ID <- "s5"
sting6$ID <- "s6"
sting7$ID <- "s7"
sting8$ID <- "s8"
sting9$ID <- "s9"
sting10$ID <- "s10"
sting11$ID <- "s11"

# merge all individuals by columns
allrays <- rbind(sting1,sting2,sting5,sting6,sting7,sting8,sting9,sting10,sting11)

# check out histogram of ODBA 
hist(allrays$ODBA[allrays$ODBA < 0.3],breaks=60,main="Histogram of ODBA", 
     xlab = "ODBA") 

#update date time
allrays$Date_Time <- as.POSIXct(allrays$Date_Time,format="%Y-%m-%d %H:%M:%S",
                               tz="America/Belize")

anyNA(allrays$Date_Time) # no

# add columns for hour 
allrays$hour <- as.integer(strftime(allrays$Date_Time, format = "%H", 
                                    tz="America/Belize"))
anyNA(allrays$hour) #none thank goodness

# check for zeroes
colSums(allrays==0) #there are 26 zeroes in ODBA, need to fix this for gamma

# find 2nd lowest value of ODBA
min_ODBA <- min(allrays$ODBA[allrays$ODBA!=0],na.rm = T) 

# randomly sample low values less than 2nd lowest ODBA value to replace zeroes
allrays$ODBA[allrays$ODBA==0] <-runif(sum(allrays$ODBA==0, na.rm = T),
                                      min_ODBA/1000,min_ODBA)
colSums(allrays==0) #fixed!

# save data as csv
write.csv(allrays,"allrays.csv", row.names = FALSE)
