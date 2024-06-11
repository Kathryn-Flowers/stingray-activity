## Grand means per hour ##

# By Katie Flowers
# Last update 26 Jan 2024

##### Load libraries #####

library(dplyr) # data org
library(ggplot2) # data vis
library(grid) # stack ggplots

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

# Add column for hour
sting1$hour <- as.integer(strftime(sting1$Date_Time, format = "%H", 
                                    tz="America/Belize"))

sting2$hour <- as.integer(strftime(sting2$Date_Time, format = "%H", 
                                   tz = "America/Belize"))

sting5$hour <- as.integer(strftime(sting5$Date_Time, format = "%H", 
                                   tz = "America/Belize"))

sting6$hour <- as.integer(strftime(sting6$Date_Time, format = "%H", 
                                   tz = "America/Belize"))

sting7$hour <- as.integer(strftime(sting7$Date_Time, format = "%H", 
                                   tz = "America/Belize"))

sting8$hour <- as.integer(strftime(sting8$Date_Time, format = "%H", 
                                   tz = "America/Belize"))

sting9$hour <- as.integer(strftime(sting9$Date_Time, format = "%H", 
                                   tz = "America/Belize"))

sting10$hour <- as.integer(strftime(sting10$Date_Time, format = "%H", 
                                    tz = "America/Belize"))

sting11$hour <- as.integer(strftime(sting11$Date_Time, format = "%H", 
                                    tz = "America/Belize"))

# Check for NAs

datalist<-list(sting1, sting2, sting5, sting6, sting7, sting8, sting9, 
               sting10, sting11)

for (i in 1:length(datalist)) {
  dataname <- paste0("sting", i)
  if (anyNA(datalist[[i]])) {
    print(paste("NA found in", dataname))
  } else {
    print(paste("No NA in", dataname))
  }
}

# Means per hour

hr.means.s1 <- sting1 %>% group_by(hour) %>% summarize(across(everything(), mean))
hr.means.s2 <- sting2 %>% group_by(hour) %>% summarize(across(everything(), mean))
hr.means.s5 <- sting5 %>% group_by(hour) %>% summarize(across(everything(), mean))
hr.means.s6 <- sting6 %>% group_by(hour) %>% summarize(across(everything(), mean))
hr.means.s7 <- sting7 %>% group_by(hour) %>% summarize(across(everything(), mean))
hr.means.s8 <- sting8 %>% group_by(hour) %>% summarize(across(everything(), mean))
hr.means.s9 <- sting9 %>% group_by(hour) %>% summarize(across(everything(), mean))
hr.means.s10 <- sting10 %>% group_by(hour) %>% summarize(across(everything(), mean))
hr.means.s11 <- sting11 %>% group_by(hour) %>% summarize(across(everything(), mean))

# Drop unwanted columns

hr.means.s1 <- hr.means.s1[,-2]
hr.means.s2 <- hr.means.s2[,-2]
hr.means.s5 <- hr.means.s5[,-2]
hr.means.s6 <- hr.means.s6[,-2]
hr.means.s7 <- hr.means.s7[,-2]
hr.means.s8 <- hr.means.s8[,-2]
hr.means.s9 <- hr.means.s9[,-2]
hr.means.s10 <- hr.means.s10[,-2]
hr.means.s11 <-hr.means.s11[,-2]

# Add column for individual

hr.means.s1$ID <- "s1"
hr.means.s2$ID <- "s2"
hr.means.s5$ID <- "s5"
hr.means.s6$ID <- "s6"
hr.means.s7$ID <- "s7"
hr.means.s8$ID <- "s8"
hr.means.s9$ID <- "s9"
hr.means.s10$ID <- "s10"
hr.means.s11$ID <- "s11"

# Aggregate by ID
RaysGrandMean <- rbind(hr.means.s1,
                       hr.means.s2,
                       hr.means.s5,
                       hr.means.s6,
                       hr.means.s7,
                       hr.means.s8,
                       hr.means.s9,
                       hr.means.s10,
                       hr.means.s11)

# save data as csv to avoid steps above in future
write.csv(RaysGrandMean,"RaysGrandMean.csv", row.names = FALSE)

##### Summary plot #####

# Load data 

allrays<-read.csv("RaysGrandMean.csv")

# change 0 to 24 in hour of day
allrays$hour[allrays$hour == 0] <- 24

# check for NAs
anyNA(allrays)

# plot mean ODBA over time
A <- ggplot(allrays, aes(x = hour, y = ODBA)) + 
            stat_summary(fun = mean, geom = "point",size=2.5) +
            stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="errorbar", width = 0.4) + # fun.args = list(mult = 1) is +/- 1 SD instead of 2 that is default
            theme_classic() +
            scale_x_continuous(breaks=seq(1,24,2)) +
            theme(text = element_text(size = 22)) +
            theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
            ylab("ODBA") +
            theme(axis.title.y = element_text(margin = margin(r = 20)))

## this would be manual calculation to give same as above and check fun.args is working
#A <- ggplot(allrays, aes(x = hour, y = ODBA)) + 
 # stat_summary(fun = mean, 
  #             geom = "pointrange",
   #            fun.max=function(hour) mean (hour) + sd(hour),
    #           fun.min=function(hour) mean (hour) - sd(hour)) +
  #theme_classic() +
  #scale_x_continuous(breaks=seq(1,24,2)) +
  #theme(text = element_text(size = 22)) +
  #theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  #theme(axis.title.y = element_blank())

# plot mean Depth over time
B <- ggplot(allrays, aes(x = hour, y = Depth)) + 
            stat_summary(fun = mean, geom = "point",size=2.5, color = "#2A597D") +
            stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="errorbar", width = 0.4, color = "#2A597D") + 
            theme_classic() +
            scale_y_reverse() +
            scale_x_continuous(breaks=seq(1,24,2)) +
            theme(text = element_text(size = 22)) +
            theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
            ylab("Depth (m)") +
            theme(axis.title.y = element_text(margin = margin(r = 20)))

# plot mean Temp over time
C <- ggplot(allrays, aes(x = hour, y = Temp)) + 
            stat_summary(fun = mean, geom = "point",size=2.5, color="#CC501B") +
            stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="errorbar", width = 0.4, color="#CC501B") + 
            theme_classic() +
            scale_x_continuous(breaks=seq(1,24,2)) +
            theme(text = element_text(size = 22)) +
            xlab ("Hour of day") +
            ylab ("Temperature (°C)") +
            theme(axis.title.y = element_text(margin = margin(r = 20))) +
            theme(axis.title.x = element_text(margin = margin(t = 20)))

# stack plots
grid.draw(rbind(ggplotGrob(A), 
                ggplotGrob(B),
                ggplotGrob(C)))


# summarize data by hour

ODBA <-allrays %>% group_by(hour) %>% summarize(mean(ODBA), sd(ODBA))
Depth <-allrays %>% group_by(hour) %>% summarize(mean(Depth), sd(Depth))
Temp <-allrays %>% group_by(hour) %>% summarize(mean(Temp), sd(Temp))
