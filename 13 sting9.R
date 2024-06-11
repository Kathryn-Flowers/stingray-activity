# HIDDEN MARKOV MODELS #

# Authored by Kathryn Flowers & Elizabeth Babcock
# With help from Ron Togunov & Natasha Klappstein
# Last updated 30 May 2024

##### Load libraries & read data #####

library(dplyr) #organize dataframes
library(lubridate) #format dates & times
library(ggplot2) #data vis
library(grid) #stack ggplots
library(momentuHMM) #fit HMMs
library(beepr) #sound alerts
library(scales) #for visualization
library(tidyverse)

# Data are averaged over 1 second

# 2019 data Glover's Reef Atoll
sting9 <- read.csv("sting9.csv")

##### Stingray 9 #####

str(sting9$Date_Time) #not as.Posixct

# update "Date_Time" into time variable
sting9$Date_Time <- as.POSIXct(sting9$Date_Time,format="%Y-%m-%d %H:%M:%S",
                               tz="America/Belize")

# any NAs
anyNA(sting9) #phew!

# Data visualization ---------------------------------------------------------

# Colors are colorblind friendly, you can check HEX codes here:
## https://davidmathlogic.com/colorblind/

gg_odba <- ggplot(sting9, aes(x = Date_Time, y = ODBA)) + 
  geom_line() + 
  ylab("ODBA") + 
  theme_classic() +
  theme(text=element_text(size = 15)) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())

gg_depth <- ggplot(sting9, aes(x = Date_Time, y = Depth)) + 
  geom_line(color="#2A597D") +
  scale_y_reverse() +
  ylab("Depth (m)") + 
  theme_classic() + 
  theme(text=element_text(size = 15)) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) 

ylab <- expression("Temperature " ( degree~C))

gg_temp <- ggplot(sting9, aes(x = Date_Time, y = Temp)) +
  geom_line(color="#CC501B") +
  labs(y=ylab, x="Date & Time") +
  theme_classic() +
  theme(text=element_text(size = 15))

grid.draw(rbind(ggplotGrob(gg_odba), 
                ggplotGrob(gg_depth),
                ggplotGrob(gg_temp)))

# Prep data -------------------------------------------------------------------

# Look for any zeros in ODBA column b/c range of gamma distribution is > 0
colSums(sting9==0) # none

# Obtain hour for cosinor 
sting9$hour <- as.integer(strftime(sting9$Date_Time, format = "%H", 
                                       tz="America/Belize"))

anyNA(sting9$hour)

# Prep data for momentuHMM
sting9data <- prepData(sting9,
                       coordNames = NULL,
                       covNames = c("Depth", "Temp", "hour"))

# Round tiny numbers to zero
sting9data$ODBA<-round(sting9data$ODBA,8)

# HMMs -------------------------------------------------------------------------

# What are we looking for? Probability of stingrays being in state of low, 
## medium, or high activity (i.e., 3 states) based on: time of day, water
### temperature, and depth. 

# Par0 = starting values for step length parameters: mean & sd for each state
## from histogram of ODBA & initial values for zero inflation
### More on selection in Michelot & Langrock 2022:
#### https://cran.r-project.org/web/packages/moveHMM/vignettes/moveHMM-starting-values.pdf 


# Histogram of ODBA to visualize state distributions

hist(sting9data$ODBA[sting9data$ODBA < 0.3],breaks=200,main="Histogram of ODBA", 
     xlab = "ODBA") #look for distributions

# Model set up
formula1 = ~hour
formula2 = ~cosinor(hour, period = 12)
formula3 = ~cosinor(hour, period = 24)
formula4 = ~Depth
formula5 = ~Temp
formula6 = ~hour + Depth
formula7 = ~hour + Temp
formula8 = ~Depth + Temp
formula9 = ~hour * Depth
formula10 = ~hour * Temp
formula11 = ~Depth * Temp
formula12 = ~cosinor(hour, period = 12) + Depth
formula13 = ~cosinor(hour, period = 24) + Depth
formula14 = ~cosinor(hour, period = 12) + Temp
formula15 = ~cosinor(hour, period = 24) + Temp
formula16 = ~cosinor(hour, period = 12) * Depth
formula17 = ~cosinor(hour, period = 24) * Depth
formula18 = ~cosinor(hour, period = 12) * Temp
formula19 = ~cosinor(hour, period = 24) * Temp
formula20 = ~hour + Depth + Temp
formula21 = ~hour + Depth * Temp
formula22 = ~hour * Depth + Temp
formula23 = ~hour * Temp + Depth
formula24 = ~hour * Temp * Depth
formula25 = ~cosinor(hour, period = 12) + Depth + Temp
formula26 = ~cosinor(hour, period = 24) + Depth + Temp
formula27 = ~cosinor(hour, period = 12) + Depth * Temp
formula28 = ~cosinor(hour, period = 24) + Depth * Temp
formula29 = ~cosinor(hour, period = 12) * Depth + Temp
formula30 = ~cosinor(hour, period = 24) * Depth + Temp
formula31 = ~cosinor(hour, period = 12) * Temp + Depth
formula32 = ~cosinor(hour, period = 24) * Temp + Depth
formula33 = ~cosinor(hour, period = 12) * Depth * Temp
formula34 = ~cosinor(hour, period = 24) * Depth * Temp

stateNames<- c("Low activity", "Medium activity", "High activity")

# 3 state models ---------------------------------------------------------------

# formula 1 (hour only)
s9.fit1 <- fitHMM(sting9data, 
                nbStates=3, 
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.01,0.04,0.095,0.01,0.04,0.095,
                                   0.02,0.03,0.08)),
                formula = formula1, 
                stateNames = stateNames)
beep(4)
s9.fit1
plot(s9.fit1, breaks = 60) 
plotPR(s9.fit1) #pseudoresiduals
plotStationary(s9.fit1, plotCI = TRUE)
s9.fit1$CIbeta #NaNs - different combos of Par0 above don't improve this but does change which activity state has NaNs
s9.fit1$CIreal #NANs

# formula 2 (cosinor 12 hours)
s9.fit2 <- fitHMM(sting9data,
                   nbStates=3,
                   dist=list(ODBA="gamma"),
                   Par0 = list(ODBA=c(0.006,0.03,0.09,0.006,0.03,0.09,
                                      0.05,0.05,0.05)),
                   formula = formula2,
                   stateNames = stateNames)
beep(4)
s9.fit2
plot(s9.fit2, breaks = 60)
plotPR(s9.fit2) #pseudoresiduals
plotStationary(s9.fit2, plotCI = TRUE)

# formula 3 (cosinor 24 hours)
s9.fit3 <- fitHMM(sting9data,
                  nbStates=3,
                  dist=list(ODBA="gamma"),
                  Par0 = list(ODBA=c(0.006,0.03,0.09,0.006,0.03,0.09,
                                     0.05,0.05,0.05)),
                  formula = formula3,
                  stateNames = stateNames)
beep(4)
s9.fit3
plot(s9.fit3, breaks = 60)
plotPR(s9.fit3) #pseudoresiduals
supp.fig <- plotStationary(s9.fit3, plotCI = TRUE,
               cex.axis = 2,
               cex.legend = 1.5,
               legend.pos = "topright",
               lwd = 2,
               return = TRUE) 

##### BEST BIC ######
# formula 4 (depth)
s9.fit4 <- fitHMM(sting9data,
                  nbStates=3,
                  dist=list(ODBA="gamma"),
                  Par0 = list(ODBA=c(0.006,0.03,0.09,0.006,0.03,0.09,
                                     0.05,0.05,0.05)),
                  formula = formula4,
                  stateNames = stateNames)
s9.fit4
plot(s9.fit4, breaks = 60, cex.axis =2,
     cex.legend = 2,
     lwd =2) 
plotPR(s9.fit4) #pseudoresiduals
s9.state.prob<- plotStationary(s9.fit4, plotCI = TRUE,
                               cex.axis = 2,
                               cex.legend = 1.5,
                               legend.pos = "topright",
                               lwd = 2,
                               return = TRUE) 

#No NAs here!
s9.fit4$CIbeta 
s9.fit4$CIreal

# update plots

depthVals<-bind_rows(s9.state.prob$Depth,.id="Activity") %>%
  rename(Depth=cov)
depthVals$Activity <-factor(depthVals$Activity, levels = c("Low activity",
                                                           "Medium activity",
                                                           "High activity"))

s9.depth<- ggplot(depthVals,
                  aes(x=Depth,y=est,ymax=uci,ymin=lci,color=Activity,
                      fill=Activity, linetype=Activity)) +
  geom_line(linewidth=0.9)+
  geom_ribbon(alpha=0.55, linewidth=0.9) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  xlab("\nDepth (m)") +
  ylab("Stationary state probabilities \n") +
  theme(text=element_text(size = 20)) +
  theme(axis.text.x = element_text(vjust=0.5)) +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  theme(legend.position = c(0.28, 0.9))
s9.depth + scale_color_manual(values=alpha(c('#6792DA', '#ECCC68','#DA5E5E'))) +
  scale_fill_manual(values=c('#6792DA', '#ECCC68','#DA5E5E')) +
  scale_linetype_manual(values=c("dotted","longdash","solid")) +
  guides(color = guide_legend(title = NULL), 
         fill = guide_legend(title = NULL),
         linetype = guide_legend(title = NULL)) 

# decode most likely state sequence
s9.states <-viterbi(s9.fit4)

# % of time in each state
table(s9.states)/nrow(sting9data)

# Add states to time series data
sting9$States <- s9.states
sting9$States <- as.factor(sting9$States)
sting9$Date_Time <- as.POSIXct(sting9$Date_Time, tz="America/Belize")

# Plot states over covariates
ggStatesODBA<- ggplot(sting9) + 
  geom_segment(aes(x = Date_Time, y = ODBA, 
                   xend=lead(Date_Time), yend=lead(ODBA),
                   color=States), 
               linetype = "solid", 
               lineend = "round",
               linejoin = "round",
               alpha = 1) +
  xlab("\nTime (24 hrs) & Day") +
  ylab("ODBA \n") +
  scale_y_continuous(labels = scales::number_format(0.1)) +
  theme_classic() + 
  ggtitle("Stingray 9") +
  theme(legend.position="top") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(labels = date_format("%H:%M\n%d %b", tz= attr(sting9$Date_Time,"tzone")),
                   date_breaks = "6 hours") +
  theme(text=element_text(size = 20)) +
  theme(axis.text.x = element_text(vjust=0.5)) +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  scale_color_manual(values=alpha(c('#6792DA', '#ECCC68','#DA5E5E'), 0.75),
                     labels = c("Low activity", "Medium activity", "High activity")) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) 

ggStatesDepth <- ggplot(sting9) + 
  geom_segment(aes(x = Date_Time, y = Depth, 
                   xend=lead(Date_Time), yend=lead(Depth),
                   color=States), linetype = "solid", 
               lineend = "round",
               linejoin = "round",
               alpha = 1, show.legend = F) +
  xlab("\nTime (24 hrs) & Day") +
  ylab("Depth (m) \n") +
  scale_y_reverse() +
  theme_classic() + 
  scale_x_datetime(labels = date_format("%H:%M\n%d %b", tz= attr(sting9$Date_Time,"tzone")),
                   date_breaks = "6 hours") +
  theme(text=element_text(size = 20)) +
  theme(axis.text.x = element_text(vjust=0.5)) +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  scale_color_manual(values=alpha(c('#6792DA', '#ECCC68','#DA5E5E'), 0.75),
                     labels = c("Low activity", "Medium activity", "High activity")) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())

ggStatesTemp <- ggplot(sting9) + 
  geom_segment(aes(x = Date_Time, y = Temp, 
                   xend=lead(Date_Time), yend=lead(Temp),
                   color=States), linetype = "solid", 
               lineend = "round",
               linejoin = "round",
               alpha = 1, show.legend = F) +
  xlab("\nTime (24 hrs) & Date (2019)") +
  ylab("Temperature (°C) \n") +
  scale_y_continuous(labels = scales::number_format(0.1)) +
  theme_classic() + 
  scale_x_datetime(labels = date_format("%H:%M\n%d %b", tz= attr(sting9$Date_Time,"tzone")),
                   date_breaks = "6 hours") +
  theme(text=element_text(size = 20)) +
  theme(axis.text.x = element_text(vjust=0.5)) +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  scale_color_manual(values=alpha(c('#6792DA', '#ECCC68','#DA5E5E'), 0.75),
                     labels = c("Low activity", "Medium activity", "High activity")) 

grid.draw(rbind(ggplotGrob(ggStatesODBA), 
                ggplotGrob(ggStatesDepth),
                ggplotGrob(ggStatesTemp)))

# formula 5 (temperature)
# Par0 issues
# s9.fit5 <- fitHMM(sting9data,
#                nbStates=3,
#                dist=list(ODBA="gamma"),
#                Par0 = list(ODBA=c(0.01,0.035,0.095,0.01,0.035,0.095,
#                                   0.1,0.1,0.1)),
#                formula = formula5,
#                stateNames = stateNames)
# beep(4)
# s9.fit5
# plot(s9.fit5, breaks = 60)
# plotPR(s9.fit5) #pseudoresiduals
# plotStationary(s9.fit5, plotCI = TRUE)

# formula 6 (hour + Depth)
# convergence issues tried many Par0 values
# s9.fit6 <- fitHMM(sting9data,
#                nbStates=3,
#                dist=list(ODBA="gamma"),
#                Par0 = list(ODBA=c(0.01,0.03,0.1,0.01,0.03,0.1, 
#                                   0.01,0.03,0.1)),
#                formula = formula6,
#                stateNames = stateNames)
# 
# beep(4)
# s9.fit6
# plot(s9.fit6, breaks = 60)
# plotPR(s9.fit6) #pseudoresiduals
# plotStationary(s9.fit6, plotCI = TRUE)

# formula 7 (hour + temperature)
# s9.fit7 <- fitHMM(sting9data,
#                nbStates=3,
#                dist=list(ODBA="gamma"),
#                Par0 = list(ODBA=c(0.01,0.031,0.09,0.01,0.031,0.09,
#                                   0.06,0.06,0.06)),
#                formula = formula7,
#                stateNames = stateNames)
# beep(4)
# s9.fit7
# plot(s9.fit7, breaks = 60)
# plotPR(s9.fit7) #pseudoresiduals
# plotStationary(s9.fit7, plotCI = TRUE)

# formula 8 (depth + temperature)
# s9.fit8 <- fitHMM(sting9data,
#                nbStates=3,
#                dist=list(ODBA="gamma"),
#                Par0 = list(ODBA=c(0.009,0.028,0.1,0.009,0.028,0.1,
#                                   0.06,0.06,0.06)),
#                formula = formula8,
#                stateNames = stateNames)
# beep(4)
# s9.fit8
# plot(s9.fit8, breaks = 60)
# plotPR(s9.fit8) #pseudoresiduals
# plotStationary(s9.fit8, plotCI = TRUE)

# formula 9 (hour interaction with depth)
# s9.fit9 <- fitHMM(sting9data,
#                nbStates=3,
#                dist=list(ODBA="gamma"),
#                Par0 = list(ODBA=c(0.003,0.028,0.09,0.003,0.028,0.09,
#                                   0.06,0.06,0.06)),
#                formula = formula9,
#                stateNames = stateNames)
# beep(4)
# s9.fit9
# plot(s9.fit9, breaks = 60)
# plotPR(s9.fit9) #pseudoresiduals
# plotStationary(s9.fit9, plotCI = TRUE)

# formula 10 (hour interaction with temperature)
s9.fit10 <- fitHMM(sting9data, 
                nbStates=3, 
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.006,0.03,0.09,0.006,0.03,0.09,
                                   0.05,0.05,0.05)),
                formula = formula10, 
                stateNames = stateNames)
beep(4)
s9.fit10
plot(s9.fit10)
plotPR(s9.fit10) #pseudoresiduals
plotStationary(s9.fit10, plotCI = TRUE,
                               cex.axis = 2,
                               cex.legend = 1.5,
                               legend.pos = "topright",
                               lwd = 2,
                               return = TRUE) #lots of warnings
                            

# formula 11 (depth interaction with temperature)
s9.fit11 <- fitHMM(sting9data,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.006,0.03,0.09,0.006,0.03,0.09,
                                   0.05,0.05,0.05)),
                formula = formula11,
                stateNames = stateNames)

s9.fit11
plot(s9.fit11, breaks = 60)
plotPR(s9.fit11) #pseudoresiduals
plotStationary(s9.fit11, plotCI = TRUE)

# formula 12 (cosinor 12 hours + depth)
s9.fit12 <- fitHMM(sting9data,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.006,0.03,0.09,0.006,0.03,0.09,
                                   0.05,0.05,0.05)),
                formula = formula12,
                stateNames = stateNames)
beep(4)
s9.fit12
plot(s9.fit12, breaks = 60)
plotPR(s9.fit12) #pseudoresiduals
plotStationary(s9.fit12, plotCI = TRUE)

# formula 13 (cosinor 24 hours + depth)
s9.fit13 <- fitHMM(sting9data,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.01,0.035,0.095,0.01,0.035,0.095,
                                   0.06,0.06,0.06)),
                formula = formula13,
                stateNames = stateNames)
beep(4)
s9.fit13
plot(s9.fit13, breaks = 60)
plotPR(s9.fit13) #pseudoresiduals
plotStationary(s9.fit13, plotCI = TRUE)

# formula 14 (cosinor 12 hours + temperature)
s9.fit14 <- fitHMM(sting9data,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.006,0.03,0.09,0.006,0.03,0.09,
                                   0.05,0.05,0.05)),
                formula = formula14,
                stateNames = stateNames)

s9.fit14
plot(s9.fit14, breaks = 60)
plotPR(s9.fit14) #pseudoresiduals
plotStationary(s9.fit14, plotCI = TRUE)

# formula 15 (cosinor 24 hours + temperature)
# s9.fit15 <- fitHMM(sting9data,
#                 nbStates=3,
#                 dist=list(ODBA="gamma"),
#                 Par0 = list(ODBA=c(0.009,0.03,0.087,0.009,0.03,0.087,
#                                    0.06,0.06,0.06)),
#                 formula = formula15,
#                 stateNames = stateNames)
# 
# s9.fit15
# plot(s9.fit15, breaks = 60)
# plotPR(s9.fit15) #pseudoresiduals
# plotStationary(s9.fit15, plotCI = TRUE)

# formula 16 (cosinor 12 * Depth)
# s9.fit16 <- fitHMM(sting9data,
#                 nbStates=3,
#                 dist=list(ODBA="gamma"),
#                 Par0 = list(ODBA=c(0.005,0.03,0.16,0.005,0.03,0.16,
#                                    0.07,0.07,0.07)),
#                 formula = formula16,
#                 stateNames = stateNames)
# 
# s9.fit16
# plot(s9.fit16, breaks = 60)
# plotPR(s9.fit16) #pseudoresiduals
# plotStationary(s9.fit16, plotCI = TRUE)

# formula 17 (cosinor 24 * Depth)
s9.fit17 <- fitHMM(sting9data,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.006,0.03,0.09,0.006,0.03,0.09,
                                   0.05,0.05,0.05)),
                formula = formula17,
                stateNames = stateNames)
beep(4)
s9.fit17
plot(s9.fit17,breaks=60)
plotPR(s9.fit17) #pseudoresiduals
plotStationary(s9.fit17, plotCI = TRUE)

# formula 18 (cosinor 12 hours * temperature)
# convergence issues
# s9.fit18 <- fitHMM(sting9data,
#                 nbStates=3,
#                 dist=list(ODBA="gamma"),
#                 Par0 = list(ODBA=c(0.008,0.025,0.09,0.008,0.025,0.009,
#                                    0.06,0.06,0.06)),
#                 formula = formula18,
#                 stateNames = stateNames)
# beep(4)
# s9.fit18
# plot(s9.fit18, breaks = 60)
# plotPR(s9.fit18) #pseudoresiduals
# plotStationary(s9.fit18, plotCI = TRUE)

# formula 19 (cosinor 24 hours * temperature)
# s9.fit19 <- fitHMM(sting9data,
#                 nbStates=3,
#                 dist=list(ODBA="gamma"),
#                 Par0 = list(ODBA=c(0.009,0.03,0.087,0.009,0.03,0.087,
#                                    0.06,0.06,0.06)),
#                 formula = formula19,
#                 stateNames = stateNames)
# 
# s9.fit19
# plot(s9.fit19, breaks = 60)
# plotPR(s9.fit19) #pseudoresiduals
# plotStationary(s9.fit19, plotCI = TRUE)

# formula 20 (hour + depth + temperature)
# s9.fit20 <- fitHMM(sting9data,
#                 nbStates=3,
#                 dist=list(ODBA="gamma"),
#                 Par0 = list(ODBA=c(0.01,0.03,0.075,0.01,0.03,0.075,
#                                    0.06,0.06,0.06)),
#                 formula = formula20,
#                 stateNames = stateNames)
# beep(4)
# s9.fit20
# plot(s9.fit20, breaks = 60)
# plotPR(s9.fit20) #pseudoresiduals
# plotStationary(s9.fit20, plotCI = TRUE)

# formula 21 (hour + interaction between depth & temperature)
s9.fit21 <- fitHMM(sting9data,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.006,0.03,0.09,0.006,0.03,0.09,
                                   0.05,0.05,0.05)),
                formula = formula21,
                stateNames = stateNames)
beep(4)
s9.fit21
plot(s9.fit21, breaks = 60)
plotPR(s9.fit21) #pseudoresiduals
plotStationary(s9.fit21, plotCI = TRUE)

# formula 22 (interaction hour & depth + temperature)
# s9.fit22 <- fitHMM(sting9data, 
#                 nbStates=3, 
#                 dist=list(ODBA="gamma"),
#                 Par0 = list(ODBA=c(0.009,0.03,0.087,0.009,0.03,0.087,
#                                    0.06,0.06,0.06)),
#                 formula = formula22, 
#                 stateNames = stateNames)
# beep(4)
# s9.fit22
# plot(s9.fit22, breaks = 60) 
# plotPR(s9.fit22) #pseudoresiduals
# plotStationary(s9.fit22, plotCI = TRUE)

# formula 23 (interaction hour & temperature + depth)
s9.fit23 <- fitHMM(sting9data,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.01,0.042,0.11,0.01,0.042,0.11,
                                   0.07,0.07,0.07)),
                formula = formula23,
                stateNames = stateNames)
beep(4)
s9.fit23
plot(s9.fit23, breaks = 60)
plotPR(s9.fit23) #pseudoresiduals
plotStationary(s9.fit23, plotCI = TRUE)

# formula 24 (3 way interaction)
# s9.fit24 <- fitHMM(sting9data,
#                 nbStates=3,
#                 dist=list(ODBA="gamma"),
#                 Par0 = list(ODBA=c(0.01,0.028,0.11,0.01,0.028,0.11,
#                                    0.07,0.07,0.07)),
#                 formula = formula24,
#                 stateNames = stateNames)
# 
# s9.fit24
# plot(s9.fit24, breaks = 60)
# plotPR(s9.fit24) #pseudoresiduals
# plotStationary(s9.fit24, plotCI = TRUE)

# formula 25 (cosinor 12 + depth + temp)
s9.fit25 <- fitHMM(sting9data,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.006,0.03,0.09,0.006,0.03,0.09,
                                   0.05,0.05,0.05)),
                formula = formula25,
                stateNames = stateNames)

s9.fit25
plot(s9.fit25, breaks = 60)
plotPR(s9.fit25) #pseudoresiduals
plotStationary(s9.fit25, plotCI = TRUE)

# formula 26 (cosinor 24 + depth + temp)
s9.fit26 <- fitHMM(sting9data,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.01,0.042,0.11,0.01,0.042,0.11,
                                   0.07,0.07,0.07)),
                formula = formula26,
                stateNames = stateNames)
beep(4)
s9.fit26
plot(s9.fit26, breaks = 60)
plotPR(s9.fit26) #pseudoresiduals
plotStationary(s9.fit26, plotCI = TRUE)

# formula 27 (cosinor 12 + depth * temp)
s9.fit27 <- fitHMM(sting9data,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.006,0.03,0.09,0.006,0.03,0.09,
                                   0.05,0.05,0.05)),
                formula = formula27,
                stateNames = stateNames)

s9.fit27
plot(s9.fit27, breaks = 60)
plotPR(s9.fit27) #pseudoresiduals
plotStationary(s9.fit27, plotCI = TRUE)

# formula 28 (cosinor 24 + depth * temp)
s9.fit28 <- fitHMM(sting9data,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.006,0.03,0.09,0.006,0.03,0.09,
                                   0.05,0.05,0.05)),
                formula = formula28,
                stateNames = stateNames)

s9.fit28
plot(s9.fit28, breaks = 60)
plotPR(s9.fit28) #pseudoresiduals
plotStationary(s9.fit28, plotCI = TRUE)

# formula 29 (cosinor 12 + depth + temp)
s9.fit29 <- fitHMM(sting9data,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.01,0.035,0.095,0.01,0.035,0.095,
                                   0.06,0.06,0.06)),
                formula = formula29,
                stateNames = stateNames)

beep(4)
s9.fit29
plot(s9.fit29, breaks = 60)
plotPR(s9.fit29) #pseudoresiduals
plotStationary(s9.fit29, plotCI = TRUE)

# formula 30 (cosinor 24 * depth + temperature)
s9.fit30 <- fitHMM(sting9data,
                  nbStates=3,
                  dist=list(ODBA="gamma"),
                  Par0 = list(ODBA=c(0.01,0.035,0.095,0.01,0.035,0.095,
                                     0.06,0.06,0.06)),
                  formula = formula30,
                  stateNames = stateNames)

s9.fit30
plot(s9.fit30, breaks = 60) 
plotPR(s9.fit30) #pseudoresiduals
plotStationary(s9.fit30, plotCI = TRUE)

# formula 31 (cosinor 12 * temperature + depth)
# s9.fit31 <- fitHMM(sting9data,
#                 nbStates=3,
#                 dist=list(ODBA="gamma"),
#                 Par0 = list(ODBA=c(0.008,0.027,0.11,0.008,0.027,0.11,
#                                    0.06,0.06,0.06)),
#                 formula = formula31,
#                 stateNames = stateNames)
# s9.fit31
# plot(s9.fit31, breaks = 60)
# plotPR(s9.fit31) #pseudoresiduals
# plotStationary(s9.fit31, plotCI = TRUE)

# formula 32 (cosinor 24 * temperature + depth)
# s9.fit32 <- fitHMM(sting9data,
#                 nbStates=3,
#                 dist=list(ODBA="gamma"),
#                 Par0 = list(ODBA=c(0.009,0.036,0.12,0.009,0.036,0.12,
#                                    0.05,0.06,0.08)),
#                 formula = formula32,
#                 stateNames = stateNames)
# beep(4)
# s9.fit32
# plot(s9.fit32, breaks = 60) 
# plotPR(s9.fit32) #pseudoresiduals
# plotStationary(s9.fit32, plotCI = TRUE)

# formula 33 (cosinor 12 * depth * temperature)
s9.fit33 <- fitHMM(sting9data,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.006,0.03,0.09,0.006,0.03,0.09,
                                   0.05,0.05,0.05)),
                formula = formula33,
                stateNames = stateNames)

s9.fit33
plot(s9.fit33, breaks = 60)
plotPR(s9.fit33) #pseudoresiduals
plotStationary(s9.fit33, plotCI = TRUE)

# formula 34 (cosinor 24 * depth * temperature)
# convergence issues
# s9.fit34 <- fitHMM(sting9data,
#                 nbStates=3,
#                 dist=list(ODBA="gamma"),
#                 Par0 = list(ODBA=c(0.007,0.036,0.09,0.007,0.036,0.09,
#                                    0.06,0.06,0.06)),
#                 formula = formula34,
#                 stateNames = stateNames)
# beep(4)
# s9.fit34
# plot(s9.fit34, breaks = 60)
# plotPR(s9.fit34) #pseudoresiduals
# plotStationary(s9.fit34, plotCI = TRUE)

##### Model selection #####

# Function for BIC 
BIC.HMM<-function(hmmModel) {
  maxLogLike <- -hmmModel$mod$minimum
  nPar <- length(hmmModel$mod$wpar)
  nData <- nrow(hmmModel$data)
  aic <- -2 * maxLogLike + 2 * nPar
  bic <- -2 * maxLogLike + nPar * log(nData)
  c(aic=aic,bic=bic,npar=nPar,maxLogLik=maxLogLike,nData=nData)
}

# List of model fits that ran
fit_list <- list(
  s9.fit1, s9.fit2, s9.fit3, s9.fit4, 
  s9.fit10, s9.fit11, s9.fit12, s9.fit13, s9.fit14,
  s9.fit17, 
  s9.fit21, s9.fit23, 
  s9.fit25, s9.fit26, s9.fit27, s9.fit28, s9.fit29, s9.fit30,
  s9.fit33 
)

# Initialize BICtab with appropriate model numbers
model_numbers <- c(1, 2, 3, 4, 10, 11, 12, 13, 14, 17, 21, 23, 25, 26, 27, 28, 29, 30, 33)
BICtab <- data.frame(Model=model_numbers, AIC=NA, BIC=NA,npar=NA,LogLike=NA,nData=NA)

# Iterate through each fit and populate BICtab
for (i in 1:length(fit_list)) {
  BICtab[i, 2:6] <- BIC.HMM(fit_list[[i]])
}

# Add delta calculations
BICtab$deltaAIC <- BICtab$AIC - min(BICtab$AIC)
BICtab$deltaBIC <- BICtab$BIC - min(BICtab$BIC)

# View the table
print(BICtab)

# Write the table to a CSV file
write.table(BICtab, file="sting9BIC.csv", sep=",", row.names=FALSE, quote=FALSE)

