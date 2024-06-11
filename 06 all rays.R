# HIDDEN MARKOV MODELS #

# Authored by Kathryn Flowers & Elizabeth Babcock
# With help from Ron Togunov & Natasha Klappstein
# Last updated 29 May 2024

##### Load libraries & read data #####

library(dplyr) #organize dataframes
library(lubridate) #format dates & times
library(ggplot2) #data vis
library(grid) #stack ggplots
library(momentuHMM) #fit HMMs
library(beepr) #sound alerts
library(scales) #for visualization
library(tidyverse)
library(gridExtra) #multipanel plots

# Data are averaged over 1 second
allrays<- read.csv("allrays.csv")
str(allrays$Date_Time) #not as.Posixct

# any NAs
anyNA(allrays) #no

# update "Date_Time" into time variable
allrays$Date_Time <- as.POSIXct(allrays$Date_Time,format="%Y-%m-%d %H:%M:%S",
                               tz="America/Belize")

# any NAs
anyNA(allrays) #yes, why does as.POSIXct do this only recently...
# where
na_positions <- which(is.na(allrays$Date_Time))
print(na_positions)
# get more information
extract_rows<-c(6420,6421,6422, 
                92820,92821, 92822, 
                179220,179221,179222,
                275667,275668,275669,
                362067,362068, 362069,
                438582,438583,438584,
                524982,524983,524984,
                627487,627488,627489,
                713887,713888,713889,
                816542,816543,816544,
                902942,902943,902944,
                990237,990238,990239,
                1076637, 1076638, 1076639,
                1172135,1172136, 1172137,
                1258535,1258536,1258537,
                1354243,1354244, 1354245,
                1440643,1440644, 1440645)
selected<-allrays[extract_rows, ]
print(selected) # midnight the problem again 
# update ORIGINAL data, so upload again, this is the only way it works
allrays <- read.csv("allrays.csv")
# add date_time back in
allrays$Date_Time[6421] <- "2017-07-09 00:00:00"
allrays$Date_Time[92821] <- "2017-07-10 00:00:00"
allrays$Date_Time[179221] <- "2017-07-11 00:00:00"
allrays$Date_Time[275668] <- "2017-07-10 00:00:00"
allrays$Date_Time[362068] <- "2017-07-11 00:00:00"
allrays$Date_Time[438583] <- "2019-06-09 00:00:00"
allrays$Date_Time[524983] <- "2019-06-10 00:00:00"
allrays$Date_Time[627488] <- "2019-06-10 00:00:00"
allrays$Date_Time[713888] <- "2019-06-11 00:00:00"
allrays$Date_Time[816543] <- "2019-06-11 00:00:00"
allrays$Date_Time[902943] <- "2019-06-12 00:00:00"
allrays$Date_Time[990238] <- "2019-06-12 00:00:00"
allrays$Date_Time[1076638] <- "2019-06-13 00:00:00"
allrays$Date_Time[1172136] <- "2019-06-13 00:00:00"
allrays$Date_Time[1258536] <- "2019-06-14 00:00:00"
allrays$Date_Time[1354244] <- "2019-06-15 00:00:00"
allrays$Date_Time[1440644] <- "2019-06-16 00:00:00"
# check updates
extract_rows<-c(6420,6421,6422, 
                92820,92821, 92822, 
                179220,179221,179222,
                275667,275668,275669,
                362067,362068, 362069,
                438582,438583,438584,
                524982,524983,524984,
                627487,627488,627489,
                713887,713888,713889,
                816542,816543,816544,
                902942,902943,902944,
                990237,990238,990239,
                1076637, 1076638, 1076639,
                1172135,1172136, 1172137,
                1258535,1258536,1258537,
                1354243,1354244, 1354245,
                1440643,1440644, 1440645)
selected<-allrays[extract_rows, ]
print(selected) # done
anyNA(allrays) # false

# Prep data -------------------------------------------------------------------

# Look for any zeros in ODBA column b/c range of gamma distribution is > 0
colSums(allrays==0) #allgood

# Prep data for momentuHMM
allraysdata <- prepData(allrays,
                       coordNames = NULL,
                       covNames = c("Depth", "Temp", "hour"))

# Round tiny numbers to zero
allraysdata$ODBA<-round(allraysdata$ODBA,8)

# HMMs -------------------------------------------------------------------------

# What are we looking for? Probability of stingrays being in state of low, 
## medium, or high activity (i.e., 3 states) based on: time of day, water
### temperature, and depth. 

# Par0 = starting values for step length parameters: mean & sd for each state
## from histogram of ODBA & initial values for zero inflation
### More on selection in Michelot & Langrock 2022:
#### https://cran.r-project.org/web/packages/moveHMM/vignettes/moveHMM-starting-values.pdf 


# Histogram of ODBA to visualize state distributions

hist(allraysdata$ODBA[allraysdata$ODBA < 0.3],breaks=200,main="Histogram of ODBA", 
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
fit1 <- fitHMM(allraysdata, 
                nbStates=3, 
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.008,0.041,0.13,0.008,0.041,0.13, 
                                   0.1,0.1,0.1)),
                formula = formula1, 
                stateNames = stateNames)
fit1
plot(fit1, breaks = 60) 
plotPR(fit1) #pseudoresiduals
plotStationary(fit1, plotCI = TRUE)

# formula 2 (cosinor 12 hours)
fit2 <- fitHMM(allraysdata,
                   nbStates=3,
                   dist=list(ODBA="gamma"),
                   Par0 = list(ODBA=c(0.008,0.041,0.13,0.008,0.041,0.13, 
                                      0.1,0.1,0.1)),
                   formula = formula2,
                   stateNames = stateNames)
beep(4)
fit2
plot(fit2, breaks = 60)
plotPR(fit2) #pseudoresiduals
plotStationary(fit2, plotCI = TRUE)

# formula 3 (cosinor 24 hours)
fit3 <- fitHMM(allraysdata,
                  nbStates=3,
                  dist=list(ODBA="gamma"),
                  Par0 = list(ODBA=c(0.008,0.041,0.13,0.008,0.041,0.13, 
                                     0.1,0.1,0.1)),
                  formula = formula3,
                  stateNames = stateNames)
beep(4)
fit3
plot(fit3, breaks = 60)
plotPR(fit3) #pseudoresiduals
plotStationary(fit3, plotCI = TRUE)

# formula 4 (depth)
fit4 <- fitHMM(allraysdata,
                  nbStates=3,
                  dist=list(ODBA="gamma"),
                  Par0 = list(ODBA=c(0.008,0.041,0.13,0.008,0.041,0.13, 
                                     0.1,0.1,0.1)),
                  formula = formula4,
                  stateNames = stateNames)
fit4
plot(fit4, breaks = 60) 
plotPR(fit4) #pseudoresiduals
plotStationary(fit4, plotCI = TRUE)

# formula 5 (temperature)
# Convergence issues - although better w/0.1 for zero 
# fit5 <- fitHMM(allraysdata,
#                nbStates=3,
#                dist=list(ODBA="gamma"),
#                Par0 = list(ODBA=c(0.008,0.041,0.13,0.008,0.041,0.13, 
#                                   0.05,0.05,0.05)),
#                formula = formula5,
#                stateNames = stateNames)
# beep(4)
# fit5
# plot(fit5, breaks = 60)
# plotPR(fit5) #pseudoresiduals
# plotStationary(fit5, plotCI = TRUE)
# fit5$CIbeta #no cosinor but still NAs
# fit5$CIreal 

# formula 6 (hour + Depth)
fit6 <- fitHMM(allraysdata,
               nbStates=3,
               dist=list(ODBA="gamma"),
               Par0 = list(ODBA=c(0.008,0.041,0.13,0.008,0.041,0.13, 
                                  0.1,0.1,0.1)),
               formula = formula6,
               stateNames = stateNames)
beep(4)
fit6
plot(fit6, breaks = 60)
plotPR(fit6) #pseudoresiduals
plotStationary(fit6, plotCI = TRUE)

# formula 7 (hour + temperature)
fit7 <- fitHMM(allraysdata,
               nbStates=3,
               dist=list(ODBA="gamma"),
               Par0 = list(ODBA=c(0.008,0.041,0.13,0.008,0.041,0.13, 
                                  0.1,0.1,0.1)),
               formula = formula7,
               stateNames = stateNames)
beep(4)
fit7
plot(fit7, breaks = 60)
plotPR(fit7) #pseudoresiduals
plotStationary(fit7, plotCI = TRUE)

# formula 8 (depth + temperature)
# Convergence issues but 0.1 better for zero mass
# fit8 <- fitHMM(allraysdata,
#                nbStates=3,
#                dist=list(ODBA="gamma"),
#                Par0 = list(ODBA=c(0.008,0.041,0.13,0.008,0.041,0.13, 
#                                   0.05,0.05,0.05)),
#                formula = formula8,
#                stateNames = stateNames)
# beep(4)
# fit8
# plot(fit8, breaks = 60)
# plotPR(fit8) #pseudoresiduals
# plotStationary(fit8, plotCI = TRUE)

# formula 9 (hour interaction with depth)
fit9 <- fitHMM(allraysdata,
               nbStates=3,
               dist=list(ODBA="gamma"),
               Par0 = list(ODBA=c(0.008,0.041,0.13,0.008,0.041,0.13, 
                                  0.1,0.1,0.1)),
               formula = formula9,
               stateNames = stateNames)
beep(4)
fit9
plot(fit9, breaks = 60)
plotPR(fit9) #pseudoresiduals
plotStationary(fit9, plotCI = TRUE)

# formula 10 (hour interaction with temperature)
fit10 <- fitHMM(allraysdata, 
                nbStates=3, 
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.008,0.041,0.13,0.008,0.041,0.13, 
                                   0.1,0.1,0.1)),
                formula = formula10, 
                stateNames = stateNames)

fit10
plot(fit10, breaks = 60) 
plotPR(fit10) #pseudoresiduals
plotStationary(fit10, plotCI = TRUE)

# formula 11 (depth interaction with temperature)
fit11 <- fitHMM(allraysdata,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.008,0.041,0.13,0.008,0.041,0.13, 
                                   0.1,0.1,0.1)),
                formula = formula11,
                stateNames = stateNames)

fit11
plot(fit11, breaks = 60)
plotPR(fit11) #pseudoresiduals
plotStationary(fit11, plotCI = TRUE)

# formula 12 (cosinor 12 hours + depth)
fit12 <- fitHMM(allraysdata,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.008,0.041,0.13,0.008,0.041,0.13, 
                                   0.1,0.1,0.1)),
                formula = formula12,
                stateNames = stateNames)
beep(4)
fit12
plot(fit12, breaks = 60)
plotPR(fit12) #pseudoresiduals
plotStationary(fit12, plotCI = TRUE)

# formula 13 (cosinor 24 hours + depth)
fit13 <- fitHMM(allraysdata,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.008,0.041,0.13,0.008,0.041,0.13, 
                                   0.1,0.1,0.1)),
                formula = formula13,
                stateNames = stateNames)
beep(4)
fit13
plot(fit13, breaks = 60)
plotPR(fit13) #pseudoresiduals
plotStationary(fit13, plotCI = TRUE)

# formula 14 (cosinor 12 hours + temperature)
# Convergence issues, 0.1 better starting Par0
# fit14 <- fitHMM(allraysdata,
#                 nbStates=3,
#                 dist=list(ODBA="gamma"),
#                 Par0 = list(ODBA=c(0.008,0.041,0.13,0.008,0.041,0.13, 
#                                    0.05,0.05,0.05)),
#                 formula = formula14,
#                 stateNames = stateNames)
# 
# fit14
# plot(fit14, breaks = 60)
# plotPR(fit14) #pseudoresiduals
# plotStationary(fit14, plotCI = TRUE)

# formula 15 (cosinor 24 hours + temperature)
fit15 <- fitHMM(allraysdata,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.008,0.041,0.13,0.008,0.041,0.13, 
                                   0.05,0.05,0.05)),
                formula = formula15,
                stateNames = stateNames)

fit15
plot(fit15, breaks = 60)
plotPR(fit15) #pseudoresiduals
plotStationary(fit15, plotCI = TRUE)

# formula 16 (cosinor 12 * Depth)
fit16 <- fitHMM(allraysdata,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.008,0.041,0.13,0.008,0.041,0.13, 
                                   0.1,0.1,0.1)),
                formula = formula16,
                stateNames = stateNames)

fit16
plot(fit16, breaks = 60)
plotPR(fit16) #pseudoresiduals
plotStationary(fit16, plotCI = TRUE)

##### BEST BIC #####
# formula 17 (cosinor 24 * Depth)
fit17 <- fitHMM(allraysdata,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.008,0.041,0.13,0.008,0.041,0.13, 
                                   0.1,0.1,0.1)),
                formula = formula17,
                stateNames = stateNames)
beep(4)
fit17
plot(fit17, 
     breaks = 60,
     plotCI = T,
     cex.axis = 2,
     cex.legend = 2,
     lwd = 2)
plotPR(fit17) #pseudoresiduals
state.prob<- plotStationary(fit17, plotCI = TRUE, cex.axis = 2,
                            cex.legend = 1.5,
                            legend.pos = "topright",
                            lwd = 2,
                            return = TRUE)
                            
# parameters & regression coeffs for trans probs
print(fit17) 

#Still NAs here (states 1 to 3 & 3 to 1)
fit17$CIbeta
fit17$CIreal

# update plots

hourVals<-bind_rows(state.prob$hour,.id="Activity") %>%
  rename(hour=cov)
hourVals$Activity <-factor(hourVals$Activity, levels = c("Low activity",
                                                         "Medium activity",
                                                         "High activity"))

hour<- ggplot(hourVals,
              aes(x=hour,y=est,ymax=uci,ymin=lci,color=Activity,
                  fill=Activity, linetype=Activity)) +
  geom_line(linewidth=0.9)+
  geom_ribbon(alpha=0.55, linewidth=0.9) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), limits= c(0,0.75), breaks = seq(0.2,0.75, by=0.2)) +
  xlab("\nHour of day") +
  ylab("Stationary state probabilities \n") +
  theme(text=element_text(size = 20)) +
  theme(axis.text.x = element_text(vjust=0.5)) +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  theme(legend.position = "inside", legend.justification.inside = "top") +
  scale_color_manual(values=alpha(c('#6792DA', '#ECCC68','#DA5E5E'))) +
  scale_fill_manual(values=c('#6792DA', '#ECCC68','#DA5E5E')) +
  scale_linetype_manual(values=c("dotted","longdash","solid")) +
  guides(color = guide_legend(title = NULL), 
         fill = guide_legend(title = NULL),
         linetype = guide_legend(title = NULL))

depthVals<-bind_rows(state.prob$Depth,.id="Activity") %>%
  rename(Depth=cov)
depthVals$Activity <-factor(depthVals$Activity, levels = c("Low activity",
                                                           "Medium activity",
                                                           "High activity"))

depth<- ggplot(depthVals,
               aes(x=Depth,y=est,ymax=uci,ymin=lci,color=Activity,
                   fill=Activity, linetype=Activity)) +
  geom_line(linewidth=0.9)+
  geom_ribbon(alpha=0.55, linewidth=0.9) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), limits= c(0,0.75), breaks = seq(0.2,0.75, by=0.2)) +
  xlab("\nDepth (m)") +
  ylab("Stationary state probabilities \n") +
  theme(text=element_text(size = 20)) +
  theme(axis.text.x = element_text(vjust=0.5)) +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  theme(legend.position.inside = c(0.32, 0.94)) +
  scale_color_manual(values=alpha(c('#6792DA', '#ECCC68','#DA5E5E'))) +
  scale_fill_manual(values=c('#6792DA', '#ECCC68','#DA5E5E')) +
  scale_linetype_manual(values=c("dotted","longdash","solid")) +
  guides(color = guide_legend(title = NULL), 
         fill = guide_legend(title = NULL),
         linetype = guide_legend(title = NULL)) 

# Multipanel plot
final.all <- grid.arrange(arrangeGrob(hour),
                          arrangeGrob(depth + theme(legend.position = "none",
                                                    axis.title.y = element_blank())),
                          ncol = 2) # Set the number of columns in the layout


# decode most likely state sequence
states <-viterbi(fit17)

# % of time in each state
table(states)/nrow(allraysdata)

# Add states to time series data
allrays$States <- states
allrays$States <- as.factor(allrays$States)
allrays$Date_Time <- as.POSIXct(allrays$Date_Time, tz="America/Belize")

# formula 18 (cosinor 12 hours * temperature)
fit18 <- fitHMM(allraysdata,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.008,0.041,0.13,0.008,0.041,0.13, 
                                   0.05,0.05,0.05)),
                formula = formula18,
                stateNames = stateNames)
beep(4)
fit18
plot(fit18, breaks = 60)
plotPR(fit18) #pseudoresiduals
plotStationary(fit18, plotCI = TRUE)

# formula 19 (cosinor 24 hours * temperature)
fit19 <- fitHMM(allraysdata,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.008,0.041,0.13,0.008,0.041,0.13, 
                                   0.05,0.05,0.05)),
                formula = formula19,
                stateNames = stateNames)

fit19
plot(fit19, breaks = 60)
plotPR(fit19) #pseudoresiduals
plotStationary(fit19, plotCI = TRUE)

# formula 20 (hour + depth + temperature)
fit20 <- fitHMM(allraysdata,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.008,0.041,0.13,0.008,0.041,0.13, 
                                   0.06,0.06,0.06)),
                formula = formula20,
                stateNames = stateNames)

fit20
plot(fit20, breaks = 60)
plotPR(fit20) #pseudoresiduals
plotStationary(fit20, plotCI = TRUE)

# formula 21 (hour + interaction between depth & temperature)
fit21 <- fitHMM(allraysdata,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.008,0.041,0.13,0.008,0.041,0.13, 
                                   0.1,0.1,0.1)),
                formula = formula21,
                stateNames = stateNames)
beep(4)
fit21
plot(fit21, breaks = 60)
plotPR(fit21) #pseudoresiduals
plotStationary(fit21, plotCI = TRUE)

# formula 22 (interaction hour & depth + temperature)
fit22 <- fitHMM(allraysdata, 
                nbStates=3, 
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.008,0.041,0.13,0.008,0.041,0.13, 
                                   0.1,0.1,0.1)),
                formula = formula22, 
                stateNames = stateNames)
beep(4)
fit22
plot(fit22, breaks = 60) 
plotPR(fit22) #pseudoresiduals
plotStationary(fit22, plotCI = TRUE)

# formula 23 (interaction hour & temperature + depth)
fit23 <- fitHMM(allraysdata,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.008,0.041,0.13,0.008,0.041,0.13, 
                                   0.1,0.1,0.1)),
                formula = formula23,
                stateNames = stateNames)
beep(4)
fit23
plot(fit23, breaks = 60)
plotPR(fit23) #pseudoresiduals
plotStationary(fit23, plotCI = TRUE)

# formula 24 (3 way interaction)
# Convergence issues - NAs in SEs
# fit24 <- fitHMM(allraysdata,
#                 nbStates=3,
#                 dist=list(ODBA="gamma"),
#                 Par0 = list(ODBA=c(0.008,0.041,0.13,0.008,0.041,0.13, 
#                                    0.05,0.05,0.05)),
#                 formula = formula24,
#                 stateNames = stateNames)
# 
# fit24
# plot(fit24, breaks = 60)
# plotPR(fit24) #pseudoresiduals
# plotStationary(fit24, plotCI = TRUE) #NAs in SE?

# formula 25 (cosinor 12 + depth + temp)
fit25 <- fitHMM(allraysdata,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.008,0.041,0.13,0.008,0.041,0.13, 
                                   0.1,0.1,0.1)),
                formula = formula25,
                stateNames = stateNames)

fit25
plot(fit25, breaks = 60)
plotPR(fit25) #pseudoresiduals
plotStationary(fit25, plotCI = TRUE)

# formula 26 (cosinor 24 + depth + temp)
fit26 <- fitHMM(allraysdata,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.006,0.041,0.11,0.006,0.041,0.11, 
                                   0.05,0.05,0.05)),
                formula = formula26,
                stateNames = stateNames)
beep(4)
fit26
plot(fit26, breaks = 60)
plotPR(fit26) #pseudoresiduals
plotStationary(fit26, plotCI = TRUE)

# formula 27 (cosinor 12 + depth * temp)
fit27 <- fitHMM(allraysdata,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.008,0.041,0.13,0.008,0.041,0.13, 
                                   0.1,0.1,0.1)),
                formula = formula27,
                stateNames = stateNames)

fit27
plot(fit27, breaks = 60)
plotPR(fit27) #pseudoresiduals
plotStationary(fit27, plotCI = TRUE)

# formula 28 (cosinor 24 + depth * temp)
fit28 <- fitHMM(allraysdata,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.008,0.041,0.11,0.008,0.041,0.11, 
                                   0.05,0.05,0.05)),
                formula = formula28,
                stateNames = stateNames)

fit28
plot(fit28, breaks = 60)
plotPR(fit28) #pseudoresiduals
plotStationary(fit28, plotCI = TRUE)

# formula 29 (cosinor 12 + depth + temp)
fit29 <- fitHMM(allraysdata,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.008,0.041,0.13,0.008,0.041,0.13, 
                                   0.1,0.1,0.1)),
                formula = formula29,
                stateNames = stateNames)

beep(4)
fit29
plot(fit29, breaks = 60)
plotPR(fit29) #pseudoresiduals
plotStationary(fit29, plotCI = TRUE)

# formula 30 (cosinor 24 * depth + temperature)
fit30 <- fitHMM(allraysdata,
                  nbStates=3,
                  dist=list(ODBA="gamma"),
                  Par0 = list(ODBA=c(0.008,0.041,0.13,0.008,0.041,0.13, 
                                     0.1,0.1,0.1)),
                  formula = formula30,
                  stateNames = stateNames)

fit30
plot(fit30, breaks = 60) 
plotPR(fit30) #pseudoresiduals
plotStationary(fit30, plotCI = TRUE)

# formula 31 (cosinor 12 * temperature + depth)
# Convergence issues
# fit31 <- fitHMM(allraysdata,
#                 nbStates=3,
#                 dist=list(ODBA="gamma"),
#                 Par0 = list(ODBA=c(0.004,0.045,0.12,0.004,0.045,0.12, 
#                                    0.05,0.05,0.05)),
#                 formula = formula31,
#                 stateNames = stateNames)
# fit31
# plot(fit31, breaks = 60)
# plotPR(fit31) #pseudoresiduals
# plotStationary(fit31, plotCI = TRUE)

# formula 32 (cosinor 24 * temperature + depth)
# Convergence issues
# fit32 <- fitHMM(allraysdata,
#                 nbStates=3,
#                 dist=list(ODBA="gamma"),
#                 Par0 = list(ODBA=c(0.008,0.041,0.13,0.008,0.041,0.13, 
#                                    0.009,0.05,0.1)),
#                 formula = formula32,
#                 stateNames = stateNames)
# 
# fit32
# plot(fit32, breaks = 60) # error w/ODBA parameter bounds
# plotPR(fit32) #pseudoresiduals
# plotStationary(fit32, plotCI = TRUE)

# formula 33 (cosinor 12 * depth * temperature)
# fit33 <- fitHMM(allraysdata,
#                 nbStates=3,
#                 dist=list(ODBA="gamma"),
#                 Par0 = list(ODBA=c(0.008,0.041,0.13,0.008,0.041,0.13, 
#                                    0.009,0.06,0.1)),
#                 formula = formula33,
#                 stateNames = stateNames)
# 
# fit33
# plot(fit33, breaks = 60)
# plotPR(fit33) #pseudoresiduals
# plotStationary(fit33, plotCI = TRUE) #convergence issues

# formula 34 (cosinor 24 * depth * temperature)
fit34 <- fitHMM(allraysdata,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.008,0.041,0.13,0.008,0.041,0.13, 
                                   0.009,0.06,0.1)),
                formula = formula34,
                stateNames = stateNames)
beep(4)
fit34
plot(fit34, breaks = 60, plotCI = T, cex.axis = 2, cex.legend = 2, lwd = 2)
plotPR(fit34) #pseudoresiduals
plotStationary(fit34, plotCI = T)

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

# list of fits used
fit_list <- list(
  fit1, fit2, fit3, fit4, # fit5 is skipped
  fit6, fit7, # fit8 is skipped
  fit9, fit10, fit11, fit12, fit13, # fit14 is skipped
  fit15, fit16, fit17, fit18, fit19, fit20, fit21, fit22, fit23, # fit24 has convergence issues
  fit25, fit26, fit27, fit28, fit29, fit30, # fit31, fit32, fit 33 have convergence issues
  fit34
)

# Initialize BICtab with appropriate model numbers
model_numbers <- c(1, 2, 3, 4, 6, 7, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 25, 26, 27, 28, 29, 30, 34)
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
write.table(BICtab, file="allraysBIC.csv", sep=",", row.names=FALSE, quote=FALSE)

