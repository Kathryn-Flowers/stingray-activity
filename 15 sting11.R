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

# 2019 Glover's Data
sting11 <- read.csv("sting11.csv")

##### Stingray 11 #####

str(sting11$Date_Time) #not as.Posixct

# update "Date_Time" into time variable
sting11$Date_Time <- as.POSIXct(sting11$Date_Time,format="%Y-%m-%d %H:%M:%S",
                               tz="America/Belize")

# any NAs
anyNA(sting11) #none

# Data visualization ---------------------------------------------------------

# Colors are colorblind friendly, you can check HEX codes here:
## https://davidmathlogic.com/colorblind/

gg_odba <- ggplot(sting11, aes(x = Date_Time, y = ODBA)) + 
  geom_line() + 
  ylab("ODBA") + 
  theme_classic() +
  theme(text=element_text(size = 15)) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())

gg_depth <- ggplot(sting11, aes(x = Date_Time, y = Depth)) + 
  geom_line(color="#2A597D") +
  scale_y_reverse() +
  ylab("Depth (m)") + 
  theme_classic() + 
  theme(text=element_text(size = 15)) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) 

ylab <- expression("Temperature " ( degree~C))

gg_temp <- ggplot(sting11, aes(x = Date_Time, y = Temp)) +
  geom_line(color="#CC501B") +
  labs(y=ylab, x="Date & Time") +
  theme_classic() +
  theme(text=element_text(size = 15))

grid.draw(rbind(ggplotGrob(gg_odba), 
                ggplotGrob(gg_depth),
                ggplotGrob(gg_temp))) # spike in temp & ODBA - this is the tag that popped off early

# Prep data -------------------------------------------------------------------

# Look for any zeros in ODBA column b/c range of gamma distribution is > 0
colSums(sting11==0) #none

# Obtain hour for cosinor 
sting11$hour <- as.integer(strftime(sting11$Date_Time, format = "%H", 
                                       tz="America/Belize"))

anyNA(sting11$hour)

# Prep data for momentuHMM
sting11data <- prepData(sting11,
                       coordNames = NULL,
                       covNames = c("Depth", "Temp", "hour"))

# Round tiny numbers to zero
sting11data$ODBA<-round(sting11data$ODBA,8)

# HMMs -------------------------------------------------------------------------

# What are we looking for? Probability of stingrays being in state of low, 
## medium, or high activity (i.e., 3 states) based on: time of day, water
### temperature, and depth. 

# Par0 = starting values for step length parameters: mean & sd for each state
## from histogram of ODBA & initial values for zero inflation
### More on selection in Michelot & Langrock 2022:
#### https://cran.r-project.org/web/packages/moveHMM/vignettes/moveHMM-starting-values.pdf 


# Histogram of ODBA to visualize state distributions

hist(sting11data$ODBA[sting11data$ODBA < 0.3],breaks=150,main="Histogram of ODBA", 
     xlab = "ODBA") #look for distributions

# Model set up - no cosinor models due to shorter sampling period 
formula1 = ~hour
formula2 = ~Depth
formula3 = ~Temp
formula4 = ~hour + Depth
formula5 = ~hour + Temp
formula6 = ~Depth + Temp
formula7 = ~hour * Depth
formula8 = ~hour * Temp
formula9 = ~Depth * Temp
formula10 = ~hour + Depth + Temp
formula11 = ~hour + Depth * Temp
formula12 = ~hour * Depth + Temp
formula13 = ~hour * Temp + Depth
formula14 = ~hour * Temp * Depth

stateNames<- c("Low activity", "Medium activity", "High activity")

# 3 state models ---------------------------------------------------------------

##### BEST BIC #####
# formula 1 (hour only)
s11.fit1 <- fitHMM(sting11data, 
                nbStates=3, 
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.007,0.025,0.1,0.007,0.025,0.1,
                                   0.1,0.1,0.1)),
                formula = formula1, 
                stateNames = stateNames)
beep(4)
s11.fit1
plot(s11.fit1, breaks = 60,plotCI = T, cex.axis = 2, cex.legend = 2,
     lwd = 2) #warnings
plotPR(s11.fit1) #pseudoresiduals
s11.state.prob<-plotStationary(s11.fit1, plotCI = TRUE,
                               cex.axis = 2,
                               cex.legend = 1.5,
                               legend.pos = "topright",
                               lwd = 2,
                               return = TRUE) 

#Still NAs here (zero mass)
s11.fit1$CIbeta 
s11.fit1$CIreal 

# update plots

hourVals<-bind_rows(s11.state.prob$hour,.id="Activity") %>%
  rename(hour=cov)
hourVals$Activity <-factor(hourVals$Activity, levels = c("Low activity",
                                                         "Medium activity",
                                                         "High activity"))

s11.hour<- ggplot(hourVals,
                  aes(x=hour,y=est,ymax=uci,ymin=lci,color=Activity,
                      fill=Activity, linetype=Activity)) +
  geom_line(linewidth=0.9)+
  geom_ribbon(alpha=0.55, linewidth=0.9) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  xlab("\nHour of day") +
  ylab("Stationary state probabilities \n") +
  theme(text=element_text(size = 20)) +
  theme(axis.text.x = element_text(vjust=0.5)) +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  theme(legend.position = c(0.56, 0.94))
s11.hour + scale_color_manual(values=alpha(c('#6792DA', '#ECCC68','#DA5E5E'))) +
  scale_fill_manual(values=c('#6792DA', '#ECCC68','#DA5E5E')) +
  scale_linetype_manual(values=c("dotted","longdash","solid")) +
  guides(color = guide_legend(title = NULL), 
         fill = guide_legend(title = NULL),
         linetype = guide_legend(title = NULL))

# decode most likely state sequence
s11.states <-viterbi(s11.fit1)

# % of time in each state
table(s11.states)/nrow(sting11data)

# Add states to time series data
sting11$States <- s11.states
sting11$States <- as.factor(sting11$States)
sting11$Date_Time <- as.POSIXct(sting11$Date_Time, tz="America/Belize")

# Plot states over covariates
ggStatesODBA<- ggplot(sting11) + 
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
  ggtitle("Stingray 11") +
  theme(legend.position="top") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(labels = date_format("%H:%M\n%d %b", tz= attr(sting11$Date_Time,"tzone")),
                   date_breaks = "2 hours") +
  theme(text=element_text(size = 20)) +
  theme(axis.text.x = element_text(vjust=0.5)) +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  scale_color_manual(values=alpha(c('#6792DA', '#ECCC68','#DA5E5E'), 0.75),
                     labels = c("Low activity", "Medium activity", "High activity")) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) 

ggStatesDepth <- ggplot(sting11) + 
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
  scale_x_datetime(labels = date_format("%H:%M\n%d %b", tz= attr(sting11$Date_Time,"tzone")),
                   date_breaks = "2 hours") +
  theme(text=element_text(size = 20)) +
  theme(axis.text.x = element_text(vjust=0.5)) +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  scale_color_manual(values=alpha(c('#6792DA', '#ECCC68','#DA5E5E'), 0.75),
                     labels = c("Low activity", "Medium activity", "High activity")) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())

ggStatesTemp <- ggplot(sting11) + 
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
  scale_x_datetime(labels = date_format("%H:%M\n%d %b", tz= attr(sting11$Date_Time,"tzone")),
                   date_breaks = "2 hours") +
  theme(text=element_text(size = 20)) +
  theme(axis.text.x = element_text(vjust=0.5)) +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  scale_color_manual(values=alpha(c('#6792DA', '#ECCC68','#DA5E5E'), 0.75),
                     labels = c("Low activity", "Medium activity", "High activity")) 

# adjust margins for x axis label
margins <- margin (5,50,5,5, "points") #top, right, bottom, left

ggStatesODBA <- ggStatesODBA + theme(plot.margin = margins)
ggStatesDepth <- ggStatesDepth + theme(plot.margin = margins)
ggStatesTemp <- ggStatesTemp + theme(plot.margin = margins)

grid.draw(rbind(ggplotGrob(ggStatesODBA), 
                ggplotGrob(ggStatesDepth),
                ggplotGrob(ggStatesTemp)))

#Zoom in near pop-off: what happened?

DepthZoom1 <- ggplot(sting11) + 
  geom_segment(aes(x = Date_Time, y = Depth, 
                   xend=lead(Date_Time), yend=lead(Depth),
                   color=States), size = 1, 
               linetype = "solid", 
               lineend = "round",
               linejoin = "round",
               alpha = 1) +
  ylab("Depth (m)\n") +
  xlab("\n20 June 2019") +
  scale_y_reverse(breaks = seq(5.0,0.2, -0.2), limits = c (5.0,0.2)) +
  theme_classic() + 
  theme(text=element_text(size = 20)) +
  theme(axis.text.x = element_text(vjust=0.5)) +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  scale_color_manual(values=alpha(c('#6792DA', '#ECCC68','#DA5E5E'), 0.75),
                     labels = c("Low activity", "Medium activity", "High activity")) +
  scale_x_datetime(limits = as.POSIXct(c("2019-06-20 19:17:00", "2019-06-20 19:31:00"),tz=
                                         attr(sting11$Date_Time,"tzone")))
DepthZoom1

# formula 2 (depth)
s11.fit2 <- fitHMM(sting11data,
                   nbStates=3,
                   dist=list(ODBA="gamma"),
                   Par0 = list(ODBA=c(0.007,0.025,0.1,0.007,0.025,0.1,
                                      0.1,0.1,0.1)),
                   formula = formula2,
                   stateNames = stateNames)
s11.fit2
plot(s11.fit2, breaks = 60)
plotPR(s11.fit2) #pseudoresiduals
plotStationary(s11.fit2, plotCI = TRUE)

# formula 3 (temperature)
# convergence issues
# s11.fit3 <- fitHMM(sting11data,
#                   nbStates=3,
#                   dist=list(ODBA="gamma"),
#                   Par0 = list(ODBA=c(0.007,0.025,0.1,0.007,0.025,0.1,
#                                      0.05,0.05,0.05)),
#                   formula = formula3,
#                   stateNames = stateNames)
# beep(4)
# s11.fit3
# plot(s11.fit3, breaks = 60)
# plotPR(s11.fit3) #pseudoresiduals
# plotStationary(s11.fit3, plotCI = TRUE)

# formula 4 (hour + Depth)
s11.fit4 <- fitHMM(sting11data,
                  nbStates=3,
                  dist=list(ODBA="gamma"),
                  Par0 = list(ODBA=c(0.007,0.025,0.1,0.007,0.025,0.1,
                                     0.1,0.1,0.1)),
                  formula = formula4,
                  stateNames = stateNames)
s11.fit4
plot(s11.fit4, breaks = 60) 
plotPR(s11.fit4) #pseudoresiduals
plotStationary(s11.fit4, plotCI = TRUE)

# formula 5 (hour + temperature)
# convergence issues
# s11.fit5 <- fitHMM(sting11data,
#                nbStates=3,
#                dist=list(ODBA="gamma"),
#                Par0 = list(ODBA=c(0.007,0.025,0.1,0.007,0.025,0.1,
#                                   0.05,0.05,0.05)),
#                formula = formula5,
#                stateNames = stateNames)
# beep(4)
# s11.fit5
# plot(s11.fit5, breaks = 60)
# plotPR(s11.fit5) #pseudoresiduals
# plotStationary(s11.fit5, plotCI = TRUE)

# formula 6 (depth + temperature)
# convergence issues
# s11.fit6 <- fitHMM(sting11data,
#                nbStates=3,
#                dist=list(ODBA="gamma"),
#                Par0 = list(ODBA=c(0.01,0.03,0.12,0.01,0.03,0.12,
#                                   0.05,0.05,0.05)),
#                formula = formula6,
#                stateNames = stateNames)
# beep(4)
# s11.fit6
# plot(s11.fit6, breaks = 60)
# plotPR(s11.fit6) #pseudoresiduals
# plotStationary(s11.fit6, plotCI = TRUE)

# formula 7 (hour interaction with depth)
s11.fit7 <- fitHMM(sting11data,
               nbStates=3,
               dist=list(ODBA="gamma"),
               Par0 = list(ODBA=c(0.007,0.025,0.1,0.007,0.025,0.1,
                                  0.1,0.1,0.1)),
               formula = formula7,
               stateNames = stateNames)
beep(4)
s11.fit7
plot(s11.fit7, breaks = 60)
plotPR(s11.fit7) #pseudoresiduals
plotStationary(s11.fit7, plotCI = TRUE)

# formula 8 (hour interaction with temperature)
s11.fit8 <- fitHMM(sting11data,
               nbStates=3,
               dist=list(ODBA="gamma"),
               Par0 = list(ODBA=c(0.007,0.025,0.1,0.007,0.025,0.1,
                                  0.1,0.1,0.1)),
               formula = formula8,
               stateNames = stateNames)
beep(4)
s11.fit8
plot(s11.fit8, breaks = 60)
plotPR(s11.fit8) #pseudoresiduals
plotStationary(s11.fit8, plotCI = TRUE)

# formula 9 (depth interaction with temperature)
s11.fit9 <- fitHMM(sting11data,
               nbStates=3,
               dist=list(ODBA="gamma"),
               Par0 = list(ODBA=c(0.007,0.025,0.1,0.007,0.025,0.1,
                                  0.1,0.1,0.1)),
               formula = formula9,
               stateNames = stateNames)
beep(4)
s11.fit9
plot(s11.fit9, breaks = 60)
plotPR(s11.fit9) #pseudoresiduals
plotStationary(s11.fit9, plotCI = TRUE) 

# formula 10 (hour + depth + temperature)
# convergence issues
# s11.fit10 <- fitHMM(sting11data, 
#                 nbStates=3, 
#                 dist=list(ODBA="gamma"),
#                 Par0 = list(ODBA=c(0.009,0.025,0.15,0.009,0.025,0.15,
#                                    0.05,0.05,0.05)),
#                 formula = formula10, 
#                 stateNames = stateNames)
# beep(4)
# s11.fit10
# plot(s11.fit10, breaks = 60) 
# plotPR(s11.fit10) #pseudoresiduals
# plotStationary(s11.fit10, plotCI = TRUE)

# formula 11 (hour + interaction between depth & temperature)
s11.fit11 <- fitHMM(sting11data,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.007,0.025,0.1,0.007,0.025,0.1,
                                   0.1,0.1,0.1)),
                formula = formula11,
                stateNames = stateNames)

s11.fit11
plot(s11.fit11, breaks = 60) 
plotPR(s11.fit11) #pseudoresiduals
plotStationary(s11.fit11, plotCI = TRUE)

# formula 12 (interaction hour & depth + temperature)
s11.fit12 <- fitHMM(sting11data,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.007,0.025,0.1,0.007,0.025,0.1,
                                   0.1,0.1,0.1)),
                formula = formula12,
                stateNames = stateNames)
beep(4)
s11.fit12
plot(s11.fit12, breaks = 60,plotCI = T)
plotPR(s11.fit12) #pseudoresiduals
plotStationary(s11.fit12, plotCI = TRUE)

# formula 13 (interaction hour & temperature + depth)
s11.fit13 <- fitHMM(sting11data,
                nbStates=3,
                dist=list(ODBA="gamma"),
                Par0 = list(ODBA=c(0.007,0.025,0.1,0.007,0.025,0.1,
                                   0.1,0.1,0.1)),
                formula = formula13,
                stateNames = stateNames)
beep(4)
s11.fit13
plot(s11.fit13, breaks = 60)
plotPR(s11.fit13) #pseudoresiduals
plotStationary(s11.fit13, plotCI = TRUE)

# formula 14 (3 way interaction)
# convergence issues
# s11.fit14 <- fitHMM(sting11data,
#                 nbStates=3,
#                 dist=list(ODBA="gamma"),
#                 Par0 = list(ODBA=c(0.01,0.032,0.12,0.01,0.032,0.12,
#                                    0.06,0.06,0.06)),
#                 formula = formula14,
#                 stateNames = stateNames)
# 
# s11.fit14
# plot(s11.fit14, breaks = 60)
# plotPR(s11.fit14) #pseudoresiduals
# plotStationary(s11.fit14, plotCI = TRUE)


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

# fits without convergence issues
fit_list <- list(
  s11.fit1, s11.fit2, 
  s11.fit4, 
  s11.fit7, s11.fit8, s11.fit9, 
  s11.fit11, s11.fit12, s11.fit13
)

# Initialize BICtab with appropriate model numbers
model_numbers <- c(1, 2, 4, 7, 8, 9, 11, 12, 13)
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
write.table(BICtab, file = "sting11BIC.csv", sep = ",", row.names = FALSE, quote = FALSE)


