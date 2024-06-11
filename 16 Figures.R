# INDIVIDUAL STINGRAY FIGURES #
# FROM BIC BEST MODELS #

# Authored by Katie Flowers with help from Beth Babcock
# Last updated 08 June 2024

# Step 1 - load each stingray's workspace ---> 
# BE CAREFUL some things are named the same OOPSIES
# Only use s1.state.prob, s2.state.prob, etc. for figures

##### Load libraries #####

library(ggplot2) #data vis
library(grid) #stack ggplots
library(scales) #for visualization
library(tidyverse)
library(gridExtra) #multipanel plots
library(cowplot)

##### Stationary state probabilities - HOUR #####

# Stingray 1 -------------------------------------------------------------------

s1.hourVals<-bind_rows(s1.state.prob$hour,.id="Activity") %>%
  rename(hour=cov)
s1.hourVals$Activity <-factor(s1.hourVals$Activity, levels = c("Low activity",
                                                               "Medium activity",
                                                               "High activity"))

s1.hour<- ggplot(s1.hourVals,
                 aes(x=hour,y=est,ymax=uci,ymin=lci,color=Activity,
                     fill=Activity, linetype=Activity)) +
  geom_line(linewidth=0.9)+
  geom_ribbon(alpha=0.55, linewidth=0.9) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), limits= c(0,1), breaks = seq(0.2,1, by=0.2)) +
  xlab("\nHour of day") +
  ylab("Stationary state probabilities \n") +
  theme(text=element_text(size = 20)) +
  theme(axis.text.x = element_text(vjust=0.5)) +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  scale_color_manual(values=alpha(c('#6792DA', '#ECCC68','#DA5E5E'))) +
  scale_fill_manual(values=c('#6792DA', '#ECCC68','#DA5E5E')) +
  scale_linetype_manual(values=c("dotted","longdash","solid")) +
  guides(color = guide_legend(title = NULL), 
         fill = guide_legend(title = NULL),
         linetype = guide_legend(title = NULL))

# Stingray 2 -------------------------------------------------------------------

s2.hourVals<-bind_rows(s2.state.prob$hour,.id="Activity") %>% rename(hour=cov)
s2.hourVals$Activity <-factor(s2.hourVals$Activity, 
                              levels = c("Low activity",
                                        "Medium activity",
                                        "High activity"))

s2.hour<- ggplot(s2.hourVals,
                 aes(x=hour,y=est,ymax=uci,ymin=lci,color=Activity,
                     fill=Activity, linetype=Activity)) +
  geom_line(linewidth=0.9) +
  geom_ribbon(alpha=0.55, linewidth=0.9) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), limits= c(0,1), breaks = seq(0.2,1, by=0.2)) +
  xlab("\nHour of day") +
  ylab("Stationary state probabilities \n") +
  theme(text=element_text(size = 20)) +
  theme(axis.text.x = element_text(vjust=0.5)) +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  scale_color_manual(values=alpha(c('#6792DA', '#ECCC68','#DA5E5E'))) +
  scale_fill_manual(values=c('#6792DA', '#ECCC68','#DA5E5E')) +
  scale_linetype_manual(values=c("dotted","longdash","solid")) +
  guides(color = guide_legend(title = NULL), 
         fill = guide_legend(title = NULL),
         linetype = guide_legend(title = NULL))

# Stingray 6 -------------------------------------------------------------------

s6.hourVals<-bind_rows(s6.state.prob$hour,.id="Activity") %>%
  rename(hour=cov)
s6.hourVals$Activity <-factor(s6.hourVals$Activity, levels = c("Low activity",
                                                               "Medium activity",
                                                               "High activity"))

s6.hour<- ggplot(s6.hourVals,
                 aes(x=hour,y=est,ymax=uci,ymin=lci,color=Activity,
                     fill=Activity, linetype=Activity)) +
  geom_line(linewidth=0.9)+
  geom_ribbon(alpha=0.55, linewidth=0.9) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), limits= c(0,1), breaks = seq(0.2,1, by=0.2)) +
  xlab("\nHour of day") +
  ylab("Stationary state probabilities \n") +
  theme(text=element_text(size = 20)) +
  theme(axis.text.x = element_text(vjust=0.5)) +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  theme(legend.position = c(0.52, 0.85)) +
  scale_color_manual(values=alpha(c('#6792DA', '#ECCC68','#DA5E5E'))) +
  scale_fill_manual(values=c('#6792DA', '#ECCC68','#DA5E5E')) +
  scale_linetype_manual(values=c("dotted","longdash","solid")) +
  guides(color = guide_legend(title = NULL), 
         fill = guide_legend(title = NULL),
         linetype = guide_legend(title = NULL))

# Multipanel plot --------------------------------------------------------------

# adjust panel margins if adding common axis titles 
# margins.s2 <-margin (10, 5.5, 5.5, 55, "points") #top, right, bottom, left
# margins.s6 <-margin (10, 10, 5.5, 5.5, "points") #top, right, bottom, left
# margins.s7 <-margin (10, 5.5, 55, 55, "points") #top, right, bottom, left
# margins.s9 <-margin (10, 10, 55, 5.5, "points") #top, right, bottom, left

# adjust margins for panel labels (stingray names)
margins <- margin (55,10,25,5, "points") #top, right, bottom, left

s1.hour <- s1.hour + theme(plot.margin = margins)
s2.hour <- s2.hour + theme(plot.margin = margins)
s6.hour <- s6.hour + theme(plot.margin = margins)

# make multipanel figure from ggplots above arranged by diel period
final.hour <- grid.arrange(arrangeGrob(s6.hour + theme(#legend.position = "none", 
                                                       axis.title.y = element_blank(),
                                                       axis.title.x = element_blank())),
                           arrangeGrob(s1.hour + theme(legend.position = "none", 
                                                       axis.title.y = element_blank(),
                                                       axis.title.x = element_blank())),
                           arrangeGrob(s2.hour + theme(legend.position = "none", 
                                                       axis.title.y = element_blank(),
                                                       axis.title.x = element_blank())),
                           ncol = 3, # columns
                           nrow = 1) # rows
                       
# add stingray names above plots 
grid.text("Stingray 6", x = 0.178, y = 0.965, gp = gpar(fontsize = 22))
grid.text("Stingray 1", x = 0.51, y = 0.965, gp = gpar(fontsize = 22))
grid.text("Stingray 2", x = 0.8425, y = 0.965, gp = gpar(fontsize = 22))

# add disc widths below titles
grid.text("63cm DW", x = 0.178, y = 0.91, gp = gpar(fontsize = 15))
grid.text("78cm DW", x = 0.51, y = 0.91, gp = gpar(fontsize = 15))
grid.text("81cm DW", x = 0.8425, y = 0.91, gp = gpar(fontsize = 15))

# add common x and y axis titles - but this looks worse than if just added in pixelmator afterwards
# it squishes the bottom row plots even when playing around with margins
# grid.text("Hour of day", x = 0.5, y = 0.051, gp = gpar(fontsize = 30)) # common x axis title
# grid.text("Stationary state probabilities", x = 0.025, y = 0.55, 
          # gp = gpar(fontsize = 30), rot = 90) # common y axis title

##### Stationary state probabilities - DEPTH #####

# Stingray 2 -------------------------------------------------------------------

s2.depthVals<-bind_rows(s2.state.prob$Depth,.id="Activity") %>%
  rename(Depth=cov)
s2.depthVals$Activity <-factor(s2.depthVals$Activity, levels = c("Low activity",
                                                           "Medium activity",
                                                           "High activity"))

s2.depth<- ggplot(s2.depthVals,
                  aes(x=Depth,y=est,ymax=uci,ymin=lci,color=Activity,
                      fill=Activity, linetype=Activity)) +
  geom_line(linewidth=0.9)+
  geom_ribbon(alpha=0.55, linewidth=0.9) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), limits= c(0,1), breaks = seq(0.2,1, by=0.2)) +
  xlab("\nDepth (m)") +
  ylab("Stationary state probabilities \n") +
  theme(text=element_text(size = 20)) +
  theme(axis.text.x = element_text(vjust=0.5)) +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  theme(legend.position = c(0.65, 0.9)) +
  scale_color_manual(values=alpha(c('#6792DA', '#ECCC68','#DA5E5E'))) +
  scale_fill_manual(values=c('#6792DA', '#ECCC68','#DA5E5E')) +
  scale_linetype_manual(values=c("dotted","longdash","solid")) +
  guides(color = guide_legend(title = NULL), 
         fill = guide_legend(title = NULL),
         linetype = guide_legend(title = NULL)) 

# Stingray 5 -------------------------------------------------------------------

s5.depthVals<-bind_rows(s5.state.prob$Depth,.id="Activity") %>%
  rename(Depth=cov)
s5.depthVals$Activity <-factor(s5.depthVals$Activity, levels = c("Low activity",
                                                           "Medium activity",
                                                           "High activity"))

s5.depth<- ggplot(s5.depthVals,
                  aes(x=Depth,y=est,ymax=uci,ymin=lci,color=Activity,
                      fill=Activity, linetype=Activity)) +
  geom_line(linewidth=0.9)+
  geom_ribbon(alpha=0.55, linewidth=0.9) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), limits= c(0,1), breaks = seq(0.2,1, by=0.2)) +
  xlab("\nDepth (m)") +
  ylab("Stationary state probabilities \n") +
  theme(text=element_text(size = 20)) +
  theme(axis.text.x = element_text(vjust=0.5)) +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  theme(legend.position = c(0.83, 0.52)) +
  scale_color_manual(values=alpha(c('#6792DA', '#ECCC68','#DA5E5E'))) +
  scale_fill_manual(values=c('#6792DA', '#ECCC68','#DA5E5E')) +
  scale_linetype_manual(values=c("dotted","longdash","solid")) +
  guides(color = guide_legend(title = NULL), 
         fill = guide_legend(title = NULL),
         linetype = guide_legend(title = NULL)) 

# Stingray 8 -------------------------------------------------------------------

s8.depthVals<-bind_rows(s8.state.prob$Depth,.id="Activity") %>%
  rename(Depth=cov)
s8.depthVals$Activity <-factor(s8.depthVals$Activity, levels = c("Low activity",
                                                           "Medium activity",
                                                           "High activity"))

s8.depth<- ggplot(s8.depthVals,
                  aes(x=Depth,y=est,ymax=uci,ymin=lci,color=Activity,
                      fill=Activity, linetype=Activity)) +
  geom_line(linewidth=0.9)+
  geom_ribbon(alpha=0.55, linewidth=0.9) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), limits= c(0,1), breaks = seq(0.2,1, by=0.2)) +
  xlab("\nDepth (m)") +
  ylab("Stationary state probabilities \n") +
  theme(text=element_text(size = 20)) +
  theme(axis.text.x = element_text(vjust=0.5)) +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  theme(legend.position = c(0.86, 0.91)) +
  scale_color_manual(values=alpha(c('#6792DA', '#ECCC68','#DA5E5E'))) +
  scale_fill_manual(values=c('#6792DA', '#ECCC68','#DA5E5E')) +
  scale_linetype_manual(values=c("dotted","longdash","solid")) +
  guides(color = guide_legend(title = NULL), 
         fill = guide_legend(title = NULL),
         linetype = guide_legend(title = NULL)) 

# Stingray 10 ------------------------------------------------------------------

s10.depthVals<-bind_rows(s10.state.prob$Depth,.id="Activity") %>%
  rename(Depth=cov)
s10.depthVals$Activity <-factor(s10.depthVals$Activity, levels = c("Low activity",
                                                           "Medium activity",
                                                           "High activity"))

s10.depth<- ggplot(s10.depthVals,
                   aes(x=Depth,y=est,ymax=uci,ymin=lci,color=Activity,
                       fill=Activity, linetype=Activity)) +
  geom_line(linewidth=0.9)+
  geom_ribbon(alpha=0.55, linewidth=0.9) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), limits= c(0,1), breaks = seq(0.2,1, by=0.2)) +
  xlab("\nDepth (m)") +
  ylab("Stationary state probabilities \n") +
  theme(text=element_text(size = 20)) +
  theme(axis.text.x = element_text(vjust=0.5)) +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  theme(legend.position = c(0.44, 0.91)) +
  scale_color_manual(values=alpha(c('#6792DA', '#ECCC68','#DA5E5E'))) +
  scale_fill_manual(values=c('#6792DA', '#ECCC68','#DA5E5E')) +
  scale_linetype_manual(values=c("dotted","longdash","solid")) +
  guides(color = guide_legend(title = NULL), 
         fill = guide_legend(title = NULL),
         linetype = guide_legend(title = NULL))

# Multipanel plot --------------------------------------------------------------

# adjust margins for panel labels (stingray names)
margins <- margin (55,10,25,5, "points") #top, right, bottom, left

s2.depth <- s2.depth + theme(plot.margin = margins)
s5.depth <- s5.depth + theme(plot.margin = margins)
s8.depth <- s8.depth + theme(plot.margin = margins)
s10.depth <- s10.depth + theme(plot.margin = margins)

# make multipanel figure from ggplots above
final.depth <- grid.arrange(arrangeGrob(s2.depth + 
                                        theme(legend.position = "none", 
                                              axis.title.y = element_blank(),
                                              axis.title.x = element_blank())),
                           arrangeGrob(s5.depth + 
                                       theme(#legend.position = "none", 
                                       axis.title.y = element_blank(),
                                       axis.title.x = element_blank())), 
                           arrangeGrob(s8.depth + 
                                       theme(legend.position = "none", 
                                             axis.title.y = element_blank(),
                                             axis.title.x = element_blank())),
                           arrangeGrob(s10.depth + 
                                      theme(legend.position = "none", 
                                            axis.title.y = element_blank(),
                                            axis.title.x = element_blank())),
                           ncol = 2, # columns
                           nrow = 2) # rows

# add stingray names above plots
grid.text("Stingray 2", x = 0.25, y = 0.97, gp = gpar(fontsize = 22))
grid.text("Stingray 5", x = 0.75, y = 0.97, gp = gpar(fontsize = 22))
grid.text("Stingray 8", x = 0.25, y = 0.47, gp = gpar(fontsize = 22))
grid.text("Stingray 10", x = 0.75, y = 0.47, gp = gpar(fontsize = 22))

##### Stationary state probabilities - TEMPERATURE #####

# Stingray 2 -------------------------------------------------------------------

s2.tempVals<-bind_rows(s2.state.prob$Temp,.id="Activity") %>% rename(Temp=cov)
s2.tempVals$Activity <-factor(s2.tempVals$Activity, levels = c("Low activity",
                                                               "Medium activity",
                                                               "High activity"))

s2.temp<- ggplot(s2.tempVals,
                 aes(x=Temp,y=est,ymax=uci,ymin=lci,color=Activity,
                     fill=Activity, linetype=Activity)) +
  geom_line(linewidth=0.9)+
  geom_ribbon(alpha=0.55, linewidth=0.9) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0), labels = label_number(accuracy = 0.1)) +
  scale_y_continuous(expand = c(0,0), limits= c(0,1), breaks = seq(0.2,1, by=0.2)) +
  xlab("\nTemp (°C)") +
  ylab("Stationary state probabilities \n") +
  theme(text=element_text(size = 20)) +
  theme(axis.text.x = element_text(vjust=0.5)) +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  theme(legend.position = c(0.84, 0.9)) +
  scale_color_manual(values=alpha(c('#6792DA', '#ECCC68','#DA5E5E'))) +
  scale_fill_manual(values=c('#6792DA', '#ECCC68','#DA5E5E')) +
  scale_linetype_manual(values=c("dotted","longdash","solid")) +
  guides(color = guide_legend(title = NULL), 
         fill = guide_legend(title = NULL),
         linetype = guide_legend(title = NULL)) 

# Stingray 7 -------------------------------------------------------------------

s7.tempVals<-bind_rows(s7.state.prob$Temp,.id="Activity") %>%
  rename(Temp=cov)
s7.tempVals$Activity <-factor(s7.tempVals$Activity, levels = c("Low activity",
                                                         "Medium activity",
                                                         "High activity"))

s7.temp<- ggplot(s7.tempVals,
                 aes(x=Temp,y=est,ymax=uci,ymin=lci,color=Activity,
                     fill=Activity, linetype=Activity)) +
  geom_line(linewidth=0.9)+
  geom_ribbon(alpha=0.55, linewidth=0.9) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0),labels = label_number(accuracy = 0.1)) +
  scale_y_continuous(expand = c(0,0), limits= c(0,1), breaks = seq(0.2,1, by=0.2)) +
  xlab("\nTemp (°C)") +
  ylab("Stationary state probabilities \n") +
  theme(text=element_text(size = 20)) +
  theme(axis.text.x = element_text(vjust=0.5)) +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  theme(legend.position = c(0.23, 0.9)) +
  scale_color_manual(values=alpha(c('#6792DA', '#ECCC68','#DA5E5E'))) +
  scale_fill_manual(values=c('#6792DA', '#ECCC68','#DA5E5E')) +
  scale_linetype_manual(values=c("dotted","longdash","solid")) +
  guides(color = guide_legend(title = NULL), 
         fill = guide_legend(title = NULL),
         linetype = guide_legend(title = NULL)) 

# Stingray 8 -------------------------------------------------------------------

s8.tempVals<-bind_rows(s8.state.prob$Temp,.id="Activity") %>%
  rename(Temp=cov)
s8.tempVals$Activity <-factor(s8.tempVals$Activity, levels = c("Low activity",
                                                         "Medium activity",
                                                         "High activity"))

s8.temp<- ggplot(s8.tempVals,
                 aes(x=Temp,y=est,ymax=uci,ymin=lci,color=Activity,
                     fill=Activity, linetype=Activity)) +
  geom_line(linewidth=0.9)+
  geom_ribbon(alpha=0.55, linewidth=0.9) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), limits= c(0,1), breaks = seq(0.2,1, by=0.2)) +
  xlab("\nTemp (°C)") +
  ylab("Stationary state probabilities \n") +
  theme(text=element_text(size = 20)) +
  theme(axis.text.x = element_text(vjust=0.5)) +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  theme(legend.position = c(0.84, 0.9)) +
  scale_color_manual(values=alpha(c('#6792DA', '#ECCC68','#DA5E5E'))) +
  scale_fill_manual(values=c('#6792DA', '#ECCC68','#DA5E5E')) +
  scale_linetype_manual(values=c("dotted","longdash","solid")) +
  guides(color = guide_legend(title = NULL), 
         fill = guide_legend(title = NULL),
         linetype = guide_legend(title = NULL)) 

# Stingray 10 ------------------------------------------------------------------

s10.tempVals<-bind_rows(s10.state.prob$Temp,.id="Activity") %>% rename(Temp=cov)
s10.tempVals$Activity <-factor(s10.tempVals$Activity, levels = c("Low activity",
                                                                 "Medium activity",
                                                                 "High activity"))

s10.temp<- ggplot(s10.tempVals,
                  aes(x=Temp,y=est,ymax=uci,ymin=lci,color=Activity,
                      fill=Activity, linetype=Activity)) +
  geom_line(linewidth=0.9)+
  geom_ribbon(alpha=0.55, linewidth=0.9) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), limits= c(0,1), breaks = seq(0.2,1, by=0.2)) +
  xlab("\nTemp (°C)") +
  ylab("Stationary state probabilities \n") +
  theme(text=element_text(size = 20)) +
  theme(axis.text.x = element_text(vjust=0.5)) +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  theme(legend.position = c(0.84, 0.9)) +
  scale_color_manual(values=alpha(c('#6792DA', '#ECCC68','#DA5E5E'))) +
  scale_fill_manual(values=c('#6792DA', '#ECCC68','#DA5E5E')) +
  scale_linetype_manual(values=c("dotted","longdash","solid")) +
  guides(color = guide_legend(title = NULL), 
         fill = guide_legend(title = NULL),
         linetype = guide_legend(title = NULL)) 

# Multipanel plot --------------------------------------------------------------

# adjust margins for panel labels (stingray names)
margins <- margin (55,15,25,5, "points") #top, right, bottom, left

s2.temp <- s2.temp + theme(plot.margin = margins)
s7.temp <- s7.temp + theme(plot.margin = margins)
s8.temp <- s8.temp + theme(plot.margin = margins)
s10.temp <- s10.temp + theme(plot.margin = margins)

# make multipanel figure from ggplots above
final.temp <- grid.arrange(arrangeGrob(s2.temp + 
                                          theme(legend.position = "none", 
                                                axis.title.y = element_blank(),
                                                axis.title.x = element_blank())),
                            arrangeGrob(s7.temp + 
                                          theme(legend.position = "none", 
                                            axis.title.y = element_blank(),
                                            axis.title.x = element_blank())), 
                            arrangeGrob(s8.temp + 
                                          theme(legend.position = "none", 
                                                axis.title.y = element_blank(),
                                                axis.title.x = element_blank())),
                            arrangeGrob(s10.temp + 
                                          theme(#legend.position = "none", 
                                                axis.title.y = element_blank(),
                                                axis.title.x = element_blank())),
                            ncol = 2, # columns
                            nrow = 2) # rows

# add stingray names above plots
grid.text("Stingray 2", x = 0.25, y = 0.97, gp = gpar(fontsize = 22))
grid.text("Stingray 7", x = 0.75, y = 0.97, gp = gpar(fontsize = 22))
grid.text("Stingray 8", x = 0.25, y = 0.47, gp = gpar(fontsize = 22))
grid.text("Stingray 10", x = 0.75, y = 0.47, gp = gpar(fontsize = 22))


##### SUPPLEMENTAL FIGS #####

##### Stationary state probabilities - HOUR #####

# Stingray 5 -------------------------------------------------------------------

s5.hourVals<-bind_rows(s5.state.prob$hour,.id="Activity") %>%
  rename(hour=cov)
s5.hourVals$Activity <-factor(hourVals$Activity, levels = c("Low activity",
                                                         "Medium activity",
                                                         "High activity"))

s5.hour<- ggplot(s5.hourVals,
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
  theme(legend.position = c(0.65, 0.9)) + 
  scale_color_manual(values=alpha(c('#6792DA', '#ECCC68','#DA5E5E'))) +
  scale_fill_manual(values=c('#6792DA', '#ECCC68','#DA5E5E')) +
  scale_linetype_manual(values=c("dotted","longdash","solid")) +
  guides(color = guide_legend(title = NULL), 
         fill = guide_legend(title = NULL),
         linetype = guide_legend(title = NULL))

# Stingray 7 -------------------------------------------------------------------

s7.hourVals<-bind_rows(s7.state.prob$hour,.id="Activity") %>%
  rename(hour=cov)
s7.hourVals$Activity <-factor(hourVals$Activity, levels = c("Low activity",
                                                         "Medium activity",
                                                         "High activity"))

s7.hour<- ggplot(s7.hourVals,
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
  theme(legend.position = c(0.41, 0.94)) +
  scale_color_manual(values=alpha(c('#6792DA', '#ECCC68','#DA5E5E'))) +
  scale_fill_manual(values=c('#6792DA', '#ECCC68','#DA5E5E')) +
  scale_linetype_manual(values=c("dotted","longdash","solid")) +
  guides(color = guide_legend(title = NULL), 
         fill = guide_legend(title = NULL),
         linetype = guide_legend(title = NULL))

# Stingray 8 -------------------------------------------------------------------

s8.hourVals<-bind_rows(s8.state.prob$hour,.id="Activity") %>%
  rename(hour=cov)
s8.hourVals$Activity <-factor(s8.hourVals$Activity, levels = c("Low activity",
                                                         "Medium activity",
                                                         "High activity"))

s8.hour<- ggplot(s8.hourVals,
                 aes(x=hour,y=est,ymax=uci,ymin=lci,color=Activity,
                     fill=Activity, linetype=Activity)) +
  geom_line(linewidth=0.9)+
  geom_ribbon(alpha=0.55, linewidth=0.9) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), limits= c(0,1), breaks = seq(0.2,1, by=0.2)) +
  xlab("\nHour of day") +
  ylab("Stationary state probabilities \n") +
  theme(text=element_text(size = 20)) +
  theme(axis.text.x = element_text(vjust=0.5)) +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  theme(legend.position = c(0.15, 0.94)) +
  scale_color_manual(values=alpha(c('#6792DA', '#ECCC68','#DA5E5E'))) +
  scale_fill_manual(values=c('#6792DA', '#ECCC68','#DA5E5E')) +
  scale_linetype_manual(values=c("dotted","longdash","solid")) +
  guides(color = guide_legend(title = NULL), 
         fill = guide_legend(title = NULL),
         linetype = guide_legend(title = NULL))

# Stingray 10 ------------------------------------------------------------------

s10.hourVals<-bind_rows(s10.state.prob$hour,.id="Activity") %>%
  rename(hour=cov)
s10.hourVals$Activity <-factor(s10.hourVals$Activity, levels = c("Low activity",
                                                                 "Medium activity",
                                                                 "High activity"))

s10.hour<- ggplot(s10.hourVals,
                  aes(x=hour,y=est,ymax=uci,ymin=lci,color=Activity,
                      fill=Activity, linetype=Activity)) +
  geom_line(linewidth=0.9)+
  geom_ribbon(alpha=0.55, linewidth=0.9) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), limits= c(0,1), breaks = seq(0.2,1, by=0.2)) +
  xlab("\nHour of day") +
  ylab("Stationary state probabilities \n") +
  theme(text=element_text(size = 20)) +
  theme(axis.text.x = element_text(vjust=0.5)) +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  theme(legend.position = c(0.41, 0.94)) +
  scale_color_manual(values=alpha(c('#6792DA', '#ECCC68','#DA5E5E'))) +
  scale_fill_manual(values=c('#6792DA', '#ECCC68','#DA5E5E')) +
  scale_linetype_manual(values=c("dotted","longdash","solid")) +
  guides(color = guide_legend(title = NULL), 
         fill = guide_legend(title = NULL),
         linetype = guide_legend(title = NULL))

# Stingray 11 ------------------------------------------------------------------

s11.hourVals<-bind_rows(s11.state.prob$hour,.id="Activity") %>%
  rename(hour=cov)
s11.hourVals$Activity <-factor(s11.hourVals$Activity, levels = c("Low activity",
                                                         "Medium activity",
                                                         "High activity"))

s11.hour<- ggplot(s11.hourVals,
                  aes(x=hour,y=est,ymax=uci,ymin=lci,color=Activity,
                      fill=Activity, linetype=Activity)) +
  geom_line(linewidth=0.9)+
  geom_ribbon(alpha=0.55, linewidth=0.9) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), limits= c(0,1), breaks = seq(0.2,1, by=0.2)) +
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

# Multipanel plot --------------------------------------------------------------

# adjust margins for panel labels (stingray names)
margins <- margin (55,10,25,5, "points") #top, right, bottom, left

s5.hour <- s5.hour + theme(plot.margin = margins)
s7.hour <- s7.hour + theme(plot.margin = margins)
s8.hour <- s8.hour + theme(plot.margin = margins)
s10.hour <- s10.hour + theme(plot.margin = margins)
s11.hour <- s11.hour + theme(plot.margin = margins)

# make multipanel figure from ggplots above
final.hour <- grid.arrange(arrangeGrob(s5.hour + theme(legend.position = "none", 
                                                       axis.title.y = element_blank(),
                                                       axis.title.x = element_blank())),
                           arrangeGrob(s7.hour + theme(legend.position = "none", 
                                                       axis.title.y = element_blank(),
                                                       axis.title.x = element_blank())), 
                           arrangeGrob(s8.hour + theme(legend.position = "none", 
                                                       axis.title.y = element_blank(),
                                                       axis.title.x = element_blank())),
                           arrangeGrob(s10.hour + theme(legend.position = "none", 
                                                       axis.title.y = element_blank(),
                                                       axis.title.x = element_blank())),
                           arrangeGrob(s11.hour + theme(legend.position = "none", 
                                                       axis.title.y = element_blank(),
                                                       axis.title.x = element_blank())),
                           legend.final<- cowplot::get_legend(s11.hour), #why doesn't this work anymore
                           ncol = 3,
                           nrow = 2)
                           
# add stingray names above plots
grid.text("Stingray 5", x = 0.17, y = 0.94, gp = gpar(fontsize = 22))
grid.text("Stingray 7", x = 0.51, y = 0.94, gp = gpar(fontsize = 22))
grid.text("Stingray 8", x = 0.85, y = 0.94, gp = gpar(fontsize = 22))
grid.text("Stingray 10", x = 0.17, y = 0.45, gp = gpar(fontsize = 22))
grid.text("Stingray 11", x = 0.51, y = 0.45, gp = gpar(fontsize = 22))


##### Stationary state probabilities - DEPTH #####

# Stingray 1 -------------------------------------------------------------------

s1.depthVals<-bind_rows(s1.state.prob$Depth,.id="Activity") %>% rename(Depth=cov)
s1.depthVals$Activity <-factor(s1.depthVals$Activity, levels = c("Low activity",
                                                                 "Medium activity",
                                                                 "High activity"))

s1.depth<- ggplot(s1.depthVals,
                  aes(x=Depth,y=est,ymax=uci,ymin=lci,color=Activity,
                      fill=Activity, linetype=Activity)) +
  geom_line(linewidth=0.9)+
  geom_ribbon(alpha=0.55, linewidth=0.9) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), limits= c(0,1), breaks = seq(0.2,1, by=0.2)) +
  xlab("\nDepth (m)") +
  ylab("Stationary state probabilities \n") +
  theme(text=element_text(size = 20)) +
  theme(axis.text.x = element_text(vjust=0.5)) +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  theme(legend.position = c(0.79, 0.6)) +
  scale_color_manual(values=alpha(c('#6792DA', '#ECCC68','#DA5E5E'))) +
  scale_fill_manual(values=c('#6792DA', '#ECCC68','#DA5E5E')) +
  scale_linetype_manual(values=c("dotted","longdash","solid")) +
  guides(color = guide_legend(title = NULL), 
         fill = guide_legend(title = NULL),
         linetype = guide_legend(title = NULL)) 

# Stingray 6 -------------------------------------------------------------------

s6.depthVals<-bind_rows(s6.state.prob$Depth,.id="Activity") %>% rename(Depth=cov)
s6.depthVals$Activity <-factor(s6.depthVals$Activity, levels = c("Low activity",
                                                                 "Medium activity",
                                                                 "High activity"))

s6.depth<- ggplot(s6.depthVals,
                  aes(x=Depth,y=est,ymax=uci,ymin=lci,color=Activity,
                      fill=Activity, linetype=Activity)) +
  geom_line(linewidth=0.9)+
  geom_ribbon(alpha=0.55, linewidth=0.9) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), limits= c(0,1), breaks = seq(0.2,1, by=0.2)) +
  xlab("\nDepth (m)") +
  ylab("Stationary state probabilities \n") +
  theme(text=element_text(size = 20)) +
  theme(axis.text.x = element_text(vjust=0.5)) +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  theme(legend.position = c(0.59, 0.59)) +
  scale_color_manual(values=alpha(c('#6792DA', '#ECCC68','#DA5E5E'))) +
  scale_fill_manual(values=c('#6792DA', '#ECCC68','#DA5E5E')) +
  scale_linetype_manual(values=c("dotted","longdash","solid")) +
  guides(color = guide_legend(title = NULL), 
         fill = guide_legend(title = NULL),
         linetype = guide_legend(title = NULL)) 


# Stingray 7 -------------------------------------------------------------------

s7.depthVals<-bind_rows(s7.state.prob$Depth,.id="Activity") %>% rename(Depth=cov)
s7.depthVals$Activity <-factor(s7.depthVals$Activity, levels = c("Low activity",
                                                                 "Medium activity",
                                                                 "High activity"))

s7.depth<- ggplot(s7.depthVals,
                  aes(x=Depth,y=est,ymax=uci,ymin=lci,color=Activity,
                      fill=Activity, linetype=Activity)) +
  geom_line(linewidth=0.9)+
  geom_ribbon(alpha=0.55, linewidth=0.9) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), limits= c(0,1), breaks = seq(0.2,1, by=0.2)) +
  xlab("\nDepth (m)") +
  ylab("Stationary state probabilities \n") +
  theme(text=element_text(size = 20)) +
  theme(axis.text.x = element_text(vjust=0.5)) +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  theme(legend.position = c(0.79, 0.88)) +
  scale_color_manual(values=alpha(c('#6792DA', '#ECCC68','#DA5E5E'))) +
  scale_fill_manual(values=c('#6792DA', '#ECCC68','#DA5E5E')) +
  scale_linetype_manual(values=c("dotted","longdash","solid")) +
  guides(color = guide_legend(title = NULL), 
         fill = guide_legend(title = NULL),
         linetype = guide_legend(title = NULL)) 

# Stingray 9 -------------------------------------------------------------------

s9.depthVals<-bind_rows(s9.state.prob$Depth,.id="Activity") %>% rename(Depth=cov)
s9.depthVals$Activity <-factor(s9.depthVals$Activity, levels = c("Low activity",
                                                                   "Medium activity",
                                                                   "High activity"))

s9.depth<- ggplot(s9.depthVals,
                   aes(x=Depth,y=est,ymax=uci,ymin=lci,color=Activity,
                       fill=Activity, linetype=Activity)) +
  geom_line(linewidth=0.9)+
  geom_ribbon(alpha=0.55, linewidth=0.9) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), limits= c(0,1), breaks = seq(0.2,1, by=0.2)) +
  xlab("\nDepth (m)") +
  ylab("Stationary state probabilities \n") +
  theme(text=element_text(size = 20)) +
  theme(axis.text.x = element_text(vjust=0.5)) +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  theme(legend.position = c(0.79, 0.6)) +
  scale_color_manual(values=alpha(c('#6792DA', '#ECCC68','#DA5E5E'))) +
  scale_fill_manual(values=c('#6792DA', '#ECCC68','#DA5E5E')) +
  scale_linetype_manual(values=c("dotted","longdash","solid")) +
  guides(color = guide_legend(title = NULL), 
         fill = guide_legend(title = NULL),
         linetype = guide_legend(title = NULL)) 

# Multipanel plot --------------------------------------------------------------

# adjust margins for panel labels (stingray names)
margins <- margin (55,10,25,5, "points") #top, right, bottom, left

s1.depth <- s1.depth + theme(plot.margin = margins)
s6.depth <- s6.depth + theme(plot.margin = margins)
s7.depth <- s7.depth + theme(plot.margin = margins)
s9.depth <- s9.depth + theme(plot.margin = margins)

# make multipanel figure from ggplots above
final.depth <- grid.arrange(arrangeGrob(s1.depth + theme(legend.position = "none", 
                                                       axis.title.y = element_blank(),
                                                       axis.title.x = element_blank())),
                           arrangeGrob(s6.depth + theme(legend.position = "none", 
                                                       axis.title.y = element_blank(),
                                                       axis.title.x = element_blank())), 
                           arrangeGrob(s7.depth + theme(#legend.position = "none", 
                                                       axis.title.y = element_blank(),
                                                       axis.title.x = element_blank())),
                           arrangeGrob(s9.depth + theme(legend.position = "none", 
                                                        axis.title.y = element_blank(),
                                                        axis.title.x = element_blank())),
                           ncol = 2, # columns
                           nrow = 2) # rows


# add stingray names above plots
grid.text("Stingray 1", x = 0.25, y = 0.97, gp = gpar(fontsize = 22))
grid.text("Stingray 6", x = 0.75, y = 0.97, gp = gpar(fontsize = 22))
grid.text("Stingray 7", x = 0.25, y = 0.47, gp = gpar(fontsize = 22))
grid.text("Stingray 9", x = 0.75, y = 0.47, gp = gpar(fontsize = 22))



