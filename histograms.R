library(tidyverse)
library(ggplot2)
library(ggpubr)
library(scales)
library(magick)

maya <- read.csv('MayaLongCount.csv')

maya.p <- ggplot(maya, aes(x=YearSpan)) +
  geom_histogram(binwidth=50) +
  theme(plot.title = element_text(size=20, face="bold.italic"), panel.grid.major.y = element_line('darkgray', size=0.25), panel.grid.minor.y=element_blank(), 
        panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), panel.background=element_blank(),
        axis.line = element_line(color='black', size = 1, linetype = "solid"),
        text=element_text(size=18)) +
  geom_vline(aes(xintercept=mean(YearSpan)), color='red', size=1) +
  geom_vline(aes(xintercept=mean(YearSpan)+(1*sd(YearSpan))), color='blue', linetype='dashed', size=1) +
  ggtitle("Maya Long Count Dates") +
  scale_x_continuous(name="Years") +
  scale_y_continuous(name="Count") 


sw <- read.csv('occ.csv')
sw$occ <- sw$end-sw$begin

sw.p <- ggplot(sw, aes(x=occ)) +
  geom_histogram(binwidth=50) +
  theme(plot.title = element_text(size=20, face="bold.italic"), panel.grid.major.y = element_line('darkgray', size=0.25), panel.grid.minor.y=element_blank(), 
        panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), panel.background=element_blank(),
        axis.line = element_line(color='black', size = 1, linetype = "solid"),
        text=element_text(size=18)) +
  geom_vline(aes(xintercept=mean(occ)), color='red', size=1) +
  geom_vline(aes(xintercept=mean(occ)+(1*sd(occ))), color='blue', linetype='dashed', size=1) +
  ggtitle("U.S. Southwest Settlements") +
  scale_x_continuous(name="Years") +
  scale_y_continuous(name="Count") 

bm <- read.csv('BoMOccLengths.csv')

bm.p <- ggplot(bm, aes(x=TeoOnly)) +
  geom_histogram(binwidth=200) +
  theme(plot.title = element_text(size=20, face="bold.italic"), panel.grid.major.y = element_line('darkgray', size=0.25), panel.grid.minor.y=element_blank(), 
        panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), panel.background=element_blank(),
        axis.line = element_line(color='black', size = 1, linetype = "solid"),
        text=element_text(size=18)) +
  geom_vline(aes(xintercept=mean(TeoOnly, na.rm=T)), color='red', size=1) +
  geom_vline(aes(xintercept=mean(TeoOnly, na.rm=T)+(1*sd(TeoOnly, na.rm=T))), color='blue', linetype='dashed', size=1) +
  ggtitle("Basin of Mexico Settlements") +
  scale_x_continuous(name="Years") +
  scale_y_continuous(name="Count") 

rome <- read.csv('RomanOccupationLengths.csv')

rome.p <- ggplot(rome, aes(x=OccLength)) +
  geom_histogram(binwidth=150) +
  theme(plot.title = element_text(size=20, face="bold.italic"), panel.grid.major.y = element_line('darkgray', size=0.25), panel.grid.minor.y=element_blank(), 
        panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), panel.background=element_blank(),
        axis.line = element_line(color='black', size = 1, linetype = "solid"),
        text=element_text(size=18)) +
  geom_vline(aes(xintercept=mean(OccLength, na.rm=T)), color='red', size=1) +
  geom_vline(aes(xintercept=mean(OccLength, na.rm=T)+(1*sd(OccLength, na.rm=T))), color='blue', linetype='dashed', size=1) +
  ggtitle("Roman Settlements - Central Italy") +
  scale_x_continuous(name="Years") +
  scale_y_continuous(name="Count") 


ggsave(filename="maya.png", plot = maya.p, width = 300, height = 300, units = c("mm"), dpi = 1000)
ggsave(filename="sw.png", plot= sw.p, width = 300, height= 300, units= c("mm"), dpi=1000)
ggsave(filename="bm.png", plot = bm.p, width = 300, height = 300, units = c("mm"), dpi = 1000)
ggsave(filename="rome_p.png", plot = rome.p, width = 300, height = 300, units = c("mm"), dpi = 1000)

maya.png <- image_read("maya.png")
sw.png <- image_read("sw.png")
bm.png <- image_read("bm.png")
rome_p.png <- image_read("rome_p.png")

layer1 <- image_append(c(maya.png,bm.png))
layer2 <- image_append(c(rome_p.png,sw.png))

All_hist.png <- image_append(c(layer1, layer2), stack=TRUE)
image_write(All_hist.png, path = "All_hist.png", format = "png")

