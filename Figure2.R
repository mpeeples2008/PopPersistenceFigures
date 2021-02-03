library(tidyverse)
library(ggplot2)
library(ggpubr)
library(scales)
library(magick)

bm <- read.csv('BasinOfMexico.csv')

bm.p <- ggplot(bm, aes(x=Half.Period)) +
  geom_histogram(binwidth=200) +
  theme(plot.title = element_text(size=20, face="bold.italic"), panel.grid.major.y = element_line('darkgray', size=0.25), panel.grid.minor.y=element_blank(), 
        panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), panel.background=element_blank(),
        axis.line = element_line(color='black', size = 1, linetype = "solid"),
        text=element_text(size=20)) +
  geom_vline(aes(xintercept=mean(Half.Period, na.rm=T)), color='red', size=1) +
  geom_vline(aes(xintercept=median(Half.Period, na.rm=T)), color='blue', size=1) +
  geom_vline(aes(xintercept=mean(Half.Period, na.rm=T)+(1*sd(Half.Period, na.rm=T))), linetype='dashed', color='red', size=1) +
  geom_vline(aes(xintercept=mean(Half.Period, na.rm=T)+(2*sd(Half.Period, na.rm=T))), linetype='dashed', color='red', size=1) +
  ggtitle("Basin of Mexico") +
  scale_x_continuous(name="Years",limits = c(0,3100)) +
  scale_y_continuous(name="Count") +
  annotate('text',x=2700,y=1625, label='median = 185',size=5, col='blue') +
  annotate('text',x=2750,y=1550, label='mean = 298',size=5, col='red') +
  annotate('text',x=700,y=1550, label='+1 \u03c3',size=4, col='red') +
  annotate('text',x=1000,y=1550, label='+2 \u03c3',size=4, col='red') 


rome <- read.csv('RomanOccupationLengths.csv')

rome.p <- ggplot(rome, aes(x=OccLength)) +
  geom_histogram(binwidth=150) +
  theme(plot.title = element_text(size=20, face="bold.italic"), panel.grid.major.y = element_line('darkgray', size=0.25), panel.grid.minor.y=element_blank(), 
        panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), panel.background=element_blank(),
        axis.line = element_line(color='black', size = 1, linetype = "solid"),
        text=element_text(size=20)) +
  geom_vline(aes(xintercept=mean(OccLength, na.rm=T)), color='red', size=1) +
  geom_vline(aes(xintercept=mean(OccLength, na.rm=T)+sd(OccLength, na.rm=T)), linetype='dashed', color='red', size=1) +
  geom_vline(aes(xintercept=mean(OccLength, na.rm=T)+(2*sd(OccLength, na.rm=T))), linetype='dashed', color='red', size=1) +
  geom_vline(aes(xintercept=median(OccLength, na.rm=T)), color='blue', size=1) +
  ggtitle("Central Italy") +
  scale_x_continuous(name="Years") +
  scale_y_continuous(name="Count") +
  annotate('text',x=1625,y=4200, label='mean = 307',size=5, col='red') +
  annotate('text',x=1600,y=4400, label='median = 230',size=5, col='blue') +
  annotate('text',x=625,y=4200, label='+1 \u03c3',size=4, col='red') +
  annotate('text',x=850,y=4200, label='+2 \u03c3',size=4, col='red') 



ggsave(filename="bm.png", plot = bm.p, width = 150, height = 150, units = c("mm"), dpi = 1000)
ggsave(filename="rome_p.png", plot = rome.p, width = 150, height = 150, units = c("mm"), dpi = 1000)

bm.png <- image_read("bm.png")
rome_p.png <- image_read("rome_p.png")

All_hist.png <- image_append(c(bm.png,rome_p.png))
image_write(All_hist.png, path = "All_hist.png", format = "png")

