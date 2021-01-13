library(ggplot2)
library(ggpubr)
library(scales)
library(ggformula)
library(magick)

dat.rome <- read.csv('Rome.csv',header=T)
dat.london <- read.csv('London.csv',header=T)
dat.teo <- read.csv('Teo.csv',header=T)
dat.byz <- read.csv('Byzantium.csv',header=T)
dat.mex <- read.csv('MexicoCity.csv',header=T)

rome <- ggplot(dat.rome) + 
  geom_smooth(aes(x = Year, y = Population), size = 1.5, color='darkblue',span=0.25,se=F) +
  theme(plot.title = element_text(size=20, face="bold.italic"), panel.grid.major.y = element_line('darkgray', size=0.25), panel.grid.minor.y=element_blank(), 
        panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), panel.background=element_blank(),
        axis.line = element_line(color='black', size = 1, linetype = "solid"),
        text=element_text(size=18)) +
  ggtitle("Rome") +
  scale_x_continuous(name="Year", limits=c(-800, 1801), breaks=c(-800,-400,0,400,800,1200,1600),labels=c("800 BC", "400 BC", "BC/AD","AD 400", "AD 800","AD 1200","AD 1600")) +
  scale_y_continuous(name="Population in Millions", oob=squish, limits=c(0, 1200000), breaks=c(0,200000,400000,600000,800000,1000000,1200000), labels=c(0,0.2,0.4,0.6,0.8,1,1.2)) #+
  #geom_segment(aes(x=550, y=90000+360000, xend=550, yend=90000), arrow=arrow(), size=2, color="red1")

rome2 <- ggplot(dat.rome) + 
  geom_smooth(aes(x = Year, y = Population), size = 1.5, color='darkblue',span=0.25,se=F) +
  theme(plot.title = element_text(size=20, face="bold.italic"), panel.grid.major.y = element_line('darkgray', size=0.25), panel.grid.minor.y=element_blank(), 
        panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), panel.background=element_blank(),
        axis.line = element_line(color='black', size = 1, linetype = "solid"),
        text=element_text(size=18)) +
  ggtitle(" ") +
  scale_x_continuous(name="Year", limits=c(1800, 2021), breaks=c(1800,2000),labels=c("AD 1800","AD 2000")) +
  scale_y_continuous(name="Population in Millions", oob=squish, limits=c(0, 4000000), breaks=c(0,1000000,2000000,3000000,4000000), labels=c(0,1,2,3,4)) #+
  #geom_segment(aes(x=550, y=250000+210000, xend=550, yend=250000), arrow=arrow(), size=2, color="red1") 

byzantium <- ggplot(dat.byz) + 
  geom_smooth(aes(x = Year, y = Population), size = 1.5, color='darkblue',span=0.2,se=F) +
  theme(plot.title = element_text(size=20, face="bold.italic"), panel.grid.major.y = element_line('darkgray', size=0.25), panel.grid.minor.y=element_blank(), 
        panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), panel.background=element_blank(),
        axis.line = element_line(color='black', size = 1, linetype = "solid"),
        text=element_text(size=18)) +
  ggtitle("Byzantium/Constantinople/Istanbul") +
  scale_x_continuous(name="Year", limits=c(-800, 1801), breaks=c(-800,-400,0,400,800,1200,1600),labels=c("800 BC", "400 BC", "BC/AD","AD 400", "AD 800","AD 1200","AD 1600")) +
  scale_y_continuous(name="Population in Thousands", limits=c(0, 800000), breaks=c(0,200000,400000,600000,800000), labels=c(0,200,400,600,800)) #+
  #geom_segment(aes(x=650, y=110000+210000, xend=650, yend=110000), arrow=arrow(), size=2, color="red1") +
  #geom_segment(aes(x=1425, y=75000+210000, xend=1425, yend=75000), arrow=arrow(), size=2, color="red1") 


byzantium2 <- ggplot(dat.byz) + 
  geom_smooth(aes(x = Year, y = Population), size = 1.5, color='darkblue',span=0.25,se=F) +
  theme(plot.title = element_text(size=20, face="bold.italic"), panel.grid.major.y = element_line('darkgray', size=0.25), panel.grid.minor.y=element_blank(), 
        panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), panel.background=element_blank(),
        axis.line = element_line(color='black', size = 1, linetype = "solid"),
        text=element_text(size=18)) +
  ggtitle(" ") +
  scale_x_continuous(name="Year", limits=c(1800, 2021), breaks=c(1800,2000),labels=c("AD 1800","AD 2000")) +
  scale_y_continuous(name="Population in Millions", limits=c(0, 16000000), breaks=c(0,4000000,8000000,12000000,16000000), labels=c(0,4,8,12,16)) 

london <- ggplot(dat.london) + 
  geom_smooth(aes(x = Year, y = Population), size = 1.5, color='darkblue',span=0.15,se=F) +
  theme(plot.title = element_text(size=20, face="bold.italic"), panel.grid.major.y = element_line('darkgray', size=0.25), panel.grid.minor.y=element_blank(), 
        panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), panel.background=element_blank(),
        axis.line = element_line(color='black', size = 1, linetype = "solid"),
        text=element_text(size=18)) +
  ggtitle("Londinium/Lundenwic/Lundenburh/London") +
  scale_x_continuous(name="Year", limits=c(0, 1501), breaks=c(0,500,1000,1500),labels=c("BC/AD","AD 500", "AD 1000","AD 1500")) +
  scale_y_continuous(name="Population in Thousands", limits=c(0,100000), breaks=c(0,25000,50000,75000,100000), labels=c(0,25,50,75,100)) #+
  #geom_segment(aes(x=400, y=5000+30000, xend=400, yend=5000), arrow=arrow(), size=2, color="red1") 
  
london2 <- ggplot(dat.london) + 
  geom_smooth(aes(x = Year, y = Population), size = 1.5, color='darkblue',span=0.25,se=F) +
  theme(plot.title = element_text(size=20, face="bold.italic"), panel.grid.major.y = element_line('darkgray', size=0.25), panel.grid.minor.y=element_blank(), 
        panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), panel.background=element_blank(),
        axis.line = element_line(color='black', size = 1, linetype = "solid"),
        text=element_text(size=18)) +
  ggtitle(" ") +
  scale_x_continuous(name="Year", limits=c(1500, 2021), breaks=c(1500,2000),labels=c("AD 1500","AD 2000")) +
  scale_y_continuous(name="Population in Millions", limits=c(100000,10000000), breaks=c(0,2500000,5000000,7500000,10000000), labels=c(0,2.5,5,7.5,10)) 



teo <- ggplot(dat.teo) +
  geom_smooth(aes(x = Year, y = Population), size = 1.5, color='darkblue',span=0.16, se=F) +
  theme(plot.title = element_text(size=20, face="bold.italic"), panel.grid.major.y = element_line('darkgray', size=0.25), panel.grid.minor.y=element_blank(), 
        panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), panel.background=element_blank(),
        axis.line = element_line(color='black', size = 1, linetype = "solid"),
        text=element_text(size=18)) +
  ggtitle("Teotihuacán") +
  scale_x_continuous(name="Year", limits=c(-500, 2021), breaks=c(-500,0,500,1000,1500,2000),labels=c("500 BC", "BC/AD","AD 500", "AD 1000","AD 1500","AD 2000")) +
  scale_y_continuous(name="Population in Thousands", limits=c(0,110000), breaks=c(0,25000,50000,75000,100000), labels=c(0,25,50,75,100)) #+
  #geom_segment(aes(x=600, y=29000+30000, xend=600, yend=29000), arrow=arrow(), size=2, color="red1") + 
  #geom_segment(aes(x=1625, y=5000+30000, xend=1625, yend=5000), arrow=arrow(), size=2, color="red1") 

mex <- ggplot(dat.mex) +
  geom_smooth(aes(x = Year, y = Population), size = 1.5, color='darkblue',span=0.25,se=F) +
  theme(plot.title = element_text(size=20, face="bold.italic"), panel.grid.major.y = element_line('darkgray', size=0.25), panel.grid.minor.y=element_blank(), 
        panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), panel.background=element_blank(),
        axis.line = element_line(color='black', size = 1, linetype = "solid"),
        text=element_text(size=18)) +
  ggtitle("Tenochitlán/México City") +
  scale_x_continuous(name="Year", limits=c(1000, 1800), breaks=c(1000,1200,1400,1600,1800),labels=c("AD 1000", "AD 1200", "AD 1400", "AD 1600","AD 1800")) +
  scale_y_continuous(name="Population in Thousands", limits=c(0,250000), breaks=c(0,50000,100000,150000,200000,250000), labels=c(0,50,100,150,200,250)) #+
  #geom_segment(aes(x=1600, y=28000+75000, xend=1600, yend=28000), arrow=arrow(), size=2, color="red1")  
  
mex2 <- ggplot(dat.mex) +
  geom_smooth(aes(x = Year, y = Population), size = 1.5, color='darkblue',span=0.25,se=F) +
  theme(plot.title = element_text(size=20, face="bold.italic"), panel.grid.major.y = element_line('darkgray', size=0.25), panel.grid.minor.y=element_blank(), 
        panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), panel.background=element_blank(),
        axis.line = element_line(color='black', size = 1, linetype = "solid"),
        text=element_text(size=18)) +
  ggtitle(" ") +
  scale_x_continuous(name="Year", limits=c(1800, 2021), breaks=c(1800,2000),labels=c("AD 1800","AD 2000")) +
  scale_y_continuous(name="Population in Millions", limits=c(0,25000000), breaks=c(0,5000000,10000000,15000000,20000000,25000000), labels=c(0,5,10,15,20,25))

ggsave(filename="Rome.png", plot = rome, width = 220, height = 90, units = c("mm"), dpi = 1000)
ggsave(filename="Rome2.png", plot= rome2, width = 80, height= 90, units= c("mm"), dpi=1000)
ggsave(filename="Teo.png", plot = teo, width = 300, height = 90, units = c("mm"), dpi = 1000)
ggsave(filename="London1.png", plot = london, width = 180, height = 90, units = c("mm"), dpi = 1000)
ggsave(filename="London2.png", plot = london2, width = 120, height = 90, units = c("mm"), dpi = 1000)
ggsave(filename="Byzantium1.png", plot = byzantium, width = 220, height = 90, units = c("mm"), dpi = 1000)
ggsave(filename="Byzantium2.png", plot = byzantium2, width = 80, height = 90, units = c("mm"), dpi = 1000)
ggsave(filename="Mex1.png", plot = mex, width = 220, height = 90, units = c("mm"), dpi = 1000)
ggsave(filename="Mex2.png", plot = mex2, width = 80, height = 90, units = c("mm"), dpi = 1000)


Rome.png <- image_read("Rome.png")
Rome2.png <- image_read("Rome2.png")
Teo.png <- image_read("Teo.png")
London1.png <- image_read("London1.png")
London2.png <- image_read("London2.png")
Byzantium1.png <- image_read("Byzantium1.png")
Byzantium2.png <- image_read("Byzantium2.png")
Mex1.png <- image_read("Mex1.png")
Mex2.png <- image_read("Mex2.png")

London_all.png <- image_append(c(London1.png,London2.png))
Mex_all.png <- image_append(c(Mex1.png,Mex2.png))
Byzantium_all.png <- image_append(c(Byzantium1.png,Byzantium2.png))
Rome_all.png <- image_append(c(Rome.png,Rome2.png))


All.png <- image_append(c(Teo.png,Mex_all.png,Rome_all.png,Byzantium_all.png,London_all.png), stack=TRUE)

image_write(All.png, path = "All.png", format = "png")


