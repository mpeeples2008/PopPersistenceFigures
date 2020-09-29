library(ggplot2)
library(ggpubr)

dat.rome <- read.csv('Rome.csv',header=T)
dat.london <- read.csv('London.csv',header=T)
dat.london1 <- dat.london[1:17,]
dat.london2 <- dat.london[18:nrow(dat.london),]
dat.teo <- read.csv('Teo.csv',header=T)
dat.byz <- read.csv('Byzantium.csv',header=T)
dat.byz1 <- dat.byz[1:20,]
dat.byz2 <- dat.byz[21:nrow(dat.byz),]
dat.mex <- read.csv('MexicoCity.csv',header=T)

rome <- ggplot(dat.rome) + 
  geom_line(aes(x = Year, y = Population), size = 1.5, color='turquoise4') +
  theme(panel.grid.major.y = element_line('darkgray', size=0.25), panel.grid.minor.y=element_blank(), panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
        panel.background = element_rect(fill = "#2D2D2D"), axis.line = element_line(color='white', size = 1, linetype = "solid")) +
  scale_x_continuous(name="Year", limits=c(-1000, 2020), breaks=c(-1000,-500,0,500,1000,1500,2000),labels=c("1000 BC", "500 BC", "BC/AD","AD 500", "AD 1000","AD 1500","AD 2000")) +
  scale_y_continuous(name="Population in Millions", limits=c(0, 4000000), breaks=c(0,500000,1000000,1500000,2000000,2500000,3000000,3500000,4000000), labels=c(0,0.5,1,1.5,2,2.5,3,3.5,4)) +
  annotate("rect", xmin = 550, xmax = 600, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) 
  
byzantium <- ggplot(dat.byz1) + 
  geom_line(aes(x = Year, y = Population), size = 1.5, color='turquoise4') +
  theme(panel.grid.major.y = element_line('darkgray', size=0.25), panel.grid.minor.y=element_blank(), panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
        panel.background = element_rect(fill = "#2D2D2D"), axis.line = element_line(color='white', size = 1, linetype = "solid")) +
  scale_x_continuous(name="Year", limits=c(-1000, 1800), breaks=c(-1000,-500,0,500,1000,1500,1800),labels=c("1000 BC", "500 BC", "BC/AD","AD 500", "AD 1000","AD 1500","AD 1800")) +
  scale_y_continuous(name="Population in Thousands", limits=c(0, 710000), breaks=c(0,100000,200000,300000,400000,500000,600000,700000), labels=c(0,100,200,300,400,500,600,700)) +
  annotate("rect", xmin = 650, xmax = 700, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) 

byzantium2 <- ggplot(dat.byz2) + 
  geom_line(aes(x = Year, y = Population), size = 1.5, color='turquoise4') +
  theme(panel.grid.major.y = element_line('darkgray', size=0.25), panel.grid.minor.y=element_blank(), panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
        panel.background = element_rect(fill = "#2D2D2D"), axis.line = element_line(color='white', size = 1, linetype = "solid")) +
  scale_x_continuous(name="Year", limits=c(1800, 2020), breaks=c(1800,2000),labels=c("AD 1800","AD 2000")) +
  scale_y_continuous(name="Population in Millions", limits=c(0, 10000000), breaks=c(0,2500000,5000000,7500000,10000000), labels=c(0,2.5,5,7.5,10)) 

london <- ggplot(dat.london1) + 
  geom_line(aes(x = Year, y = Population), size = 1.5, color='turquoise4') +
  theme(panel.grid.major.y = element_line('darkgray', size=0.25), panel.grid.minor.y=element_blank(), panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
        panel.background = element_rect(fill = "#2D2D2D"), axis.line = element_line(color='white', size = 1, linetype = "solid")) +
  scale_x_continuous(name="Year", limits=c(-1000, 1500), breaks=c(-1000,-500,0,500,1000,1500),labels=c("1000 BC", "500 BC", "BC/AD","AD 500", "AD 1000","AD 1500")) +
  scale_y_continuous(name="Population in Thousands", limits=c(0,100000), breaks=c(0,25000,50000,75000,100000), labels=c(0,25,50,75,100)) +
  annotate("rect", xmin = 400, xmax = 450, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) 

london2 <- ggplot(dat.london2) + 
  geom_line(aes(x = Year, y = Population), size = 1.5, color='turquoise4') +
  theme(panel.grid.major.y = element_line('darkgray', size=0.25), panel.grid.minor.y=element_blank(), panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
        panel.background = element_rect(fill = "#2D2D2D"), axis.line = element_line(color='white', size = 1, linetype = "solid")) +
  scale_x_continuous(name="Year", limits=c(1500, 2020), breaks=c(1500,2000),labels=c("AD 1500","AD 2000")) +
  scale_y_continuous(name="Population in Millions", limits=c(100000,10000000), breaks=c(0,2500000,5000000,7500000,10000000), labels=c(0,2.5,5,7.5,10)) 

teo <- ggplot(dat.teo) +
  geom_line(aes(x = Year, y = Population), size = 1.5, color='turquoise4') +
  theme(panel.grid.major.y = element_line('darkgray', size=0.25), panel.grid.minor.y=element_blank(), panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
        panel.background = element_rect(fill = "#2D2D2D"), axis.line = element_line(color='white', size = 1, linetype = "solid")) +
  scale_x_continuous(name="Year", limits=c(-1000, 2020), breaks=c(-1000,-500,0,500,1000,1500,2000),labels=c("1000 BC", "500 BC", "BC/AD","AD 500", "AD 1000","AD 1500","AD 2000")) +
  scale_y_continuous(name="Population in Thousands", limits=c(0,110000), breaks=c(0,25000,50000,75000,100000), labels=c(0,25,50,75,100)) +
  annotate("rect", xmin = 1000, xmax = 1050, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) 
 
mex <- ggplot(dat.mex) +
  geom_line(aes(x = Year, y = Population), size = 1.5, color='turquoise4') +
  theme(panel.grid.major.y = element_line('darkgray', size=0.25), panel.grid.minor.y=element_blank(), panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
        panel.background = element_rect(fill = "#2D2D2D"), axis.line = element_line(color='white', size = 1, linetype = "solid")) +
  scale_x_continuous(name="Year", limits=c(-1000, 1800), breaks=c(-1000,-500,0,500,1000,1500,1800),labels=c("1000 BC", "500 BC", "BC/AD","AD 500", "AD 1000","AD 1500","AD 1800")) +
  scale_y_continuous(name="Population in Thousands", limits=c(0,250000), breaks=c(0,50000,100000,150000,200000,250000), labels=c(0,50,100,150,200,250)) +
  annotate("rect", xmin = 1600, xmax = 1650, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) 

mex2 <- ggplot(dat.mex) +
  geom_line(aes(x = Year, y = Population), size = 1.5, color='turquoise4') +
  theme(panel.grid.major.y = element_line('darkgray', size=0.25), panel.grid.minor.y=element_blank(), panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
        panel.background = element_rect(fill = "#2D2D2D"), axis.line = element_line(color='white', size = 1, linetype = "solid")) +
  scale_x_continuous(name="Year", limits=c(1800, 2021), breaks=c(1800,2000),labels=c("AD 1800","AD 2000")) +
  scale_y_continuous(name="Population in Millions", limits=c(0,25000000), breaks=c(0,5000000,10000000,15000000,20000000,25000000), labels=c(0,5,10,15,20,25))

ggsave(filename="Rome.png", plot = rome, width = 300, height = 90, units = c("mm"), dpi = 300)
ggsave(filename="Teo.png", plot = teo, width = 300, height = 90, units = c("mm"), dpi = 300)
ggsave(filename="London1.png", plot = london, width = 220, height = 90, units = c("mm"), dpi = 300)
ggsave(filename="London2.png", plot = london2, width = 80, height = 90, units = c("mm"), dpi = 300)
ggsave(filename="Byzantium1.png", plot = byzantium, width = 240, height = 90, units = c("mm"), dpi = 300)
ggsave(filename="Byzantium2.png", plot = byzantium2, width = 60, height = 90, units = c("mm"), dpi = 300)
ggsave(filename="Mex1.png", plot = mex, width = 240, height = 90, units = c("mm"), dpi = 300)
ggsave(filename="Mex2.png", plot = mex2, width = 60, height = 90, units = c("mm"), dpi = 300)


ggarrange(teo, rome, ggarrange(byzantium,byzantium2,ncol=2,nrow=1), ggarrange(london,london2,ncol=2,nrow=1), mex, ncol = 1, nrow = 5)

