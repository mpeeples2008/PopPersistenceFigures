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

rome <- ggplot(dat.rome) + 
  geom_line(aes(x = Year, y = Population), size = 1.5, color='turquoise4') +
  theme(panel.grid.major.y = element_line('darkgray', size=0.25), panel.grid.minor.y=element_blank(), panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
        panel.background = element_rect(fill = "#2D2D2D"), axis.line = element_line(color='white', size = 1, linetype = "solid")) +
  scale_x_continuous(name="Year", limits=c(-1000, 2020), breaks=c(-1000,-500,0,500,1000,1500,2000),labels=c("1000 BC", "500 BC", "BC/AD","AD 500", "AD 1000","AD 1500","AD 2000")) +
  scale_y_continuous(name="Population in Millions", limits=c(0, 3000000), breaks=c(0,500000,1000000,1500000,2000000,2500000,3000000), labels=c(0,0.5,1,1.5,2,2.5,3)) +
  annotate("rect", xmin = -753, xmax = -750, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) +
  annotate("text", label="Founding of Rome", x = -730, y =2900000, color="orange1", angle=270, hjust=0) +
  annotate("rect", xmin = -509, xmax = -503, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) +
  annotate("text", label="Roman Republic", x = -483, y =2900000, color="orange1", angle=270, hjust=0) +
  annotate("rect", xmin = -27, xmax = -23, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) +
  annotate("text", label="Roman Empire", x = 3, y =2900000, color="orange1", angle=270, hjust=0) +
  annotate("rect", xmin = 359, xmax = 410, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) +
  annotate("text", label="Capitol moves/sacking of Rome", x = 430, y =2900000, color="orange1", angle=270, hjust=0) +
  annotate("rect", xmin = 1380, xmax = 1600, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) +
  annotate("text", label="Church funded reconstruction", x = 1620, y =2900000, color="orange1", angle=270, hjust=0) +
  annotate("rect", xmin = 1861, xmax = 1880, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) +
  annotate("text", label="Italian reunification", x = 1900, y =2900000, color="orange1", angle=270, hjust=0) 
  
byzantium <- ggplot(dat.byz1) + 
  geom_line(aes(x = Year, y = Population), size = 1.5, color='turquoise4') +
  theme(panel.grid.major.y = element_line('darkgray', size=0.25), panel.grid.minor.y=element_blank(), panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
        panel.background = element_rect(fill = "#2D2D2D"), axis.line = element_line(color='white', size = 1, linetype = "solid")) +
  scale_x_continuous(name="Year", limits=c(-1000, 1500), breaks=c(-1000,-500,0,500,1000,1500),labels=c("1000 BC", "500 BC", "BC/AD","AD 500", "AD 1000","AD 1500")) +
  scale_y_continuous(name="Population in Thousands", limits=c(0, 250000), breaks=c(0,50000,100000,150000,200000,250000), labels=c(0,50,100,150,200,250)) 

byzantium2 <- ggplot(dat.byz2) + 
  geom_line(aes(x = Year, y = Population), size = 1.5, color='turquoise4') +
  theme(panel.grid.major.y = element_line('darkgray', size=0.25), panel.grid.minor.y=element_blank(), panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
        panel.background = element_rect(fill = "#2D2D2D"), axis.line = element_line(color='white', size = 1, linetype = "solid")) +
  scale_x_continuous(name="Year", limits=c(1500, 2020), breaks=c(1500,2000),labels=c("AD 1500","AD 2000")) +
  scale_y_continuous(name="Population in Millions", limits=c(0, 10000000), breaks=c(0,2500000,5000000,7500000,10000000), labels=c(0,2.5,5,7.5,10)) 


london <- ggplot(dat.london1) + 
  geom_line(aes(x = Year, y = Population), size = 1.5, color='turquoise4') +
  theme(panel.grid.major.y = element_line('darkgray', size=0.25), panel.grid.minor.y=element_blank(), panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
        panel.background = element_rect(fill = "#2D2D2D"), axis.line = element_line(color='white', size = 1, linetype = "solid")) +
  scale_x_continuous(name="Year", limits=c(-1000, 1500), breaks=c(-1000,-500,0,500,1000,1500),labels=c("1000 BC", "500 BC", "BC/AD","AD 500", "AD 1000","AD 1500")) +
  scale_y_continuous(name="Population in Thousands", limits=c(0,100000), breaks=c(0,25000,50000,75000,100000), labels=c(0,25,50,75,100)) +
  annotate("rect", xmin = 47, xmax = 57, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) +
  annotate("text", label="Founding of Roman Londinium", x = 77, y =95000, color="orange1", angle=270, hjust=0) +
  annotate("rect", xmin = 400, xmax = 410, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) +
  annotate("text", label="Abaondonment of Londinium", x = 430, y =95000, color="orange1", angle=270, hjust=0) +
  annotate("rect", xmin = 600, xmax = 610, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) +
  annotate("text", label="Anglo-Saxon Lundenwic", x = 630, y =95000, color="orange1", angle=270, hjust=0) +
  annotate("rect", xmin = 1066, xmax = 1076, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) +
  annotate("text", label="Norman London", x = 1096, y =95000, color="orange1", angle=270, hjust=0) 

london2 <- ggplot(dat.london2) + 
  geom_line(aes(x = Year, y = Population), size = 1.5, color='turquoise4') +
  theme(panel.grid.major.y = element_line('darkgray', size=0.25), panel.grid.minor.y=element_blank(), panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
        panel.background = element_rect(fill = "#2D2D2D"), axis.line = element_line(color='white', size = 1, linetype = "solid")) +
  scale_x_continuous(name="Year", limits=c(1500, 2020), breaks=c(1500,2000),labels=c("AD 1500","AD 2000")) +
  scale_y_continuous(name="Population in Millions", limits=c(100000,10000000), breaks=c(0,2500000,5000000,7500000,10000000), labels=c(0,2.5,5,7.5,10)) +
  annotate("rect", xmin = 1707, xmax = 1727, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) +
  annotate("text", label="Kingdom of Great Britain", x = 1747, y =9500000, color="orange1", angle=270, hjust=0) +
  annotate("rect", xmin = 1801, xmax = 1821, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) +
  annotate("text", label="United Kingdom", x = 1841, y =9500000, color="orange1", angle=270, hjust=0) +
  annotate("rect", xmin = 1922, xmax = 1932, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) +
  annotate("text", label="Irish Free State", x = 1952, y =9500000, color="orange1", angle=270, hjust=0) 

teo <- ggplot(dat.teo) +
  geom_line(aes(x = Year, y = Population), size = 1.5, color='turquoise4') +
  theme(panel.grid.major.y = element_line('darkgray', size=0.25), panel.grid.minor.y=element_blank(), panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
        panel.background = element_rect(fill = "#2D2D2D"), axis.line = element_line(color='white', size = 1, linetype = "solid")) +
  scale_x_continuous(name="Year", limits=c(-1000, 2020), breaks=c(-1000,-500,0,500,1000,1500,2000),labels=c("1000 BC", "500 BC", "BC/AD","AD 500", "AD 1000","AD 1500","AD 2000")) +
  scale_y_continuous(name="Population in Thousands", limits=c(0,110000), breaks=c(0,25000,50000,75000,100000), labels=c(0,25,50,75,100)) +
  annotate("rect", xmin = -150, xmax = -140, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) +
  annotate("text", label="Founding of Teotihuacan", x = -120, y =110000, color="orange1", angle=270, hjust=0) +
  annotate("rect", xmin = 500, xmax = 600, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) +
  annotate("text", label="Depopulation of urban core", x = 620, y =110000, color="orange1", angle=270, hjust=0) +
  annotate("rect", xmin = 1150, xmax = 1350, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) +
  annotate("text", label="Early Aztec period", x = 1370, y =110000, color="orange1", angle=270, hjust=0) +
  annotate("rect", xmin = 1521, xmax = 1531, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) +
  annotate("text", label="Spanish conquest", x = 1551, y =110000, color="orange1", angle=270, hjust=0) 


ggsave(filename="Rome.png", plot = rome, width = 300, height = 90, units = c("mm"), dpi = 300)
ggsave(filename="Teo.png", plot = teo, width = 300, height = 90, units = c("mm"), dpi = 300)
ggsave(filename="London1.png", plot = london, width = 220, height = 90, units = c("mm"), dpi = 300)
ggsave(filename="London2.png", plot = london2, width = 80, height = 90, units = c("mm"), dpi = 300)
ggsave(filename="Byzantium1.png", plot = byzantium, width = 220, height = 90, units = c("mm"), dpi = 300)
ggsave(filename="Byzantium2.png", plot = byzantium2, width = 80, height = 90, units = c("mm"), dpi = 300)


ggarrange(teo, rome, ggarrange(byzantium,byzantium2,ncol=2,nrow=1), ggarrange(london,london2,ncol=2,nrow=1), ncol = 1, nrow = 4)

