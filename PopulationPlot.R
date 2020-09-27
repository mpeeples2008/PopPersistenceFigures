library(ggplot2)
library(egg)

dat <- read.csv('Rome.csv',header=T)

rome <- ggplot(dat) + 
  geom_line(aes(x = Year, y = Population), size = 1.5, color='turquoise4') +
  theme(panel.grid.major.y = element_line('darkgray', size=0.25), panel.grid.minor.y=element_blank(), panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
        panel.background = element_rect(fill = "#2D2D2D"), axis.line = element_line(color='white', size = 1, linetype = "solid")) +
  #theme_dark() +
  scale_x_continuous(name="Year", limits=c(-1000, 2020), breaks=c(-1000,-500,0,500,1000,1500,2000),labels=c("1000 BC", "500 BC", "BC/AD","AD 500", "AD 1000","AD 1500","AD 2000")) +
  scale_y_continuous(name="Population in Millions", limits=c(0, 3000000), breaks=c(0,500000,1000000,1500000,2000000,2500000,3000000), labels=c(0,0.5,1,1.5,2,2.5,3)) +
  annotate("rect", xmin = -753, xmax = -750, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) +
  annotate("text", label="Founding of Rome", x = -730, y =2900000, color="orange1", angle=270, hjust=0) +
  annotate("rect", xmin = -509, xmax = -503, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) +
  annotate("text", label="Roman Republic Founded", x = -483, y =2900000, color="orange1", angle=270, hjust=0) +
  annotate("rect", xmin = -27, xmax = -23, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) +
  annotate("text", label="Empire established with Rome as capitol", x = 3, y =2900000, color="orange1", angle=270, hjust=0) +
  annotate("rect", xmin = 359, xmax = 410, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) +
  annotate("text", label="Imperial capitol moves and sacking of Rome", x = 430, y =2900000, color="orange1", angle=270, hjust=0) +
  annotate("rect", xmin = 1380, xmax = 1600, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) +
  annotate("text", label="Church funded reconstruction of Rome", x = 1620, y =2900000, color="orange1", angle=270, hjust=0) +
  annotate("rect", xmin = 1861, xmax = 1880, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) +
  annotate("text", label="Italian reunification and Rome established as capitol", x = 1900, y =2900000, color="orange1", angle=270, hjust=0) 
  
  
london <- ggplot(dat.london) + 
  geom_line(aes(x = Year, y = Population), size = 1.5, color='turquoise4') +
  theme(panel.grid.major.y = element_line('darkgray', size=0.25), panel.grid.minor.y=element_blank(), panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
        panel.background = element_rect(fill = "#2D2D2D"), axis.line = element_line(color='white', size = 1, linetype = "solid")) +
  scale_x_continuous(name="Year", limits=c(-1000, 2020), breaks=c(-1000,-500,0,500,1000,1500,2000),labels=c("1000 BC", "500 BC", "BC/AD","AD 500", "AD 1000","AD 1500","AD 2000")) +
  scale_y_continuous(name="Population in Thousands", limits=c(0,100000), breaks=c(0,25000,50000,75000,100000), labels=c(0,25,50,75,100)) +
  annotate("rect", xmin = 47, xmax = 57, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) +
  annotate("text", label="Founding of Roman Londinium", x = 77, y =95000, color="orange1", angle=270, hjust=0) +
  annotate("rect", xmin = 400, xmax = 410, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) +
  annotate("text", label="Abaondonment of Londinium", x = 430, y =95000, color="orange1", angle=270, hjust=0) +
  annotate("rect", xmin = 600, xmax = 610, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) +
  annotate("text", label="Anglo-Saxon Lundenwic", x = 630, y =95000, color="orange1", angle=270, hjust=0) +
  annotate("rect", xmin = 1066, xmax = 1076, ymin = -Inf, ymax = Inf, fill = "orange1", alpha = 0.6) +
  annotate("text", label="Norman London", x = 1096, y =95000, color="orange1", angle=270, hjust=0) 

ggarrange(rome, rome, ncol = 1, nrow = 2, labels=c('Rome','Teotihuacan'))