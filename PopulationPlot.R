library(ggplot2)

dat <- read.csv('Rome.csv',header=T)

ggplot(dat) + 
  geom_line(aes(x = Year, y = Population), size = 1) +
  theme_bw()
