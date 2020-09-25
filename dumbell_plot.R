library(ggplot2)
library(ggalt)
library(tidyr)
library(dplyr)

df <- read.csv('SiteDates.csv',header=T)
df <- df[,-10]

df %>% 
  mutate(difference=abs(End.Occupation-Start.Occupation)) %>%
  ggplot(aes(y=reorder(Site.City, -Start.Occupation), x=Start.Occupation, xend=End.Occupation)) +
  geom_dumbbell(size=1, color="yellow",
                colour_x = "yellow", colour_xend = "yellow") +
  labs(x='Years A.D.', y=NULL, title="U.S. Southwest") +
  #geom_dumbbell(aes(y=Site.City,x=Start.Peak, xend=End.Peak), size=1, color="purple2",
  #             colour_x = "purple2", colour_xend = "purple2") +
  theme_dark()
