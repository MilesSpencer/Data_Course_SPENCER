library(tidyverse)

df <- read.csv('random_fake_data.csv')

df %>% 
  ggplot(aes(x=Month,y=Ave_Time)) +
  geom_col() +
  scale_x_discrete(limits=df$Month)
