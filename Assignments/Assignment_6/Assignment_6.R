library(tidyverse)
library(gganimate)
library(janitor)

dat <- read_csv("../../Data/BioLog_Plate_Data.csv")

dat %>% glimpse

dat_clean <- dat %>% 
  pivot_longer(cols = c('Hr_24','Hr_48','Hr_144'),
               #cols = starts_with('Hr_')
               names_to = 'Time',
               values_to = 'Absorbance') %>%
               #names_prefix = 'Hr_' Have to do mutate to change to dbl
  mutate(Time = case_when( #change the values in the time column to doubles
    Time == 'Hr_24' ~ 24,
    Time == 'Hr_48' ~ 48,
    Time == 'Hr_144' ~ 144)) %>% 
  mutate(Type = case_when( #create type column to separate the water and soil
    `Sample ID` == 'Clear_Creek' ~ 'Water',
    `Sample ID` == 'Waste_Water' ~ 'Water',
    # TRUE ~ 'Soil' makes everything else other than previously defined
    `Sample ID` == 'Soil_1' ~ 'Soil',
    `Sample ID` == 'Soil_2' ~ 'Soil')) %>% 
  rename(Time_Hr = Time) %>% 
  clean_names()

dat_clean %>% glimpse

dat_clean %>% 
  filter(dilution == 0.1) %>% 
  ggplot(aes(x = time_hr,
             y = absorbance,
             color = type)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~substrate) +
  theme_minimal() +
  labs(title = 'Just dilution 0.1',
       x = 'Time (hrs)',
       y = 'Absorbance (AU)',
       color = 'Type') +
  theme(strip.text.x = element_text(size = 7),
        panel.grid.major = element_line(color = 'grey97'),
        panel.grid.minor = element_line(color = 'grey97'),
        plot.background = element_rect(fill = 'grey95'),
        title = element_text(size = 9))

ggsave('graph_1.png', dpi = 400)

# absorbance values are mean 
# of all 3 replicates for each group):
# This plot is just showing values for the substrate “Itaconic Acid”

dat_clean %>% 
  filter(substrate == 'Itaconic Acid') %>% 
  select(-c(well,type)) %>% 
  group_by(time_hr, dilution, sample_id) %>% 
  summarise(average = mean(absorbance)) %>% 
    ggplot(aes(x = time_hr,
               y = average,
               color = sample_id)) +
    geom_line() +
    facet_wrap(~dilution) +
    transition_reveal(time_hr)

?anim_save
anim_save('graph_2.gif')


?group_by

# dat_clean %>% 
#   filter(substrate == 'L-Arginine') %>% 
#   ggplot(aes(x = time_hr,y=absorbance)) +
#   geom_point() +
#   facet_wrap(~rep) +
#   gganimate::transition_reveal(time_hr)





