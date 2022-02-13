library(tidyverse)

# I. Read the cleaned_covid_data.csv file into an R data frame. (20 pts)
df <- read_csv('cleaned_covid_data.csv')

# II. Subset the data set to just show states that begin with “A” and save this 
# as an object called A_states. (20 pts)
A_states <- df %>% 
  filter(Province_State == c('Alabama', 'Alaska', "Arizona", 'Arkansas'))

# III. Create a plot of that subset showing Deaths over time, with a separate 
# facet for each state. (20 pts)

A_states %>%
  ggplot(aes(x = Last_Update, 
             y = Deaths)) +
  geom_point(color = 'purple',
             size = .7) +
  facet_wrap(~Province_State,
             scales = 'free') +
  geom_smooth(method = 'loess',
              se = FALSE,
              color = 'cyan') +
  scale_y_continuous(labels = scales::comma) +
  labs(y = 'Cumulative Deaths',
       title = 'COVID 19 Deaths in "A" States') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# IV. (Back to the full dataset) Find the “peak” of Case_Fatality_Ratio for each 
# state and save this as a new data frame object called state_max_fatality_rate. 
# (20 pts)

state_max_fatality_rate <- df %>%
  filter(Case_Fatality_Ratio > 0) %>% 
  group_by(Province_State) %>% 
  summarise(Max_Fatality_Rate = max(Case_Fatality_Ratio)) %>% 
  arrange(desc(Max_Fatality_Rate))

# V. Use that new data frame from task IV to create another plot. (20 pts)

state_max_fatality_rate %>% 
  mutate(Ordered_States = 
           factor(Province_State,
                  levels = state_max_fatality_rate$Province_State)) %>% 
  ggplot(aes(x=Ordered_States,
             y=Max_Fatality_Rate)) +
  geom_col(fill = 'darkred') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5,
                                  face = 'bold')) +
  labs(x = 'State',
       y = 'Fatality Rate (%)',
       title = 'Maximum Daily Fatality Rates')

# VI. (BONUS 10 pts) Using the FULL data set, plot cumulative deaths for the 
# entire US over time


us_cum_deaths <- df %>% 
  group_by(Last_Update) %>% 
  summarise(Cum_Death = sum(Deaths)) 

us_cum_deaths %>% 
  ggplot(aes(x = Last_Update,
             y = Cum_Death)) +
  geom_point(color = 'darkorange') +
  scale_y_continuous(labels = scales::comma) +
  labs(y = 'Cumulative Deaths',
       title = 'Total COVID 19 Deaths in US') +
  theme_minimal() +
  theme(axis.text.y = element_text(angle=45),
        plot.title = element_text(hjust = 0.5,
                                  face = 'bold'))
