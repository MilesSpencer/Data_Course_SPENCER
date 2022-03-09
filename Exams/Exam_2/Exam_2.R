library(tidyverse)
library(modelr)
library(easystats)

# 1. Read in the unicef data (10 pts)
# 2. Get it into tidy format (10 pts) 
df <- read_csv("unicef-u5mr.csv") %>% 
  pivot_longer(cols = -c(CountryName, Continent, Region), #variables to keep
               names_to = 'year', #name of pivot variable
               values_to = "u5mr") %>% #name of values related to pivot variable
  separate(col = year, #break year into two columns
           into = c("extra", "year"), #U5MR is extra
           convert = TRUE) %>% #make year values into int
  select(-extra) #remove the extra since it isn't a variable.

# 3. Plot each country’s U5MR over time (20 points)
# - Create a line plot (not a smooth trend line) for each country
# - Facet by continent

df %>%
  group_by(CountryName) %>% #each line needs to be single country
  ggplot(aes(x = year, y = u5mr)) +
  geom_path(size = 0.1) +
  facet_wrap(~Continent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Year", y = "U5MR")


# 4. Save this plot as LASTNAME_Plot_1.png (5 pts) 

ggsave("SPENCER_Plot_1.png", dpi = 300)

# 5. Create another plot that shows the mean U5MR for all the countries within a 
# given continent at each year (20 pts)
# - Another line plot (not smooth trendline)
# - Colored by continent

df %>% 
  group_by(year, Continent) %>% 
  summarize(Mean_U5MR = mean(u5mr, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = Mean_U5MR, color = Continent)) +
  geom_path(size = 1.5) +
  theme_minimal() +
  labs(x = "Year", y = "Mean U5MR")

# 6. Save that plot as LASTNAME_Plot_2.png (5 pts) 

ggsave("SPENCER_Plot_2.png", dpi = 300)

# 7. Create three models of U5MR (20 pts)
# - mod1 should account for only Year
# - mod2 should account for Year and Continent
# - mod3 should account for Year, Continent, and their interaction term

mod1 <- df %>% 
  glm(data = .,
      formula = u5mr ~ year)

mod2 <- df %>% 
  glm(data = .,
      formula = u5mr ~ year + Continent)

mod3 <- df %>% 
  glm(data = .,
      formula = u5mr ~ year * Continent)

# 8. Compare the three models with respect to their performance
# - Your code should do the comparing
# - Include a comment line explaining which of these three models you think is best

compare_models(mod1,mod2,mod3,style='se_p')
compare_performance(mod1,mod2,mod3) %>% plot()
#looking at the plot of the performance we can see that mod3 is the best model 
#because it is furthest from center on all 7 points

# 9. Plot the 3 models’ predictions like so: (10 pts)

df %>% 
  gather_predictions(mod1,mod2,mod3) %>% 
  ggplot(aes(x = year,y = pred,color=Continent)) +
  geom_smooth(method = 'lm') +
  facet_wrap(~model) +
  labs(y = "Predicted U5MR")

ggsave("SPENCER_Plot_3.png", dpi = 300)

# 10. BONUS - Using your preferred model, predict what the U5MR would be for 
# Ecuador in the year 2020. The real value for Ecuador for 2020 was 13 under-5 
# deaths per 1000 live births. How far off was your model prediction??

df %>% 
  filter(CountryName == "Ecuador") %>% 
  ggplot(aes(x = year, y = u5mr)) +
  geom_point()

mod4 <- df %>% 
  glm(data=.,formula = u5mr ~ poly(year, 3) + Region + Continent) 
  
df %>% 
  filter(CountryName == "Ecuador") %>% 
  gather_predictions(mod4) %>% 
  ggplot(aes(x = year, y = u5mr)) +
  geom_point() +
  geom_path(aes(y = pred))
  
ecu_pred <- predict(mod4, 
                    newdata = data.frame(Continent = "Americas",
                                         year = 2020,
                                         Region = "South America"))
ecu_pred
ecu_residual <- 13 - ecu_pred
ecu_residual
