# Make a new Rproj and Rscript in your personal Assignment_8 directory and work from there.
# 
library(modelr)
library(easystats)
library(broom)
library(tidyverse)
library(fitdistrplus)

# Write a script that:
# 1. loads the “/Data/mushroom_growth.csv” data set

df <- read_csv("../../Data/mushroom_growth.csv")
df %>% glimpse()

# 2. creates several plots exploring relationships between the response and predictors

df %>% ggplot(aes(x = Species, 
                  y = GrowthRate)) +
  geom_boxplot()

df %>% ggplot(aes(x = Humidity, 
                  y = GrowthRate)) +
  geom_boxplot()

df %>% ggplot(aes(x = Nitrogen, 
                  y = GrowthRate)) +
  geom_point() +
  facet_wrap(~Species)

df %>% ggplot(aes(x = Nitrogen, 
                  y = GrowthRate,
                  color = Light,
                  shape = Humidity)) +
  geom_point() +
  facet_wrap(~Temperature)

# 3. defines at least 4 models that explain the dependent variable “GrowthRate”

mod1 <- df %>% glm(data = .,
                   formula = GrowthRate ~ Species) 
mod2 <- df %>% glm(data = .,
                   formula = GrowthRate ~ Humidity) 
mod3 <- df %>% glm(data = .,
                   formula = GrowthRate ~ Species + Nitrogen) 
mod4 <- df %>% glm(data = .,
                   formula = GrowthRate ~ Species + 
                     Light * Humidity + 
                     Nitrogen * Temperature) 
mod5 <- df %>% glm(data = .,
                   formula = GrowthRate ~ Species + 
                     Light * Humidity) 

# 4. calculates the mean sq. error of each model

mean(mod1$residuals^2)
mean(mod2$residuals^2)
mean(mod3$residuals^2)
mean(mod4$residuals^2)
mean(mod5$residuals^2)

# 5. selects the best model you tried

compare_models(mod1,mod2,mod3,mod4,mod5,style = "se_p") 
compare_performance(mod1,mod2,mod3,mod4,mod5) %>% plot()

# 6. adds predictions based on new hypothetical values for the independent variables used in your model

df_pred <- data.frame(Light = c(6,12,18,24,6,12,18,24,
                                6,12,18,24,6,12,18,24),
                      Humidity = c("Low","Low","Low","Low","Low","Low","Low","Low",
                                   "High","High","High","High","High","High","High","High"),
                      Species = c("P.cornucopiae","P.cornucopiae","P.cornucopiae","P.cornucopiae",
                                  "P.ostreotus","P.ostreotus","P.ostreotus","P.ostreotus",
                                  "P.cornucopiae","P.cornucopiae","P.cornucopiae","P.cornucopiae",
                                  "P.ostreotus","P.ostreotus","P.ostreotus","P.ostreotus"))

df_pred$Prediction <- predict(mod5, df_pred)
df$Type <- "Real"
df_pred$Type <- "Hypothesis"


# 7. plots these predictions alongside the real data

full_join(df,df_pred) %>% 
  ggplot(aes(x = Light, 
             y = GrowthRate,
             shape = Humidity)) +
  geom_point() +
  geom_smooth() +
  geom_point(aes(y = Prediction), color = "red") +
  facet_wrap(~Species)

# Upload responses to the following as a numbered plain text document to Canvas:
# 1. Are any of your predicted response values from your best model scientifically meaningless? Explain.
# 2. In your plots, did you find any non-linear relationships? Do a bit of research online and give a link
# to at least one resource explaining how to deal with modeling non-linear relationships in R.
# 3. Write the code you would use to model the data found in “/Data/non_linear_relationship.csv” with a
# linear model (there are a few ways of doing this)


