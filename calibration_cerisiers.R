################################################################################

# -------------------------- Initialization ---------------------------------- #

################################################################################


########## Packages ------------------------------------------------------------

library(tidyverse)



########## Loading and cleaning the data ---------------------------------------

setwd("F:/Analyses/Vegetation/calibration/") #working directory

## Loading calibration data
cherry_trees_data <- read.csv2(
  "calibration_cerisiers.csv", sep = ";", dec = ".",
  colClasses = c("harvest_group" = "factor"))

## Rename the table
names(cherry_trees_data) <- c("ID", "species_fr", "species_en", "species_latin",
                              "diameter", "approximate_height", "harvest_group",
                              "weight", "bag_size", "bag_weight", "biomass")

head(cherry_trees_data)
summary(cherry_trees_data)




################################################################################

# ----------------------- Potential calibrations ----------------------------- #

################################################################################


########## Plots of the data ---------------------------------------------------

## Plot biomass ~ diameter to see if difference between the 2 species
ggplot(data = cherry_trees_data) +
  geom_point(aes(x = diameter, y = biomass, col = species_latin)) +
  theme_classic()
# No visual difference in trends

## Plot biomass ~ diameter to see if difference between the 4 harvesting groups
ggplot(data = cherry_trees_data) +
  geom_point(aes(x = diameter, y = biomass, col = harvest_group)) +
  theme_classic()
# No visual difference in trends

## Plot log(biomass) ~ diameter
ggplot(data = cherry_trees_data) +
  geom_point(aes(x = diameter, y = log(biomass), col = species_latin)) +
  theme_classic()
# Not very good

## Plot sqrt(biomass) ~ diameter
ggplot(data = cherry_trees_data) +
  geom_point(aes(x = diameter, y = sqrt(biomass), col = species_latin)) +
  theme_classic()

## ## Plot (biomass)^(1/3) ~ diameter
ggplot(data = cherry_trees_data) +
  geom_point(aes(x = diameter, y = biomass**(1/3), col = species_latin)) +
  theme_classic()
# The two last ones seem better


########## Finding the best regression ---------------------------------------

## biomass ~ diameter
model1 <- lm(data = cherry_trees_data, formula = biomass ~ diameter)
summary(model1)
par(mfrow = c(2, 2))
plot(model1)

## log(biomass) ~ diameter
model2 <- lm(data = cherry_trees_data, formula = log(biomass) ~ diameter)
summary(model2)
par(mfrow = c(2, 2))
plot(model2)

## sqrt(biomass) ~ diameter
model3 <- lm(data = cherry_trees_data, formula = sqrt(biomass) ~ diameter)
summary(model3)
par(mfrow = c(2, 2))
plot(model3)

## (biomass)^(1/3) ~ diameter
model4 <- lm(data = cherry_trees_data, formula = biomass**(1/3) ~ diameter)
summary(model4)
par(mfrow = c(2, 2))
plot(model4)

## AIC of the models
AIC(model1) #--> 371
AIC(model2) #--> 96
AIC(model3) #--> 127
AIC(model4) #--> 46


# It seems that the best overall model is the model4, with (biomass)^(1/3)


########## Plot the regression and compare with the data -----------------------

##### Build model predictions

## x vector (diameter)
diameter <- c(0:60)

## model 1
biomass_model1 <- 5.0353 * diameter - 48.5234  #predicted weights according 
                                               #to the model 1
pred_model1 <- data.frame(diameter, biomass_model1)

## model 2
biomass_model2 <- exp(0.10458*diameter)  #predicted weights according 
                                         #to the model 2
pred_model2 <- data.frame(diameter, biomass_model2)

## model 3
biomass_model3 <- (0.28238*diameter)**2
pred_model3 <- data.frame(diameter, biomass_model3)

## model 4
biomass_model4 <- (0.757295 + 0.100278*diameter)**3
pred_model4 <- data.frame(diameter, biomass_model4)


##### Plot predictions and data

## For the model 1
ggplot() +
  geom_point(data = cherry_trees_data, aes(x = diameter, y = biomass), 
             col = "blue") + #actual data
  geom_line(data = pred_model1, aes(x = diameter, y = biomass_model1), 
            col = "red") + #predictions
  theme_classic()

## For the model 2
ggplot() +
  geom_point(data = cherry_trees_data, aes(x = diameter, y = biomass), 
             col = "blue") + #actual data
  geom_line(data = pred_model2, aes(x = diameter, y = biomass_model2), 
            col = "red") + #predictions
  theme_classic()

## For the model 3
ggplot() +
  geom_point(data = cherry_trees_data, aes(x = diameter, y = biomass), 
             col = "blue") + #actual data
  geom_line(data = pred_model3, aes(x = diameter, y = biomass_model3), 
            col = "red") + #predictions
  theme_classic()

# For the model 4
ggplot() +
  geom_point(data = cherry_trees_data, aes(x = diameter, y = biomass), 
             col = "blue") + #actual data
  geom_line(data = pred_model4, aes(x = diameter, y = biomass_model4), 
            col = "red") + #predictions
  theme_classic()



