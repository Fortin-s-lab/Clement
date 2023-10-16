################################################################################

# -------------------------- Initialization ---------------------------------- #

################################################################################


########## Packages ------------------------------------------------------------

library(tidyverse)



########## Loading and cleaning the data ---------------------------------------

setwd("F:/Analyses/Vegetation/calibration/") #working directory

## Loading calibration data
fir_calibration <- read.csv2("calibration_sapin.csv", dec = ".")
colnames(fir_calibration) <- c("diameter", "biomass")


################################################################################

# ----------------------- Potential calibrations ----------------------------- #

################################################################################


########## Plots of the data ---------------------------------------------------

## Plot biomass ~ diameter to see if difference between the 2 species
ggplot(data = fir_calibration) + 
  geom_point(aes(x = diameter, y = biomass)) + theme_classic()

## Plot log(biomass) ~ diameter to see if difference between the 2 species
ggplot(data = fir_calibration) + 
  geom_point(aes(x = diameter, y = log(biomass))) + theme_classic()

## Plot sqrt(biomass) ~ diameter to see if difference between the 2 species
ggplot(data = fir_calibration) + 
  geom_point(aes(x = diameter, y = sqrt(biomass))) + theme_classic()

## Plot (biomass)^(1/3) ~ diameter to see if difference between the 2 species
ggplot(data = fir_calibration) + 
  geom_point(aes(x = diameter, y = (biomass)^(1/3))) + theme_classic()



########## Finding the best regression ---------------------------------------

## biomass ~ diameter
model1 <- lm(data = fir_calibration, formula = biomass ~ diameter)
summary(model1)
par(mfrow = c(2, 2))
plot(model1)

## log(biomass) ~ diameter
model2 <- lm(data = fir_calibration, formula = log(biomass+1) ~ diameter)
summary(model2)
par(mfrow = c(2, 2))
plot(model2)
# diameter not significant

## sqrt(biomass) ~ diameter
model3 <- lm(data = fir_calibration, formula = sqrt(biomass) ~ diameter)
summary(model3)
par(mfrow = c(2, 2))
plot(model3)

## (biomass)^(1/3) ~ diameter
model4 <- lm(data = fir_calibration, formula = (biomass)^(1/3) ~ diameter)
summary(model4)
par(mfrow = c(2, 2))
plot(model4)

## AIC of the models
AIC(model1) #--> 863
AIC(model3) #--> 435
AIC(model4) #--> 298

## BIC of the models
BIC(model1) #--> 869
BIC(model3) #--> 441
BIC(model4) #--> 304



########## Plot the regression and compare with the data -----------------------

##### Build model predictions

## x vector (diameter)
diameter <- c(0:300)

## model 1
biomass_model1 <- 2.4783 * diameter  #predicted weights according 
                                     #to the model 1
pred_model1 <- data.frame(diameter, biomass_model1)

## model 3
biomass_model3 <- (0.05784*diameter + 4.56396)**2
pred_model3 <- data.frame(diameter, biomass_model3)

## model 4
biomass_model4 <- (0.012909*diameter + 2.599708)**3
pred_model4 <- data.frame(diameter, biomass_model4)


##### Plot predictions and data

## For the model 1
ggplot() +
  geom_point(data = fir_calibration, aes(x = diameter, y = biomass), 
             col = "blue") + #actual data
  geom_line(data = pred_model1, aes(x = diameter, y = biomass_model1), 
            col = "red") + #predictions
  theme_classic()

## For the model 3
ggplot() +
  geom_point(data = fir_calibration, aes(x = diameter, y = biomass), 
             col = "blue") + #actual data
  geom_line(data = pred_model3, aes(x = diameter, y = biomass_model3), 
            col = "red") + #predictions
  theme_classic()

## For the model 4
ggplot() +
  geom_point(data = fir_calibration, aes(x = diameter, y = biomass), 
             col = "blue") + #actual data
  geom_line(data = pred_model4, aes(x = diameter, y = biomass_model4), 
            col = "red") + #predictions
  theme_classic()



################################################################################

# ----------------- Potential calibrations under 100mm ----------------------- #

################################################################################

########## Cut the data

fir_calibration_100mm <- filter(fir_calibration, diameter <= 100)

########## Plots of the data ---------------------------------------------------

## Plot biomass ~ diameter to see if difference between the 2 species
ggplot(data = fir_calibration_100mm) + 
  geom_point(aes(x = diameter, y = biomass)) + theme_classic()

## Plot log(biomass) ~ diameter to see if difference between the 2 species
ggplot(data = fir_calibration_100mm) + 
  geom_point(aes(x = diameter, y = log(biomass))) + theme_classic()

## Plot sqrt(biomass) ~ diameter to see if difference between the 2 species
ggplot(data = fir_calibration_100mm) + 
  geom_point(aes(x = diameter, y = sqrt(biomass))) + theme_classic()

## Plot (biomass)^(1/3) ~ diameter to see if difference between the 2 species
ggplot(data = fir_calibration_100mm) + 
  geom_point(aes(x = diameter, y = (biomass)^(1/3))) + theme_classic()



########## Finding the best regression ---------------------------------------

## biomass ~ diameter
model1_100mm <- lm(data = fir_calibration_100mm, formula = biomass ~ diameter)
summary(model1_100mm)
par(mfrow = c(2, 2))
plot(model1_100mm)

## log(biomass) ~ diameter
model2_100mm <- 
  lm(data = fir_calibration_100mm, formula = log(biomass+1) ~ diameter)
summary(model2_100mm)
par(mfrow = c(2, 2))
plot(model2_100mm)
# diameter not significant

## sqrt(biomass) ~ diameter
model3_100mm <- 
  lm(data = fir_calibration_100mm, formula = sqrt(biomass) ~ diameter)
summary(model3_100mm)
par(mfrow = c(2, 2))
plot(model3_100mm)

## (biomass)^(1/3) ~ diameter
model4_100mm <- 
  lm(data = fir_calibration_100mm, formula = (biomass)^(1/3) ~ diameter)
summary(model4_100mm)
par(mfrow = c(2, 2))
plot(model4_100mm)

## AIC of the models
AIC(model1_100mm) #--> 605
AIC(model2_100mm) #--> 166
AIC(model3_100mm) #--> 285
AIC(model4_100mm) #--> 180

## BIC of the models
BIC(model1_100mm) #--> 611
BIC(model2_100mm) #--> 171
BIC(model3_100mm) #--> 291
BIC(model4_100mm) #--> 186



########## Plot the regression and compare with the data -----------------------

##### Build model predictions

## x vector (diameter)
diameter_100mm <- c(0:100)

## model 1
biomass_model1_100mm <- 4.8361 * diameter_100mm  #predicted weights according 
#to the model 1
pred_model1_100mm <- data.frame(diameter_100mm, biomass_model1_100mm)

## model 2
biomass_model2_100mm <- exp(0.048651*diameter_100mm + 1.672044)
pred_model2_100mm <- data.frame(diameter_100mm, biomass_model2_100mm)

## model 3
biomass_model3_100mm <- (0.20447*diameter_100mm)**2
pred_model3_100mm <- data.frame(diameter_100mm, biomass_model3_100mm)

## model 4
biomass_model4_100mm <- (0.064194*diameter_100mm + 1.510331)**3
pred_model4_100mm <- data.frame(diameter_100mm, biomass_model4_100mm)


##### Plot predictions and data

## For the model 1
ggplot() +
  geom_point(data = fir_calibration_100mm, aes(x = diameter, y = biomass), 
             col = "blue") + #actual data
  geom_line(data = pred_model1_100mm, 
            aes(x = diameter_100mm, y = biomass_model1_100mm), 
            col = "red") + #predictions
  theme_classic()

## For the model 2
ggplot() +
  geom_point(data = fir_calibration_100mm, aes(x = diameter, y = biomass), 
             col = "blue") + #actual data
  geom_line(data = pred_model2_100mm, 
            aes(x = diameter_100mm, y = biomass_model2_100mm), 
            col = "red") + #predictions
  theme_classic()

## For the model 3
ggplot() +
  geom_point(data = fir_calibration_100mm, aes(x = diameter, y = biomass), 
             col = "blue") + #actual data
  geom_line(data = pred_model3_100mm, 
            aes(x = diameter_100mm, y = biomass_model3_100mm), 
            col = "red") + #predictions
  theme_classic()

## For the model 4
ggplot() +
  geom_point(data = fir_calibration_100mm, aes(x = diameter, y = biomass), 
             col = "blue") + #actual data
  geom_line(data = pred_model4_100mm, 
            aes(x = diameter_100mm, y = biomass_model4_100mm), 
            col = "red") + #predictions
  theme_classic()



########## Test model 4 under 100mm extended -----------------------------------

biomass_model4_extend <- (0.064194*diameter + 1.510331)**3
pred_model4_extend <- data.frame(diameter, biomass_model4_extend)

ggplot() +
  geom_point(data = fir_calibration, aes(x = diameter, y = biomass), 
             col = "blue") + #actual data
  geom_line(data = pred_model4_extend, 
            aes(x = diameter, y = biomass_model4_extend), 
            col = "red") + #predictions
  theme_classic()


biomass_model4_100mm_extend <- c(biomass_model4_100mm, 
                                rep((0.064194*100 + 1.510331)**3, 200))
pred_model4_100mm_extend <- data.frame(diameter, biomass_model4_100mm_extend)

ggplot() +
  geom_point(data = fir_calibration, aes(x = diameter, y = biomass), 
             col = "blue") + #actual data
  geom_line(data = pred_model4_100mm_extend, 
            aes(x = diameter, y = biomass_model4_100mm_extend), 
            col = "red") + #predictions
  theme_classic()
