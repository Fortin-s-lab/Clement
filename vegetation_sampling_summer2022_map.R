
################################################################################

# --------------------------- Initialization --------------------------------- #

################################################################################


########## Packages

library(tidyverse)
library(sf)


########## Data loading

setwd("F:/Analyses/Maps/")
biomass_site <- read.csv(
  file="../../Data/Vegetation/Sampling_summer_2022/Biomasses_par_site.csv")


################################################################################

# ------------------------------ Spatialization ------------------------------ #

################################################################################


########## Spatialize the biomass_site data frame

biomass_site_sf <- st_as_sf(biomass_site, coords = c("Longitude", "Latitude"),
                            crs = 4326, remove = FALSE) #crs = 4326 because 
                                                    #it is the code for  WGS84

