
# ============================================================================ #

# ----------------------------- Initialization ------------------------------- #

# ============================================================================ #


########## Packages ------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(sf)
library(terra)
library(amt)
library(rlist)



########## Loading and cleaning the data ---------------------------------------


###### working directory
setwd("F:/Analyses/Site_fidelity_caribou_and_SBW/") 


###### Loading caribou GPS data on the Cote Nord
GPS_Caribou_Cote_Nord <- read.csv(
  "/../Data/GPS/Caribou/Cote_Nord/GPSdata_Dataset_Caribou_AllID_20052018_CoteNord.csv") %>%
  mutate(date = ymd_hms(date)) %>% #to recognize date as date-time
  mutate(Date = format(date, format = "%Y:%m:%d"), #new column with only the date
         Time = format(date, format = "%H:%M:%S"), #new column with only the time
         Year = as.factor(year(date)))


###### Longitude and latitude in UTM 19N
#First, we need to spatialize the data, here with sf package
GPS_Caribou_Cote_Nord_sf <- st_as_sf(
  GPS_Caribou_Cote_Nord, coords = c("longitude", "latitude"),
  crs = 4326, remove = FALSE) #crs = 4326 because CRS is WGS84
GPS_Caribou_Cote_Nord_sf <- st_transform(
  GPS_Caribou_Cote_Nord_sf, crs = 32619) #transform CRS WGS84 to UTM 19N

GPS_Caribou_Cote_Nord$longitude_UTM19N <- st_coordinates(
  GPS_Caribou_Cote_Nord_sf)[, "X"] #put the UTM 19N longitude in the table 
GPS_Caribou_Cote_Nord$latitude_UTM19N <- st_coordinates(
  GPS_Caribou_Cote_Nord_sf)[, "Y"] #put the UTM 19N latitude in the table


###### Keep just one individuals for tests
GPS_Caribou_CA12 <- dplyr::filter(GPS_Caribou_Cote_Nord, id == "CA12")




################################################################################

# ---------------------- Net Square Displacement (NSD) ---------------------- #

################################################################################


########## NSD function

#The input as to be a data frame with individual ID, the date, and the 
#longitude and latitude in UTM, in this order

NSD <- function(locations) {
  colnames(locations) <- c("id", "date", "X", "Y")
  locations <- locations %>% arrange(id, locations) #arrange the data by id 
                                                    #and date
  locations <- locations %>% 
    group_by(id) %>% #group by individual
    mutate(First_X = first(X), #create new columns with the first longitude
           First_Y = first(Y)) %>% #and the first latitude of the individual
    ungroup() %>%
    mutate(displacement = sqrt((X - First_X)**2 + (Y - First_Y)**2), #distance 
                               #between the individual first location  
                               #and all other individual locations
           NSD = displacement**2) #square of the previously calculated distances 
                                  #--> NSD
  locations
}


########## Test with caribous

locations <- GPS_Caribou_Cote_Nord[,c("id", "date", "longitude_UTM19N",
                                      "latitude_UTM19N")]
Caribou_NSD <- NSD(locations)

levels(factor(GPS_Caribou_Cote_Nord$id))
filter(Caribou_NSD, id == "CA12")
filter(Caribou_NSD, id == "CA122")
filter(Caribou_NSD, id == "25771")




################################################################################

# ----------------------------- Data exploration ----------------------------- #

################################################################################


########## Individuals

length(levels(factor(GPS_Caribou_Cote_Nord$id))) # --> 73 individuals
levels(factor(GPS_Caribou_Cote_Nord$id)) # --> individuals ID


########## Add useful information in Caribou_NSD

Caribou_NSD <- Caribou_NSD %>%
  mutate(month = format(date, format = "%m"),
         year = format(date, format = "%Y"))


########## Plot NSD ~ date

### Get the plots and store them in a list
IND <- levels(factor(Caribou_NSD$id)) #vectors of the individual IDs
Plots_NSD_date <- list() #empty list, to store the plots afterwards
for (ind in IND) {
  plot_NSD_date <- Caribou_NSD %>% filter(id == ind) %>%
    ggplot(aes(x = date, y = NSD, col = month)) +
    geom_point() #plot NSD ~ date, colored by month
  Plots_NSD_date <- list.append(Plots_NSD_date, plot_NSD_date) #store the plot
                                                               #in the list
}
names(Plots_NSD_date) <- IND #change the names of the list elements 
                             #with the individual IDs

### Plots
Plots_NSD_date$CA100
filter(Caribou_NSD, id == "CA100")$NSD


########## Strange results with some IDs

Plots_NSD_date$`2011012`
Plots_NSD_date$`2011013`
filter(Caribou_NSD, id == "2011012")
filter(Caribou_NSD, id == "2011013")
#these 2 ones are more or less ok, actually (2011012 is just quite short)

Plots_NSD_date$`25769`
Plots_NSD_date$`25770`
Plots_NSD_date$`25771`
Plots_NSD_date$`25772`
Plots_NSD_date$`25773`
Plots_NSD_date$`25774`
Plots_NSD_date$`25775`
Plots_NSD_date$`25776`
Plots_NSD_date$`25777`
Plots_NSD_date$`25778`
Plots_NSD_date$`25779`
filter(Caribou_NSD, id == "25769")
filter(Caribou_NSD, id == "25770")
filter(Caribou_NSD, id == "25771")
filter(Caribou_NSD, id == "25772")
filter(Caribou_NSD, id == "25773")
#These ones are flat, and the starting point is always in the same location
#-> possible that the collars recorded locations before being on the animals


Plots_NSD_date$CA125
Plots_NSD_date$CA132
#Some plots seem quite flat, because the last points are very high


