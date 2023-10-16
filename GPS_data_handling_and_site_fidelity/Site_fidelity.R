
# ============================================================================ #

# ----------------------------- Initialization ------------------------------- #

# ============================================================================ #


########## Packages ------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(hms)
library(sf)
library(terra)
library(amt)
library(rlist)


########## Loading the data ----------------------------------------------------


###### working directory
setwd("G:/Analyses/Site_fidelity_caribou_and_SBW/") 


###### Loading caribou GPS data on the Cote Nord
GPS_Caribou_Cote_Nord <- read.csv(
  "/../Data/GPS/Caribou/Cote_Nord/GPSdata_Dataset_Caribou_AllID_20052018_CoteNord.csv") %>%
  mutate(date = ymd_hms(date)) %>% #to recognize date as date-time
  mutate(Date = date(date), #new column with only the date
         Time = as_hms(date), #new column with only the time
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



########## Cleaning caribou data -----------------------------------------------

#Need to filter GPS data to keep only locations when the collar is actually on
#the caribou

##### Loading metadata about tracking

### Load the table
caribou_metadata <- read.csv(
  "/../Data/GPS/Caribou/Cote_Nord/Caribou_Life_CN_tracking.csv",
  sep = ";", dec = ",")

### Change the column names
colnames(caribou_metadata) <- 
  c("ID", "Sex", "Study_Area", "track_ID", "Collar", "Date_Capture", 
    "Date_Recovery", "Mode_Recovery", "Sampling", "track_ID", "Collar", 
    "Date_Capture", "Date_Recovery", "Mode_Recovery", "Sampling", 
    "track_ID", "Collar", "Date_Capture", "Date_Recovery", 
    "Mode_Recovery", "Sampling" )

### Put the second and third potential individual tracking period as other rows 
caribou_metadata <- rbind(caribou_metadata[,1:9],
                          caribou_metadata[,c(1:3,10:15)],
                          caribou_metadata[,c(1:3,16:21)])

### Dates and NAs handling
caribou_metadata <- caribou_metadata %>% 
  mutate(Date_Capture = ymd(Date_Capture), #to recognize date as date-time
         Date_Recovery = ymd(Date_Recovery)) %>%
  na.omit() #to remove rows without data


##### Keep locations after capture and before recovery

#Keep only date strictly superior to the capture date and strictly inferior to
#the recovery date

### Function to do it for one individual
ID <- "CA12"
GPS_Caribou_CA12_1 <- GPS_Caribou_CA12[0,]
metadata_CA12 <- filter(caribou_metadata, ID == "CA12")
for (i in 1:nrow(metadata_CA12)) {
  capture <- metadata_CA12[i,"Date_Capture"]
  recovery <- metadata_CA12[i, "Date_Recovery"]
  GPS_filtered <- filter(GPS_Caribou_CA12, 
                         Date > capture & Date < recovery)
  GPS_Caribou_CA12_1 <- rbind(GPS_Caribou_CA12_1, GPS_filtered)
}



##### Remove locations after drop-off but before recovery (if applicable)



################################################################################

# ------------------------ Where are the caribous? ----------------------------#

################################################################################

##### Load political regions to visualize
political_regions <-st_read(
  "/../Data/SIG/canvec_1M_CA_Admin_shp/canvec_1M_CA_Admin/geo_political_region_2.shp")
st_crs(political_regions)

##### Define WGS84 as the CRS
political_regions <- st_transform(political_regions, crs = 4326)
GPS_Caribou_Cote_Nord_sf_2 <- st_transform(GPS_Caribou_Cote_Nord_sf, 
                                           crs = 4326) %>%
  filter(longitude < 0) #exclude one aberrant point


##### Longitude and latitude limits
xmin <- min(GPS_Caribou_Cote_Nord_sf_2$longitude)
xmax <- max(GPS_Caribou_Cote_Nord_sf_2$longitude)
ymin <- min(GPS_Caribou_Cote_Nord_sf_2$latitude)
ymax <- max(GPS_Caribou_Cote_Nord_sf_2$latitude)


##### Plot
ggplot() + geom_sf(data = political_regions) + 
  geom_sf(data = GPS_Caribou_Cote_Nord_sf_2, aes(col = id)) +
  coord_sf(xlim = c(xmin-0.5, xmax+0.5), ylim = c(ymin-0.5, ymax+0.5))



# ============================================================================ #

# ----------------------------- Centroids ------------------------------------ #

# ============================================================================ #


########## Centroids coordinates -----------------------------------------------

GPS_Caribou_centroids <- GPS_Caribou_Cote_Nord %>%
  group_by(id, Year) %>%
  summarise_at(vars(c(longitude_UTM19N, latitude_UTM19N)),
               list(centroid = mean))


########## Distance between the centroids --------------------------------------


##### Function to calculate the distance between the centroids of two 
##### consecutive lines, and to add it to the data frame of the centroids

distance_centroids <- function(data) {
  data <- data.frame(data)
  distances <- rep(NA, nrow(data)) #distances vector, with NAs for every years to start
  if (nrow(data) > 1) {
    for (i in 2:nrow(data)) { #start at 2 because can't calculate distance between year 1 and year before 
                              #(like year 1 is the first)
     longitudes <- data[(i-1):i, "longitude_UTM19N_centroid"] #longitudess of year i and the year before
     latitudes <- data[(i-1):i, "latitude_UTM19N_centroid"] #latitudess of year i and the year before
     D <- sqrt((longitudes[1]-longitudes[2])**2 + (latitudes[1]-latitudes[2])**2) #euclidian distance between the centroid of 
                                                                                  #the year 1 and the centroid of the year before
     distances[i] <- D #put the distance at the right place in the distances vector
    }
  }
  data <- data.frame(data, distances) #add the distances vector to data
  data
}


##### Apply this function to GPS_Caribou_centroids

GPS_Caribou_centroids <- GPS_Caribou_centroids %>%
  group_by(id) %>%
  group_modify(~ distance_centroids(.x)) #add the column distances thanks to the 
                                         #function distance_centroids, grouping
                                         #by id



# ============================================================================ #

# ----------------------------- Home ranges ---------------------------------- #

# ============================================================================ #


########## Test on 100 locations of CA12 ---------------------------------------

GPS_Caribou_C12_short <- GPS_Caribou_CA12[1:100,]

##### To use amt, data must be in the form "track_xyt"
track_Caribou_C12_short <- make_track(
  GPS_Caribou_C12_short, .x = longitude_UTM19N, .y = latitude_UTM19N, 
  .t = date, crs = 32619)

ggplot(track_Caribou_C12_short) +
  geom_point(aes(x=x_, y=y_))

##### Home range by KDE, with LSCV to chose h
hr_CA12_short_kde_lscv <- hr_kde(
  track_Caribou_C12_short, 
  h = hr_kde_lscv(
    track_Caribou_C12_short, 
    range = do.call(
      seq, as.list(c(hr_kde_ref(track_Caribou_C12_short) * c(0.1, 5),
                     length.out = 100)))), 
  levels = 0.95)
#Did not converge,

##### Home range by KDE, with href to chose h
hr_CA12_short_kde_href <- hr_kde(
  track_Caribou_C12_short, 
  h = hr_kde_ref(track_Caribou_C12_short), levels = 0.95)
plot(hr_CA12_short_kde_href)


##### Home range by aKDE --> fait buger ordi
#hr_CA12_short_akde_iid <- hr_akde(
#  track_Caribou_C12_short, 
#  model = fit_ctmm(track_Caribou_C12_short, "iid"), levels = 0.95)

#hr_CA12_short_akde_ou <- hr_akde(
#  track_Caribou_C12_short, 
#  model = fit_ctmm(track_Caribou_C12_short, "ou"), levels = 0.95)

#hr_CA12_short_akde_ouf <- hr_akde(
#  track_Caribou_C12_short, 
#  model = fit_ctmm(track_Caribou_C12_short, "ouf"), levels = 0.95)



########## Home range by individual and by year --------------------------------

##### Function to get the HR for each individual each year
HR_ind_year <- function(data) {
  Home_Ranges_all <- list()
  for (i in levels(factor(data$id))) { #loop for each individual
    data_id <- filter(data, id == i) #keep only the data of the individual i
    track_data <- make_track(
      tbl = data_id, .x = longitude_UTM19N, .y = latitude_UTM19N, 
      .t = date, crs = 32619) #make a track, 
                              #necessary to use amt package to do the home range
    home_ranges <- track_data %>%
      group_by(year(t_)) %>% #group by year, to have one home range each year
      group_map(~ hr_kde(.x, h = hr_kde_ref(.x), levels = 0.95)) #make HR
                                              #here kernel density home range
                                              #return a list of the home ranges
    Home_Ranges_all <- list.append(Home_Ranges_all, home_ranges) #add the new
                                  # list of home ranges in the list, as one new
                                  # element (so we get a list of lists)
  }
  names(Home_Ranges_all) <- levels(factor(data$id)) #put names to find the lists 
                                                    #of home ranges in the list
                                                    #thanks to the id
  Home_Ranges_all
}

##### Apply the function on the GPS data 
HR_Caribou_Cote_Nord <- HR_ind_year(GPS_Caribou_Cote_Nord)

#For now, we do it with KDE and href for h value, we will see if we keep that





# ============================================================================ #

# ------------------------- Home ranges overlap ------------------------------ #

# ============================================================================ #


########## Test with CA12 ------------------------------------------------------

HR_CA12 <- HR_Caribou_Cote_Nord$CA12

HR_CA12_2005 <- HR_CA12[[1]]
HR_CA12_2006 <- HR_CA12[[2]]

test_overlap1 <- hr_overlap(HR_CA12_2005, HR_CA12_2006, type = "hr") 
test_overlap2 <- hr_overlap(HR_CA12_2006, HR_CA12_2005, type = "hr")
test_overlap1 # % hr 2005 in 2006
test_overlap2 # % hr 2006 in 2005
plot(HR_CA12_2005) 
plot(HR_CA12_2006)


plot(HR_CA12_2005) 
ggplot() + 
  geom_sf(data = hr_isopleths(HR_CA12_2005, levels=0.95), col="red", fill=NA) +
  geom_sf(data = hr_isopleths(HR_CA12_2006, levels=0.95), col="blue", fill=NA)



########## Overlap between the successive years for each individual ------------

###### Function to calculate overlap for one inidvidual between the years

hr_years_overlap <- function(home_ranges) { #home_ranges must be list 
                                            #of home ranges 
  #create of 2 vectors of NAs, of the length of the number of years
  overlap1in2 <- rep(NA, length(home_ranges)) #for the % of hr i-1 into hr i
  overlap2in1 <- rep(NA, length(home_ranges)) #for the % of hr i into hr i-1
  if (length(home_ranges) > 1) { #do not do anything if only 1 year
    for (i in 2:length(home_ranges)) { #from the year 2 (because we are going 
                                #to calculate the overlap with the year before)
      HR_1 <- home_ranges[[i-1]] #take the home range of the year i-1
      HR_2 <- home_ranges[[i]] #take the home range of the year i
      overlap1in2[i] <- hr_overlap(HR_1, HR_2, type = "hr")$overlap 
                                      #overlap of the year i on i-1 
                                      #(or % of home range i-1 in home range i)
      overlap2in1[i] <- hr_overlap(HR_2, HR_1, type = "hr")$overlap 
                                      ##overlap of the year i-1 on i 
    }
  }
  overlap_tot <- data.frame(overlap1in2, overlap2in1)
  overlap_tot
}

# Test on individual CA12
hr_years_overlap(HR_Caribou_Cote_Nord$CA12)


##### Apply on all individuals

# Function to apply to all individuals the previous function, and return
# a data frame with the overlaps data, the id and the years (easier for after) 
overlap_all_individuals <- function(home_ranges, Data_GPS) {
  overlap_all <- lapply(X = home_ranges, 
                        FUN = hr_years_overlap) #apply the hr_years_overlap 
                                                #function to all individuals 
  for (i in levels(factor(Data_GPS$id))) { #loop to look at each individual
    Ind <- dplyr::filter(Data_GPS, id==i) #keep only the GPS data of
                                          #individual i
    overlap_all[[i]]$Year <- levels(factor(Ind$Year)) #add a column with 
                                                      #the years
  }
  overlap_all_df <- as.data.frame(do.call(rbind, overlap_all)) %>% #transform 
                                                    #the list into a data frame
    rownames_to_column(var = "id_nb") %>% #create a column with the rownames, 
                                          #to be able to split it afterwards
    separate(col = id_nb, into = c("id", NA), fill = "right", remove = T) #split
                                            #the new column, keeping only th id
  overlap_all_df
}

# Aplly the new function on the home range data 
# (with the GPS data for the years column)
overlap_Caribou_Cote_Nord <- overlap_all_individuals(HR_Caribou_Cote_Nord, 
                                                     GPS_Caribou_Cote_Nord)


##### Put this new info in GPS_Caribou_centroids

#for that, we use full_join, to do a joint between GPS_Caribou_centroids 
# and overlap_Caribou_Cote_Nord by the columns id and Year
GPS_Caribou_centroids_hr_overlap <- full_join(
  x = GPS_Caribou_centroids, y = overlap_Caribou_Cote_Nord,
   by = c("id", "Year"))





# ============================================================================ #

# ---------------------- SBW severity in home ranges ------------------------- #

# ============================================================================ #

