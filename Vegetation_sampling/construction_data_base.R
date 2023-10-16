
################################################################################

# ----------------------------- Initialization ------------------------------- #

################################################################################


########## Packages ------------------------------------------------------------

library("tidyverse")
library("e1071")
library("FactoMineR")
library("factoextra")
library("corrplot")


########## Data loading --------------------------------------------------------

##### Working directory
setwd <- "F:/Analyses/Vegetation/"

##### Trees (twigs biomass) 

Biomasse_tiges <- read.csv(
  file = "../../Data/Vegetation/Sampling_summer_2022/Donnees_allCoord_tiges.csv", 
  header = TRUE, sep = ";", dec = ".",
  na.strings = c("NA", "", "Trouver_calibration", "Choisir_la_calibration", 
                 "Trouvercalibration", "Choisirlacalibration"),
  colClasses = c("no" = "numeric", "Site" = "numeric", "Espece" = "character", 
                 "Espece_revisee" = "character", "Diametre" = "numeric", 
                 "Biomasse" = "numeric", "Biomasse_corrigee" = "numeric"))


##### Lichen

Biomasse_lichen <- read.csv(
  file = "../../Data/Vegetation/Sampling_summer_2022/Donnees_allCoord_lichen.csv",
  header = TRUE, sep = ";", dec = ".")

colnames(Biomasse_lichen) <- 
  c("no", "Site", "Location", "locationsuite", "Localisation", "Latitude", 
    "Longitude", "X", "Y", "type_eco", "desc_type_eco", "perturbation", 
    "Forest_Cover_QGIS", "description.generale", "Prop_Lichen_Sol_N", 
    "Hauteur_Lichen_Sol_N_cm", "Lichen_Arboricole_Fruticuleux_N_0_a_1m", 
    "Lichen_Arboricole_Fruticuleux_N_1_a_3m", "Lichen_Arboricole_Foliace_N", 
    "Canopee_N_sur_24.", "Prop_Lichen_Sol_S", "Hauteur_Lichen_Sol_S_cm",
    "Lichen_Arboricole_Fruticuleux_S_0_a_1m", 
    "Lichen_Arboricole_Fruticuleux_S_1_a_3m", "Lichen_Arboricole_Foliace_S", 
    "Canopee_S_sur_24.", "Prop_Lichen_Sol_E", "Hauteur_Lichen_Sol_E_cm.", 
    "Lichen_Arboricole_Fruticuleux_E_0_a_1m", 
    "Lichen_Arboricole_Fruticuleux_E_1_a_3m", "Lichen_Arboricole_Foliace_E", 
    "Canopee_E_sur_24.", "Prop_Lichen_Sol_O", "Hauteur_Lichen_Sol_O_cm.", 
    "Lichen_Arboricole_Fruticuleux_O_0_a_1m", 
    "Lichen_Arboricole_Fruticuleux_O_1_a_3m", "Lichen_Arboricole_Foliace_O", 
    "Canopee_O_sur24", "pct_lichen_1m2", "Hauteur_lichen", 
    "Biomass_kg_ha_suivant_Crete_et_al", "Moyenne_biomasse_lichen_frut_0_1m", 
    "Moyenne_biomasse_lichen_frut_1_3m")



################################################################################

# -------------------------- Handling the tables ----------------------------- #

################################################################################

########## Delete the sites without any measures (probably not visited) --------

Biomasse_tiges <- filter(
  Biomasse_tiges, !(Site %in% c(7, 13, 67, 85, 86, 87, 91, 97, 100, 103, 104, 
                                105, 106, 108, 113, 115, 116, 122, 123, 125, 
                                127, 140, 143, 154, 155, 156, 157, 167, 168)))

Biomasse_lichen <- filter(
  Biomasse_lichen, !(Site %in% c(7, 13, 67, 85, 86, 87, 91, 97, 100, 103, 104, 
                                 105, 106, 108, 113, 115, 116, 122, 123, 125, 
                                 127, 140, 143, 154, 155, 156, 157, 167, 168)))


########## Grouping the twigs biomass ------------------------------------------

##### For the biomass before correction

### Per site

Biomasse_tiges_site <- aggregate(
  x=Biomasse_tiges$Biomasse, by=list(Biomasse_tiges$Site), 
  FUN=sum, na.rm=T)
names(Biomasse_tiges_site) <- c("Site", "Biomasse_tiges")


### Per site and per species

Biomasse_tiges_site_sp <- aggregate(
  x=Biomasse_tiges$Biomasse, 
  by=list(Biomasse_tiges$Site, Biomasse_tiges$Espece_revisee), FUN=sum, na.rm=T)
names(Biomasse_tiges_site_sp) <- c("Site", "Espece", "Biomasse_tiges")

## Make a table with the sites in lines and the species in columns

Biomasse_tiges_site_sp_wide <- pivot_wider(
  data=Biomasse_tiges_site_sp,names_from=Espece, values_from=Biomasse_tiges,
  values_fill=0) #make the new table

Biomasse_tiges_site_sp_wide <- 
  Biomasse_tiges_site_sp_wide[order(Biomasse_tiges_site_sp_wide$Site),]
#order the new table by sites in ascending order



##### For the corrected biomass

### Per site

Biomasse_tiges_corrigee_site <- aggregate(
  x=Biomasse_tiges$Biomasse_corrigee, by=list(Biomasse_tiges$Site), 
  FUN=sum, na.rm=T)
names(Biomasse_tiges_corrigee_site) <- c("Site", "Biomasse_tiges_corrigee")


### Per site and per species

Biomasse_tiges_corrigee_site_sp <- aggregate(
  x=Biomasse_tiges$Biomasse_corrigee, 
  by=list(Biomasse_tiges$Site, Biomasse_tiges$Espece_revisee), FUN=sum, na.rm=T)
names(Biomasse_tiges_corrigee_site_sp) <- c("Site", "Espece", 
                                            "Biomasse_tiges_corrigee")

## Make a table with the sites in lines and the species in columns

Biomasse_tiges_corrigee_site_sp_wide <- pivot_wider(
  data=Biomasse_tiges_corrigee_site_sp, names_from=Espece, 
  values_from=Biomasse_tiges_corrigee, values_fill=0) # make the new table

Biomasse_tiges_corrigee_site_sp_wide <- 
  Biomasse_tiges_corrigee_site_sp_wide[order(
    Biomasse_tiges_corrigee_site_sp_wide$Site),] 
#order the new table by sites in ascending order



########## Make a complete table with all information --------------------------

##### Merge the biomass tables (per site and per site and species) 
##### in a same table 

Biomasse_tiges_site_et_site_sp <- merge(
  Biomasse_tiges_site, Biomasse_tiges_corrigee_site, by="Site", all=T)
Biomasse_tiges_site_et_site_sp <- merge(
  Biomasse_tiges_site_et_site_sp, Biomasse_tiges_site_sp_wide, by="Site", all=T)
Biomasse_tiges_site_et_site_sp <- merge(
  Biomasse_tiges_site_et_site_sp, Biomasse_tiges_corrigee_site_sp_wide, 
  by="Site", all=T)
Biomasse_tiges_site_et_site_sp[is.na(Biomasse_tiges_site_et_site_sp)] <- 0
            #put 0 for the NAs (because merging creates NAs)


##### Merge this twig biomass table with the lichen biomass table

Biomasses_site <- merge(Biomasse_lichen, Biomasse_tiges_site_et_site_sp, 
                        by="Site", all=T)
Biomasses_site[is.na(Biomasses_site)] <- 0 #put 0 for the NAs 
                                           #(because merging creates NAs)


##### Modify column names (tree species names)
colnames(Biomasses_site) <- gsub("\\.x", "", colnames(Biomasses_site))
colnames(Biomasses_site) <- gsub("\\.y", "_corrige", colnames(Biomasses_site))
#.x correspond to the first twig data (no corrected), 
#and .y correspond to the second twig data (corrected)
colnames(Biomasses_site) <- gsub(" ", "_", colnames(Biomasses_site))
colnames(Biomasses_site) <- gsub("-", "_", colnames(Biomasses_site))
colnames(Biomasses_site) <- gsub("à", "a", colnames(Biomasses_site))
colnames(Biomasses_site) <- gsub("é", "e", colnames(Biomasses_site))
colnames(Biomasses_site) <- gsub("è", "e", colnames(Biomasses_site))
colnames(Biomasses_site) <- gsub("É", "E", colnames(Biomasses_site))
colnames(Biomasses_site) <- gsub("\\.", "", colnames(Biomasses_site))
colnames(Biomasses_site) <- gsub("'", "_", colnames(Biomasses_site))

##### Correct some mistakes or abbreviations
Biomasses_site$Forest_Cover_QGIS <- gsub("yrs", "years", 
                                         Biomasses_site$Forest_Cover_QGIS)
Biomasses_site$Forest_Cover_QGIS <- gsub(
  " Young natural diusturbances", "Young natural diusturbances", 
  Biomasses_site$Forest_Cover_QGIS)



########## Manipulating and grouping the biomass -------------------------------


##### Corrected twig biomass without balsam fir
Biomasses_site$Biomasse_tiges_corrigee_sans_sapin <- 
  Biomasses_site$Biomasse_tiges_corrigee - Biomasses_site$Sapin_baumier_corrige


##### Eaten biomass for each species (without balsam fir)

#NB: this part is not relevant at the moment

### Moose
Biomasses_site$Biomasse_tiges_moose <- Biomasses_site$Amelanchier_corrige + 
  Biomasses_site$Aulne_spp_corrige + Biomasses_site$Bouleau_a_papier_corrige +
  Biomasses_site$Bouleau_glanduleux_corrige + 
  Biomasses_site$Cerisier_de_pennsylvanie_corrige + 
  Biomasses_site$Erable_rouge_corrige + 
  Biomasses_site$Peuplier_faux_tremble_corrige + 
  Biomasses_site$Salix_spp_corrige + Biomasses_site$Sorbier_d_Amerique_corrige

### Caribou
Biomasses_site$Biomasse_tiges_caribou <- Biomasses_site$Aulne_spp_corrige + 
  Biomasses_site$Bouleau_a_papier_corrige +
  Biomasses_site$Bouleau_glanduleux_corrige + Biomasses_site$Meleze_corrige + 
  Biomasses_site$Salix_spp_corrige



################################################################################

# --------------------- Writing and saving the table ------------------------- #

################################################################################

write.csv(x=Biomasses_site, file="Biomasses_par_site.csv")

