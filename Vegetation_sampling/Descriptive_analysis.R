
################################################################################

# ----------------------------- Initialization ------------------------------- #

################################################################################

########## Packages ------------------------------------------------------------
library("tidyverse")

########## loading the data ----------------------------------------------------
biomass_site <- read.csv(file="Biomasses_par_site.csv")



################################################################################

# --------------------------- Descriptive analysis --------------------------- #

################################################################################


########## Number of sites of presence per species -----------------------------

Biomasse_tiges_corrigee_site_sp %>% #use a table defined in
                                    #"construction_data_base.R"
  group_by(by=Espece) %>%
  count() #give the number of sites for each species

nrow(Biomasse_tiges_corrigee_site) #-->140, to compare with the previous results


########## Mean biomass in each habitat ----------------------------------------

### Mean biomass in each habitat
mean_biomass <- biomass_site %>% 
  group_by(Forest_Cover_QGIS) %>% #group by habitat
  summarise_at(vars(c(
    Biomass_kg_ha_suivant_Crete_et_al, Moyenne_biomasse_lichen_frut_0_1m,
    Moyenne_biomasse_lichen_frut_1_3m, Biomasse_tiges_corrigee_sans_sapin,
    Sapin_baumier_corrige)), 
    list(mean)) #summarise_at allow to calculate a metric (in the list, 
                #so here the mean) for each called column (in vars)

### Number of sites for each habitat
nb_sites <- biomass_site %>% 
  group_by(Forest_Cover_QGIS) %>% 
  count() 

### Data frame with the mean biomass and the number of sites for each habitat
mean_biomass <- merge(nb_sites, mean_biomass, by="Forest_Cover_QGIS")
colnames(mean_biomass) <- c(
  "Forest_Cover_QGIS","Number of sites", "Terrestrial lichen (kg/ha)", 
  "Arboreal lichen 0-1m (g/tree)", "Arboreal lichen 1-3m (g/tree)", 
  "Twigs biomass (g/ sampled area) (without balsam fir)", 
  "Balsam fir twigs biomass (g/sampled area)" )

write.csv2(mean_biomass, "mean_biomass.csv")



# ----------------------------- Plots ---------------------------------------- #

########## 0 or 1 for presence absence for each biomass ------------------------

#to filter and make plots without the zeros
Biomasses_site <- Biomasses_site %>%
  mutate(Presence_terricolous_lichen = 
           if_else(Biomass_kg_ha_suivant_Crete_et_al==0,0,1)) 


########## For the deciduous twig biomass --------------------------------------

Biomasses_site %>% filter(Forest_Cover_QGIS == "Closed coniferous forest") %>% 
  ggplot() +  
  geom_point(aes(x = Latitude, y = Biomasse_tiges_corrigee_sans_sapin))

Biomasses_site %>% filter(Forest_Cover_QGIS == "Open coniferous forest") %>% 
  ggplot() +  
  geom_point(aes(x = Latitude, y = Biomasse_tiges_corrigee_sans_sapin))

Biomasses_site %>% 
  filter(Forest_Cover_QGIS == 
           "Young natural diusturbances (fire < 10 years old)") %>% 
  ggplot() +  
  geom_point(aes(x = Latitude, y = Biomasse_tiges_corrigee_sans_sapin))

Biomasses_site %>% 
  filter(Forest_Cover_QGIS == "Young clearcut ( < 10 years old)") %>% 
  ggplot() +  
  geom_point(aes(x = Latitude, y = Biomasse_tiges_corrigee_sans_sapin))

Biomasses_site %>% 
  filter(Forest_Cover_QGIS == "Natural disturbance (10-19 years old)") %>% 
  ggplot() +  
  geom_point(aes(x = Latitude, y = Biomasse_tiges_corrigee_sans_sapin))

Biomasses_site %>% filter(Forest_Cover_QGIS == "Clearcut 10-19 years old") %>% 
  ggplot() +  
  geom_point(aes(x = Latitude, y = Biomasse_tiges_corrigee_sans_sapin))



########## For the terricolous lichen biomass ----------------------------------

Biomasses_site %>% 
  ggplot() +  
  geom_point(aes(x = Latitude, y = Biomass_kg_ha_suivant_Crete_et_al))

Biomasses_site %>% filter(Presence_terricolous_lichen == 1) %>%
  filter(Forest_Cover_QGIS == "Open coniferous forest") %>% 
  ggplot() +  
  geom_point(aes(x = Latitude, y = log(Biomass_kg_ha_suivant_Crete_et_al)))

Biomasses_site %>% 
  filter(Forest_Cover_QGIS == "Open coniferous forest") %>% 
  ggplot() +  
  geom_point(aes(x = Latitude, y = Biomass_kg_ha_suivant_Crete_et_al))

Biomasses_site %>%  filter(Presence_terricolous_lichen == 1) %>%
  filter(Forest_Cover_QGIS == "Closed coniferous forest") %>% 
  ggplot() +  
  geom_point(aes(x = Latitude, y = Biomass_kg_ha_suivant_Crete_et_al))



################################################################################

# ---------------------------- Correlations ---------------------------------- #

################################################################################

cor_var <- cor(biomass_site[c(
  "Latitude", "Longitude", "Biomass_kg_ha_suivant_Crete_et_al", 
  "Moyenne_biomasse_lichen_frut_0_1m", "Moyenne_biomasse_lichen_frut_1_3m", 
  "Biomasse_tiges_corrigee")])

corrplot(cor_var, method="number")


