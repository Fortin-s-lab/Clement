library("tidyverse")
library("e1071")
library("FactoMineR")
library("factoextra")
library("corrplot")
library(lme4)
library(lmerTest)
library(emmeans)

##### PCA of the various biomass variables
PCA_biomass <- PCA(Biomasses_site[c("Biomass_kg_ha_suivant_Cr?te_et_al", "Moyenne_biomasse_lichen_frut_0_1m", 
                                    "Moyenne_biomasse_lichen_frut_1_3m", "Biomasse_tiges")], graph = FALSE)

fviz_eig(PCA_biomass, addlabels = TRUE)

fviz_pca_var(PCA_biomass, axes = c(1, 2), col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

cor_biomass <- cor(Biomasses_site[c("Biomass_kg_ha_suivant_Cr?te_et_al", "Moyenne_biomasse_lichen_frut_0_1m", 
                                    "Moyenne_biomasse_lichen_frut_1_3m", "Biomasse_tiges")])
corrplot(cor_biomass)

ggplot(data=Biomasses_site) + 
  geom_point(aes(x=Biomass_kg_ha_suivant_Cr?te_et_al, y=Moyenne_biomasse_lichen_frut_0_1m),
             color="blue")


##### Dispersion Biomass variables 
skewness(Biomasses_site$Biomass_kg_ha_suivant_Cr?te_et_al)
skewness(Biomasses_site$Moyenne_biomasse_lichen_frut_0_1m)
skewness(Biomasses_site$Moyenne_biomasse_lichen_frut_1_3m)
skewness(Biomasses_site$Biomasse_tiges)

#They all are >1 --> switch to log


##### Mettre la classe d'habitat "de reference" en premiere
Biomasses_site$Forest_Cover_QGIS <- factor(Biomasses_site$Forest_Cover_QGIS, 
                                           levels = c("Open coniferous forest", "Closed coniferous forest", "Mixed forest", "Deciduous forest", "Lande a lichen",
                                                      "Young natural diusturbances (fire < 10 years old)", "Natural disturbance (10-19 years old)", "Natural disturbance (10-19 yrs old)", "Natural disturbance (20 years old)",
                                                      "Young clearcut ( < 10 years old)", "Clearcut 10-19 years old", "Clearcut 20 years old"))



##### Choix des modèles

### Biomasse tiges corrige en fonction de latitude et forest cover

reg_biom_tiges_lat <- lm(data = Biomasses_site, 
                        formula = Biomasse_tiges_corrigee ~ Latitude)
par(mfrow = c(2, 2))
plot(reg_biom_tiges_lat)
anova(reg_biom_tiges_lat)
summary(reg_biom_tiges_lat)

reg_biom_tiges_vege <- lm(data = Biomasses_site, 
                         formula = Biomasse_tiges_corrigee ~ Forest_Cover_QGIS)
par(mfrow = c(2, 2))
plot(reg_biom_tiges_vege)
anova(reg_biom_tiges_vege)
summary(reg_biom_tiges_vege)

reg_log_biom_tiges_nul <- lm(data = Biomasses_site, 
                             formula = log(Biomasse_tiges_corrigee + 1) ~ 1)

reg_log_biom_tiges_lat <- lm(data = Biomasses_site, 
                             formula = log(Biomasse_tiges_corrigee + 1) ~ Latitude)
par(mfrow = c(2, 2))
plot(reg_log_biom_tiges_lat)
anova(reg_log_biom_tiges_lat)
summary(reg_log_biom_tiges_lat)

reg_log_biom_tiges_vege <- lm(data = Biomasses_site, 
                             formula = log(Biomasse_tiges_corrigee + 1) ~ Forest_Cover_QGIS)
par(mfrow = c(2, 2))
plot(reg_log_biom_tiges_vege)
anova(reg_log_biom_tiges_vege)
summary(reg_log_biom_tiges_vege)

reg_log_biom_tiges_lat_vege_mixte <- lmer(data = Biomasses_site, 
                                          formula = log(Biomasse_tiges_corrigee + 1) ~ Latitude + (1 | Forest_Cover_QGIS))
par(mfrow = c(2, 2))
plot(reg_log_biom_tiges_lat_vege_mixte)
anova(reg_log_biom_tiges_lat_vege_mixte)
car::Anova(reg_log_biom_tiges_lat_vege_mixte)
summary(reg_log_biom_tiges_lat_vege_mixte)

reg_log_biom_tiges_lat_vege <- lm(data = Biomasses_site, 
                                          formula = log(Biomasse_tiges_corrigee + 1) ~ Latitude + Forest_Cover_QGIS)
par(mfrow = c(2, 2))
plot(reg_log_biom_tiges_lat_vege)
anova(reg_log_biom_tiges_lat_vege)
car::Anova(reg_log_biom_tiges_lat_vege)
summary(reg_log_biom_tiges_lat_vege)

reg_log_biom_tiges_lat_vege_inter <- lm(data = Biomasses_site, 
                                  formula = log(Biomasse_tiges_corrigee + 1) ~ Latitude * Forest_Cover_QGIS)
par(mfrow = c(2, 2))
plot(reg_log_biom_tiges_lat_vege_inter)
anova(reg_log_biom_tiges_lat_vege_inter)
car::Anova(reg_log_biom_tiges_lat_vege_inter)
summary(reg_log_biom_tiges_lat_vege_inter)

AIC(reg_biom_tiges_lat)
AIC(reg_biom_tiges_vege)
AIC(reg_log_biom_tiges_lat)
AIC(reg_log_biom_tiges_vege)
AIC(reg_log_biom_tiges_lat_vege_mixte)
AIC(reg_log_biom_tiges_lat_vege)
AIC(reg_log_biom_tiges_lat_vege_inter)
anova(reg_log_biom_tiges_lat, reg_log_biom_tiges_nul)
anova(reg_log_biom_tiges_vege, reg_log_biom_tiges_nul)
anova(reg_log_biom_tiges_lat_vege_mixte, reg_log_biom_tiges_lat)
anova(reg_log_biom_tiges_lat_vege, reg_log_biom_tiges_lat)
anova(reg_log_biom_tiges_lat_vege_inter, reg_log_biom_tiges_lat_vege)

#--> Best model : only latitude
summary(reg_log_biom_tiges_lat)




### Biomasse ter_lichen corrige en fonction de latitude et forest cover

reg_biom_ter_lichen_lat <- lm(data = Biomasses_site, 
                         formula = Biomass_kg_ha_suivant_Crete_et_al ~ Latitude)
par(mfrow = c(2, 2))
plot(reg_biom_ter_lichen_lat)
anova(reg_biom_ter_lichen_lat)
summary(reg_biom_ter_lichen_lat)

reg_biom_ter_lichen_vege <- lm(data = Biomasses_site, 
                          formula = Biomass_kg_ha_suivant_Crete_et_al ~ Forest_Cover_QGIS)
par(mfrow = c(2, 2))
plot(reg_biom_ter_lichen_vege)
anova(reg_biom_ter_lichen_vege)
summary(reg_biom_ter_lichen_vege)

reg_log_biom_ter_lichen_nul <- lm(data = Biomasses_site, 
                             formula = log(Biomass_kg_ha_suivant_Crete_et_al + 1) ~ 1)

reg_log_biom_ter_lichen_lat <- lm(data = Biomasses_site, 
                             formula = log(Biomass_kg_ha_suivant_Crete_et_al + 1) ~ Latitude)
par(mfrow = c(2, 2))
plot(reg_log_biom_ter_lichen_lat)
anova(reg_log_biom_ter_lichen_lat)
summary(reg_log_biom_ter_lichen_lat)

reg_log_biom_ter_lichen_vege <- lm(data = Biomasses_site, 
                              formula = log(Biomass_kg_ha_suivant_Crete_et_al + 1) ~ Forest_Cover_QGIS)
par(mfrow = c(2, 2))
plot(reg_log_biom_ter_lichen_vege)
anova(reg_log_biom_ter_lichen_vege)
summary(reg_log_biom_ter_lichen_vege)

reg_log_biom_ter_lichen_lat_vege_mixte <- lmer(data = Biomasses_site, 
                                          formula = log(Biomass_kg_ha_suivant_Crete_et_al + 1) ~ Latitude + (1 | Forest_Cover_QGIS))
par(mfrow = c(2, 2))
plot(reg_log_biom_ter_lichen_lat_vege_mixte)
anova(reg_log_biom_ter_lichen_lat_vege_mixte)
car::Anova(reg_log_biom_ter_lichen_lat_vege_mixte)
summary(reg_log_biom_ter_lichen_lat_vege_mixte)

reg_log_biom_ter_lichen_lat_vege <- lm(data = Biomasses_site, 
                                  formula = log(Biomass_kg_ha_suivant_Crete_et_al + 1) ~ Latitude + Forest_Cover_QGIS)
par(mfrow = c(2, 2))
plot(reg_log_biom_ter_lichen_lat_vege)
anova(reg_log_biom_ter_lichen_lat_vege)
car::Anova(reg_log_biom_ter_lichen_lat_vege)
summary(reg_log_biom_ter_lichen_lat_vege)

reg_log_biom_ter_lichen_lat_vege_inter <- lm(data = Biomasses_site, 
                                        formula = log(Biomass_kg_ha_suivant_Crete_et_al + 1) ~ Latitude * Forest_Cover_QGIS)
par(mfrow = c(2, 2))
plot(reg_log_biom_ter_lichen_lat_vege_inter)
anova(reg_log_biom_ter_lichen_lat_vege_inter)
car::Anova(reg_log_biom_ter_lichen_lat_vege_inter)
summary(reg_log_biom_ter_lichen_lat_vege_inter)

AIC(reg_biom_ter_lichen_lat)
AIC(reg_biom_ter_lichen_vege)
AIC(reg_log_biom_ter_lichen_lat)
AIC(reg_log_biom_ter_lichen_vege)
AIC(reg_log_biom_ter_lichen_lat_vege_mixte)
AIC(reg_log_biom_ter_lichen_lat_vege)
AIC(reg_log_biom_ter_lichen_lat_vege_inter)
anova(reg_log_biom_ter_lichen_lat, reg_log_biom_ter_lichen_nul)
anova(reg_log_biom_ter_lichen_vege, reg_log_biom_ter_lichen_nul)
anova(reg_log_biom_ter_lichen_lat_vege_mixte, reg_log_biom_ter_lichen_lat)
anova(reg_log_biom_ter_lichen_lat_vege, reg_log_biom_ter_lichen_lat)
anova(reg_log_biom_ter_lichen_lat_vege_inter, reg_log_biom_ter_lichen_lat_vege)

#--> Best model : latitute + vegetation
summary(reg_log_biom_ter_lichen_lat_vege)
emmeans(reg_log_biom_ter_lichen_lat_vege, pairwise ~ Forest_Cover_QGIS, adjust = "holm")



### Biomasse arb_lichen_01 en fonction de latitude et forest cover

reg_biom_arb_lichen_01_lat <- lm(data = Biomasses_site, 
                              formula = Moyenne_biomasse_lichen_frut_0_1m ~ Latitude)
par(mfrow = c(2, 2))
plot(reg_biom_arb_lichen_01_lat)
anova(reg_biom_arb_lichen_01_lat)
summary(reg_biom_arb_lichen_01_lat)

reg_biom_arb_lichen_01_vege <- lm(data = Biomasses_site, 
                               formula = Moyenne_biomasse_lichen_frut_0_1m ~ Forest_Cover_QGIS)
par(mfrow = c(2, 2))
plot(reg_biom_arb_lichen_01_vege)
anova(reg_biom_arb_lichen_01_vege)
summary(reg_biom_arb_lichen_01_vege)

reg_log_biom_arb_lichen_01_nul <- lm(data = Biomasses_site, 
                                  formula = log(Moyenne_biomasse_lichen_frut_0_1m + 1) ~ 1)

reg_log_biom_arb_lichen_01_lat <- lm(data = Biomasses_site, 
                                  formula = log(Moyenne_biomasse_lichen_frut_0_1m + 1) ~ Latitude)
par(mfrow = c(2, 2))
plot(reg_log_biom_arb_lichen_01_lat)
anova(reg_log_biom_arb_lichen_01_lat)
summary(reg_log_biom_arb_lichen_01_lat)

reg_log_biom_arb_lichen_01_vege <- lm(data = Biomasses_site, 
                                   formula = log(Moyenne_biomasse_lichen_frut_0_1m + 1) ~ Forest_Cover_QGIS)
par(mfrow = c(2, 2))
plot(reg_log_biom_arb_lichen_01_vege)
anova(reg_log_biom_arb_lichen_01_vege)
summary(reg_log_biom_arb_lichen_01_vege)

reg_log_biom_arb_lichen_01_lat_vege_mixte <- lmer(data = Biomasses_site, 
                                               formula = log(Moyenne_biomasse_lichen_frut_0_1m + 1) ~ Latitude + (1 | Forest_Cover_QGIS))
par(mfrow = c(2, 2))
plot(reg_log_biom_arb_lichen_01_lat_vege_mixte)
anova(reg_log_biom_arb_lichen_01_lat_vege_mixte)
car::Anova(reg_log_biom_arb_lichen_01_lat_vege_mixte)
summary(reg_log_biom_arb_lichen_01_lat_vege_mixte)

reg_log_biom_arb_lichen_01_lat_vege <- lm(data = Biomasses_site, 
                                       formula = log(Moyenne_biomasse_lichen_frut_0_1m + 1) ~ Latitude + Forest_Cover_QGIS)
par(mfrow = c(2, 2))
plot(reg_log_biom_arb_lichen_01_lat_vege)
anova(reg_log_biom_arb_lichen_01_lat_vege)
car::Anova(reg_log_biom_arb_lichen_01_lat_vege)
summary(reg_log_biom_arb_lichen_01_lat_vege)

reg_log_biom_arb_lichen_01_lat_vege_inter <- lm(data = Biomasses_site, 
                                             formula = log(Moyenne_biomasse_lichen_frut_0_1m + 1) ~ Latitude * Forest_Cover_QGIS)
par(mfrow = c(2, 2))
plot(reg_log_biom_arb_lichen_01_lat_vege_inter)
anova(reg_log_biom_arb_lichen_01_lat_vege_inter)
car::Anova(reg_log_biom_arb_lichen_01_lat_vege_inter)
summary(reg_log_biom_arb_lichen_01_lat_vege_inter)

AIC(reg_biom_arb_lichen_01_lat)
AIC(reg_biom_arb_lichen_01_vege)
AIC(reg_log_biom_arb_lichen_01_lat)
AIC(reg_log_biom_arb_lichen_01_vege)
AIC(reg_log_biom_arb_lichen_01_lat_vege_mixte)
AIC(reg_log_biom_arb_lichen_01_lat_vege)
AIC(reg_log_biom_arb_lichen_01_lat_vege_inter)
anova(reg_log_biom_arb_lichen_01_lat, reg_log_biom_arb_lichen_01_nul)
anova(reg_log_biom_arb_lichen_01_vege, reg_log_biom_arb_lichen_01_nul)
anova(reg_log_biom_arb_lichen_01_lat_vege_mixte, reg_log_biom_arb_lichen_01_lat)
anova(reg_log_biom_arb_lichen_01_lat_vege, reg_log_biom_arb_lichen_01_lat)
anova(reg_log_biom_arb_lichen_01_lat_vege_inter, reg_log_biom_arb_lichen_01_lat_vege)

#--> Best model : latitude + vegetation
summary(reg_log_biom_arb_lichen_01_lat_vege)
emmeans(reg_log_biom_arb_lichen_01_lat_vege, pairwise ~ Forest_Cover_QGIS, adjust="holm")



### Biomasse arb_lichen_13 en fonction de latitude et forest cover

reg_biom_arb_lichen_13_lat <- lm(data = Biomasses_site, 
                                 formula = Moyenne_biomasse_lichen_frut_1_3m ~ Latitude)
par(mfrow = c(2, 2))
plot(reg_biom_arb_lichen_13_lat)
anova(reg_biom_arb_lichen_13_lat)
summary(reg_biom_arb_lichen_13_lat)

reg_biom_arb_lichen_13_vege <- lm(data = Biomasses_site, 
                                  formula = Moyenne_biomasse_lichen_frut_1_3m ~ Forest_Cover_QGIS)
par(mfrow = c(2, 2))
plot(reg_biom_arb_lichen_13_vege)
anova(reg_biom_arb_lichen_13_vege)
summary(reg_biom_arb_lichen_13_vege)

reg_log_biom_arb_lichen_13_nul <- lm(data = Biomasses_site, 
                                     formula = log(Moyenne_biomasse_lichen_frut_1_3m + 1) ~ 1)

reg_log_biom_arb_lichen_13_lat <- lm(data = Biomasses_site, 
                                     formula = log(Moyenne_biomasse_lichen_frut_1_3m + 1) ~ Latitude)
par(mfrow = c(2, 2))
plot(reg_log_biom_arb_lichen_13_lat)
anova(reg_log_biom_arb_lichen_13_lat)
summary(reg_log_biom_arb_lichen_13_lat)

reg_log_biom_arb_lichen_13_vege <- lm(data = Biomasses_site, 
                                      formula = log(Moyenne_biomasse_lichen_frut_1_3m + 1) ~ Forest_Cover_QGIS)
par(mfrow = c(2, 2))
plot(reg_log_biom_arb_lichen_13_vege)
anova(reg_log_biom_arb_lichen_13_vege)
summary(reg_log_biom_arb_lichen_13_vege)

reg_log_biom_arb_lichen_13_lat_vege_mixte <- lmer(data = Biomasses_site, 
                                                  formula = log(Moyenne_biomasse_lichen_frut_1_3m + 1) ~ Latitude + (1 | Forest_Cover_QGIS))
par(mfrow = c(2, 2))
plot(reg_log_biom_arb_lichen_13_lat_vege_mixte)
anova(reg_log_biom_arb_lichen_13_lat_vege_mixte)
car::Anova(reg_log_biom_arb_lichen_13_lat_vege_mixte)
summary(reg_log_biom_arb_lichen_13_lat_vege_mixte)

reg_log_biom_arb_lichen_13_lat_vege <- lm(data = Biomasses_site, 
                                          formula = log(Moyenne_biomasse_lichen_frut_1_3m + 1) ~ Latitude + Forest_Cover_QGIS)
par(mfrow = c(2, 2))
plot(reg_log_biom_arb_lichen_13_lat_vege)
anova(reg_log_biom_arb_lichen_13_lat_vege)
car::Anova(reg_log_biom_arb_lichen_13_lat_vege)
summary(reg_log_biom_arb_lichen_13_lat_vege)

reg_log_biom_arb_lichen_13_lat_vege_inter <- lm(data = Biomasses_site, 
                                                formula = log(Moyenne_biomasse_lichen_frut_1_3m + 1) ~ Latitude * Forest_Cover_QGIS)
par(mfrow = c(2, 2))
plot(reg_log_biom_arb_lichen_13_lat_vege_inter)
anova(reg_log_biom_arb_lichen_13_lat_vege_inter)
car::Anova(reg_log_biom_arb_lichen_13_lat_vege_inter)
summary(reg_log_biom_arb_lichen_13_lat_vege_inter)

AIC(reg_biom_arb_lichen_13_lat)
AIC(reg_biom_arb_lichen_13_vege)
AIC(reg_log_biom_arb_lichen_13_lat)
AIC(reg_log_biom_arb_lichen_13_vege)
AIC(reg_log_biom_arb_lichen_13_lat_vege_mixte)
AIC(reg_log_biom_arb_lichen_13_lat_vege)
AIC(reg_log_biom_arb_lichen_13_lat_vege_inter)
anova(reg_log_biom_arb_lichen_13_lat, reg_log_biom_arb_lichen_13_nul)
anova(reg_log_biom_arb_lichen_13_vege, reg_log_biom_arb_lichen_13_nul)
anova(reg_log_biom_arb_lichen_13_lat_vege_mixte, reg_log_biom_arb_lichen_13_lat)
anova(reg_log_biom_arb_lichen_13_lat_vege, reg_log_biom_arb_lichen_13_lat)
anova(reg_log_biom_arb_lichen_13_lat_vege_inter, reg_log_biom_arb_lichen_13_lat_vege)

#--> Best model : Vegetation only
summary(reg_log_biom_arb_lichen_13_vege)
emmeans(reg_log_biom_arb_lichen_13_vege, pairwise ~ Forest_Cover_QGIS, adjust = "holm")



## Biomass twigs balsam fir (Abies balsamea)

reg_biom_abies_lat <- lm(data = Biomasses_site, 
                                 formula = Sapin_baumier_corrige ~ Latitude)
par(mfrow = c(2, 2))
plot(reg_biom_abies_lat)
anova(reg_biom_abies_lat)
summary(reg_biom_abies_lat)

reg_biom_abies_vege <- lm(data = Biomasses_site, 
                                  formula = Sapin_baumier_corrige ~ Forest_Cover_QGIS)
par(mfrow = c(2, 2))
plot(reg_biom_abies_vege)
anova(reg_biom_abies_vege)
summary(reg_biom_abies_vege)

reg_log_biom_abies_nul <- lm(data = Biomasses_site, 
                                     formula = log(Sapin_baumier_corrige + 1) ~ 1)

reg_log_biom_abies_lat <- lm(data = Biomasses_site, 
                                     formula = log(Sapin_baumier_corrige + 1) ~ Latitude)
par(mfrow = c(2, 2))
plot(reg_log_biom_abies_lat)
anova(reg_log_biom_abies_lat)
summary(reg_log_biom_abies_lat)

reg_log_biom_abies_vege <- lm(data = Biomasses_site, 
                                      formula = log(Sapin_baumier_corrige + 1) ~ Forest_Cover_QGIS)
par(mfrow = c(2, 2))
plot(reg_log_biom_abies_vege)
anova(reg_log_biom_abies_vege)
summary(reg_log_biom_abies_vege)

reg_log_biom_abies_lat_vege_mixte <- lmer(data = Biomasses_site, 
                                                  formula = log(Sapin_baumier_corrige + 1) ~ Latitude + (1 | Forest_Cover_QGIS))
par(mfrow = c(2, 2))
plot(reg_log_biom_abies_lat_vege_mixte)
anova(reg_log_biom_abies_lat_vege_mixte)
car::Anova(reg_log_biom_abies_lat_vege_mixte)
summary(reg_log_biom_abies_lat_vege_mixte)

reg_log_biom_abies_lat_vege <- lm(data = Biomasses_site, 
                                          formula = log(Sapin_baumier_corrige + 1) ~ Latitude + Forest_Cover_QGIS)
par(mfrow = c(2, 2))
plot(reg_log_biom_abies_lat_vege)
anova(reg_log_biom_abies_lat_vege)
car::Anova(reg_log_biom_abies_lat_vege)
summary(reg_log_biom_abies_lat_vege)

reg_log_biom_abies_lat_vege_inter <- lm(data = Biomasses_site, 
                                                formula = log(Sapin_baumier_corrige + 1) ~ Latitude * Forest_Cover_QGIS)
par(mfrow = c(2, 2))
plot(reg_log_biom_abies_lat_vege_inter)
anova(reg_log_biom_abies_lat_vege_inter)
car::Anova(reg_log_biom_abies_lat_vege_inter)
summary(reg_log_biom_abies_lat_vege_inter)

AIC(reg_biom_abies_lat)
AIC(reg_biom_abies_vege)
AIC(reg_log_biom_abies_lat)
AIC(reg_log_biom_abies_vege)
AIC(reg_log_biom_abies_lat_vege_mixte)
AIC(reg_log_biom_abies_lat_vege)
AIC(reg_log_biom_abies_lat_vege_inter)
anova(reg_log_biom_abies_lat, reg_log_biom_abies_nul)
anova(reg_log_biom_abies_vege, reg_log_biom_abies_nul)
anova(reg_log_biom_abies_lat_vege_mixte, reg_log_biom_abies_lat)
anova(reg_log_biom_abies_lat_vege, reg_log_biom_abies_lat)
anova(reg_log_biom_abies_lat_vege, reg_log_biom_abies_vege)
anova(reg_log_biom_abies_lat_vege_inter, reg_log_biom_abies_lat_vege)

#--> Best model : (latitude +) vegetation
summary(reg_log_biom_abies_vege)
summary(reg_log_biom_abies_lat_vege)
emmeans(reg_log_biom_abies_lat_vege, pairwise ~ Forest_Cover_QGIS, adjust = "holm")



## Biomass twigs moose

reg_biom_twigs_moose_lat <- lm(data = Biomasses_site, 
                         formula = Biomasse_tiges_moose ~ Latitude)
par(mfrow = c(2, 2))
plot(reg_biom_twigs_moose_lat)
anova(reg_biom_twigs_moose_lat)
summary(reg_biom_twigs_moose_lat)

reg_biom_twigs_moose_vege <- lm(data = Biomasses_site, 
                          formula = Biomasse_tiges_moose ~ Forest_Cover_QGIS)
par(mfrow = c(2, 2))
plot(reg_biom_twigs_moose_vege)
anova(reg_biom_twigs_moose_vege)
summary(reg_biom_twigs_moose_vege)

reg_log_biom_twigs_moose_nul <- lm(data = Biomasses_site, 
                             formula = log(Biomasse_tiges_moose + 1) ~ 1)

reg_log_biom_twigs_moose_lat <- lm(data = Biomasses_site, 
                             formula = log(Biomasse_tiges_moose + 1) ~ Latitude)
par(mfrow = c(2, 2))
plot(reg_log_biom_twigs_moose_lat)
anova(reg_log_biom_twigs_moose_lat)
summary(reg_log_biom_twigs_moose_lat)

reg_log_biom_twigs_moose_vege <- lm(data = Biomasses_site, 
                              formula = log(Biomasse_tiges_moose + 1) ~ Forest_Cover_QGIS)
par(mfrow = c(2, 2))
plot(reg_log_biom_twigs_moose_vege)
anova(reg_log_biom_twigs_moose_vege)
summary(reg_log_biom_twigs_moose_vege)

reg_log_biom_twigs_moose_lat_vege_mixte <- lmer(data = Biomasses_site, 
                                          formula = log(Biomasse_tiges_moose + 1) ~ Latitude + (1 | Forest_Cover_QGIS))
par(mfrow = c(2, 2))
plot(reg_log_biom_twigs_moose_lat_vege_mixte)
anova(reg_log_biom_twigs_moose_lat_vege_mixte)
car::Anova(reg_log_biom_twigs_moose_lat_vege_mixte)
summary(reg_log_biom_twigs_moose_lat_vege_mixte)

reg_log_biom_twigs_moose_lat_vege <- lm(data = Biomasses_site, 
                                  formula = log(Biomasse_tiges_moose + 1) ~ Latitude + Forest_Cover_QGIS)
par(mfrow = c(2, 2))
plot(reg_log_biom_twigs_moose_lat_vege)
anova(reg_log_biom_twigs_moose_lat_vege)
car::Anova(reg_log_biom_twigs_moose_lat_vege)
summary(reg_log_biom_twigs_moose_lat_vege)

reg_log_biom_twigs_moose_lat_vege_inter <- lm(data = Biomasses_site, 
                                        formula = log(Biomasse_tiges_moose + 1) ~ Latitude * Forest_Cover_QGIS)
par(mfrow = c(2, 2))
plot(reg_log_biom_twigs_moose_lat_vege_inter)
anova(reg_log_biom_twigs_moose_lat_vege_inter)
car::Anova(reg_log_biom_twigs_moose_lat_vege_inter)
summary(reg_log_biom_twigs_moose_lat_vege_inter)

AIC(reg_biom_twigs_moose_lat)
AIC(reg_biom_twigs_moose_vege)
AIC(reg_log_biom_twigs_moose_lat)
AIC(reg_log_biom_twigs_moose_vege)
AIC(reg_log_biom_twigs_moose_lat_vege_mixte)
AIC(reg_log_biom_twigs_moose_lat_vege)
AIC(reg_log_biom_twigs_moose_lat_vege_inter)
anova(reg_log_biom_twigs_moose_lat, reg_log_biom_twigs_moose_nul)
anova(reg_log_biom_twigs_moose_vege, reg_log_biom_twigs_moose_nul)
anova(reg_log_biom_twigs_moose_lat_vege_mixte, reg_log_biom_twigs_moose_lat)
anova(reg_log_biom_twigs_moose_lat_vege, reg_log_biom_twigs_moose_lat)
anova(reg_log_biom_twigs_moose_lat_vege_inter, reg_log_biom_twigs_moose_lat_vege)

#--> Best model : latitude or nul
summary(reg_log_biom_twigs_moose_lat)



## Biomass twigs caribou

reg_biom_twigs_caribou_lat <- lm(data = Biomasses_site, 
                               formula = Biomasse_tiges_caribou ~ Latitude)
par(mfrow = c(2, 2))
plot(reg_biom_twigs_caribou_lat)
anova(reg_biom_twigs_caribou_lat)
summary(reg_biom_twigs_caribou_lat)

reg_biom_twigs_caribou_vege <- lm(data = Biomasses_site, 
                                formula = Biomasse_tiges_caribou ~ Forest_Cover_QGIS)
par(mfrow = c(2, 2))
plot(reg_biom_twigs_caribou_vege)
anova(reg_biom_twigs_caribou_vege)
summary(reg_biom_twigs_caribou_vege)

reg_log_biom_twigs_caribou_nul <- lm(data = Biomasses_site, 
                                   formula = log(Biomasse_tiges_caribou + 1) ~ 1)

reg_log_biom_twigs_caribou_lat <- lm(data = Biomasses_site, 
                                   formula = log(Biomasse_tiges_caribou + 1) ~ Latitude)
par(mfrow = c(2, 2))
plot(reg_log_biom_twigs_caribou_lat)
anova(reg_log_biom_twigs_caribou_lat)
summary(reg_log_biom_twigs_caribou_lat)

reg_log_biom_twigs_caribou_vege <- lm(data = Biomasses_site, 
                                    formula = log(Biomasse_tiges_caribou + 1) ~ Forest_Cover_QGIS)
par(mfrow = c(2, 2))
plot(reg_log_biom_twigs_caribou_vege)
anova(reg_log_biom_twigs_caribou_vege)
summary(reg_log_biom_twigs_caribou_vege)

reg_log_biom_twigs_caribou_lat_vege_mixte <- lmer(data = Biomasses_site, 
                                                formula = log(Biomasse_tiges_caribou + 1) ~ Latitude + (1 | Forest_Cover_QGIS))
par(mfrow = c(2, 2))
plot(reg_log_biom_twigs_caribou_lat_vege_mixte)
anova(reg_log_biom_twigs_caribou_lat_vege_mixte)
car::Anova(reg_log_biom_twigs_caribou_lat_vege_mixte)
summary(reg_log_biom_twigs_caribou_lat_vege_mixte)

reg_log_biom_twigs_caribou_lat_vege <- lm(data = Biomasses_site, 
                                        formula = log(Biomasse_tiges_caribou + 1) ~ Latitude + Forest_Cover_QGIS)
par(mfrow = c(2, 2))
plot(reg_log_biom_twigs_caribou_lat_vege)
anova(reg_log_biom_twigs_caribou_lat_vege)
car::Anova(reg_log_biom_twigs_caribou_lat_vege)
summary(reg_log_biom_twigs_caribou_lat_vege)

reg_log_biom_twigs_caribou_lat_vege_inter <- lm(data = Biomasses_site, 
                                              formula = log(Biomasse_tiges_caribou + 1) ~ Latitude * Forest_Cover_QGIS)
par(mfrow = c(2, 2))
plot(reg_log_biom_twigs_caribou_lat_vege_inter)
anova(reg_log_biom_twigs_caribou_lat_vege_inter)
car::Anova(reg_log_biom_twigs_caribou_lat_vege_inter)
summary(reg_log_biom_twigs_caribou_lat_vege_inter)

AIC(reg_biom_twigs_caribou_lat)
AIC(reg_biom_twigs_caribou_vege)
AIC(reg_log_biom_twigs_caribou_lat)
AIC(reg_log_biom_twigs_caribou_vege)
AIC(reg_log_biom_twigs_caribou_lat_vege_mixte)
AIC(reg_log_biom_twigs_caribou_lat_vege)
AIC(reg_log_biom_twigs_caribou_lat_vege_inter)
anova(reg_log_biom_twigs_caribou_lat, reg_log_biom_twigs_caribou_nul)
anova(reg_log_biom_twigs_caribou_vege, reg_log_biom_twigs_caribou_nul)
anova(reg_log_biom_twigs_caribou_lat_vege_mixte, reg_log_biom_twigs_caribou_lat)
anova(reg_log_biom_twigs_caribou_lat_vege, reg_log_biom_twigs_caribou_lat)
anova(reg_log_biom_twigs_caribou_lat_vege_inter, reg_log_biom_twigs_caribou_lat_vege)

#--> Best model : latitude or nul
summary(reg_log_biom_twigs_caribou_lat)





## Results table

results1 <- summary(reg_log_biom1_lat_vege)$coefficients
Variables <- c(row.names(results1))
results1 <- data.frame(Variables, results1)

results2 <- summary(reg_log_biom2_lat_vege)$coefficients
results2 <- data.frame(Variables, results2)

results3 <- summary(reg_log_biom3_lat_vege)$coefficients
results3 <- data.frame(Variables, results3)

results4 <- summary(reg_log_biom4_lat_vege)$coefficients
results4 <- data.frame(Variables, results4)

results <- merge(results1, results2, by="Variables")
results <- merge(results, results3, by="Variables")
results <- merge(results, results4, by="Variables")
colnames(results) <- c("Variables", "Estimate_1", "Std_Error_1", "t-value_1", "Pr[>|t|]_1",
                       "Estimate_2", "Std_Error_2", "t-value_2", "Pr[>|t|]_2",
                       "Estimate_3", "Std_Error_3", "t-value_3", "Pr[>|t|]_3",
                       "Estimate_4", "Std_Error_4", "t-value_4", "Pr[>|t|]_4")

write.table(x=results, file="linear_models_log_biomass.csv", dec=".", sep=";")