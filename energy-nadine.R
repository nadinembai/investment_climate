
library(readxl)
library(tidyr)
library(openxlsx)

#renommer une variable
names(Finance)[names(Finance) == "Sector (detailed)"] <- "Sector"

## filtrer

energyaof<- Finance%>% filter(Sector%in% c("II.3. Energy"))
#summary (energyaof)
summary(energyaof)


#### 6.1 Réalisation des cartes suivant le secteur energie

# Extraire les données pour plusieurs pays et une seule année
library(dplyr)
Afrique <- Sda_Afriq %>% filter(`Recipient.Region` %in% c("Africa", "North of Sahara","South of Sahara"), year == 2021)


#### Adaptation pour toute l'Afrique 2021

names(Afrique)[names(Afrique) == "Adaptation-related.development.finance.-.Commitment.-.2021.USD.thousand"] <- "Adaption"

names(Afrique)[names(Afrique) == "Mitigation-related.development.finance.-.Commitment.-.2021.USD.thousand"] <- "Mitigation"

Afrique$Financ2021 <- Afrique$Adapt2021 + Afrique$Mitig2021

summary(Afrique)


library(maps)
library(ggplot2)
library(dplyr)
library(readr)
#library(rgdal)
library(sp)
library(stringr)
library(tidyverse)
library(broom)
library(readxl)
library(tidyr)
library(openxlsx)

#Pour charger la database world
worldgood <- map_data("world")

# Pour sélectionner des modalités au sein de la variable, 

energia<- Afrique%>%filter(Sector %in% c("II.3. Energy"))
summary(energia)

ene_afri <- filter(energia, "Financ2021" >= 0)

names(ene_afri)[names(ene_afri) == "Recipient"] <- "region"

ene_afrif <- left_join(worldgood, ene_afri, by = "region")

# Pour enlever les lignes vides de la variable Adaptation
library(tidyr)
ene_afri_f <- ene_afrif %>% 
  drop_na(c(Financ2021))

ene_afri_f <- filter(ene_afri_f, "Financ2021" > 0)

summary(ene_afri_f$Financ2021)

ggplot(data = ene_afri_f, aes(x = long, y = lat, group = group, fill = Financ2021)) + theme_void() +
  theme(legend.position = "right") +
  geom_polygon(color = "black", size = 0.8) +
  scale_fill_continuous(high = "#ff7f00"  , low = "#8FBC8F") +
  labs(title = "Financement de l'energie en Afrique en 2021",
       caption = "Source: Nüance-R")


### 6.2 provider type Code pour faire histogramme

#renommer une variable
names(Finance)[names(Finance) == "Sector (detailed)"] <- "Sector"

# Pour sélectionner des modalités au sein de la variable,

energyaof<- Finance%>% filter(Sector %in% c("II.3. Energy"))
summary(energyaof)

library(ggplot2)

ggplot(data= energyaof, aes(x = Provider.Type, fill = 'Provider type')) +
  geom_bar(position = "stack") +
  labs(title = "Financement energetique suivant le type de donateur",
       x = "type de donateur",
       y = "Fréquence ",
       fill = "Provider") +
  theme_minimal()

## Financement energetique par rapport aux pays AOf
ggplot(data= energyaof, aes(x = Recipient, fill = 'Provider type')) +
  geom_bar(position = "stack") +
  labs(title = "Financement energetique par rapport aux pays AOF ",
       x = "Pays membres de l'AOF",
       y = "Fréquence ",
       fill = "Provider") +
  theme_minimal()

