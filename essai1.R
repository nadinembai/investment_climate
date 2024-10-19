library(readxl)
library(tidyr)
library(dplyr)
library(openxlsx)

safri<-read.csv2(Data_AOF)


library(dplyr)

energy<- safri%>%
  filter(Sector%in% c("II.3. Energy"))
#summary (energy)
summary (Data_AOF)

library(readxl)
library(tidyr)
library(openxlsx)

saoenergy<- Data_AOF%>% filter(Sector %in% c("II.3.Energy"))

energyaof<- Finance%>% filter(Sector%in% c("II.3. Energy"))
#summary (energy)
summary (Data_AOF)
View(Data_AOF)
drop("ene_mitif")

#renommer une variable
names(Finance)[names(Finance) == "Sector (detailed)"] <- "Sector"

## Making a map with "Mitigation of 2021" for AOF
```{r}
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
energi<- Finance%>%
  filter(Sector %in% c("II.3.Energy"))
summary(energi)

ener<- Finance%>%
  filter(Sector%in% c())
summary(ener)

names (energyaof)

ene_mitig <- filter(energyaof, "Mitigation-related development finance - Commitment - 2021 USD thousand" >= 0)

names(ene_mitig)[names(ene_mitig) == "Recipient"] <- "region"
names(ene_mitig)[names(ene_mitig) == "Mitigation-related.development.finance.-.Commitment.-.2021.USD.thousand"] <- "Mitigation"

ene_mitigf <- left_join(worldgood, ene_mitig, by = "region")

# Pour enlever les lignes vides de la variable Mitigation
library(tidyr)
ene_mitif <- ene_mitigf %>% 
  drop_na(c(Mitigation))

#ene_mitif <- filter(ene_mitif, "Mitigation" > 0)

summary(ene_mitif$Mitigation)
library(ggplot2)
ggplot(data = ene_mitif, aes(x = long, y = lat, group = group, fill = Mitigation)) + 
  theme_void() +
  theme(legend.position = "right") +
  geom_polygon (color = "black", size = 0.8) +
  scale_fill_continuous(high = "#FFFF00"  , low = "#8FBC8F") +
  labs(title = "Finance relative to energy mitigation 2021",
       caption = "Source: Nüance-R")

## Making a map with "Adaptation of 2021" for AOF
```{r}
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
energi<- Finance%>%
  filter(Sector %in% c("II.3.Energy"))
summary(energi)

ener<- Finance%>%
  filter(Sector%in% c())
summary(ener)

names (energyaof)

ene_adap <- filter(energyaof, "Adaptation-related.development.finance.-.Commitment.-.2021.USD.thousand" >= 0)

names(ene_adap)[names(ene_adap) == "Recipient"] <- "region"
names(ene_adap)[names(ene_mitig) == "Adaptation-related.development.finance.-.Commitment.-.2021.USD.thousand"] <- "Adaptation"

ene_adapft <- left_join(worldgood, ene_adap, by = "region")

# Pour enlever les lignes vides de la variable Adaptation
library(tidyr)
ene_adapf <- ene_adapft %>% 
  drop_na(c(Adaptation))

#ene_adapf <- filter(ene_adapf, "Adaptation" > 0)

summary(ene_adapf$Adaptation)

ggplot(data = ene_adapf, aes(x = long, y = lat, group = group, fill = Adaptation)) + 
  theme_void() +
  theme(legend.position = "right") +
  geom_polygon (color = "black", size = 0.8) +
  scale_fill_continuous(high = "#ff7f00"  , low = "#8FBC8F") +
  labs(title = "Finance relative to energy adaptation 2021",
       caption = "Source: Nüance-R")



##PROVIDER DAC
enerdac<- energyaof%>%
  filter(Provider.Type%in% c("DAC member"))
summary(enerdac)
ggplot(data=enerdac, aes(x = Recipient, fill = 'Provider.Type')) +
  geom_bar(position = "stack") +
  labs(title = "Histogramme provider par rapport au pays",
       x = "Pays de l'AOF",
       y = "Fréquence ",
       fill = "providerdac") +
  theme_minimal()

names(energyaof)

##PROVIDER BANK
enerbank<- energyaof%>%
  filter(Provider.Type%in% c("Multilateral development bank"))
summary(enerbank)
ggplot(data=enerbank, aes(x = Recipient, fill = 'Provider.Type')) +
  geom_bar(position = "stack") +
  labs(title = "Histogramme provider par rapport au pays",
       x = "Pays de l'AOF",
       y = "Fréquence ",
       fill = "providerbank") +
  theme_minimal()

##PROVIDER other
enerother<- energyaof%>%
  filter(Provider.Type%in% c("Other multilateral"))
summary(enerother)
ggplot(data=enerother, aes(x = Recipient, fill = 'Provider.Type')) +
  geom_bar(position = "stack") +
  labs(title = "Histogramme provider par rapport au pays",
       x = "Pays de l'AOF",
       y = "Fréquence ",
       fill = "providerother") +
  theme_minimal()

