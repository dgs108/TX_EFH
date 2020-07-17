# Analysing Elasmobranch Longline Data

This markdown reviews the analysis associated with the paper entitled "Identification and delineation of essential habitat for elasmobranchs in estuaries on the Texas coast".

#### Packages

```{r warning=FALSE, message=FALSE}

.libPaths("/usr/share/R/library")

setwd("~/Projects/EFH/Code")

library(dplyr)
library(readr)
library(ggplot2)
library(ggmap)
library(gbm.auto)
library(patchwork)
library(raster)
library(naniar)
library(rmarkdown)
library(car)
library(ade4)
library(lme4)
library(lmerTest)
library(tidyverse)
library(grid)
library(usmap)
library(vegan)
library(ggord)
library(gdistance)
library(knitr)
library(sf)
library(sp)
library(rgeos)
library(rgdal)
library(FSA)
library(ggrepel)

source("../../../Projects/Code/ggplot.R")

col3 <- c('#f1a340','#762a83','#c51b7d')
col10 <- c('#e41a1c','darkblue','#33a02c','#984ea3','#ff7f00','gold','#a65628','#f781bf','black', 'deeppink2')

shape3 <- c(24, 21, 23)

```

#### Import Data and Assign Meta Data

```{r warning=FALSE, message=FALSE}

Elasmos <- read_csv("../Data/Elasmobranches.csv")

Meta <- read_csv("../Data/Set_Metadata.csv")

Catfish <- read_csv("../Data/Catfish.csv")

All_Animals <- read.csv("../Data/All_Animals.csv")

Channels <- read.csv("../Data/channels_coordinates.csv")

# Assign Set ID

Elasmos <- Elasmos %>%
  unite(Set_ID, Year, Month, Day, Set, sep = "_", remove = FALSE) %>%
  arrange(Set_ID)

Set_Meta <- Meta %>%
  unite(Set_ID, Year, Month, Day, Set, sep = "_", remove = FALSE) %>%
  arrange(Set_ID)

Catfish <- Catfish %>%
  unite(Set_ID, Year, Month, Day, Set, sep = "_", remove = FALSE) %>%
  arrange(Set_ID)

All_Animals <- All_Animals %>%
  unite(Set_ID, Year, Month, Day, Set, sep = "_", remove = FALSE) %>%
  arrange(Set_ID)

Temp <- Set_Meta %>%
  dplyr::select(-Set_Number, -Set, -Day, -Month, -Year, -Site)

# Assign meta data to Elasmos and Catfish

Elasmos <- inner_join(Elasmos, Temp, by = "Set_ID")

Catfish <- inner_join(Catfish, Temp, by = "Set_ID")

All_Animals <- inner_join(All_Animals, Temp, by = "Set_ID")

```

## Diversity & Abundance

Tally and plot the number of species and individuals of each species per site. 

```{r fig.height=6, fig.width=11, warning=FALSE, message=FALSE}

Species_Site <- Elasmos %>%
  group_by(Species, Site) %>% 
  tally() %>%
  spread(Site, n) %>%
  replace(is.na(.), 0) %>%
  mutate(Total = Aransas_Bay + Corpus_Christi_Bay + Redfish_Bay)

write.csv(Species_Site, "../Results/Species_Site.csv")

Species_Site_tidy <- Species_Site %>%
  gather(key = Site, value = n, 2:5)
Species_Site_tidy$Species <- str_replace_all(Species_Site_tidy$Species , "_", " ")
Species_Site_tidy$Site <- str_replace_all(Species_Site_tidy$Site , "_", " ")

ggplot(Species_Site_tidy, aes(y=n, x=Site, color=Species, fill=Species)) + 
    geom_bar(position="dodge", stat="identity") +
    theme_standard +
    scale_fill_manual(values=col10) +
    scale_color_manual(values=col10) +
    theme(legend.title = element_text(size=15), legend.text = element_text(c(size=13), face = "italic"), legend.position = "right")

ggsave("../Results/Species_Site_Plot.png", last_plot())

```

## Map Locations by Site and Species

#### Map Locations at All Sites

```{r fig.height=7, fig.width=7, warning=FALSE, message=FALSE}

# Read in shipping channel coordinates

# Plot locations across all sites

temp <- Set_Meta %>%
  mutate(Group = ifelse(Site=="Aransas_Bay", "Aransas Bay",
                        ifelse(Site=="Corpus_Christi_Bay", "Corpus Christi Bay", "Redfish Bay")))

png("../Results/All_Sites_Map.png", width = 1000, height = 1000, units = "px", pointsize = 12)

Sites_Boundaries <- c(left = -97.5, bottom = 27.6, right = -96.9, top = 28.1)

Sites_Map <- get_stamenmap(Sites_Boundaries, maptype = "terrain-background", color = "bw")

All_Sites <- ggmap(Sites_Map, extent ="normal") +
  geom_point(data = temp, aes(x = Longitude, y = Latitude, shape = Group), size = 2, color = "black") +
  scale_shape_manual(values = shape3) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_x_continuous(breaks = c(-97.5, -97.4, -97.3, -97.2, -97.1, -97.0, -96.9)) +
 # scale_y_continuous(breaks = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07)) +
  theme_standard +
  theme(legend.title = element_text(size=12), legend.text = element_text(size=12), plot.title = element_text(hjust = 0.5, size=20))

All_Sites

# Map of TX with sampling region highlighted
 
states <- map_data("state")

counties <- map_data("county")

texas <- states %>%
  filter(region == "texas")

tx_counties <- counties %>%
  filter(subregion %in% c("aransas", "nueces", "san patricio"))

Texas <- ggplot() +
  geom_polygon(data = texas, aes(x = long, y = lat), fill = "white") +
  geom_polygon(data = tx_counties, aes(x = long, y = lat), fill = "black") +
  coord_quickmap() + 
  theme_void()

# Use grid to insert TX map onto sampling map

grid.newpage()
vp <- viewport(width = 1, height = 1)
print(All_Sites, vp = vp)

subvp <- viewport(width = 0.3, height = 0.3, x = 0.25, y = 0.8)
print(Texas, vp = subvp)

ggsave("../Results/All_Sites_Map.png", last_plot())

```

#### Map Locations in Aransas Bay

```{r fig.height=7, fig.width=7, warning=FALSE, message=FALSE}

# Plot locations in Aransas Bay

Aransas_Elasmos <- filter(Elasmos, Site == "Aransas_Bay")

Aransas_Boundaries <- c(left = -97.1, bottom = 27.9, right = -96.9, top = 28.05)

Aransas_Map <-get_stamenmap(Aransas_Boundaries, maptype = "terrain-background")

Aransas_Map <- ggmap(Aransas_Map) +
  geom_point(data = Elasmos, aes(x = Longitude, y = Latitude, colour = Species), size = 2) +
  geom_point(data = Channels, aes(x = X_DD, y = Y_DD), size = 0.1) +
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("Aransas Bay") +
  scale_color_manual(values=col10) +
  theme_standard +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size=20))

```

#### Map Locations in Corpus Christi Bay

```{r fig.height=7, fig.width=7, warning=FALSE, message=FALSE}

# Plot locations in Corpus Christi Bay

Corpus_Elasmos <- filter(Elasmos, Site == "Corpus_Christi_Bay")

Corpus_Boundaries <- c(left = -97.4, bottom = 27.75, right = -97.2, top = 27.9)

Corpus_Map <-get_stamenmap(Corpus_Boundaries, maptype = "terrain-background")

Corpus_Map <- ggmap(Corpus_Map) +
  geom_point(data = Elasmos, aes(x = Longitude, y = Latitude, colour = Species), size = 2) +
  geom_point(data = Channels, aes(x = X_DD, y = Y_DD), size = 0.1) +
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("Corpus Christi Bay") +
  scale_color_manual(values=col10) +
  theme_standard +
  theme(legend.title = element_text(size=12), legend.text = element_text(size=11), legend.position = "right", legend.direction = "vertical", plot.title = element_text(hjust = 0.5, size=20))

```

#### Map Locations in Redfish Bay

```{r fig.height=7, fig.width=7, warning=FALSE, message=FALSE}

# Plot locations in Redfish Bay

Redfish_Elasmos <- filter(Elasmos, Site == "Redfish_Bay")

Redfish_Boundaries <- c(left = -97.25, bottom = 27.75, right = -97.05, top = 27.9)

Redfish_Map <-get_stamenmap(Redfish_Boundaries, maptype = "terrain-background")

Redfish_Map <- ggmap(Redfish_Map) +
  geom_point(data = Elasmos, aes(x = Longitude, y = Latitude, colour = Species), fill = "black", linetype="solid", size = 2) +
  geom_point(data = Channels, aes(x = X_DD, y = Y_DD), size = 0.1) +
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("Redfish Bay") +
  scale_color_manual(values=col10) +
  theme_standard +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size=20))

```

#### Multiplot Maps

```{r fig.height=12, fig.width=7, warning=FALSE, message=FALSE}

Combined_Map <-  Aransas_Map + Corpus_Map + Redfish_Map + plot_layout(ncol = 1, widths = c(5, 7, 5))

ggsave("../Results/Combined_Map.png", last_plot())

```

#### Incorporate Distance from Tidal Inlets

```#{r}

# Unzip zip for Texas shoreline shapefile

#unzip("../Data/shp/SL2/R3shoreline_2.zip", exdir = "../Data/shp/SL2/", junkpaths = TRUE, overwrite = TRUE)

# Import shape file

bay <- st_read("../Data/shp/Region3.shp") %>%
       st_transform(crs = 32619)

# Produce dataframe with Set_ID and lat/longs for each longline deployment then add the lat/long of the nearest tidal inlet at the top

Set_Lat_Longs <- Set_Meta %>%
  dplyr::select(c(Set_ID, Longitude, Latitude)) %>%
  mutate(Longitude = as.numeric(Longitude), 
         Latitude = as.numeric(Latitude)) %>%
  add_row(Set_ID="Inlet", Longitude=-97.044492, Latitude=27.836921, .before = 1)

# Convert lat longs to UTMs

coords <- st_as_sf(Set_Lat_Longs, coords = c("Longitude", "Latitude"),
                   crs = 4326, stringsAsFactors = FALSE) %>%
          st_transform(crs = 32619)

# Estimate distance 'as the crow files' between the tidal inlet and each deployment location

dist1 <- st_distance(x = coords[1,], y = coords[-1,])

dist1_df <- data.frame(dist1, row.names = "Crow_Distance")
colnames(dist1_df) <- c(Set_Lat_Longs$Set_ID[-1])

# Transpose and clean up dataframe

dist1_df <- data.frame(t(dist1_df)) %>%
  rownames_to_column() %>%
  rename(Set_ID=rowname)

# Rasterize shapefile and change land (forbidden) to 0 and water (allowed) to 1

raster_bay <- raster(x = extent(bay), nrow = 500,  ncol = 500)
raster_bay <- rasterize(x = bay, y = raster_bay, field = 1)
raster_bay[1==(raster_bay)] <- 2
raster_bay[is.na(raster_bay)] <- 1
raster_bay[2==(raster_bay)] <- 0

# PLot raster

plot(raster_bay)

# Transition raster and apply geoCorrection

raster_bay_tr <- transition(raster_bay, transitionFunction = mean, directions = 16)
raster_bay_tr <- geoCorrection(raster_bay_tr, type = "c")

# Estimate least-cost distance 'as the fish swims' between the tidal inlet and each deployment location

dist2 <- costDistance(raster_bay_tr,
                      fromCoords = as(as_Spatial(coords[1,]), "SpatialPoints"),
                      toCoords = as(as_Spatial(coords[-1,]), "SpatialPoints"))

dist2_df <- data.frame(dist2, row.names = "Fish_Distance")
colnames(dist2_df) <- c(Set_Lat_Longs$Set_ID[-1])

# Transpose and clean up dataframe

dist2_df <- data.frame(t(dist2_df)) %>%
  rownames_to_column() %>%
  rename(Set_ID=rowname)

# Join distances together in 1 dataframe

Dist_df <- full_join(dist1_df, dist2_df)

write_csv(Dist_df, "../Results/Distance_Crow_Fish.csv")

```

Read in distance dataframe

```{r}

Dist_df <- read.csv("../Results/Distance_Crow_Fish.csv")

# Calculate differences between the 2 distances and confirm no crows fly farther than fish swim (lazy crows)

Diff <- data.frame((Dist_df$Fish_Distance - Dist_df$Crow_Distance)/1000)

# Incorporate distances into Set_Meta

Set_Meta <- full_join(Set_Meta, Dist_df)

# Incorporate distances into Elasmos

Elasmos <- inner_join(Elasmos, Dist_df)

```

### Life-history Stage Estimation

Based on literature, estimate life-history stages for individuals based on measurements. 

```{r warning=FALSE, message=FALSE}

# C. brevipinna, Carlson & Baremore 2005

C.brevipinna_YOY <- filter(Elasmos, Species=="Carcharhinus_brevipinna" & Sex=="M" & FL<=812 | Species=="Carcharhinus_brevipinna" & Sex=="F" & FL<=844 | Species=="Carcharhinus_brevipinna" & Sex=="U" & FL<=844) %>%
  mutate(Estimated_Stage="YOY")

C.brevipinna_JUV <- filter(Elasmos, Species=="Carcharhinus_brevipinna" & Sex=="M" & FL>812 & FL<=1380 | Species=="Carcharhinus_brevipinna" & Sex=="F" & FL>844 & FL<=1360 | Species=="Carcharhinus_brevipinna" & Sex=="U" & FL>844 & FL<=1360) %>%
  mutate(Estimated_Stage="JUV")

C.brevipinna_MAT <- filter(Elasmos, Species=="Carcharhinus_brevipinna" & Sex=="M" & FL>1380 | Species=="Carcharhinus_brevipinna" & Sex=="F" & FL>1360 | Species=="Carcharhinus_brevipinna" & Sex=="U" & FL>1360) %>%
  mutate(Estimated_Stage="MAT")

# C. leucas, Neer et al. 2005

C.leucas_YOY <- filter(Elasmos, Species=="Carcharhinus_leucas" & FL<=700) %>%
  mutate(Estimated_Stage="YOY")

C.leucas_JUV <- filter(Elasmos, Species=="Carcharhinus_leucas" & Sex=="M" & FL>700 & FL<=2100 | Species=="Carcharhinus_leucas" & Sex=="F" & FL>700 & FL<=2250 | Species=="Carcharhinus_leucas" & Sex=="U" & FL>700 & FL<=2250) %>%
  mutate(Estimated_Stage="JUV")

C.leucas_MAT <- filter(Elasmos, Species=="Carcharhinus_leucas" & Sex=="M" & FL>2100 | Species=="Carcharhinus_leucas" & Sex=="F" & FL>2250 | Species=="Carcharhinus_leucas" & Sex=="U" & FL>2250) %>%
  mutate(Estimated_Stage="MAT")

# C. limbatus, Parsons 2007

C.limbatus_YOY <- filter(Elasmos, Species=="Carcharhinus_limbatus" & STL<=800) %>%
  mutate(Estimated_Stage="YOY")

C.limbatus_JUV <- filter(Elasmos, Species=="Carcharhinus_limbatus" & Sex=="M" & STL>800 & STL<=1340 | Species=="Carcharhinus_limbatus" & Sex=="F" & STL>800 & STL<=1540 | Species=="Carcharhinus_limbatus" & Sex=="U" & STL>800 & STL<=1540) %>%
  mutate(Estimated_Stage="JUV")

C.limbatus_MAT <- filter(Elasmos, Species=="Carcharhinus_limbatus" & Sex=="M" & STL>1340 | Species=="Carcharhinus_limbatus" & Sex=="F" & STL>1540 | Species=="Carcharhinus_limbatus" & Sex=="U" & STL>1540) %>%
  mutate(Estimated_Stage="MAT")

# C. porosus, Rossa & Santana, 1998

C.porosus_YOY <- filter(Elasmos, Species=="Carcharhinus_porosus" & STL<=374) %>%
  mutate(Estimated_Stage="YOY")

C.porosus_JUV <- filter(Elasmos, Species=="Carcharhinus_porosus" & Sex=="M" & STL>374 & STL<=710 | Species=="Carcharhinus_porosus" & Sex=="F" & STL>374 & STL<=700 | Species=="Carcharhinus_porosus" & Sex=="U" & STL>374 & STL<=700) %>%
  mutate(Estimated_Stage="JUV")

C.porosus_MAT <- filter(Elasmos, Species=="Carcharhinus_porosus" & Sex=="M" & STL>710 | Species=="Carcharhinus_porosus" & Sex=="F" & STL>700 | Species=="Carcharhinus_porosus" & Sex=="U" & STL>700) %>%
  mutate(Estimated_Stage="MAT")

# H. americanus, Ramirez-Mosqueda 2012

H.americanus_JUV <- filter(Elasmos, Species=="Hypanus_americanus" & Sex=="M" & STL<=517 | Species=="Hypanus_americanus" & Sex=="F" & STL<=764 | Species=="Hypanus_americanus" & Sex=="U" & STL<=764) %>%
  mutate(Estimated_Stage="JUV")

H.americanus_MAT <- filter(Elasmos, Species=="Hypanus_americanus" & Sex=="M" & STL>517 | Species=="Hypanus_americanus" & Sex=="F" & STL>764 | Species=="Hypanus_americanus" & Sex=="U" & STL>764) %>%
  mutate(Estimated_Stage="MAT")

# H. sabina, Snelson et al. 1988

H.sabina_JUV <- filter(Elasmos, Species=="Hypanus_sabina" & Sex=="M" & STL<=200 | Species=="Hypanus_sabina" & Sex=="F" & STL<=240 | Species=="Hypanus_sabina" & Sex=="U" & STL<=240) %>%
  mutate(Estimated_Stage="JUV")

H.sabina_MAT <- filter(Elasmos, Species=="Hypanus_sabina" & Sex=="M" & STL>200 | Species=="Hypanus_sabina" & Sex=="F" & STL>240 | Species=="Hypanus_sabina" & Sex=="U" & STL>240) %>%
  mutate(Estimated_Stage="MAT")

# R. bonasus, Neer & Thompson 2005

R.bonasus_JUV <- filter(Elasmos, Species=="Rhinoptera_bonasus" & Sex=="M" & STL<=642 | Species=="Rhinoptera_bonasus" & Sex=="F" & STL<=653 | Species=="Rhinoptera_bonasus" & Sex=="U" & STL<=653) %>%
  mutate(Estimated_Stage="JUV")

R.bonasus_MAT <- filter(Elasmos, Species=="Rhinoptera_bonasus" & Sex=="M" & STL>642 | Species=="Rhinoptera_bonasus" & Sex=="F" & STL>653 | Species=="Rhinoptera_bonasus" & Sex=="U" & STL>653) %>%
  mutate(Estimated_Stage="MAT")

# R. terraenovae, Carlson & Baremore 2003

R.terraenovae_YOY <- filter(Elasmos, Species=="Rhizoprionodon_terraenovae" & STL<=600) %>%
  mutate(Estimated_Stage="YOY")

R.terraenovae_JUV <- filter(Elasmos, Species=="Rhizoprionodon_terraenovae" & Sex=="M" & STL>600 & STL<=726 | Species=="Rhizoprionodon_terraenovae" & Sex=="F" & STL>600 & STL<=758 | Species=="Rhizoprionodon_terraenovae" & Sex=="U" & STL>600 & STL<=758) %>%
mutate(Estimated_Stage="JUV")

R.terraenovae_MAT <- filter(Elasmos, Species=="Rhizoprionodon_terraenovae" & Sex=="M" & STL>726 | Species=="Rhizoprionodon_terraenovae" & Sex=="F" & STL>758 | Species=="Rhizoprionodon_terraenovae" & Sex=="U" & STL>758) %>%
  mutate(Estimated_Stage="MAT")

# S. lewini, NMFS (get citation from Amanda)

S.lewini_YOY <- filter(Elasmos, Species=="Sphyrna_lewini" & STL<=600) %>%
  mutate(Estimated_Stage="YOY")

S.lewini_JUV <- filter(Elasmos, Species=="Sphyrna_lewini" & STL>600 & STL<=1790) %>%
  mutate(Estimated_Stage="JUV")

S.lewini_MAT <- filter(Elasmos, Species=="Sphyrna_lewini" & STL>1790) %>%
  mutate(Estimated_Stage="MAT")

# S. tiburo, Lombardi et al. 2003 and Carlson & Parsons 1997

S.tiburo_YOY <- filter(Elasmos, Species=="Sphyrna_tiburo" & STL<=600) %>%
  mutate(Estimated_Stage="YOY")

S.tiburo_JUV <- filter(Elasmos, Species=="Sphyrna_tiburo" & Sex=="M" & STL>600 & STL<=830 | Species=="Sphyrna_tiburo" & Sex=="F" & STL>600 & STL<=944 | Species=="Sphyrna_tiburo" & Sex=="U" & STL>600 & STL<=944) %>%
  mutate(Estimated_Stage="JUV")

S.tiburo_MAT <- filter(Elasmos, Species=="Sphyrna_tiburo" & Sex=="M" & STL>830 | Species=="Sphyrna_tiburo" & Sex=="F" & STL>944 | Species=="Sphyrna_tiburo" & Sex=="U" & STL>944) %>%
  mutate(Estimated_Stage="MAT")

# Update Elasmos with Estimated_Stage

Elasmos_Est_Stages <- bind_rows(C.brevipinna_YOY, C.brevipinna_JUV, C.brevipinna_MAT, C.leucas_YOY, C.leucas_JUV, C.leucas_MAT, C.limbatus_YOY, C.limbatus_JUV, C.limbatus_MAT, C.porosus_YOY, C.porosus_JUV, C.porosus_MAT, H.americanus_JUV, H.americanus_MAT, H.sabina_JUV, H.sabina_MAT, R.bonasus_JUV, R.bonasus_MAT, R.terraenovae_YOY, R.terraenovae_JUV, R.terraenovae_MAT, S.lewini_YOY, S.lewini_JUV, S.lewini_MAT, S.tiburo_YOY, S.tiburo_JUV, S.tiburo_MAT)

No_Est_Stage <-anti_join(Elasmos, Elasmos_Est_Stages)

# Update Elasmos with STAGE
 
YOY <- filter(Elasmos_Est_Stages, Observed_Stage=="YOY" | is.na(Observed_Stage) & Estimated_Stage=="YOY") %>%
  mutate(STAGE="YOY")

JUV <- filter(Elasmos_Est_Stages, Observed_Stage=="JUV" | is.na(Observed_Stage) & Estimated_Stage=="JUV") %>%
  mutate(STAGE="JUV")

MAT <- filter(Elasmos_Est_Stages, Observed_Stage=="MAT" | is.na(Observed_Stage) & Estimated_Stage=="MAT") %>%
  mutate(STAGE="MAT")

UND <- filter(No_Est_Stage) %>%
  mutate(STAGE="UND")

Elasmos_STAGE <- bind_rows(YOY, JUV, MAT, UND)

No_STAGE <-anti_join(Elasmos, Elasmos_STAGE)

# Produce final dataset

Elasmos_Complete <- Elasmos_STAGE %>%
  dplyr::select(c(Animal_ID, Site, Species, Sex, STAGE, STL, Hook_Size, Set_ID, Latitude, Longitude, Surface_Temp, Bottom_Temp, Surface_Salinity, Bottom_Salinity, Surface_DO, Bottom_DO, Depth, Crow_Distance, Fish_Distance)) %>%
  arrange(Animal_ID)

write.csv(Elasmos_Complete, "../Results/Elasmos_Complete.csv", row.names = FALSE, na = "")

#View(Elasmos_Complete)

# Produce df with all elasmobranchs grouped by site and life history stage

Site_LH_df <- Elasmos_Complete %>%
  group_by(Species, Site, STAGE) %>% 
  tally() %>%
  spread(STAGE, n) %>%
  replace(is.na(.), 0) %>%
    mutate(Total = YOY + JUV + MAT + UND) %>%
  dplyr::select(c(Species, Site, Total, YOY, JUV, MAT, UND))

```


## Catch Per Unit Effort (CPUE)

#### All Sites

Calculate CPUE for all animals and elasmobranchs only across all sites

```{r warning=FALSE, message=FALSE}

# Count number of animals (all species) caught per set

Animals_Set <- All_Animals %>%
  count(Set_ID)

# Count number of elasmobranchs caught per set

Elasmos_Set <- Elasmos_Complete %>%
  count(Set_ID)

# Calculate the number of hook hours per set

Set_Meta <- Set_Meta %>%
  mutate(Hook_Hours = Hooks * Soak_Time)

# Total hook hours across all sets

Total_Hook_Hours <- sum(Set_Meta$Hook_Hours)

# Calculate CPUE for all animals by set

Animals_Set <- full_join(Set_Meta, Animals_Set) %>%
  replace_na(list(n = 0)) %>%
  mutate(CPUE = n/Hook_Hours)

# Calculate CPUE for elasmobranchs only by set

Elasmos_Set <- full_join(Set_Meta, Elasmos_Set) %>%
  replace_na(list(n = 0)) %>%
  mutate(CPUE = n/Hook_Hours)

# Mean and standard deviation CPUE for all animals

Total_Animals_Mean <- mean(Animals_Set$CPUE)
Total_Animals_sd <- sd(Animals_Set$CPUE)

# Mean and standard deviation CPUE for elasmobranchs only

Total_Elasmos_Mean <- mean(Elasmos_Set$CPUE)
Total_Elasmos_sd <- sd(Elasmos_Set$CPUE)

write_csv(Elasmos_Set, "../Results/Elasmos_Set.csv")

write_csv(Set_Meta, "../Results/Set_Meta.csv")

```

#### By Site

Calculate CPUE for all animals and elasmobranchs only by site 

```{r warning=FALSE, message=FALSE}

# Aransas Bay

AB_Animals <- Animals_Set %>%
  filter(Site=="Aransas_Bay")

AB_Elasmos<- Elasmos_Set %>%
  filter(Site=="Aransas_Bay")

# Corpus Christi Bay

CC_Animals <- Animals_Set %>%
  filter(Site=="Corpus_Christi_Bay")

CC_Elasmos<- Elasmos_Set %>%
  filter(Site=="Corpus_Christi_Bay")

# Redfish Bay

RF_Animals <- Animals_Set %>%
  filter(Site=="Redfish_Bay")

RF_Elasmos<- Elasmos_Set %>%
  filter(Site=="Redfish_Bay")

# Produce dataframe with sum of hook hours, mean and sd CPUE for all animals and elasmobranchs only by site

# Mean and standard deviation CPUE for all animals

AB_Hook_Hours <- sum(AB_Animals$Hook_Hours)
AB_Animals_Mean <- mean(AB_Animals$CPUE)
AB_Animals_sd <- sd(AB_Animals$CPUE)
AB_Elasmos_Mean <- mean(AB_Elasmos$CPUE)
AB_Elasmos_sd <- sd(AB_Elasmos$CPUE)

CC_Hook_Hours <- sum(CC_Animals$Hook_Hours)
CC_Animals_Mean <- mean(CC_Animals$CPUE)
CC_Animals_sd <- sd(CC_Animals$CPUE)
CC_Elasmos_Mean <- mean(CC_Elasmos$CPUE)
CC_Elasmos_sd <- sd(CC_Elasmos$CPUE)

RF_Hook_Hours <- sum(RF_Animals$Hook_Hours)
RF_Animals_Mean <- mean(RF_Animals$CPUE)
RF_Animals_sd <- sd(RF_Animals$CPUE)
RF_Elasmos_Mean <- mean(RF_Elasmos$CPUE)
RF_Elasmos_sd <- sd(RF_Elasmos$CPUE)

# Make dataframe

Sites <- c("Aransas_Bay", "Corpus Christi Bay", "Redfish Bay", "Total")
Hook_Hours <- c(AB_Hook_Hours, CC_Hook_Hours, RF_Hook_Hours, Total_Hook_Hours)
Mean_Animals_CPUEs <- c(AB_Animals_Mean, CC_Animals_Mean, RF_Animals_Mean, Total_Animals_Mean)
sd_Animals_CPUEs <- c(AB_Animals_sd, CC_Animals_sd, RF_Animals_sd, Total_Animals_sd)
Mean_Elasmos_CPUEs <- c(AB_Elasmos_Mean, CC_Elasmos_Mean, RF_Elasmos_Mean, Total_Elasmos_Mean)
sd_Elasmos_CPUEs <- c(AB_Elasmos_sd, CC_Elasmos_sd, RF_Elasmos_sd, Total_Elasmos_sd)

CPUE_Hook_Hours_df <- data.frame(Sites, Hook_Hours, Mean_Animals_CPUEs, sd_Animals_CPUEs, Mean_Elasmos_CPUEs, sd_Elasmos_CPUEs)
colnames(CPUE_Hook_Hours_df) <- c("Sites", "Hook_Hours", "Mean CPUE (All Animals)", "CPUE Standard Error (All Animals)", "Mean CPUE (Elasmobranchs Only)", "CPUE Standard Error (Elasmobranchs Only)")

#View(CPUE_Hook_Hours_df)

```

##### Test Hook Hours Vs. Site

Perform a Kruskal-Wallis with Hook_Hours ~ Site. 

```{r, warning=FALSE, message=FALSE}

kruskal.test(Hook_Hours ~ Site, data = Elasmos_Set)

```

**No difference in hook hours among sites**

Perform pairwise comparisons using Dunn test to determine if the mean difference in hook hours among pairs of sites are statistically significant.

```{r, warning=FALSE, message=FALSE}

dunnTest(Hook_Hours ~ Site, data = Elasmos_Set, method="bh")

```

**No differences in hook hours between pairs of sites**

##### Test Elasmobranch CPUE Vs. Site

Perform a KW with CPUE ~ Site. 

```{r, warning=FALSE, message=FALSE}

kruskal.test(CPUE ~ Site, data = Elasmos_Set)

```

**Difference in elasmobranch CPUE among sites**

Perform pairwise comparisons using Dunn test to determine if the mean difference in CPUE among pairs of sites are statistically significant.

```{r, warning=FALSE, message=FALSE}

dunnTest(CPUE ~ Site, data = Elasmos_Set, method="bh")

```

**Differences in CPUE among Corpus Christi Bay-Aransas Bay and Corpus-Redfish Bay but not Redfish Bay-Aransas Bay**

##### Test Catfish CPUE Vs. Site

```{r warning=FALSE, message=FALSE}

# Calculate Catfish CPUE per set

Set_Catfish_Tally <- Catfish %>%
  group_by(Set_ID) %>% 
  tally()

Catfish_Set <- full_join(Set_Meta, Set_Catfish_Tally) %>%
  replace(is.na(.), 0) %>%
  mutate(CPUE = n/Hook_Hours) %>%
  arrange(Set_Number)

Total_Catfish_CPUE <- mean(Catfish_Set$CPUE)

# Catfish CPUE by Site

Aransas_Catfish <- Catfish_Set %>%
  filter(Site == "Aransas_Bay")

Aransas_Catfish_CPUE <- mean(Aransas_Catfish$CPUE)

Corpus_Catfish <- Catfish_Set %>%
  filter(Site == "Corpus_Christi_Bay")

Corpus_Catfish_CPUE <- mean(Corpus_Catfish$CPUE)

Redfish_Catfish <- Catfish_Set %>%
  filter(Site == "Redfish_Bay")

Redfish_Catfish_CPUE <- mean(Redfish_Catfish$CPUE)

# Produce dataframe of CPUEs per site and in total

CPUEs <- c(Aransas_Catfish_CPUE, Corpus_Catfish_CPUE, Redfish_Catfish_CPUE, Total_Catfish_CPUE)
Hook_Hours <- c(AB_Hook_Hours, CC_Hook_Hours, RF_Hook_Hours, Total_Hook_Hours)
Sites <- c("Aransas Bay", "Corpus Christi Bay", "Redfish Bay", "Total")

Catfish_CPUE_df <- data.frame(Sites, CPUEs, Hook_Hours)
colnames(Catfish_CPUE_df) <- c("Sites", "CPUE", "Hook_Hours")
Catfish_CPUE_df

```

Perform a KW with CPUE ~ Site. 

```{r, warning=FALSE, message=FALSE}

kruskal.test(CPUE ~ Site, data = Catfish_Set)

```
**Difference in catfish CPUE among sites**

Perform multiple pairwise comparisons using Dunn test to determine if the mean difference in catfish CPUE among pairs of sites are statistically significant.

```{r, warning=FALSE, message=FALSE}

dunnTest(CPUE ~ Site, data = Catfish_Set, method = "bh")

```

**Differences in catfish CPUE among Corpus Christi Bay-Aransas Bay and Redfish Bay-Aransas Bay, but not Corpus-Redfish Bay**

##### Plot Elasmobranch & Catfish CPUE by Set

Plot catfish and elasmobranch CPUE by set for each site to look for correlation between the two.

```{r fig.height=7, fig.width=14, warning=FALSE, message=FALSE}

# Plot locations across all sites

Sites_Boundaries <- c(left = -97.5, bottom = 27.65, right = -96.9, top = 28.1)

Sites_Map <-get_stamenmap(Sites_Boundaries, maptype = "terrain-background")

Temp <- Elasmos_Set %>%
  filter(Latitude > 0) %>%
  distinct(Latitude, .keep_all = TRUE) %>%
  distinct(Longitude, .keep_all = TRUE)

p1 <- ggmap(Sites_Map) + 
  geom_point(data = Temp, aes(x = Longitude, y = Latitude, color = CPUE), size = 2) +
  scale_color_viridis_c() +
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("Elasmobranch CPUE Per Set") +
  theme_standard +
  theme(plot.title = element_text(hjust = 0.5, size=18), legend.title = element_text(size=15), legend.text = element_text(size=13), legend.position = "right")

Temp <- Catfish_Set %>%
  filter(Latitude > 0) %>%
  distinct(Latitude, .keep_all = TRUE) %>%
  distinct(Longitude, .keep_all = TRUE)

p2 <- ggmap(Sites_Map) + 
  geom_point(data = Temp, aes(x = Longitude, y = Latitude, color = CPUE), size = 2) +
  scale_color_viridis_c() +
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("Catfish CPUE Per Set") +
  theme_standard +
  theme(plot.title = element_text(hjust = 0.5, size=18), legend.title = element_text(size=15), legend.text = element_text(size=13), legend.position = "right")

multiplot(p1, p2, cols = 2)

ggsave("../Results/CPUE_Map.png")

```

#### By Species By Site

Calculate CPUE for each elasmobranch by site

```{r warning=FALSE, message=FALSE}

# Carcharhinus_brevipinna

C.brev <- filter(Elasmos_Complete, Species=="Carcharhinus_brevipinna") %>%
  count(Set_ID) %>% 
  full_join(Set_Meta) %>%
  replace_na(list(n = 0)) %>%
  mutate(CPUE = n/Hook_Hours)

C.brev_AB <- filter(C.brev, Site=="Aransas_Bay")

C.brev_CC <- filter(C.brev, Site=="Corpus_Christi_Bay")

C.brev_RF <- filter(C.brev, Site=="Redfish_Bay")

AB_C.brev_Mean <- mean(C.brev_AB$CPUE)
AB_C.brev_sd <- sd(C.brev_AB$CPUE)

CC_C.brev_Mean <- mean(C.brev_CC$CPUE)
CC_C.brev_sd <- sd(C.brev_CC$CPUE)

RF_C.brev_Mean <- mean(C.brev_RF$CPUE)
RF_C.brev_sd <- sd(C.brev_RF$CPUE)

# Carcharhinus_leucas

C.leu <- filter(Elasmos_Complete, Species=="Carcharhinus_leucas") %>%
  count(Set_ID) %>% 
  full_join(Set_Meta) %>%
  replace_na(list(n = 0)) %>%
  mutate(CPUE = n/Hook_Hours)

C.leu_AB <- filter(C.leu, Site=="Aransas_Bay")

C.leu_CC <- filter(C.leu, Site=="Corpus_Christi_Bay")

C.leu_RF <- filter(C.leu, Site=="Redfish_Bay")

AB_C.leu_Mean <- mean(C.leu_AB$CPUE)
AB_C.leu_sd <- sd(C.leu_AB$CPUE)

CC_C.leu_Mean <- mean(C.leu_CC$CPUE)
CC_C.leu_sd <- sd(C.leu_CC$CPUE)

RF_C.leu_Mean <- mean(C.leu_RF$CPUE)
RF_C.leu_sd <- sd(C.leu_RF$CPUE)

# Carcharhinus_limbatus

C.lim <- filter(Elasmos_Complete, Species=="Carcharhinus_limbatus") %>%
  count(Set_ID) %>% 
  full_join(Set_Meta) %>%
  replace_na(list(n = 0)) %>%
  mutate(CPUE = n/Hook_Hours)

C.lim_AB <- filter(C.lim, Site=="Aransas_Bay")

C.lim_CC <- filter(C.lim, Site=="Corpus_Christi_Bay")

C.lim_RF <- filter(C.lim, Site=="Redfish_Bay")

AB_C.lim_Mean <- mean(C.lim_AB$CPUE)
AB_C.lim_sd <- sd(C.lim_AB$CPUE)

CC_C.lim_Mean <- mean(C.lim_CC$CPUE)
CC_C.lim_sd <- sd(C.lim_CC$CPUE)

RF_C.lim_Mean <- mean(C.lim_RF$CPUE)
RF_C.lim_sd <- sd(C.lim_RF$CPUE)

# Carcharhinus_porosus

C.por <- filter(Elasmos_Complete, Species=="Carcharhinus_porosus") %>%
  count(Set_ID) %>% 
  full_join(Set_Meta) %>%
  replace_na(list(n = 0)) %>%
  mutate(CPUE = n/Hook_Hours)

C.por_AB <- filter(C.por, Site=="Aransas_Bay")

C.por_CC <- filter(C.por, Site=="Corpus_Christi_Bay")

C.por_RF <- filter(C.por, Site=="Redfish_Bay")

AB_C.por_Mean <- mean(C.por_AB$CPUE)
AB_C.por_sd <- sd(C.por_AB$CPUE)

CC_C.por_Mean <- mean(C.por_CC$CPUE)
CC_C.por_sd <- sd(C.por_CC$CPUE)

RF_C.por_Mean <- mean(C.por_RF$CPUE)
RF_C.por_sd <- sd(C.por_RF$CPUE)

# Hypanus_americanus

H.ame <- filter(Elasmos_Complete, Species=="Hypanus_americanus") %>%
  count(Set_ID) %>% 
  full_join(Set_Meta) %>%
  replace_na(list(n = 0)) %>%
  mutate(CPUE = n/Hook_Hours)

H.ame_AB <- filter(H.ame, Site=="Aransas_Bay")

H.ame_CC <- filter(H.ame, Site=="Corpus_Christi_Bay")

H.ame_RF <- filter(H.ame, Site=="Redfish_Bay")

AB_H.ame_Mean <- mean(H.ame_AB$CPUE)
AB_H.ame_sd <- sd(H.ame_AB$CPUE)

CC_H.ame_Mean <- mean(H.ame_CC$CPUE)
CC_H.ame_sd <- sd(H.ame_CC$CPUE)

RF_H.ame_Mean <- mean(H.ame_RF$CPUE)
RF_H.ame_sd <- sd(H.ame_RF$CPUE)

# Hypanus_sabina

H.sab <- filter(Elasmos_Complete, Species=="Hypanus_sabina") %>%
  count(Set_ID) %>% 
  full_join(Set_Meta) %>%
  replace_na(list(n = 0)) %>%
  mutate(CPUE = n/Hook_Hours)

H.sab_AB <- filter(H.sab, Site=="Aransas_Bay")

H.sab_CC <- filter(H.sab, Site=="Corpus_Christi_Bay")

H.sab_RF <- filter(H.sab, Site=="Redfish_Bay")

AB_H.sab_Mean <- mean(H.sab_AB$CPUE)
AB_H.sab_sd <- sd(H.sab_AB$CPUE)

CC_H.sab_Mean <- mean(H.sab_CC$CPUE)
CC_H.sab_sd <- sd(H.sab_CC$CPUE)

RF_H.sab_Mean <- mean(H.sab_RF$CPUE)
RF_H.sab_sd <- sd(H.sab_RF$CPUE)

# Rhinoptera_bonasus

R.bon <- filter(Elasmos_Complete, Species=="Rhinoptera_bonasus") %>%
  count(Set_ID) %>% 
  full_join(Set_Meta) %>%
  replace_na(list(n = 0)) %>%
  mutate(CPUE = n/Hook_Hours)

R.bon_AB <- filter(R.bon, Site=="Aransas_Bay")

R.bon_CC <- filter(R.bon, Site=="Corpus_Christi_Bay")

R.bon_RF <- filter(R.bon, Site=="Redfish_Bay")

AB_R.bon_Mean <- mean(R.bon_AB$CPUE)
AB_R.bon_sd <- sd(R.bon_AB$CPUE)

CC_R.bon_Mean <- mean(R.bon_CC$CPUE)
CC_R.bon_sd <- sd(R.bon_CC$CPUE)

RF_R.bon_Mean <- mean(R.bon_RF$CPUE)
RF_R.bon_sd <- sd(R.bon_RF$CPUE)

# Rhizoprionodon_terraenovae

R.ter <- filter(Elasmos_Complete, Species=="Rhizoprionodon_terraenovae") %>%
  count(Set_ID) %>% 
  full_join(Set_Meta) %>%
  replace_na(list(n = 0)) %>%
  mutate(CPUE = n/Hook_Hours)

R.ter_AB <- filter(R.ter, Site=="Aransas_Bay")

R.ter_CC <- filter(R.ter, Site=="Corpus_Christi_Bay")

R.ter_RF <- filter(R.ter, Site=="Redfish_Bay")

AB_R.ter_Mean <- mean(R.ter_AB$CPUE)
AB_R.ter_sd <- sd(R.ter_AB$CPUE)

CC_R.ter_Mean <- mean(R.ter_CC$CPUE)
CC_R.ter_sd <- sd(R.ter_CC$CPUE)

RF_R.ter_Mean <- mean(R.ter_RF$CPUE)
RF_R.ter_sd <- sd(R.ter_RF$CPUE)

# Sphyrna_lewini

S.lew <- filter(Elasmos_Complete, Species=="Sphyrna_lewini") %>%
  count(Set_ID) %>% 
  full_join(Set_Meta) %>%
  replace_na(list(n = 0)) %>%
  mutate(CPUE = n/Hook_Hours)

S.lew_AB <- filter(S.lew, Site=="Aransas_Bay")

S.lew_CC <- filter(S.lew, Site=="Corpus_Christi_Bay")

S.lew_RF <- filter(S.lew, Site=="Redfish_Bay")

AB_S.lew_Mean <- mean(S.lew_AB$CPUE)
AB_S.lew_sd <- sd(S.lew_AB$CPUE)

CC_S.lew_Mean <- mean(S.lew_CC$CPUE)
CC_S.lew_sd <- sd(S.lew_CC$CPUE)

RF_S.lew_Mean <- mean(S.lew_RF$CPUE)
RF_S.lew_sd <- sd(S.lew_RF$CPUE)

# Sphyrna_tiburo

S.tib <- filter(Elasmos_Complete, Species=="Sphyrna_tiburo") %>%
  count(Set_ID) %>% 
  full_join(Set_Meta) %>%
  replace_na(list(n = 0)) %>%
  mutate(CPUE = n/Hook_Hours)

S.tib_AB <- filter(S.tib, Site=="Aransas_Bay")

S.tib_CC <- filter(S.tib, Site=="Corpus_Christi_Bay")

S.tib_RF <- filter(S.tib, Site=="Redfish_Bay")

AB_S.tib_Mean <- mean(S.tib_AB$CPUE)
AB_S.tib_sd <- sd(S.tib_AB$CPUE)

CC_S.tib_Mean <- mean(S.tib_CC$CPUE)
CC_S.tib_sd <- sd(S.tib_CC$CPUE)

RF_S.tib_Mean <- mean(S.tib_RF$CPUE)
RF_S.tib_sd <- sd(S.tib_RF$CPUE)

# Produce dataframe of each species CPUE per site

AB_Mean <- c(AB_C.brev_Mean, AB_C.leu_Mean, AB_C.lim_Mean, AB_C.por_Mean, AB_H.ame_Mean, AB_H.sab_Mean, AB_R.bon_Mean, AB_R.ter_Mean, AB_S.lew_Mean, AB_S.tib_Mean)

AB_sd <- c(AB_C.brev_sd, AB_C.leu_sd, AB_C.lim_sd, AB_C.por_sd, AB_H.ame_sd, AB_H.sab_sd, AB_R.bon_sd, AB_R.ter_sd, AB_S.lew_sd, AB_S.tib_sd)

CC_Mean <- c(CC_C.brev_Mean, CC_C.leu_Mean, CC_C.lim_Mean, CC_C.por_Mean, CC_H.ame_Mean, CC_H.sab_Mean, CC_R.bon_Mean, CC_R.ter_Mean, CC_S.lew_Mean, CC_S.tib_Mean)

CC_sd <- c(CC_C.brev_sd, CC_C.leu_sd, CC_C.lim_sd, CC_C.por_sd, CC_H.ame_sd, CC_H.sab_sd, CC_R.bon_sd, CC_R.ter_sd, CC_S.lew_sd, CC_S.tib_sd)

RF_Mean <- c(RF_C.brev_Mean, RF_C.leu_Mean, RF_C.lim_Mean, RF_C.por_Mean, RF_H.ame_Mean, RF_H.sab_Mean, RF_R.bon_Mean, RF_R.ter_Mean, RF_S.lew_Mean, RF_S.tib_Mean)

RF_sd <- c(RF_C.brev_sd, RF_C.leu_sd, RF_C.lim_sd, RF_C.por_sd, RF_H.ame_sd, RF_H.sab_sd, RF_R.bon_sd, RF_R.ter_sd, RF_S.lew_sd, RF_S.tib_sd)

CPUEs_Site_Species <- data.frame(AB_Mean, AB_sd, CC_Mean, CC_sd, RF_Mean, RF_sd) %>%
  t()
colnames(CPUEs_Site_Species) <- c("Carcharhinus_brevipinna", "Carcharhinus_leucas", "Carcharhinus_limbatus", "Carcharhinus_porosus", "Hypanus_americanus", "Hypanus_sabina", "Rhinoptera_bonasus", "Rhizoprionodon_terraenovae", "Sphyrna_lewini", "Sphyrna_tiburo")

# Write to csv

write.csv(CPUEs_Site_Species, "../Results/CPUEs_Site_Species.csv", row.names = TRUE)

```

**Plot CPUE By Species**

```{r fig.height=6, fig.width=10, warning=FALSE, message=FALSE}

# Produce graph of CPUEs for each species per site and in total

CPUEs_Site_Species_Mean <- CPUEs_Site_Species %>%
  t() %>%
  data.frame() %>%
  rownames_to_column("Species") %>%
  dplyr::select(c(1,2,4,6)) %>%
  rename(Aransas_Bay="AB_Mean") %>%
  rename(Corpus_Christi_Bay="CC_Mean") %>%
  rename(Redfish_Bay="RF_Mean") %>%
  gather(key = Site, value = Mean_CPUE, 2:4)

CPUEs_Site_Species_sd <- CPUEs_Site_Species %>%
  t() %>%
  data.frame() %>%
  rownames_to_column("Species") %>%
  dplyr::select(c(1,3,5,7)) %>%
  rename(Aransas_Bay="AB_sd") %>%
  rename(Corpus_Christi_Bay="CC_sd") %>%
  rename(Redfish_Bay="RF_sd") %>%
  gather(key = Site, value = sd_CPUE, 2:4)

temp <- full_join(CPUEs_Site_Species_Mean, CPUEs_Site_Species_sd)
temp$Species <- str_replace_all(temp$Species, "_", " ")
temp$Site <- str_replace_all(temp$Site, "_", " ")

ggplot(temp, aes(y=Mean_CPUE, x=Site, color=Species, fill=Species)) + 
    geom_bar(position="dodge", stat="identity") +
    geom_errorbar(aes(ymin=0, ymax=Mean_CPUE+sd_CPUE), width=.2,
                 position=position_dodge(.9)) +
    ylab("Mean Elasmobranch CPUE by Site") +
    theme_standard +
    scale_fill_manual(values=col10) +
    scale_color_manual(values=col10) +
    theme(legend.title = element_text(size=15), legend.text = element_text(c(size=13), face = "italic"), legend.position = "right")

ggsave("../Results/CPUEs_Site_Species_Plot.png", plot = last_plot())

```

##### Test *C. brevipinna* CPUE By Site

Perform a KW with CPUE ~ Site. 

```{r, warning=FALSE, message=FALSE}

kruskal.test(CPUE ~ Site, data = C.brev)

```

**Difference in spinner shark CPUE among sites**

Perform post-doc Dunn test.

```{r, warning=FALSE, message=FALSE}

dunnTest(CPUE ~ Site, data = C.brev, method = "bh")

```

**Differences in CPUE among Corpus Christi Bay-Aransas Bay and Corpus-Redfish Bay but not Redfish Bay-Aransas Bay**

##### Test *C. brevipinna* CPUE By Month Within Sites

Aransas Bay

```{r}

temp <- C.brev %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Aransas_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```
**No difference**

Corpus Christi Bay

```{r}

temp <- C.brev %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Corpus_Christi_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```

**Difference among months**

Perform post-doc Dunn test.

```{r, warning=FALSE, message=FALSE}

dunnTest(CPUE ~ Group, data = temp, method="bh")

```

**Spinner CPUE is lower in Corpus Christi Bay in May compared to October**

Redfish Bay

```{r}

temp <- C.brev %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Redfish_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```

**No difference**

##### Test *C. leucas* CPUE By Site

Perform a KW with CPUE ~ Site. 

```{r, warning=FALSE, message=FALSE}

kruskal.test(CPUE ~ Site, data = C.leu)

```
**No difference**

##### Test *C. leucas* CPUE By Month Within Sites

Aransas Bay

```{r}

temp <- C.leu %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Aransas_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```
**No difference**

Corpus Christi Bay

```{r}

temp <- C.leu %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Corpus_Christi_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```

**No difference**

Redfish Bay

```{r}

temp <- C.leu %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Redfish_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```
**No difference**

##### Test *C. limbatus* CPUE By Site

Perform a KW with CPUE ~ Site. 

```{r, warning=FALSE, message=FALSE}

kruskal.test(CPUE ~ Site, data = C.lim)

```
**No difference**

##### Test *C. limbatus* CPUE By Month Within Sites

Aransas Bay

```{r}

temp <- C.lim %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Aransas_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```
**No difference**

Corpus Christi Bay

```{r}

temp <- C.lim %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Corpus_Christi_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```
**No difference**

Redfish Bay

```{r}

temp <- C.lim %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Redfish_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```
**No difference**

##### Test *C. porosus* CPUE By Site

Perform a KW with CPUE ~ Site. 

```{r, warning=FALSE, message=FALSE}

kruskal.test(CPUE ~ Site, data = C.por)

```

**No difference**

##### Test *C. porosus* CPUE By Month Within Sites

Aransas Bay

```{r}

temp <- C.por %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Aransas_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```

**No difference**

Corpus Christi Bay

```{r}

temp <- C.por %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Corpus_Christi_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```

**No difference**

Redfish Bay

```{r}

temp <- C.por %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Redfish_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```

**No difference**

##### Test *H. americanus* CPUE By Site

Perform a KW with CPUE ~ Site. 

```{r, warning=FALSE, message=FALSE}

kruskal.test(CPUE ~ Site, data = H.ame)

```

**No difference**

##### Test *H. americanus* CPUE By Month Within Sites

Aransas Bay

```{r}

temp <- H.ame %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Aransas_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```

**No difference**

Corpus Christi Bay

```{r}

temp <- H.ame %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Corpus_Christi_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```

**No difference**

Redfish Bay

```{r}

temp <- H.ame %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Redfish_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```
**Difference among months**

Perform post-doc Dunn test.

```{r, warning=FALSE, message=FALSE}

dunnTest(CPUE ~ Group, data = temp, method="bh")

```

**H. americanus CPUE is greater in Redfish Bay in July compared to June, August, and October**

##### Test *H. sabina* CPUE By Site

Perform a KW with CPUE ~ Site. 

```{r, warning=FALSE, message=FALSE}

kruskal.test(CPUE ~ Site, data = H.sab)

```

Perform post-doc Dunn test.

```{r, warning=FALSE, message=FALSE}

dunnTest(CPUE ~ Site, data = H.sab, method = "bh")

```

**Differences in CPUE among Aransas Bay-Corpus Christi Bay and Aransas Bay-Redfish Bay but not Corpus Christi Bay-Redfish Bay**

##### Test *H. sabina* CPUE By Month Within Sites

Aransas Bay

```{r}

temp <- H.sab %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Aransas_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```

**No difference**

Corpus Christi Bay

```{r}

temp <- H.sab %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Corpus_Christi_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```

**No difference**

Redfish Bay

```{r}

temp <- H.sab %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Redfish_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```
**No difference**

##### Test *R. bonasus* CPUE By Site

Perform a KW with CPUE ~ Site. 

```{r, warning=FALSE, message=FALSE}

kruskal.test(CPUE ~ Site, data = R.bon)

```

**No difference**

##### Test *R. bonasus* CPUE By Month Within Sites

Aransas Bay

```{r}

temp <- R.bon %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Aransas_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```

**No difference**

Corpus Christi Bay

```{r}

temp <- R.bon %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Corpus_Christi_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```

**No difference**

Redfish Bay

```{r}

temp <- R.bon %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Redfish_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```

**No difference**

##### Test *R. terraenovae* CPUE By Site

Perform a KW with CPUE ~ Site. 

```{r, warning=FALSE, message=FALSE}

kruskal.test(CPUE ~ Site, data = R.ter)

```

**No difference**

##### Test *R. terraenovae* CPUE By Month Within Sites

Aransas Bay

```{r}

temp <- R.ter %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Aransas_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```

**No difference**

Corpus Christi Bay

```{r}

temp <- R.ter %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Corpus_Christi_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```

**No difference**

Redfish Bay

```{r}

temp <- R.ter %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Redfish_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```

**No difference**

##### Test *S. lewini* CPUE By Site

Perform a KW with CPUE ~ Site. 

```{r, warning=FALSE, message=FALSE}

kruskal.test(CPUE ~ Site, data = S.lew)

```

**Difference in scalloped hammerhead shark CPUE among sites**

Perform post-doc Dunn test.

```{r, warning=FALSE, message=FALSE}

dunnTest(CPUE ~ Site, data = S.lew, method = "bh")

```

**Differences in CPUE among Corpus Christi Bay-Aransas Bay and Corpus-Redfish Bay but not Redfish Bay-Aransas Bay**


##### Test *S. lewini* CPUE By Month Within Sites

Aransas Bay

```{r}

temp <- S.lew %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Aransas_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```

**No difference**

Corpus Christi Bay

```{r}

temp <- S.lew %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Corpus_Christi_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```

**No difference**

Redfish Bay

```{r}

temp <- S.lew %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Redfish_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```

**No difference**

##### Test *S. tiburo* CPUE By Site

Perform a KW with CPUE ~ Site. 

```{r, warning=FALSE, message=FALSE}

kruskal.test(CPUE ~ Site, data = S.tib)

```

**Difference in bonnethead shark CPUE among sites**

Perform post-doc Dunn test.

```{r, warning=FALSE, message=FALSE}

dunnTest(CPUE ~ Site, data = S.tib, method = "bh")

```

**Differences in CPUE among Aransas Bay-Corpus Christi Bay and Aransas Bay-Redfish Bay but not Corpus Christi Bay-Aransas Bay**

##### Test *S. tiburo* CPUE By Month Within Sites

Aransas Bay

```{r}

temp <- S.tib %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Aransas_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```

**No difference**

Corpus Christi Bay

```{r}

temp <- S.tib %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Corpus_Christi_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```
**Significant difference among months**

Perform post-doc Dunn test.

```{r, warning=FALSE, message=FALSE}

dunnTest(CPUE ~ Group, data = temp, method="bh")

```

**Does not differ between pairs of months**

Redfish Bay

```{r}

temp <- S.tib %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Redfish_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```

**No difference**

#### By Life-history Stage by Site

##### Test YOY CPUE By Site

Perform a KW with CPUE ~ Site. 

```{r, warning=FALSE, message=FALSE}

YOY_CPUE <- filter(Elasmos_Complete, STAGE=="YOY") %>%
  count(Set_ID) %>% 
  full_join(Set_Meta) %>%
  replace_na(list(n = 0)) %>%
  mutate(CPUE = n/Hook_Hours)

kruskal.test(CPUE ~ Site, data = YOY_CPUE)

```
**Difference in YOY individuals CPUE among sites **

Perform post-doc Dunn test.

```{r, warning=FALSE, message=FALSE}

dunnTest(CPUE ~ Site, data = YOY_CPUE, method = "bh")

YOY_CPUE_AB <- filter(YOY_CPUE, Site =="Aransas_Bay")
mean(YOY_CPUE_AB$CPUE)

YOY_CPUE_CC <- filter(YOY_CPUE, Site =="Corpus_Christi_Bay")
mean(YOY_CPUE_CC$CPUE)

YOY_CPUE_RF <- filter(YOY_CPUE, Site =="Redfish_Bay")
mean(YOY_CPUE_RF$CPUE)

```

**Differences in CPUE among Corpus Christi Bay-Aransas Bay and Corpus-Redfish Bay but not Redfish Bay-Aransas Bay**

##### Test YOY CPUE By Month Within Sites

Aransas Bay

```{r}

temp <- YOY_CPUE %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Aransas_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```

**No difference**

Corpus Christi Bay

```{r}

temp <- YOY_CPUE %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Corpus_Christi_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```

Redfish Bay

```{r}

temp <- YOY_CPUE %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Redfish_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```

**No difference**

##### Test JUV CPUE By Site

Perform a KW with CPUE ~ Site. 

```{r, warning=FALSE, message=FALSE}

JUV_CPUE <- filter(Elasmos_Complete, STAGE=="JUV") %>%
  count(Set_ID) %>% 
  full_join(Set_Meta) %>%
  replace_na(list(n = 0)) %>%
  mutate(CPUE = n/Hook_Hours)

kruskal.test(CPUE ~ Site, data = JUV_CPUE)

```

**Difference in JUV individuals CPUE among sites**

Perform post-doc Dunn test.

```{r, warning=FALSE, message=FALSE}

dunnTest(CPUE ~ Site, data = JUV_CPUE, method = "bh")

```

**JUV CPUE does not differ between pairs of sites**

##### Test JUV CPUE By Month Within Sites

Aransas Bay

```{r}

temp <- JUV_CPUE %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Aransas_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```

**No difference**

Corpus Christi Bay

```{r}

temp <- JUV_CPUE %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Corpus_Christi_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```

**Difference among months**

Perform post-doc Dunn test.

```{r, warning=FALSE, message=FALSE}

dunnTest(CPUE ~ Group, data = temp, method="bh")

```

**JUV CPUE is lower in Corpus Christi Bay in May compared to October**

Redfish Bay

```{r}

temp <- JUV_CPUE %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Redfish_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```

**No difference**

##### Test MAT CPUE By Site

Perform a KW with CPUE ~ Site. 

```{r, warning=FALSE, message=FALSE}

MAT_CPUE <- filter(Elasmos_Complete, STAGE=="MAT") %>%
  count(Set_ID) %>% 
  full_join(Set_Meta) %>%
  replace_na(list(n = 0)) %>%
  mutate(CPUE = n/Hook_Hours)

kruskal.test(CPUE ~ Site, data = MAT_CPUE)

```

**No difference**

##### Test MAT CPUE By Month Within Sites

Aransas Bay

```{r}

temp <- MAT_CPUE %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Aransas_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```

**No difference**

Corpus Christi Bay

```{r}

temp <- MAT_CPUE %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Corpus_Christi_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```

Redfish Bay

```{r}

temp <- MAT_CPUE %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Redfish_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```

**No difference**

#### By Month By Site, All Elasmobranchs

Calculate mean CPUE per month for all elasmobranchs

```{r fig.height=6, fig.width=18, warning=FALSE, message=FALSE}

# Calculate mean and sd CPUE per month by site for all animals

# Aransas Bay

AB_Elasmos_May <- AB_Elasmos %>%
  filter(Month == "5")

AB_Elasmos_May_Mean <- mean(AB_Elasmos_May$CPUE)
AB_Elasmos_May_sd <- sd(AB_Elasmos_May$CPUE)

AB_Elasmos_Jun <- AB_Elasmos %>%
  filter(Month == "6")

AB_Elasmos_Jun_Mean <- mean(AB_Elasmos_Jun$CPUE)
AB_Elasmos_Jun_sd <- sd(AB_Elasmos_Jun$CPUE)

AB_Elasmos_Jul <- AB_Elasmos %>%
  filter(Month == "7")

AB_Elasmos_Jul_Mean <- mean(AB_Elasmos_Jul$CPUE)
AB_Elasmos_Jul_sd <- sd(AB_Elasmos_Jul$CPUE)

AB_Elasmos_Aug <- AB_Elasmos %>%
  filter(Month == "8")

AB_Elasmos_Aug_Mean <- mean(AB_Elasmos_Aug$CPUE)
AB_Elasmos_Aug_sd <- sd(AB_Elasmos_Aug$CPUE)

AB_Elasmos_Sep <- AB_Elasmos %>%
  filter(Month == "9")

AB_Elasmos_Sep_Mean <- mean(AB_Elasmos_Sep$CPUE)
AB_Elasmos_Sep_sd <- sd(AB_Elasmos_Sep$CPUE)

AB_Elasmos_Oct <- AB_Elasmos %>%
  filter(Month == "10")

AB_Elasmos_Oct_Mean <- mean(AB_Elasmos_Oct$CPUE)
AB_Elasmos_Oct_sd <- sd(AB_Elasmos_Oct$CPUE)

AB_Elasmos_Nov <- AB_Elasmos %>%
  filter(Month == "11")

AB_Elasmos_Nov_Mean <- mean(AB_Elasmos_Nov$CPUE)
AB_Elasmos_Nov_sd <- sd(AB_Elasmos_Nov$CPUE)

# Corpus Christi Bay

CC_Elasmos_May <- CC_Elasmos %>%
  filter(Month == "5")

CC_Elasmos_May_Mean <- mean(CC_Elasmos_May$CPUE)
CC_Elasmos_May_sd <- sd(CC_Elasmos_May$CPUE)

CC_Elasmos_Jun <- CC_Elasmos %>%
  filter(Month == "6")

CC_Elasmos_Jun_Mean <- mean(CC_Elasmos_Jun$CPUE)
CC_Elasmos_Jun_sd <- sd(CC_Elasmos_Jun$CPUE)

CC_Elasmos_Jul <- CC_Elasmos %>%
  filter(Month == "7")

CC_Elasmos_Jul_Mean <- mean(CC_Elasmos_Jul$CPUE)
CC_Elasmos_Jul_sd <- sd(CC_Elasmos_Jul$CPUE)

CC_Elasmos_Aug <- CC_Elasmos %>%
  filter(Month == "8")

CC_Elasmos_Aug_Mean <- mean(CC_Elasmos_Aug$CPUE)
CC_Elasmos_Aug_sd <- sd(CC_Elasmos_Aug$CPUE)

CC_Elasmos_Sep <- CC_Elasmos %>%
  filter(Month == "9")

CC_Elasmos_Sep_Mean <- mean(CC_Elasmos_Sep$CPUE)
CC_Elasmos_Sep_sd <- sd(CC_Elasmos_Sep$CPUE)

CC_Elasmos_Oct <- CC_Elasmos %>%
  filter(Month == "10")

CC_Elasmos_Oct_Mean <- mean(CC_Elasmos_Oct$CPUE)
CC_Elasmos_Oct_sd <- sd(CC_Elasmos_Oct$CPUE)

CC_Elasmos_Nov <- CC_Elasmos %>%
  filter(Month == "11")

CC_Elasmos_Nov_Mean <- mean(CC_Elasmos_Nov$CPUE)
CC_Elasmos_Nov_sd <- sd(CC_Elasmos_Nov$CPUE)

# Redfish Bay

RF_Elasmos_May <- RF_Elasmos %>%
  filter(Month == "5")

RF_Elasmos_May_Mean <- mean(RF_Elasmos_May$CPUE)
RF_Elasmos_May_sd <- sd(RF_Elasmos_May$CPUE)

RF_Elasmos_Jun <- RF_Elasmos %>%
  filter(Month == "6")

RF_Elasmos_Jun_Mean <- mean(RF_Elasmos_Jun$CPUE)
RF_Elasmos_Jun_sd <- sd(RF_Elasmos_Jun$CPUE)

RF_Elasmos_Jul <- RF_Elasmos %>%
  filter(Month == "7")

RF_Elasmos_Jul_Mean <- mean(RF_Elasmos_Jul$CPUE)
RF_Elasmos_Jul_sd <- sd(RF_Elasmos_Jul$CPUE)

RF_Elasmos_Aug <- RF_Elasmos %>%
  filter(Month == "8")

RF_Elasmos_Aug_Mean <- mean(RF_Elasmos_Aug$CPUE)
RF_Elasmos_Aug_sd <- sd(RF_Elasmos_Aug$CPUE)

RF_Elasmos_Sep <- RF_Elasmos %>%
  filter(Month == "9")

RF_Elasmos_Sep_Mean <- mean(RF_Elasmos_Sep$CPUE)
RF_Elasmos_Sep_sd <- sd(RF_Elasmos_Sep$CPUE)

RF_Elasmos_Oct <- RF_Elasmos %>%
  filter(Month == "10")

RF_Elasmos_Oct_Mean <- mean(RF_Elasmos_Oct$CPUE)
RF_Elasmos_Oct_sd <- sd(RF_Elasmos_Oct$CPUE)

RF_Elasmos_Nov <- RF_Elasmos %>%
  filter(Month == "11")

RF_Elasmos_Nov_Mean <- mean(RF_Elasmos_Nov$CPUE)
RF_Elasmos_Nov_sd <- sd(RF_Elasmos_Nov$CPUE)

# Produce dataframe with mean and sd CPUE per site per month for all animals

Mean_Month <- c(AB_Elasmos_May_Mean, CC_Elasmos_May_Mean, RF_Elasmos_May_Mean, AB_Elasmos_Jun_Mean, CC_Elasmos_Jun_Mean, RF_Elasmos_Jun_Mean, AB_Elasmos_Jul_Mean, CC_Elasmos_Jul_Mean, RF_Elasmos_Jul_Mean, AB_Elasmos_Aug_Mean, CC_Elasmos_Aug_Mean, RF_Elasmos_Aug_Mean, AB_Elasmos_Sep_Mean, CC_Elasmos_Sep_Mean, RF_Elasmos_Sep_Mean, AB_Elasmos_Oct_Mean, CC_Elasmos_Oct_Mean, RF_Elasmos_Oct_Mean, AB_Elasmos_Nov_Mean, CC_Elasmos_Nov_Mean, RF_Elasmos_Nov_Mean)

sd_Month <- c(AB_Elasmos_May_sd, CC_Elasmos_May_sd, RF_Elasmos_May_sd, AB_Elasmos_Jun_sd, CC_Elasmos_Jun_sd, RF_Elasmos_Jun_sd, AB_Elasmos_Jul_sd, CC_Elasmos_Jul_sd, RF_Elasmos_Jul_sd, AB_Elasmos_Aug_sd, CC_Elasmos_Aug_sd, RF_Elasmos_Aug_sd, AB_Elasmos_Sep_sd, CC_Elasmos_Sep_sd, RF_Elasmos_Sep_sd, AB_Elasmos_Oct_sd, CC_Elasmos_Oct_sd, RF_Elasmos_Oct_sd, AB_Elasmos_Nov_sd, CC_Elasmos_Nov_sd, RF_Elasmos_Nov_sd)

Month <- as.numeric(rep(c("5", "6", "7", "8", "9", "10", "11"), each = 3))

Site <- rep(c("Aransas Bay", "Corpus Christi Bay", "Redfish Bay"), times = 7)

Elasmos_CPUEs_Month <- data.frame(Month, Site, Mean_Month, sd_Month) %>%
  replace(is.na(.), 0) %>%
  mutate(Min = Mean_Month - sd_Month)

```

#### Hook Hours By Month By Site

Plot hook hours by month by site

```{r fig.height=6, fig.width=18, warning=FALSE, message=FALSE}

# Aransas Bay

AB_Hook_Hours_May_Mean <- mean(AB_Elasmos_May$Hook_Hours)
AB_Hook_Hours_May_sd <- sd(AB_Elasmos_May$Hook_Hours)

AB_Hook_Hours_Jun_Mean <- mean(AB_Elasmos_Jun$Hook_Hours)
AB_Hook_Hours_Jun_sd <- sd(AB_Elasmos_Jun$Hook_Hours)

AB_Hook_Hours_Jul_Mean <- mean(AB_Elasmos_Jul$Hook_Hours)
AB_Hook_Hours_Jul_sd <- sd(AB_Elasmos_Jul$Hook_Hours)

AB_Hook_Hours_Aug_Mean <- mean(AB_Elasmos_Aug$Hook_Hours)
AB_Hook_Hours_Aug_sd <- sd(AB_Elasmos_Aug$Hook_Hours)

AB_Hook_Hours_Sep_Mean <- mean(AB_Elasmos_Sep$Hook_Hours)
AB_Hook_Hours_Sep_sd <- sd(AB_Elasmos_Sep$Hook_Hours)

AB_Hook_Hours_Oct_Mean <- mean(AB_Elasmos_Oct$Hook_Hours)
AB_Hook_Hours_Oct_sd <- sd(AB_Elasmos_Oct$Hook_Hours)

AB_Hook_Hours_Nov_Mean <- mean(AB_Elasmos_Nov$Hook_Hours)
AB_Hook_Hours_Nov_sd <- sd(AB_Elasmos_Nov$Hook_Hours)

# Corpus Christi Bay

CC_Hook_Hours_May_Mean <- mean(CC_Elasmos_May$Hook_Hours)
CC_Hook_Hours_May_sd <- sd(CC_Elasmos_May$Hook_Hours)

CC_Hook_Hours_Jun_Mean <- mean(CC_Elasmos_Jun$Hook_Hours)
CC_Hook_Hours_Jun_sd <- sd(CC_Elasmos_Jun$Hook_Hours)

CC_Hook_Hours_Jul_Mean <- mean(CC_Elasmos_Jul$Hook_Hours)
CC_Hook_Hours_Jul_sd <- sd(CC_Elasmos_Jul$Hook_Hours)

CC_Hook_Hours_Aug_Mean <- mean(CC_Elasmos_Aug$Hook_Hours)
CC_Hook_Hours_Aug_sd <- sd(CC_Elasmos_Aug$Hook_Hours)

CC_Hook_Hours_Sep_Mean <- mean(CC_Elasmos_Sep$Hook_Hours)
CC_Hook_Hours_Sep_sd <- sd(CC_Elasmos_Sep$Hook_Hours)

CC_Hook_Hours_Oct_Mean <- mean(CC_Elasmos_Oct$Hook_Hours)
CC_Hook_Hours_Oct_sd <- sd(CC_Elasmos_Oct$Hook_Hours)

CC_Hook_Hours_Nov_Mean <- mean(CC_Elasmos_Nov$Hook_Hours)
CC_Hook_Hours_Nov_sd <- sd(CC_Elasmos_Nov$Hook_Hours)

# Redfish Bay

RF_Hook_Hours_May_Mean <- mean(RF_Elasmos_May$Hook_Hours)
RF_Hook_Hours_May_sd <- sd(RF_Elasmos_May$Hook_Hours)

RF_Hook_Hours_Jun_Mean <- mean(RF_Elasmos_Jun$Hook_Hours)
RF_Hook_Hours_Jun_sd <- sd(RF_Elasmos_Jun$Hook_Hours)

RF_Hook_Hours_Jul_Mean <- mean(RF_Elasmos_Jul$Hook_Hours)
RF_Hook_Hours_Jul_sd <- sd(RF_Elasmos_Jul$Hook_Hours)

RF_Hook_Hours_Aug_Mean <- mean(RF_Elasmos_Aug$Hook_Hours)
RF_Hook_Hours_Aug_sd <- sd(RF_Elasmos_Aug$Hook_Hours)

RF_Hook_Hours_Sep_Mean <- mean(RF_Elasmos_Sep$Hook_Hours)
RF_Hook_Hours_Sep_sd <- sd(RF_Elasmos_Sep$Hook_Hours)

RF_Hook_Hours_Oct_Mean <- mean(RF_Elasmos_Oct$Hook_Hours)
RF_Hook_Hours_Oct_sd <- sd(RF_Elasmos_Oct$Hook_Hours)

RF_Hook_Hours_Nov_Mean <- mean(RF_Elasmos_Nov$Hook_Hours)
RF_Hook_Hours_Nov_sd <- sd(RF_Elasmos_Nov$Hook_Hours)

# Produce dataframe with mean and sd CPUE per site per month for all Elasmos

Mean_Month <- c(AB_Hook_Hours_May_Mean, CC_Hook_Hours_May_Mean, RF_Hook_Hours_May_Mean, AB_Hook_Hours_Jun_Mean, CC_Hook_Hours_Jun_Mean, RF_Hook_Hours_Jun_Mean, AB_Hook_Hours_Jul_Mean, CC_Hook_Hours_Jul_Mean, RF_Hook_Hours_Jul_Mean, AB_Hook_Hours_Aug_Mean, CC_Hook_Hours_Aug_Mean, RF_Hook_Hours_Aug_Mean, AB_Hook_Hours_Sep_Mean, CC_Hook_Hours_Sep_Mean, RF_Hook_Hours_Sep_Mean, AB_Hook_Hours_Oct_Mean, CC_Hook_Hours_Oct_Mean, RF_Hook_Hours_Oct_Mean, AB_Hook_Hours_Nov_Mean, CC_Hook_Hours_Nov_Mean, RF_Hook_Hours_Nov_Mean)

sd_Month <- c(AB_Hook_Hours_May_sd, CC_Hook_Hours_May_sd, RF_Hook_Hours_May_sd, AB_Hook_Hours_Jun_sd, CC_Hook_Hours_Jun_sd, RF_Hook_Hours_Jun_sd, AB_Hook_Hours_Jul_sd, CC_Hook_Hours_Jul_sd, RF_Hook_Hours_Jul_sd, AB_Hook_Hours_Aug_sd, CC_Hook_Hours_Aug_sd, RF_Hook_Hours_Aug_sd, AB_Hook_Hours_Sep_sd, CC_Hook_Hours_Sep_sd, RF_Hook_Hours_Sep_sd, AB_Hook_Hours_Oct_sd, CC_Hook_Hours_Oct_sd, RF_Hook_Hours_Oct_sd, AB_Hook_Hours_Nov_sd, CC_Hook_Hours_Nov_sd, RF_Hook_Hours_Nov_sd)

Month <- as.numeric(rep(c("5", "6", "7", "8", "9", "10", "11"), each = 3))

Site <- rep(c("Aransas Bay", "Corpus Christi Bay", "Redfish Bay"), times = 7)

Hook_Hours_Month <- data.frame(Month, Site, Mean_Month, sd_Month) %>%
  replace(is.na(.), 0)

```

**Plot**

```{r fig.height=8, fig.width=20, warning=FALSE, message=FALSE}

# PLot

p1 <- ggplot(Elasmos_CPUEs_Month, aes(y=Mean_Month, x=Month, group=Site, line=Site)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=0, ymax=Mean_Month+sd_Month), width=.2, position=position_dodge(0.05)) +
  #yxlab(bquote('Mean CPUE for All Elasmobranchs (*Animal Hook-Hour^-1*)') +
  ylab(bquote('Mean Elasmobranch CPUE ('*Animal~Hook~Hour^-1*')')) +
  scale_x_continuous(breaks = c(5, 6, 7, 8, 9, 10, 11)) +
  scale_y_continuous(breaks = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07)) +
  facet_grid(~Site) +
  theme_standard +
  theme(axis.text=element_text(size=22),
        axis.title=element_text(size=22), strip.text.x = element_text(size = 30))

p1

ggsave("../Results/All_Elasmos_CPUE_Site_Month.pdf", plot = last_plot())

# PLot

p2 <- ggplot(Hook_Hours_Month, aes(y=Mean_Month, x=Month, group=Site, line=Site)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=Mean_Month-sd_Month, ymax=Mean_Month+sd_Month), width=.2,
                 position=position_dodge(0.05)) +
 # ggtitle("Hook Hours") +
  ylab("Mean Hook Hours") +
  scale_x_continuous(breaks = c(5, 6, 7, 8, 9, 10, 11)) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150)) +
  facet_grid(~Site) +
  theme_standard +
  theme(axis.text=element_text(size=22),
        axis.title=element_text(size=22), strip.text.x = element_text(size = 30))

p2

ggsave("../Results/Hook_Hours_Site_Month.pdf", plot = last_plot())

```


**Multiplot**

```{r fig.height=16, fig.width=20, warning=FALSE, message=FALSE}

multiplot(p2, p1, cols = 1)

ggsave("../Results/Elasmos_CPUE_Hook_Hours_Site_Month.pdf", plot = last_plot())

```

##### Test Hook Hours By Month for All Sites

Perform a KW with Hook Hours ~ Month for all sites for elasmobranchs only

```{r, warning=FALSE, message=FALSE}

temp <- Elasmos_Set %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November")))))))

kruskal.test(Hook_Hours ~ Group, data = temp)

```
**No difference in hook hours**

##### Test Hook Hours By Month for Aransas Bay

Perform a KW with Hook Hours ~ Month for AB for elasmobranchs only

```{r, warning=FALSE, message=FALSE}

temp <- Elasmos_Set %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Aransas_Bay")

kruskal.test(Hook_Hours ~ Group, data = temp)

```
**No difference in hook hours**

##### Test Hook Hours By Month for Corpus Christi Bay

Perform a KW with Hook Hours ~ Month for CC for elasmobranchs only

```{r, warning=FALSE, message=FALSE}

temp <- Elasmos_Set %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Corpus_Christi_Bay")

kruskal.test(Hook_Hours ~ Group, data = temp)

```
**No difference in hook hours**

##### Test Hook Hours By Month for Redfish Bay

Perform a KW with Hook Hours ~ Month for RF for elasmobranchs only

```{r, warning=FALSE, message=FALSE}

temp <- Elasmos_Set %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Redfish_Bay")

kruskal.test(Hook_Hours ~ Group, data = temp)

```
**No difference in hook hours**

##### Test CPUE By Month for All Sites

Perform KW with CPUE ~ Month for all sites for elasmobranchs only

```{r, warning=FALSE, message=FALSE}

temp <- Elasmos_Set %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November")))))))

kruskal.test(CPUE ~ Group, data = temp)

```
**No difference in hook hours**

##### Test CPUE By Month for Aransas Bay

Perform KW with CPUE ~ Month for all sites for elasmobranchs only

```{r, warning=FALSE, message=FALSE}

temp <- Elasmos_Set %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Aransas_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```
**No difference in hook hours**

##### Test CPUE By Month for Corpus Christi Bay

Perform KW with CPUE ~ Month for all sites for elasmobranchs only

```{r, warning=FALSE, message=FALSE}

temp <- Elasmos_Set %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Corpus_Christi_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```
**Difference in elasmobranch CPUE**

Perform post-doc Dunn test.

```{r, warning=FALSE, message=FALSE}

dunnTest(CPUE ~ Group, data = temp, method="bh")

```

**Elasmobranch CPUE is lower in Corpus Christi Bay in May compared with August, September, and October**

##### Test CPUE By Month for Redfish Bay

Perform KW with CPUE ~ Month for all sites for elasmobranchs only

```{r, warning=FALSE, message=FALSE}

temp <- Elasmos_Set %>%
  mutate(Group = ifelse(Month=="5", "May",
                        ifelse(Month=="6", "June",
                        ifelse(Month=="7", "July",
                        ifelse(Month=="8", "August",
                        ifelse(Month=="9", "September",
                        ifelse(Month=="10", "October", "November"))))))) %>%
  dplyr::filter(Site=="Redfish_Bay")

kruskal.test(CPUE ~ Group, data = temp)

```
**No difference in elasmobranch CPUE**

#### By Month By Site, All Animals

Calculate mean CPUE per month for all animals (including catfish)

```{r fig.height=6, fig.width=18, warning=FALSE, message=FALSE}

# Calculate mean and sd CPUE per month by site for all animals

# Aransas Bay

AB_Animals_May <- AB_Animals %>%
  filter(Month == "5")

AB_Animals_May_Mean <- mean(AB_Animals_May$CPUE)
AB_Animals_May_sd <- sd(AB_Animals_May$CPUE)

AB_Animals_Jun <- AB_Animals %>%
  filter(Month == "6")

AB_Animals_Jun_Mean <- mean(AB_Animals_Jun$CPUE)
AB_Animals_Jun_sd <- sd(AB_Animals_Jun$CPUE)

AB_Animals_Jul <- AB_Animals %>%
  filter(Month == "7")

AB_Animals_Jul_Mean <- mean(AB_Animals_Jul$CPUE)
AB_Animals_Jul_sd <- sd(AB_Animals_Jul$CPUE)

AB_Animals_Aug <- AB_Animals %>%
  filter(Month == "8")

AB_Animals_Aug_Mean <- mean(AB_Animals_Aug$CPUE)
AB_Animals_Aug_sd <- sd(AB_Animals_Aug$CPUE)

AB_Animals_Sep <- AB_Animals %>%
  filter(Month == "9")

AB_Animals_Sep_Mean <- mean(AB_Animals_Sep$CPUE)
AB_Animals_Sep_sd <- sd(AB_Animals_Sep$CPUE)

AB_Animals_Oct <- AB_Animals %>%
  filter(Month == "10")

AB_Animals_Oct_Mean <- mean(AB_Animals_Oct$CPUE)
AB_Animals_Oct_sd <- sd(AB_Animals_Oct$CPUE)

AB_Animals_Nov <- AB_Animals %>%
  filter(Month == "11")

AB_Animals_Nov_Mean <- mean(AB_Animals_Nov$CPUE)
AB_Animals_Nov_sd <- sd(AB_Animals_Nov$CPUE)

# Corpus Christi Bay

CC_Animals_May <- CC_Animals %>%
  filter(Month == "5")

CC_Animals_May_Mean <- mean(CC_Animals_May$CPUE)
CC_Animals_May_sd <- sd(CC_Animals_May$CPUE)

CC_Animals_Jun <- CC_Animals %>%
  filter(Month == "6")

CC_Animals_Jun_Mean <- mean(CC_Animals_Jun$CPUE)
CC_Animals_Jun_sd <- sd(CC_Animals_Jun$CPUE)

CC_Animals_Jul <- CC_Animals %>%
  filter(Month == "7")

CC_Animals_Jul_Mean <- mean(CC_Animals_Jul$CPUE)
CC_Animals_Jul_sd <- sd(CC_Animals_Jul$CPUE)

CC_Animals_Aug <- CC_Animals %>%
  filter(Month == "8")

CC_Animals_Aug_Mean <- mean(CC_Animals_Aug$CPUE)
CC_Animals_Aug_sd <- sd(CC_Animals_Aug$CPUE)

CC_Animals_Sep <- CC_Animals %>%
  filter(Month == "9")

CC_Animals_Sep_Mean <- mean(CC_Animals_Sep$CPUE)
CC_Animals_Sep_sd <- sd(CC_Animals_Sep$CPUE)

CC_Animals_Oct <- CC_Animals %>%
  filter(Month == "10")

CC_Animals_Oct_Mean <- mean(CC_Animals_Oct$CPUE)
CC_Animals_Oct_sd <- sd(CC_Animals_Oct$CPUE)

CC_Animals_Nov <- CC_Animals %>%
  filter(Month == "11")

CC_Animals_Nov_Mean <- mean(CC_Animals_Nov$CPUE)
CC_Animals_Nov_sd <- sd(CC_Animals_Nov$CPUE)

# Redfish Bay

RF_Animals_May <- RF_Animals %>%
  filter(Month == "5")

RF_Animals_May_Mean <- mean(RF_Animals_May$CPUE)
RF_Animals_May_sd <- sd(RF_Animals_May$CPUE)

RF_Animals_Jun <- RF_Animals %>%
  filter(Month == "6")

RF_Animals_Jun_Mean <- mean(RF_Animals_Jun$CPUE)
RF_Animals_Jun_sd <- sd(RF_Animals_Jun$CPUE)

RF_Animals_Jul <- RF_Animals %>%
  filter(Month == "7")

RF_Animals_Jul_Mean <- mean(RF_Animals_Jul$CPUE)
RF_Animals_Jul_sd <- sd(RF_Animals_Jul$CPUE)

RF_Animals_Aug <- RF_Animals %>%
  filter(Month == "8")

RF_Animals_Aug_Mean <- mean(RF_Animals_Aug$CPUE)
RF_Animals_Aug_sd <- sd(RF_Animals_Aug$CPUE)

RF_Animals_Sep <- RF_Animals %>%
  filter(Month == "9")

RF_Animals_Sep_Mean <- mean(RF_Animals_Sep$CPUE)
RF_Animals_Sep_sd <- sd(RF_Animals_Sep$CPUE)

RF_Animals_Oct <- RF_Animals %>%
  filter(Month == "10")

RF_Animals_Oct_Mean <- mean(RF_Animals_Oct$CPUE)
RF_Animals_Oct_sd <- sd(RF_Animals_Oct$CPUE)

RF_Animals_Nov <- RF_Animals %>%
  filter(Month == "11")

RF_Animals_Nov_Mean <- mean(RF_Animals_Nov$CPUE)
RF_Animals_Nov_sd <- sd(RF_Animals_Nov$CPUE)

# Produce dataframe with mean and sd CPUE per site per month for all animals

Mean_Month <- c(AB_Animals_May_Mean, CC_Animals_May_Mean, RF_Animals_May_Mean, AB_Animals_Jun_Mean, CC_Animals_Jun_Mean, RF_Animals_Jun_Mean, AB_Animals_Jul_Mean, CC_Animals_Jul_Mean, RF_Animals_Jul_Mean, AB_Animals_Aug_Mean, CC_Animals_Aug_Mean, RF_Animals_Aug_Mean, AB_Animals_Sep_Mean, CC_Animals_Sep_Mean, RF_Animals_Sep_Mean, AB_Animals_Oct_Mean, CC_Animals_Oct_Mean, RF_Animals_Oct_Mean, AB_Animals_Nov_Mean, CC_Animals_Nov_Mean, RF_Animals_Nov_Mean)

sd_Month <- c(AB_Animals_May_sd, CC_Animals_May_sd, RF_Animals_May_sd, AB_Animals_Jun_sd, CC_Animals_Jun_sd, RF_Animals_Jun_sd, AB_Animals_Jul_sd, CC_Animals_Jul_sd, RF_Animals_Jul_sd, AB_Animals_Aug_sd, CC_Animals_Aug_sd, RF_Animals_Aug_sd, AB_Animals_Sep_sd, CC_Animals_Sep_sd, RF_Animals_Sep_sd, AB_Animals_Oct_sd, CC_Animals_Oct_sd, RF_Animals_Oct_sd, AB_Animals_Nov_sd, CC_Animals_Nov_sd, RF_Animals_Nov_sd)

Month <- as.numeric(rep(c("5", "6", "7", "8", "9", "10", "11"), each = 3))

Site <- rep(c("Aransas Bay", "Corpus Christi Bay", "Redfish Bay"), times = 7)

Animal_CPUEs_Month <- data.frame(Month, Site, Mean_Month, sd_Month) %>%
  replace(is.na(.), 0)

# PLot

ggplot(Animal_CPUEs_Month, aes(y=Mean_Month, x=Month, group=Site, line=Site)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=Mean_Month-sd_Month, ymax=Mean_Month+sd_Month), width=.2,
                 position=position_dodge(0.05)) +
  ggtitle("All Animals") +
  ylab("Mean CPUE by Site by Month") +
  scale_x_continuous(breaks = c(5, 6, 7, 8, 9, 10, 11)) +
  scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25)) +
  facet_grid(~Site) +
  theme_standard +
  theme(plot.title = element_text(size=25, hjust=0.5))

ggsave("../Results/All_Animals_CPUE_Site_Month.png", plot = last_plot())

```

## Sex Differences

#### By Species

For each species individually, use chi-squared test to determine if there is a difference in sex frequencies.

```{r}

Species_Count <- count(Elasmos_Complete, Species)
Species_Count

```

##### Carcharhinus_brevipinna

```{r, warning=FALSE, message=FALSE}

temp <- filter(Elasmos_Complete, Species=="Carcharhinus_brevipinna")

C.brev_Sex_Table <- table(temp$Sex)
C.brev_Sex_Table

chisq.test(x = c(47, 21), p = c(0.5, 0.5))

C.brev_Female <- 47/(47+21+2)
C.brev_p <- 0.001616

```

**Skewed sex ratio for Carcharhinus brevipinna**

##### Carcharhinus_leucas

```{r, warning=FALSE, message=FALSE}

temp <- filter(Elasmos_Complete, Species=="Carcharhinus_leucas")

C.leuc_Sex_Table <- table(temp$Sex)
C.leuc_Sex_Table

chisq.test(x = c(6, 2), p = c(0.5, 0.5))

C.leuc_Female <- 6/(6+2)
C.leuc_p <- 0.1573

```

**Even sex ratio for Carcharhinus leucas**

##### Carcharhinus_limbatus

```{r, warning=FALSE, message=FALSE}

temp <- filter(Elasmos_Complete, Species=="Carcharhinus_limbatus")

C.limb_Sex_Table <- table(temp$Sex)
C.limb_Sex_Table

chisq.test(x = c(1, 1), p = c(0.5, 0.5))

C.limb_Female <- 1/(1+1)
C.limb_p <- 1

```

##### Carcharhinus_porosus

```{r, warning=FALSE, message=FALSE}

temp <- filter(Elasmos_Complete, Species=="Carcharhinus_porosus")

C.poro_Sex_Table <- table(temp$Sex)
C.poro_Sex_Table

#chisq.test(x = c(0, 0), p = c(0.5, 0.5))

C.poro_Female <- 0/(0+0)
C.poro_p <- NA

```

**Even sex ratio for Carcharhinus limbatus**

##### Hypanus_americanus

```{r, warning=FALSE, message=FALSE}

temp <- filter(Elasmos_Complete, Species=="Hypanus_americanus")

H.ameri_Sex_Table <- table(temp$Sex)
H.ameri_Sex_Table

chisq.test(x = c(9, 2), p = c(0.5, 0.5))

H.ameri_Female <- 9/(9+2)
H.ameri_p <- 0.03481

```

**Skewed sex ratio for Hypanus americanus**

##### Hypanus_sabina

```{r, warning=FALSE, message=FALSE}

temp <- filter(Elasmos_Complete, Species=="Hypanus_sabina")

H.sabi_Sex_Table <- table(temp$Sex)
H.sabi_Sex_Table

chisq.test(x = c(11, 0), p = c(0.5, 0.5))

H.sabi_Female <- 11/(11+0)
H.sabi_p <- 0.0009111

```

***Skewed sex ratio for Hypanus sabina**

##### Rhinoptera_bonasus

```{r, warning=FALSE, message=FALSE}

temp <- filter(Elasmos_Complete, Species=="Rhinoptera_bonasus")

R.bona_Sex_Table <- table(temp$Sex)
R.bona_Sex_Table

chisq.test(x = c(1, 0), p = c(0.5, 0.5))

R.bona_Female <- 1/(1+0)
R.bona_p <- 0.3173

```

**Even sex ratio for Rhinoptera bonasus**

##### Rhizoprionodon_terraenovae

```{r, warning=FALSE, message=FALSE}

temp <- filter(Elasmos_Complete, Species=="Rhizoprionodon_terraenovae")

R.terr_Sex_Table <- table(temp$Sex)
R.terr_Sex_Table

chisq.test(x = c(6, 7), p = c(0.5, 0.5))

R.terr_Female <- 6/(6+7+1)
R.terr_p <- 0.7815

```

**Even sex ratio for for Rhizoprionodon terraenovae**

##### Sphyrna_lewini

```{r, warning=FALSE, message=FALSE}

temp <- filter(Elasmos_Complete, Species=="Sphyrna_lewini")

S.lewi_Sex_Table <- table(temp$Sex)
S.lewi_Sex_Table

chisq.test(x = c(3, 1), p = c(0.5, 0.5))

S.lewi_Female <- 3/(3+1)
S.lewi_p <- 0.3173

```

**Even sex ratio for Sphyrna lewini**

##### Sphyrna_tiburo

```{r, warning=FALSE, message=FALSE}

temp <- filter(Elasmos_Complete, Species=="Sphyrna_tiburo")

S.tib_Sex_Table <- table(temp$Sex)
S.tib_Sex_Table

chisq.test(x = c(18, 16), p = c(0.5, 0.5))

S.tib_Female <- 18/(18+16+1)
S.tib_p <- 0.7316

```
**Even sex ratio for Sphyrna tiburo**

### By Life-history Stage

Use chi-squared test to determine if there is a difference in sex ratios for each life-history stage

```{r, warning=FALSE, message=FALSE}

YOY_df <- filter(Elasmos_Complete, STAGE == "YOY")

JUV_df <- filter(Elasmos_Complete, STAGE == "JUV")

MAT_df <- filter(Elasmos_Complete, STAGE == "MAT")

```

##### YOY

```{r, warning=FALSE, message=FALSE}

YOY_Sex_Table <- table(YOY_df$Sex)
YOY_Sex_Table

chisq.test(x = c(58, 27), p = c(0.5, 0.5))

```

**Skewed sex ratio for YOY**

##### JUV

```{r, warning=FALSE, message=FALSE}

JUV_Sex_Table <- table(JUV_df$Sex)
JUV_Sex_Table

chisq.test(x = c(22, 15), p = c(0.5, 0.5))

```

**Even sex ratio for for JUV**

##### MAT

```{r, warning=FALSE, message=FALSE}

MAT_Sex_Table <- table(MAT_df$Sex)
MAT_Sex_Table

chisq.test(x = c(21, 8), p = c(0.5, 0.5))

```

**Skewed sex ratio for MAT**

## Environmental Differences Among Sites

Use KW to test for signficant differences in environmental variables among sites.

### Temperature

Visualise surface and bottom temperatures by site.

```{r, fig.height=8, fig.width=8, warning=FALSE, message=FALSE}

Temp <- na.omit(Set_Meta, cols=c("Surface_Temp", "Bottom_Temp"))

Temp_Surf_Site_Plot <- ggplot(Temp, aes(x = Site, y = Surface_Temp, color=Site)) +
  geom_boxplot() +
  theme_standard +
  labs(x = "", y = "Surface Temperature (ºC)") +
  scale_fill_manual(values=col3) +
  scale_color_manual(values=col3) +
  theme(axis.text.x = element_text(size=0), legend.position = "none")

Temp_Bott_Site_Plot <- ggplot(Temp, aes(x = Site, y = Bottom_Temp, color=Site)) +
  geom_boxplot() +
  theme_standard +
  labs(x = "Site", y = "Bottom Temperature (ºC)") +
  scale_fill_manual(values=col3) +
  scale_color_manual(values=col3) +
  theme(axis.text.x = element_text(size=0), legend.title = element_text(size=18), legend.text = element_text(size=18))

multiplot(Temp_Surf_Site_Plot, Temp_Bott_Site_Plot, cols = 1)

```

Perform KW with Surface and Bottom Temperature ~ Site. 

```{r, warning=FALSE, message=FALSE}

kruskal.test(Surface_Temp + Bottom_Temp ~ Site, data = Temp)

```

**No difference in surface and bottom temperature among sites**

### Salinity

Visualise surface and bottom salinity by site.

```{r, fig.height=8, fig.width=8, warning=FALSE, message=FALSE}

Temp <- na.omit(Set_Meta, cols=c("Surface_Salinity", "Bottom_Salinity"))

Sal_Surf_Site_Plot <- ggplot(Temp, aes(x = Site, y = Surface_Salinity, color=Site)) +
  geom_boxplot() +
  theme_standard +
  labs(x = "", y = "Surface Salinity (ppt)") +
  scale_fill_manual(values=col3) +
  scale_color_manual(values=col3) +
  theme(axis.text.x = element_text(size=0), legend.position = "none")

Sal_Bott_Site_Plot <- ggplot(Temp, aes(x = Site, y = Bottom_Salinity, color=Site)) +
  geom_boxplot() +
  theme_standard +
  labs(x = "Site", y = "Bottom Salinity (ppt)") +
  scale_fill_manual(values=col3) +
  scale_color_manual(values=col3) +
  theme(axis.text.x = element_text(size=0), legend.title = element_text(size=18), legend.text = element_text(size=18))

multiplot(Sal_Surf_Site_Plot, Sal_Bott_Site_Plot, cols = 1)

# Mean and sd Surface Salinity in AB

temp <- filter(Set_Meta, Site == "Aransas_Bay") %>%
  na.omit()

mean(temp$Surface_Salinity)
sd(temp$Surface_Salinity)

# Mean and sd Bottom Salinity in AB

mean(temp$Bottom_Salinity)
sd(temp$Bottom_Salinity)

# Mean and sd Surface Salinity in CC

temp <- filter(Set_Meta, Site == "Corpus_Christi_Bay") %>%
  na.omit()

mean(temp$Surface_Salinity)
sd(temp$Surface_Salinity)

# Mean and sd Bottom Salinity in CC
mean(temp$Bottom_Salinity)
sd(temp$Bottom_Salinity)

# Mean and sd Surface Salinity in RF

temp <- filter(Set_Meta, Site == "Redfish_Bay") %>%
  na.omit()

mean(temp$Surface_Salinity)
sd(temp$Surface_Salinity)

# Mean and sd Bottom Salinity in RF

mean(temp$Bottom_Salinity)
sd(temp$Bottom_Salinity)

```

Perform KW with Surface and Bottom Salinity ~ Site. 

```{r, warning=FALSE, message=FALSE}

kruskal.test(Surface_Salinity + Bottom_Salinity ~ Site, data = Temp)

```
**Difference in surface and bottom salinity among sites**

Perform post-hoc tests

```{r, warning=FALSE, message=FALSE}

dunnTest(Surface_Salinity ~ Site, data = Temp, method="bh")

dunnTest(Bottom_Salinity ~ Site, data = Temp, method="bh")

```

**Differences in surface and bottom salinity among Corpus-Aransas, Redfish-Aransas, but not Corpus-Redfish**


### Dissolved Oxygen

Visualise surface and bottom dissolved oxygen by site.

```{r, fig.height=8, fig.width=8, warning=FALSE, message=FALSE}

Temp <- na.omit(Set_Meta, cols=c("Surface_DO", "Bottom_DO"))

DO_Surf_Site_Plot <- ggplot(Temp, aes(x = Site, y = Surface_DO, color=Site)) +
  geom_boxplot() +
  theme_standard +
  labs(x = "", y = "Surface Dissolved Oxygen (mg/L)") +
  scale_fill_manual(values=col3) +
  scale_color_manual(values=col3) +
  theme(axis.text.x = element_text(size=0), legend.position = "none")

DO_Bott_Site_Plot <- ggplot(Temp, aes(x = Site, y = Bottom_DO, color=Site)) +
  geom_boxplot() +
  theme_standard +
  labs(x = "Site", y = "Surface Dissolved Oxygen (mg/L)") +
  scale_fill_manual(values=col3) +
  scale_color_manual(values=col3) +
  theme(axis.text.x = element_text(size=0), legend.title = element_text(size=18), legend.text = element_text(size=18))

multiplot(DO_Surf_Site_Plot, DO_Bott_Site_Plot, cols = 1)

```

Perform KW with Surface and Bottom Dissolved Oxygen ~ Site. 

```{r, warning=FALSE, message=FALSE}

kruskal.test(Surface_DO + Bottom_DO ~ Site, data = Temp)

```

**No difference in surface dissolved oxygen among sites**

### Distance from Tidal Inlet

Visualise distance from tidal inlet by site.

```{r, fig.height=5, fig.width=8, warning=FALSE, message=FALSE}

ggplot(Set_Meta, aes(x = Site, y = Fish_Distance, color=Site)) +
  geom_boxplot() +
  theme_standard +
  labs(x = "", y = "Distance from Tidal Inlet") +
  scale_fill_manual(values=col3) +
  scale_color_manual(values=col3) +
  theme(axis.text.x = element_text(size=0), legend.title = element_text(size=18), legend.text = element_text(size=14), 
        legend.position = "right")

# Mean and sd Distance in AB

temp <- filter(Set_Meta, Site == "Aransas_Bay")

mean(temp$Fish_Distance)
sd(temp$Fish_Distance)

# Mean and sd Distance in CC

temp <- filter(Set_Meta, Site == "Corpus_Christi_Bay")

mean(temp$Fish_Distance)
sd(temp$Fish_Distance)

# Mean and sd Distance in RF

temp <- filter(Set_Meta, Site == "Redfish_Bay")

mean(temp$Fish_Distance)
sd(temp$Fish_Distance)

```

Perform KW with Distance from Tidal Inlet ~ Site. 

```{r, warning=FALSE, message=FALSE}

kruskal.test(Fish_Distance ~ Site, data = Set_Meta)

```

**Difference in distance from tidal inlet among sites**

Perform post-doc tests 

```{r, warning=FALSE, message=FALSE}

dunnTest(Fish_Distance ~ Site, data = Set_Meta, method="bh")

```

**Differences in distance from tidal inlet among all site comparisons**

### Depth

Visualise depth by site.

```{r, fig.height=5, fig.width=8, warning=FALSE, message=FALSE}

ggplot(Set_Meta, aes(x = Site, y = Depth, color=Site)) +
  geom_boxplot() +
  theme_standard +
  labs(x = "", y = "Depth (m)") +
  scale_fill_manual(values=col3) +
  scale_color_manual(values=col3) +
  theme(axis.text.x = element_text(size=0), legend.title = element_text(size=18), legend.text = element_text(size=14), 
        legend.position = "right")

# Mean and sd Depth in AB

temp <- filter(Set_Meta, Site == "Aransas_Bay")

mean(temp$Depth)
sd(temp$Depth)

# Mean and sd Depth in CC

temp <- filter(Set_Meta, Site == "Corpus_Christi_Bay")

mean(temp$Depth)
sd(temp$Depth)

# Mean and sd Depth in RF

temp <- filter(Set_Meta, Site == "Redfish_Bay")

mean(temp$Depth)
sd(temp$Depth)


```

Perform KW with Distance from Depth ~ Site. 

```{r, warning=FALSE, message=FALSE}

kruskal.test(Depth ~ Site, data = Set_Meta)

```

**Difference in distance from depth among sites**

Perform post-hoc tests.

```{r, warning=FALSE, message=FALSE}

dunnTest(Depth ~ Site, data = Set_Meta, method="bh")

```

**Differences in depth between Corpus Christi Bay and the other two bays but not Aransas and Redfish Bay**

## Environmental Differences Among Species

##### Temperature

Use boxplots to show the surface and bottom water temperatures when individuals of each species were caught.

```{r, fig.height=7, fig.width=11, warning=FALSE, message=FALSE}

Species_Surface_Temp_Plot <- ggplot(Elasmos_Complete, aes(x = Species, y = Surface_Temp, color=Species)) +
  geom_boxplot(outlier.alpha = NULL) +
  theme_standard +
  labs(x = "", y = "Surface Temperature (ºC)") +
  scale_fill_manual(values=col10) +
  scale_color_manual(values=col10) +
  scale_y_continuous(breaks = c(24, 25, 26, 27, 28, 29, 30, 31, 32, 33)) +
  scale_x_discrete(labels = c("C. brevipinna", "C. leucas", "C. limbatus", "C. porosus", "H. americanus", "H. sabina", "R. bonasus", "R. terrnovae", "S. lewini", "S. tiburo")) +
  theme(axis.text.x = element_text(size=0, color = col10), legend.position = "none")

Species_Bottom_Temp_Plot <- ggplot(Elasmos_Complete, aes(x = Species, y = Bottom_Temp, color=Species)) +
  geom_boxplot() +
  theme_standard +
  labs(x = "Species", y = "Bottom Temperature (ºC)") +
  scale_fill_manual(values=col10) +
  scale_color_manual(values=col10) +
  scale_y_continuous(breaks = c(24, 25, 26, 27, 28, 29, 30, 31, 32, 33)) +
  scale_x_discrete(labels = c("C. brevipinna", "C. leucas", "C. limbatus", "C. porosus", "H. americanus", "H. sabina", "R. bonasus", "R. terrnovae", "S. lewini", "S. tiburo")) +
  theme(axis.text.x = element_text(size=12, color = col10), legend.position = "none")

multiplot(Species_Surface_Temp_Plot, Species_Bottom_Temp_Plot, cols = 1)

ggsave("../Results/Species_Temp_Plot.png")

# Create df

Surface_Temp_Min <- Elasmos_Complete %>%
  group_by(Species) %>%
  summarize(Min_S.Temp = min(Surface_Temp, na.rm=TRUE))

Surface_Temp_Mean <- Elasmos_Complete %>%
  group_by(Species) %>%
  summarize(Mean_S.Temp = mean(Surface_Temp, na.rm=TRUE)) %>%
  dplyr::select(-Species)

Surface_Temp_Max <- Elasmos_Complete %>%
  group_by(Species) %>%
  summarize(Max_S.Temp = max(Surface_Temp, na.rm=TRUE)) %>%
  dplyr::select(-Species)

Surface_Temp_sd <- Elasmos_Complete %>%
  group_by(Species) %>%
  summarize(sd_S.Temp = sd(Surface_Temp, na.rm=TRUE)) %>%
  dplyr::select(-Species)

Bottom_Temp_Min <- Elasmos_Complete %>%
  group_by(Species) %>%
  summarize(Min_B.Temp = min(Bottom_Temp, na.rm=TRUE)) %>%
  dplyr::select(-Species)

Bottom_Temp_Mean <- Elasmos_Complete %>%
  group_by(Species) %>%
  summarize(Mean_B.Temp = mean(Bottom_Temp, na.rm=TRUE)) %>%
  dplyr::select(-Species)

Bottom_Temp_Max <- Elasmos_Complete %>%
  group_by(Species) %>%
  summarize(Max_B.Temp = max(Bottom_Temp, na.rm=TRUE)) %>%
  dplyr::select(-Species)

Bottom_Temp_sd <- Elasmos_Complete %>%
  group_by(Species) %>%
  summarize(sd_B.Temp = sd(Bottom_Temp, na.rm=TRUE)) %>%
  dplyr::select(-Species)

Temp_df <- cbind(Surface_Temp_Min, Surface_Temp_Mean, Surface_Temp_Max, Surface_Temp_sd, Bottom_Temp_Min, Bottom_Temp_Mean, Bottom_Temp_Max, Bottom_Temp_sd)

```

##### Salinity

Use boxplots to show the surface and bottom water salinity when individuals of each species were caught.

```{r, fig.height=7, fig.width=11, warning=FALSE, message=FALSE}

Species_Surface_Salinity_Plot <- ggplot(Elasmos_Complete, aes(x = Species, y = Surface_Salinity, color=Species)) +
  geom_boxplot() +
  theme_standard +
  labs(x = "", y = "Surface Salinity (ppt)") +
  scale_fill_manual(values=col10) +
  scale_color_manual(values=col10) +
  scale_y_continuous(breaks = c(5, 10, 15, 20, 25, 30, 35, 40)) +
  scale_x_discrete(labels = c("C. brevipinna", "C. leucas", "C. limbatus", "C. porosus", "H. americanus", "H. sabina", "R. bonasus", "R. terrnovae", "S. lewini", "S. tiburo")) +
  theme(axis.text.x = element_text(size=0, color = col10), legend.position = "none")

Species_Bottom_Salinity_Plot <- ggplot(Elasmos_Complete, aes(x = Species, y = Bottom_Salinity, color=Species)) +
  geom_boxplot() +
  theme_standard +
  labs(x = "Species", y = "Bottom Salinity (ppt)") +
  scale_fill_manual(values=col10) +
  scale_color_manual(values=col10) +
  scale_y_continuous(breaks = c(5, 10, 15, 20, 25, 30, 35, 40)) +
  scale_x_discrete(labels = c("C. brevipinna", "C. leucas", "C. limbatus", "C. porosus", "H. americanus", "H. sabina", "R. bonasus", "R. terrnovae", "S. lewini", "S. tiburo")) +
  theme(axis.text.x = element_text(size=12, color = col10), legend.position = "none")

multiplot(Species_Surface_Salinity_Plot, Species_Bottom_Salinity_Plot, cols = 1)

ggsave("../Results/Species_Salinity_Plot.png")

# Create df

Surface_Salinity_Min <- Elasmos_Complete %>%
  group_by(Species) %>%
  summarize(Min_S.Sal = min(Surface_Salinity, na.rm=TRUE))

Surface_Salinity_Mean <- Elasmos_Complete %>%
  group_by(Species) %>%
  summarize(Mean_S.Sal = mean(Surface_Salinity, na.rm=TRUE)) %>%
  dplyr::select(-Species)

Surface_Salinity_Max <- Elasmos_Complete %>%
  group_by(Species) %>%
  summarize(Max_S.Sal = max(Surface_Salinity, na.rm=TRUE))%>%
  dplyr::select(-Species)

Surface_Salinity_sd <- Elasmos_Complete %>%
  group_by(Species) %>%
  summarize(sd_S.Sal = sd(Surface_Salinity, na.rm=TRUE))%>%
  dplyr::select(-Species)

Bottom_Salinity_Min <- Elasmos_Complete %>%
  group_by(Species) %>%
  summarize(Min_B.Sal = min(Bottom_Salinity, na.rm=TRUE)) %>%
  dplyr::select(-Species)

Bottom_Salinity_Mean <- Elasmos_Complete %>%
  group_by(Species) %>%
  summarize(Mean_B.Sal = mean(Bottom_Salinity, na.rm=TRUE)) %>%
  dplyr::select(-Species)

Bottom_Salinity_Max <- Elasmos_Complete %>%
  group_by(Species) %>%
  summarize(Max_B.Sal = max(Bottom_Salinity, na.rm=TRUE)) %>%
  dplyr::select(-Species)

Bottom_Salinity_sd <- Elasmos_Complete %>%
  group_by(Species) %>%
  summarize(sd_B.Sal = sd(Bottom_Salinity, na.rm=TRUE)) %>%
  dplyr::select(-Species)

Salinity_df <- cbind(Surface_Salinity_Min, Surface_Salinity_Mean, Surface_Salinity_Max, Surface_Salinity_sd, Bottom_Salinity_Min, Bottom_Salinity_Mean, Bottom_Salinity_Max, Bottom_Salinity_sd)

```

##### Dissolved Oxygen

Use boxplots to show the surface and bottom water dissolved oxygen when individuals of each species were caught.

```{r, fig.height=7, fig.width=11, warning=FALSE, message=FALSE}

Species_Surface_DO_Plot <- ggplot(Elasmos_Complete, aes(x = Species, y = Surface_DO, color=Species)) +
  geom_boxplot() +
  theme_standard +
  labs(x = "", y = "Surface Dissolved Oxygen (mg/L)") +
  scale_fill_manual(values=col10) +
  scale_color_manual(values=col10) +
  scale_y_continuous(breaks = c(4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5)) +
  scale_x_discrete(labels = c("C. brevipinna", "C. leucas", "C. limbatus", "C. porosus", "H. americanus", "H. sabina", "R. bonasus", "R. terrnovae", "S. lewini", "S. tiburo")) +
  theme(axis.text.x = element_text(size=0, color = col10), axis.text.y = element_text(size=12), axis.title.y = element_text(size=14), legend.position = "none")

Species_Bottom_DO_Plot <- ggplot(Elasmos_Complete, aes(x = Species, y = Bottom_DO, color=Species)) +
  geom_boxplot() +
  theme_standard +
  labs(x = "Species", y = "Surface Dissolved Oxygen (mg/L)") +
  scale_fill_manual(values=col10) +
  scale_color_manual(values=col10) +
  scale_y_continuous(breaks = c(4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8)) +
  scale_x_discrete(labels = c("C. brevipinna", "C. leucas", "C. limbatus", "C. porosus", "H. americanus", "H. sabina", "R. bonasus", "R. terrnovae", "S. lewini", "S. tiburo")) +
  theme(axis.text.x = element_text(size=12, color = col10), axis.text.y = element_text(size=12), axis.title.y = element_text(size=14), legend.position = "none")

multiplot(Species_Surface_DO_Plot, Species_Bottom_DO_Plot, cols = 1)

ggsave("../Results/Species_DO_Plot.png")

# Create df

Surface_DO_Min <- Elasmos_Complete %>%
  group_by(Species) %>%
  summarize(Min_S.DO = min(Surface_DO, na.rm=TRUE))

Surface_DO_Mean <- Elasmos_Complete %>%
  group_by(Species) %>%
  summarize(Mean_S.DO = mean(Surface_DO, na.rm=TRUE)) %>%
  dplyr::select(-Species)

Surface_DO_Max <- Elasmos_Complete %>%
  group_by(Species) %>%
  summarize(Max_S.DO = max(Surface_DO, na.rm=TRUE))%>%
  dplyr::select(-Species)

Surface_DO_sd <- Elasmos_Complete %>%
  group_by(Species) %>%
  summarize(sd_S.DO = sd(Surface_DO, na.rm=TRUE))%>%
  dplyr::select(-Species)

Bottom_DO_Min <- Elasmos_Complete %>%
  group_by(Species) %>%
  summarize(Min_B.DO = min(Bottom_DO, na.rm=TRUE)) %>%
  dplyr::select(-Species)

Bottom_DO_Mean <- Elasmos_Complete %>%
  group_by(Species) %>%
  summarize(Mean_B.DO = mean(Bottom_DO, na.rm=TRUE)) %>%
  dplyr::select(-Species)

Bottom_DO_Max <- Elasmos_Complete %>%
  group_by(Species) %>%
  summarize(Max_B.DO = max(Bottom_DO, na.rm=TRUE)) %>%
  dplyr::select(-Species)

Bottom_DO_sd <- Elasmos_Complete %>%
  group_by(Species) %>%
  summarize(sd_B.DO = sd(Bottom_DO, na.rm=TRUE)) %>%
  dplyr::select(-Species)

DO_df <- cbind(Surface_DO_Min, Surface_DO_Mean, Surface_DO_Max, Surface_DO_sd, Bottom_DO_Min, Bottom_DO_Mean, Bottom_DO_Max, Bottom_DO_sd)

```

##### Distance from Tidal Inlet

Use boxplots to show the distance from the tidal inlet when individuals of each species were caught.

```{r, fig.height=7, fig.width=11, warning=FALSE, message=FALSE}

temp <- Elasmos_Complete %>%
  dplyr::mutate(Dist = Fish_Distance/1000)

ggplot(temp, aes(x = Species, y = Dist, color=Species)) +
  geom_boxplot() +
  theme_standard +
  labs(x = "Species", y = "Distance from Tidal Inlet (km)") +
  scale_fill_manual(values=col10) +
  scale_color_manual(values=col10) +
  scale_y_continuous(breaks = c(10, 15, 20, 25, 30, 35)) +
  scale_x_discrete(labels = c("C. brevipinna", "C. leucas", "C. limbatus", "C. porosus", "H. americanus", "H. sabina", "R. bonasus", "R. terrnovae", "S. lewini", "S. tiburo")) +
  theme(axis.text.x = element_text(size=12, color = col10), axis.text.y = element_text(size=12), axis.title.y = element_text(size=14), legend.position = "none")

ggsave("../Results/Dist_Plot.png")

# Create df

Dist_Min <- temp %>%
  group_by(Species) %>%
  summarize(Min_Dist = min(Dist, na.rm=TRUE))

Dist_Mean <- temp %>%
  group_by(Species) %>%
  summarize(Mean_Dist = mean(Dist, na.rm=TRUE)) %>%
  dplyr::select(-Species)

Dist_Max <- temp %>%
  group_by(Species) %>%
  summarize(Max_Dist = max(Dist, na.rm=TRUE))%>%
  dplyr::select(-Species)

Dist_sd <- temp %>%
  group_by(Species) %>%
  summarize(sd_Dist = sd(Dist, na.rm=TRUE))%>%
  dplyr::select(-Species)

Dist_df <- cbind(Dist_Min, Dist_Mean, Dist_Max, Dist_sd)

```

##### Depth

Use boxplots to show the depth when individuals of each species were caught.

```{r, fig.height=7, fig.width=11, warning=FALSE, message=FALSE}

ggplot(Elasmos_Complete, aes(x = Species, y = Depth, color=Species)) +
  geom_boxplot() +
  theme_standard +
  labs(x = "Species", y = "Depth (m)") +
  scale_fill_manual(values=col10) +
  scale_color_manual(values=col10) +
  scale_y_continuous(breaks = c(2, 2.5, 3, 3.5, 4, 4.5, 5)) +
  scale_x_discrete(labels = c("C. brevipinna", "C. leucas", "C. limbatus", "C. porosus", "H. americanus", "H. sabina", "R. bonasus", "R. terrnovae", "S. lewini", "S. tiburo")) +
  theme(axis.text.x = element_text(size=12, color = col10), axis.text.y = element_text(size=12), axis.title.y = element_text(size=14), legend.position = "none")

ggsave("../Results/Dist_Plot.png")

# Create df

Depth_Min <- Elasmos_Complete %>%
  group_by(Species) %>%
  summarize(Min_Depth = min(Depth, na.rm=TRUE))

Depth_Mean <- Elasmos_Complete %>%
  group_by(Species) %>%
  summarize(Mean_Depth = mean(Depth, na.rm=TRUE)) %>%
  dplyr::select(-Species)

Depth_Max <- Elasmos_Complete %>%
  group_by(Species) %>%
  summarize(Max_Depth = max(Depth, na.rm=TRUE))%>%
  dplyr::select(-Species)

Depth_sd <- Elasmos_Complete %>%
  group_by(Species) %>%
  summarize(sd_Depth = sd(Depth, na.rm=TRUE))%>%
  dplyr::select(-Species)

Depth_df <- cbind(Depth_Min, Depth_Mean, Depth_Max, Depth_sd)

Environmental_df <- left_join(Temp_df, Salinity_df) %>%
  left_join(DO_df) %>%
  left_join(Dist_df) %>%
  left_join(Depth_df) %>%
  dplyr::select(c(Species, Mean_S.Temp, sd_S.Temp, Mean_B.Temp, sd_B.Temp, Mean_S.Sal, sd_S.Sal, Mean_B.Sal, sd_B.Sal, Mean_S.DO, sd_S.DO, Mean_B.DO, sd_B.DO, Mean_Dist, sd_Dist, Mean_Depth, sd_Depth)) %>%
  format(digits=3, nsmall=1) %>%
  unite(S.Temp, Mean_S.Temp, sd_S.Temp, sep = " +/- ", remove = TRUE) %>%
  unite(B.Temp, Mean_B.Temp, sd_B.Temp, sep = " +/- ", remove = TRUE) %>%
  unite(S.Sal, Mean_S.Sal, sd_S.Sal, sep = " +/- ", remove = TRUE) %>%
  unite(B.Sal, Mean_B.Sal, sd_B.Sal, sep = " +/- ", remove = TRUE) %>%
  unite(S.DO, Mean_S.DO, sd_S.DO, sep = " +/- ", remove = TRUE) %>%
  unite(B.DO, Mean_B.DO, sd_B.DO, sep = " +/- ", remove = TRUE) %>%
  unite(Dist, Mean_Dist, sd_Dist, sep = " +/- ", remove = TRUE) %>%
  unite(Depth, Mean_Depth, sd_Depth, sep = " +/- ", remove = TRUE)

write.csv(Environmental_df, "../Results/Environmental_df.csv")

```

### Test for Differences in Environmental Conditions When Species Were Caught

```{r}

# Test for significant differences betwen batoids and sharks in depth and distance from tidal inlet

temp <- Elasmos_Complete %>%
  mutate(Shark.Batoid = ifelse(Species %in% c("Hypanus_sabina", "Hypanus_americanus", "Rhinoptera_bonasus"), "Batoid", "Shark"))

```

##### Salinity

```{r}

kruskal.test(Surface_Salinity + Bottom_Salinity ~ Species, data = Elasmos_Complete)

# Surface

Sur_Sal_DT <- dunnTest(Surface_Salinity ~ Species, data = Elasmos_Complete, method ="bh")

Sur_Sal_DT_df <- data.frame(Sur_Sal_DT$res$Comparison, Sur_Sal_DT$res$P.adj) %>%
  dplyr::filter(Sur_Sal_DT.res.P.adj <= 0.05)

#View(Sur_Sal_DT_df)

# Bottom

Bot_Sal_DT <- dunnTest(Bottom_Salinity ~ Species, data = Elasmos_Complete, method ="bh")

Bot_Sal_DT_df <- data.frame(Bot_Sal_DT$res$Comparison, Bot_Sal_DT$res$P.adj) %>%
  dplyr::filter(Bot_Sal_DT.res.P.adj <= 0.05)

#View(Bot_Sal_DT_df)

```
**Differences in salinities at which different species were caught**

##### Distance From Tidal Inlet

```{r}

kruskal.test(Fish_Distance ~ Shark.Batoid, data = temp)

# Fish_Distance

Dist_DT <- dunnTest(Fish_Distance ~ Species, data = Elasmos_Complete, method ="bh")

Dist_DT_df <- data.frame(Dist_DT$res$Comparison, Dist_DT$res$P.adj) %>%
  dplyr::filter(Dist_DT.res.P.adj <= 0.05)

# Mean Shark Distance 

s <- dplyr::filter(temp, Shark.Batoid=="Shark")

mean(s$Fish_Distance)/1000
sd(s$Fish_Distance)/1000

# Mean Batoid Distance 

b <- dplyr::filter(temp, Shark.Batoid=="Batoid")

mean(b$Fish_Distance)/1000
sd(b$Fish_Distance)/1000

```

**Differences in distance from tidal inlet at which different species were caught**

##### Depth

```{r}

kruskal.test(Depth ~ Shark.Batoid, data = temp)

# Depth

Depth_DT <- dunnTest(Depth ~ Species, data = Elasmos_Complete, method ="bh")

Depth_DT_df <- data.frame(Depth_DT$res$Comparison, Depth_DT$res$P.adj) %>%
  dplyr::filter(Depth_DT.res.P.adj <= 0.05)

# Mean Depth

s <- dplyr::filter(temp, Shark.Batoid=="Shark")

mean(s$Depth)
sd(s$Depth)

# Mean Depth

b <- dplyr::filter(temp, Shark.Batoid=="Batoid")

mean(b$Depth)
sd(b$Depth)

```

**Differences in depth at which different species were caught**

## Environmental Drivers of Shark Abundance

Use `gbm.auto` package to identify environmental drivers of shark presence/absence and abundance. 

```{r}

# Produce dataset for gbm.auto

Species_Counts <- count(Elasmos_Complete, Species, Set_ID) %>%
  spread(key = Species, value = n) %>%
  replace(is.na(.), 0) %>%
  full_join(Elasmos_Set) %>%
  rename(Spinner = Carcharhinus_brevipinna) %>%
  rename(Bull = Carcharhinus_leucas) %>%
  rename(Blacktip = Carcharhinus_limbatus) %>%
  rename(Smalltail = Carcharhinus_porosus) %>%
  rename(Southern = Hypanus_americanus) %>%
  rename(Atlantic = Hypanus_sabina) %>%
  rename(Cownose = Rhinoptera_bonasus) %>%
  rename(Sharpnose = Rhizoprionodon_terraenovae) %>%
  rename(Scalloped = Sphyrna_lewini) %>%
  rename(Bonnethead = Sphyrna_tiburo) %>%
  replace_na(list(Spinner = 0, Bull = 0, Blacktip = 0, Smalltail = 0, Southern = 0, Atlantic = 0, Cownose = 0, Sharpnose = 0, Scalloped = 0, Bonnethead = 0)) %>%
  dplyr::select(c(12, 1, 13, 20:28, 30, 2:11)) %>%
  mutate(Batoids = Southern + Atlantic + Cownose) %>%
  mutate(Sharks = Spinner + Bull + Blacktip + Smalltail + Sharpnose + Scalloped + Bonnethead) %>%
  mutate(LCS = Spinner + Bull + Blacktip + Scalloped) %>%
  mutate(Elasmos = Spinner + Bull + Blacktip + Smalltail + Sharpnose + Scalloped + Bonnethead + Southern + Atlantic + Cownose) %>%
  arrange(Set_Number) %>%
  #na.omit() %>%
  data.frame()

write.csv(Species_Counts, "../Results/Species_Counts.csv", row.names=FALSE)

# Import catch data 

Species_Counts <- read.csv("../Results/Species_Counts.csv")

# Check optimum bf value

gbm.bfcheck(samples = Species_Counts, resvar = "LCS", ZI = "CHECK")

# Bin bf > 0.105

# Gaus bf > 0.4375

gbm.auto(grids = NULL, samples = Species_Counts, expvar = c(6:13), resvar = 26, tc = 8, lr = list(0.0027, 0.0037), bf = 0.7)

Report <- read.csv("../Code/LCS/Report.csv")
View(Report)

```

## Community Differences 

### Bray-Curtis Dissimilarity Analysis Among Sites

```{r}

BC_Mat <- Elasmos_Complete %>%
  group_by(Species) %>%
  count(Site) %>%
  spread(key = Species, value = n) %>%
  replace(is.na(.), 0)

vegdist(BC_Mat[,2:11], method="bray")

```

### Non-metric Multidimensional Scaling (NMDS)

Illustrate similarity/dissimilarity in species composition among the bays. The goal of NMDS is to collapse information from multiple dimensions (e.g. multiple sites) into a few so that they can be visualised. Unlike other ordination techniques which rely primarily on Euclidean distances (e.g. PCA), NMDS uses rank orders and thus is an extremely flexible technique which can accomodate a variety of different kinds of data.

First we must produce a matrix with the number of individuals of each species (column names) in each site (row name).

### By Site

```{r}

# Species matrix

NMDS_Mat_Site <- Elasmos_Complete %>%
  group_by(Species, Site) %>% 
  tally() %>%
  spread(Species, n) %>%
  replace(is.na(.), 0) %>%
  rownames_to_column() %>%
  dplyr::select(-c(Site, rowname))

# Environmental matrix

Elasmo_Meta_Site <- Elasmos_Complete %>%
  dplyr::select(c(Site, Bottom_Temp, Bottom_Salinity, Bottom_DO, Depth, Fish_Distance)) %>%
  group_by(Site) %>% 
  summarise_at(c("Bottom_Temp", "Bottom_DO", "Bottom_Salinity", "Depth", "Fish_Distance"), mean, na.rm = TRUE) %>%
  dplyr::select(-Site)

```
 
`metaMDS` is a wrapper to perform non-metric multidimensional scaling (NMDS) like recommended in community ordination: it uses adequate dissimilarity measures (`vegdist`), then runs NMDS several times with random starting configurations, compares results (`procrustes`), and stops after finding twice a similar minimum stress solution. Finally it scales and rotates the solution and adds species scores to the configuration as weighted averages (`wascores`).

```{r}

ord <- metaMDS(NMDS_Mat_Site, distance = "bray", k = 2)

plot(ord, type = "t")

p <- ggord(ord, poly = FALSE, polylntyp = NMDS_Mat_Site)
p

ef <- envfit(ord, Elasmo_Meta_Site, nperm = 1000)

vec.sp.df <- as.data.frame(ef$vectors$arrows*sqrt(ef$vectors$r)) %>%
  rownames_to_column() %>%
  rename(Variable = rowname)

Bottom_Temp_Seg <- vec.sp.df[1,]
Bottom_DO_Seg <- vec.sp.df[2,]
Bottom_Salinity_Seg <- vec.sp.df[3,]
Depth_Seg <- vec.sp.df[4,]
Dist_Seg <- vec.sp.df[5,]

```

Ordination is just a way of drawing graphs and it is best to inspect ordinations graphically. 

```{r fig.height=8, fig.width=8}

ord <- metaMDS(NMDS_Mat_Site, distance = "bray", k = 2)

data.scores <- as.data.frame(scores(ord))
data.scores$Site <- c("Aransas Bay", "Corpus Christi Bay", "Redfish Bay")

species.scores <- as.data.frame(scores(ord, "species"))
species.scores$species <- c("Carcharhinus brevipinna", "Carcharhinus leucas", "Carcharhinus limbatus", "Carcharhinus porosus", "Hypanus americanus", "Hypanus sabina", "Rhinoptera bonasus", "Rhizoprionodon terraenovae", "Sphyrna lewini", "Sphyrna tiburo")

x <- species.scores$NMDS1
y <- species.scores$NMDS2

x <- data.frame(species.scores$NMDS1)
y <- data.frame(species.scores$NMDS2)

colnames(x) <- "X"
colnames(y) <- "Y"

species.scores <- bind_cols(species.scores, x, y) %>%
  mutate(Group = ifelse(species=="H. ame", "Batoid",
                        ifelse(species=="H. sab", "Batoid", 
                               ifelse(species=="R. bon", "Batoid", "Shark"))))
  
ggplot() + 
  geom_point(data=data.scores, aes(x=NMDS1, y=NMDS2, shape = Site), size=8, fill ="black") +
  #stat_ellipse(data=species.scores, aes(x=NMDS1, y=NMDS2, color = Group)) +
 # geom_polygon(data=species.scores,aes(x=NMDS1,y=NMDS2,fill=Group,group=Group),alpha=0.30) +
  geom_segment(data=species.scores, aes(x=0, xend=NMDS1, y=0, yend=NMDS2),
      arrow = arrow(length = unit(0.5, "cm")), colour="grey", linetype = "dashed", size = 1) + 
  geom_text_repel(data=species.scores, aes(x=X, y=Y, label=species, fontface=3), alpha=1, size = 5, color = "black") +
  scale_shape_manual(values = shape3) +
  #geom_segment(data=vec.sp.df, aes(x=0, xend=NMDS1, y=0, yend=NMDS2),
              # arrow = arrow(length = unit(0.25, "cm")), colour="red", linetype = "dashed", size = 1) +
   #geom_text(data=vec.sp.df, aes(x=NMDS1, y=NMDS2, label=Variable), size = 4, color = "red") +
  #xlim(-0.6, 0.6) +
  #ylim(-0.6, 0.6) +
  labs(x = "NMDS1", y = "NMDS2") +
  theme_standard +
  theme(legend.title = element_text(size=12), legend.text = element_text(size=12), plot.title = element_text(hjust = 0.5, size=20))

ggsave("../Results/NMDS_Plot_Site.png")

```

### By Set

```{r}

# Filter species that were caught <2 times

count(Elasmos_Complete, Species)

Elasmos_Clean <- filter(Elasmos_Complete, Species != "Rhinoptera_bonasus" & Species != "Carcharhinus_porosus") %>%
  na.omit()

# Species matrix

NMDS_Mat_Set <- Elasmos_Clean %>%
  group_by(Species, Set_ID) %>% 
  tally() %>%
  spread(Species, n) %>%
  replace(is.na(.), 0) %>%
  rownames_to_column() %>%
  dplyr::select(-c(Set_ID, rowname))

#NMDS_Mat_Set_Trans <- decostand(NMDS_Mat_Set, method = "hellinger")

#NMDS_Mat_Set_Trans <- (NMDS_Mat_Set + 2) %>%
 # log()

# Environmental matrix

Elasmo_Meta_Set <- Elasmos_Complete %>%
  filter(Species != "Rhinoptera_bonasus" & Species != "Carcharhinus_porosus") %>%
  na.omit() %>%
  dplyr::select(c(Set_ID, Bottom_Temp, Bottom_Salinity, Bottom_DO, Depth, Fish_Distance)) %>%
  group_by(Set_ID) %>% 
  summarise_at(c("Fish_Distance", "Bottom_Salinity"), mean) %>%
  dplyr::select(-Set_ID)

```
 
`metaMDS` is a wrapper to perform non-metric multidimensional scaling (NMDS) like recommended in community ordination: it uses adequate dissimilarity measures (`vegdist`), then runs NMDS several times with random starting configurations, compares results (`procrustes`), and stops after finding twice a similar minimum stress solution. Finally it scales and rotates the solution and adds species scores to the configuration as weighted averages (`wascores`).

Ordination is just a way of drawing graphs and it is best to inspect ordinations graphically. 

```{r fig.height=8, fig.width=8}

ord <- metaMDS(NMDS_Mat_Set, distance = "bray")

p <- ggord(ord, poly = FALSE, polylntyp = NMDS_Mat_Set)

ef <- envfit(ord, Elasmo_Meta_Set, nperm = 999)

ef

vec.sp.df <- as.data.frame(ef$vectors$arrows*sqrt(ef$vectors$r)) %>%
  rownames_to_column() %>%
  rename(Variable = rowname)

vec.sp.df$Variable <- c("Distance", "Salinity")

Set_IDs_Site <- Elasmos_Clean %>%
  group_by(Species, Set_ID, Site) %>% 
  tally() %>%
  spread(Species, n) %>%
  replace(is.na(.), 0) %>%
  rownames_to_column() %>%
  dplyr::select(c(Set_ID, Site, rowname))

data.scores <- as.data.frame(scores(ord))
data.scores$Site <- Set_IDs_Site$Site
data.scores$Set <- Set_IDs_Site$rowname

species.scores <- as.data.frame(scores(ord, "species"))
species.scores$species <- c("C. bre", "C. leu", "C. lim", "H. ame", "H. sab", "R. ter", "S. lew", "S. tib")

#x <- species.scores$NMDS1
#y <- species.scores$NMDS2

#jit_x <- data.frame(jitter(x, factor = 10))
#jit_y <- data.frame(jitter(y, factor = 10))

#colnames(jit_x) <- "X"
#colnames(jit_y) <- "Y"

x <- data.frame(species.scores$NMDS1)
y <- data.frame(species.scores$NMDS2)

#jit_x <- data.frame(jitter(x, factor = 10))
#jit_y <- data.frame(jitter(y, factor = 10))

colnames(x) <- "X"
colnames(y) <- "Y"

species.scores <- bind_cols(species.scores, x, y) %>%
  mutate(Group = ifelse(species=="H. ame", "Batoid",
                        ifelse(species=="H. sab", "Batoid", 
                               ifelse(species=="R. bon", "Batoid", "Shark"))))
  
ggplot() + 
  geom_point(data=data.scores, aes(x=NMDS1, y=NMDS2, fill = Site, color = Site), size=1) +
  stat_ellipse(data=data.scores, aes(x=NMDS1, y=NMDS2, color = Site), level = 0.5) +
  geom_text(data=species.scores, aes(x=X, y=Y, label=species, fontface=3), alpha=1, size = 5, color = "black") +
  geom_segment(data=species.scores, aes(x=0, xend=NMDS1, y=0, yend=NMDS2),
      arrow = arrow(length = unit(0.5, "cm")), colour="grey", linetype = "dashed", size = 0.5) + 
  geom_segment(data=vec.sp.df, aes(x=0, xend=NMDS1, y=0, yend=NMDS2),
      arrow = arrow(length = unit(0.5, "cm")), colour="black", linetype = "dashed", size = 0.5) +
  geom_text(data=vec.sp.df, aes(x=NMDS1+0.1, y=NMDS2-0.1, label=Variable), size = 4, color = "black") +
  labs(x = "NMDS1", y = "NMDS2") +
  theme_standard
  
ggsave("../Results/NMDS_Plot_Set.png")

```

### adonis

Run NMDS

```{r}

all.mds <- metaMDS(NMDS_Mat_Set) 

```

Plot

```{r}

data.scores <- as.data.frame(scores(all.mds))
data.scores$Site <- Set_IDs_Site$Site
data.scores$Set <- Set_IDs_Site$rowname

species.scores <- as.data.frame(scores(all.mds, "species"))
species.scores$species <- c("C. bre", "C. leu", "C. lim", "H. ame", "H. sab", "R. ter", "S. lew", "S. tib")

ggplot(data=data.scores) +
  geom_point(aes(x=NMDS1,y=NMDS2, shape=Site, color = Site), size=2) + 
  stat_ellipse(aes(x=NMDS1,y=NMDS2, lty=Site, color = Site), level = 0.75) +
  scale_shape_manual(values = shape3) +
  scale_color_manual(values = col3) +
  scale_linetype_manual(values=c(1,2,3)) +
  theme_standard

```

Results

```{r}

NMDS_Mat_Sites <- as.vector(data.scores$Site)
#NMDS_Mat_Distance <- as.vector(Elasmo_Meta_Set$Dist_Tidal_Inlet)
#NMDS_Mat_Bottom_Sal <- as.vector(Elasmo_Meta_Set$Bottom_Salinity)
  
adon.results <- adonis(NMDS_Mat_Set ~ NMDS_Mat_Sites, method="bray", perm=999)
print(adon.results)

```

adonis works by first finding the centroids for each group and then calculates the squared deviations of each of site to that centroid. Then significance tests are performed using F-tests based on sequential sums of squares from permutations of the raw data.

Calculate multivariant dispersions

```{r}

## Bray-Curtis distances between samples

dis <- vegdist(NMDS_Mat_Set, method="bray")

## Calculate multivariate dispersions

mod <- betadisper(dis, NMDS_Mat_Sites)
mod

```
