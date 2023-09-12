# Purpose: Count the number of papers per geographical area, separating papers predicting a positive,
# negative and neutral impact of climate change
# Dissociate near from far future, studies with and without CO2, with and without irrigation
# Dissociate studies per type of model and scenario
# Author: M. Marteau-Bazouni
# Date: 18/07/2023

# Packages ----------------------------------------------------------------
library(dplyr);library(sf); library(giscoR); library(mapview); library(raster); library(RColorBrewer); library(stringr); library(tidyr);
library(leafsync); library(tibble)


# Input data --------------------------------------------------------------
data <- read.csv2("../Input/Data_review.csv",fileEncoding="latin1")

data2 <- data %>% filter(Cat!="Adapt" & Region_map !="") %>% # Keep only "impact" corpus
  mutate(CO2=grepl('CO2',Climate_factors)) %>% # New variable indicating whether CO2 is considered in the study
  mutate(Model_cat = case_match(Model_cat,"Statistical model;Process-based model"~"Process-based model",.default = Model_cat)) %>% mutate(Scenario = case_match(Scenario,c("A2","RCP8.5","SSP5-8.5")~"RCP8.5",c("B1","SSP2-4.5","RCP4.5")~"RCP4.5",c("A1B","RCP6.0")~"RCP6.0",.default = Scenario)) # Group together scenarios and types of model

# Functions ---------------------------------------------------------------
count_me <- function(data,var){
  # Count the number of papers with a positive, negative or neutral result per territorial unit, depending on a variable var
  x <- data %>% 
    group_by(Doi,Crop,Region_map,{{var}}) %>%
    add_count(name="Weight") %>% mutate(Weight=1/Weight) %>% ungroup() %>%
    group_by(Crop,Region_map,{{var}},CC_impact_cat) %>%
    summarise(Nb=sum(Weight)) %>%
    mutate(CC_impact_cat= substr(CC_impact_cat, start = 1, stop = 3))%>%
    spread(CC_impact_cat,Nb,fill=0)
  
  # Calculate the part of positive, negative or neutral results
  y <- x %>% rowwise() %>% mutate(Tot=sum(Pos,Neu,Neg)) %>%
    mutate(Pos=Pos/Tot,Neu=Neu/Tot,Neg=Neg/Tot)
  y$Tot_cat <- cut(y$Tot, breaks=c(1, 2, 4, 6, 8, 10, 12,14,16), right = FALSE, labels = c("1","2-3","4-5","6-7","8-9","10-11","12-13","14-15"))
  
  # Get the corresponding geometry from the "shapes" table
  z <- merge(shapes,y,by.y="Region_map",by.x="FID",all.x = TRUE)
  
  return(z)
}



# Build layers (from country scale to NUTS3) ------------------------------

# Get country & NUTS names used in studies
country_names <- as.vector(unique(data2$Region_map[which(data2$Scale_map=="Country")]))
nuts_names <- as.vector(unique(data2$Region_map[which(data2$Scale_map %in% c("Nuts1","Nuts2","Country_cropped") & !data2$Region_map %in% c("ITG1","ITH5"))]))
nuts3_names <- c(unique(data2$Region_map[which(data2$Scale_map== "Nuts3")]),"ITG1","ITH5")

# For each territorial unit, gets the country name
c1 <- data.frame(nuts_names) %>%
  mutate(Country=substr(nuts_names, start = 1, stop = 2)) %>%
  group_by(Country) %>%
  summarize(Nuts = paste(nuts_names, collapse = ';'))

# For each NUTS3 territorial unit, gets the NUTS2 name
c3 <- data.frame(nuts3_names) %>%
  mutate(Nuts=ifelse(nuts3_names == "HR04B","HR",substr(nuts3_names, start = 1, stop = 3)))

# Build base layers
base_layer <- data2 %>% filter(!Region_map %in% unique(c1$Country))

# Split "whole" countries into NUTS 1 & 2
layer1 <- data2 %>% filter(Region_map %in% unique(c1$Country)) %>%
  mutate(ID=Region_map) %>%
  merge(c1,by.x="ID",by.y="Country",all.x=TRUE) %>%
  mutate(Region_map=Nuts) %>%
  separate_longer_delim(Region_map,";") %>% dplyr::select(-ID,-Nuts)

# Merge layers
impact <- rbind(base_layer,layer1)

# Split "whole" NUTS2 into NUTS 3
layer2 <- impact %>% filter(Region_map %in% unique(c3$Nuts)) %>%
  mutate(ID=Region_map) %>% merge(c3,by.x="ID",by.y="Nuts",all.x=TRUE) %>%
  mutate(Region_map=nuts3_names) %>% dplyr::select(-ID,-nuts3_names)

# Merge layers
impact <- rbind(impact,layer2)

# Get territorial unit shapes---------------------------------------------------------------------
names<-as.vector(unique(impact$Region_map[which(impact$Region_map!="RS22")]))

UA<-gisco_get_countries(country = "UA", epsg = 4326) #Split Ukraine in 2 parts as Nuts1 were not available
st_agr(UA) = "constant"
UA1 <- st_crop(UA,xmin=10,xmax=31,ymin=30,ymax=55)
UA2 <- st_crop(UA,xmin=31,xmax=45,ymin=30,ymax=55)
UA1$FID<-"UA1" ; UA2$FID<-"UA2"
UA1<-subset(UA1,select=c("FID","geometry"));UA2<-subset(UA2,select=c("FID","geometry"))

RS22<-gisco_get_nuts(nuts_id= "RS22", epsg = 4326, nuts_level=2)
RS22_geom<- st_make_valid(st_difference(gisco_get_countries(country="RS", epsg = 4326)$geometry,st_union(gisco_get_nuts(nuts_id= "RS21", epsg = 4326, nuts_level=2),gisco_get_nuts(nuts_id= "RS1", epsg = 4326, nuts_level=1)))[2])
RS22 <- st_set_geometry(RS22,RS22_geom)
RS22 <- subset(RS22,select=c("FID","geometry"))

shapes<-rbind(subset(gisco_get_countries(region = "Europe", epsg = 4326),FID!="RU",select=c("FID","geometry")),
              subset(gisco_get_nuts(nuts_id = names, epsg = 4326,nuts_level = 1),select=c("FID","geometry")),
              subset(gisco_get_nuts(nuts_id = names, epsg = 4326,nuts_level = 2),select=c("FID","geometry")),
              subset(gisco_get_nuts(nuts_id = names, epsg = 4326,nuts_level = 3),select=c("FID","geometry")),
              UA1,UA2,RS22)
shapes <- st_crop(shapes,xmin=-12,xmax=40,ymin=32,ymax=70)


# Count the number of papers per territorial unit -------------------------
impact <- subset(impact,Crop=="Soybean") # Keep only soybean

fulldata <- count_me(impact)
timedata<- count_me(impact,Time_period) # Dissociate near from far future
CO2data<- count_me(impact,CO2) # Dissociate studies with and without CO2
irridata<- count_me(impact,Irrigation) # Dissociate studies with and without irrigation
modeldata<- count_me(impact,Model_cat) # Dissociate studies per type of model
scendata<- count_me(impact,Scenario) # Dissociate studies per scenario


# Map options -------------------------------------------------------------
mapviewOptions(fgb = FALSE, na.color = NA, homebutton = FALSE, basemaps = "CartoDB.VoyagerNoLabels") # Basemap choice : https://leaflet-extras.github.io/leaflet-providers/preview/

# Palettes
palPink <- c("white",'#feedf6','#fcdbed','#f5c4e1','#eda8d0','#e285b8','#d6599d','#c82882','#ac1069','#8e0152')
palGreen <- c("white","#f1f9e4","#e1f3c8","#c7e89f","#abd976","#8cc450","#6eae36","#539724","#3c7d1d","#276419")
palGrey <- c("white","#f6f6f6","#ededed","#e0e0e0","#d3d3d3","#c3c3c3","#b0b0b0","#9a9a9a","#868686","#737373")
palBlue<-c("#C6DBEF","#9ECAE1","#6BAED6","#4292C6","#2171B5","#084594","#08306B","#081A3A")

# Remove unwanted graphical elements
clean <- function(m){
  return(removeMapJunk(m,c("zoomControl","drawToolbar","layersControl","easyButton")))}

clean_all <- function(m){
  return(removeMapJunk(m,c("zoomControl","drawToolbar","layersControl","easyButton","scaleBar")))}

# Export maps
export_maps <- function(x,leg,name){
  name <- paste("../Output/CC_impact/",name,sep="")
  n <- paste(name,"_nb.jpg",sep="")
  m <- mapview(x,zcol="Tot_cat",legend=leg,layer.name = "Nb papers",col.regions=palBlue,alpha.regions=1)
  mapshot(m,file = n,zoom=2,vwidth=650,vheight=800)
  
  n1 <- paste(name,"_neu.jpg",sep="");n2 <- paste(name,"_neg.jpg",sep="");n3 <- paste(name,"_pos.jpg",sep="")
  m1 <- mapview(x,zcol="Neu",legend=leg,col.regions=palGrey, at=seq(0,1,0.1),alpha.regions=1,na.alpha=0)
  m2 <- mapview(x,zcol="Neg",legend=leg,col.regions=palPink, at=seq(0,1,0.1),alpha.regions=1,na.alpha=0)
  m3 <- mapview(x,zcol="Pos",legend=leg,col.regions=palGreen, at=seq(0,1,0.1),alpha.regions=1,na.alpha=0)
  mapshot(m1,file = n1,zoom=2,vwidth=650,vheight=800)
  mapshot(m2,file = n2,zoom=2,vwidth=650,vheight=800)
  mapshot(m3,file = n3,zoom=2,vwidth=650,vheight=800)
}


# Export maps -------------------------------------------------------------

leg <- FALSE

export_maps(x=fulldata,leg=leg,name="full") # All papers

near <- subset(timedata,Time_period=="Near"); export_maps(x=near,leg=leg,name="near") # Near future
far <- subset(timedata,Time_period=="Far"); export_maps(x=far,leg=leg,name="far") # Far future

no_CO2 <- subset(CO2data,CO2==FALSE); export_maps(x=no_CO2,leg=leg,name="no_CO2") # Studies without CO2
CO2 <- subset(CO2data,CO2==TRUE); export_maps(x=CO2,leg=leg,name="CO2") # Studies with CO2

rainfed <- subset(irridata,Irrigation=="Rainfed"); export_maps(x=rainfed,leg=leg,name="rainfed") # Rainfed soybean

for (i in c(1,2,3)) { # Per type of model
  x <- c("Niche model","Process-based model","Statistical model")[i]
  name <- c("niche","process","statistical")[i]
  y <- subset(modeldata,Model_cat==x); export_maps(x=y,leg=leg,name=name) 
}

for (x in c("RCP2.6","RCP4.5","RCP6.0","RCP8.5")) { # Per scenario
  y <- subset(scendata,Scenario==x); export_maps(x=y,leg=leg,name=x) 
}