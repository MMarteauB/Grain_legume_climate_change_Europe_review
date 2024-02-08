# Purpose: Describe Koppen-Geiger classification within the study area
# Author: M. Marteau-Bazouni
# Date: 18/07/2023

# Data and algorithm adapted from the recent analysis from Rubel et al. (2017), available on https://koeppen-geiger.vu-wien.ac.at/present.htm [Access date 22th May 2023]

# Packages ----------------------------------------------------------------
library(raster); library(rasterVis); library(rworldxtra); data(countriesHigh)
library(latticeExtra) ; library(dplyr); library(ggplot2)

# Input data --------------------------------------------------------------

r <- raster("../Input/Map_KG-Global/KG_1986-2010.grd") # Koppen-Geiger areas
load("../Input/CroplandArea2000_Geotiff/Cropland2000_5m.RData") # Map of cropland

data <- read.csv2("../Input/Data_review.csv",fileEncoding="latin1",dec=',',colClasses=c(x_map="numeric",y_map="numeric"))

# Mask non-cropland area --------------------------------------------------

cropland <- resample(cropland_0,r)
cropland[cropland == 0] <- NA ; cropland[cropland > 0] <- 1
mask <- r*cropland # Mask non-cropland area

ocean <- r # Keep ocean area
ocean[ocean!=32] <- NA

r <- max(mask,ocean,na.rm = TRUE)

# Legend for Koppen-Geiger zones (code from Rubel et al. (2017)) ------------------------------------------

## Color palette for climate classification
climate.colors=c("#960000", "#FF0000", "#FF6E6E", "#FFCCCC", "#CC8D14", "#CCAA54", "#FFCC00", "#FFFF64", "#007800", "#005000", "#003200", "#96FF00", "#00D700", "#00AA00", "#BEBE00", "#8C8C00", "#5A5A00", "#550055", "#820082", "#C800C8", "#FF6EFF", "#646464", "#8C8C8C", "#BEBEBE", "#E6E6E6", "#6E28B4", "#B464FA", "#C89BFA", "#C8C8FF", "#6496FF", "#64FFFF", "#F5FFFF")

## Legend must correspond to all climate classes, insert placeholders
r0 <- r[1:32]; r[1:32] <- seq(1,32,1)

## Converts raster field to categorical data
r <- ratify(r); rat <- levels(r)[[1]]

## Legend is always drawn in alphabetic order
legend_KG <- c('Af', 'Am', 'As', 'Aw', 'BSh', 'BSk', 'BWh', 'BWk', 'Cfa', 'Cfb','Cfc', 'Csa', 'Csb', 'Csc', 'Cwa','Cwb', 'Cwc', 'Dfa', 'Dfb', 'Dfc','Dfd', 'Dsa', 'Dsb', 'Dsc', 'Dsd','Dwa', 'Dwb', 'Dwc', 'Dwd', 'EF','ET', 'Ocean')
rat$climate <- c('Af', 'Am', 'As', 'Aw', 'BSh', 'BSk', 'BWh', 'BWk', 'Cfa', 'Cfb','Cfc', 'Csa', 'Csb', 'Csc', 'Cwa','Cwb', 'Cwc', 'Dfa', 'Dfb', 'Dfc','Dfd', 'Dsa', 'Dsb', 'Dsc', 'Dsd','Dwa', 'Dwb', 'Dwc', 'Dwd', 'EF','ET', 'Ocean')

## Remove the placeholders
r[1:32] <- r0; levels(r) <- rat


# Identify KG zones in Europe ---------------------------------------------
x1=-20; x2=40; y1=36; y2=72; xat=5; yat=5	# Limits of Europe
rEU <- crop(r, extent(x1, x2, y1, y2)) # European climate classification

# Count the number of pixels in Europe for each zone
nb_pix <- data.frame(zone = c(1:32)) %>% rowwise() %>%
  mutate(Nb=length(which(values(rEU)==zone)))
include <-nb_pix$zone[nb_pix$Nb>1000] # Area included
exclude <-nb_pix$zone[nb_pix$Nb<1000] # Area excluded


# Keep only climate zones present in Europe -------------------------------

x1=-140; x2=160; y1=-60; y2=80; xat=20; yat=10
r_world <- crop(r, extent(x1, x2, y1, y2))
climate.colors[exclude]<-NA

# Add papers coordinates
coord<- data %>% filter(Cat!="Impact"&!is.na(x_map)) %>%
  distinct(Doi,x_map,y_map) %>%
  add_count(x_map,y_map,name="nb_paper") %>%
  distinct(x_map,y_map,nb_paper)

# Add legend points
legend_size <- data.frame(x_leg=c(-135,-132,-129,-125,-120),y_leg=rep(-45,5),nb_leg=c(1,2,3,5,7))

# Draw map

jpeg("../Output/Koppen_Geiger_classification/KG_similar_areas.jpg",width=5000, height=5000)

print(levelplot(r_world, col.regions=climate.colors, xlab="", ylab="", 
                scales=list(x=list(limits=c(xmin(r_world), xmax(r_world)), at=seq(xmin(r_world), xmax(r_world), xat)), 
                            y=list(limits=c(ymin(r_world), ymax(r_world)), at=seq(ymin(r_world), ymax(r_world), yat))), colorkey=list(space="top", tck=0, maxpixels=ncell(r_world)),main="Map of the areas climatically similar to Europe (KG classification 1986-2010)")
      +latticeExtra::layer(sp.polygons(countriesHigh, lwd=0.25))
      +latticeExtra::layer(panel.points(x_map, y_map, col="black",fill="white", pch=21,alpha=0.8,cex=1.5*nb_paper+4), data=coord)
      +latticeExtra::layer(panel.points(x_leg, y_leg, col="black",fill="white", pch=21,alpha=0.8,cex=1.5*nb_leg+4), data=legend_size))


dev.off()


# Plot only study area ----------------------------------------------------

x1=-180; x2=180; y1=-90; y2=90 #Global scale
r_world <- crop(r, extent(x1, x2, y1, y2))
climate.colors[include]<-"#bababa";climate.colors[32]<-"#F5FFFF"
  
jpeg("../Output/Koppen_Geiger_classification/Study_area.jpg",width=1500, height=1000)
  
print(levelplot(r_world, col.regions=climate.colors, xlab="", ylab="", 
                scales=list(x=list(limits=c(xmin(r_world), xmax(r_world)), at=seq(xmin(r_world), xmax(r_world), xat)), 
                            y=list(limits=c(ymin(r_world), ymax(r_world)), at=seq(ymin(r_world), ymax(r_world), yat))), colorkey=list(space="top", tck=0, maxpixels=ncell(r_world)),main="")
      +latticeExtra::layer(sp.polygons(countriesHigh, lwd=0.25)))

dev.off()


# Find KG classifcation for each paper ------------------------------------
climate.colors=c("#960000", "#FF0000", "#FF6E6E", "#FFCCCC", "#CC8D14", "#CCAA54", "#FFCC00", "#FFFF64", "#007800", "#005000", "#003200", "#96FF00", "#00D700", "#00AA00", "#BEBE00", "#8C8C00", "#5A5A00", "#550055", "#820082", "#C800C8", "#FF6EFF", "#646464", "#8C8C8C", "#BEBEBE", "#E6E6E6", "#6E28B4", "#B464FA", "#C89BFA", "#C8C8FF", "#6496FF", "#64FFFF", "#F5FFFF")

xy <- coord %>% dplyr::select(x_map,y_map)
KG=r[cellFromXY(r, xy)]
coord$KG <- KG

# Count the number of papers per KG zone
table <- coord %>% mutate(KG=ifelse(KG==5,6,KG)) %>% # Correct KG classification for Baez et al.
  group_by(KG) %>% mutate(total=sum(nb_paper)) %>% distinct(KG,total) %>%
  mutate(legend=legend_KG[KG],color=climate.colors[KG]) %>%
  arrange(KG)
  
# Plot
ggplot(table, aes(x=legend, y=total,fill=legend)) +
  geom_bar(stat="identity")+
  labs(y="Number of studies",x="")+
  scale_fill_manual(values=table$color,guide=FALSE)+
  theme_classic()

# Export
ggsave("../Output/Koppen_Geiger_classification/Nb_paper_per_KG_class.jpg",width=1.6, height=2.5)
