# Purpose: Draw a map showing the study area and the Koppen-Geiger classification
# Author: M. Marteau-Bazouni
# Date: 22/08/2023


# Packages ----------------------------------------------------------------
library(magick);library(plotrix)


# Import images --------------------------------------------------------------

KG <- image_read("../Output/Koppen_Geiger_classification/KG_similar_areas.jpg")
study_zone <- image_read("../Output/Koppen_Geiger_classification/Study_area.jpg")
barplot <- image_read("../Output/Koppen_Geiger_classification/Nb_paper_per_KG_class.jpg")

# Zoom on zone of interest --------------------------------------------------
zoom_map <- function(m,legend,coord){
  size = 4
  x <- image_crop(m,coord)
  x <- image_draw(x)
  rect(0,0,4000,80, col= rgb(1,1,1,alpha=0.9),border=NA)
  text(20,40,legend,family="Helvetica",cex=size,adj=0)
  dev.off()
  x <- image_border(x,"1x1",col="black")
  return(x)
}

USA <- zoom_map(KG,"North America: 27 papers","800x720+400+1680");USA
EU <- zoom_map(KG,"Europe: 7 papers","800x700+2150+1400");EU
Iran <- zoom_map(KG,"Iran: 5 papers","400x400+3050+1900");Iran
Austr <- zoom_map(KG,"Australia: 4 papers","700x500+4200+2850");Austr
Braz <- zoom_map(KG,"Southern Brazil: 4 papers","600x500+1200+2900");Braz


# Superimpose maps --------------------------------------------------------

world <- image_border(image_crop(study_zone,"1500x725+50+140"),"100x550",col="white");world
result <- image_composite(image_scale(world,"x2500"), USA, offset = "+40+10")
result <- image_composite(result,barplot,offset='+1750+1800')
result <- image_composite(result,EU,offset='+940+10')
result <- image_composite(result,Iran,offset='+1800+250')
result <- image_composite(result,Braz,offset='+40+1400')
result <- image_composite(result,Austr,offset='+950+1450')

# Add legend --------------------------------------------------------------
result <- image_draw(result)
rect(450,950,800,1200,col=NA) ;lines(x=c(600,625),y=c(730,950))
rect(1050,850,1350,1050,col=NA) ;lines(x=c(1200,1200),y=c(710,850))
rect(1355,1020,1500,1120,col=NA) ; lines(x=c(1425,2100),y=c(1020,650))
rect(1720,1300,1980,1480,col=NA) ; lines(x=c(1720,1650),y=c(1400,1500))
rect(790,1350,890,1480,col=NA) ; lines(x=c(790,640),y=c(1420,1450))


text(40,1980,substitute(paste(bold("Legend"))),family="Helvetica",cex=5,adj=0)
gradient.rect(50,2050,150,2200,gradient = "y",col=c("#bababa","white"))
text(170,c(2075,2160),c("Study area","Region outside \nthe study area"),cex=3.5,adj=0)

text(50,2275,substitute(paste(bold("Number of papers"))),family="Helvetica",cex=3.6,adj=0)
text(c(110,160,210,280,355),2450,c("1","2",'3',"5","7"),family="Helvetica",cex=3.5,adj=0)

gradient.rect(650,2030,750,2480,gradient = "y",col=c("#CCAA54","#007800","#005000","#96FF00","#00D700","#550055","#820082","#C800C8","#8C8C8C"))
text(650,2000,substitute(paste(bold("Koppen-Geiger classification within the study area"))),cex=3.6,adj=0)
text(770,seq(2055,2455,50),c("BSk: arid,steppe, cold",
                                       "Cfa: temperate, no dry season, hot summer",
                                       "Cfb: temperate, no dry season, warm summer",
                                       "Csa: temperate, dry season, hot summer",
                                       "Csb: temperate, dry season, warm summer",
                                       "Dfa: cold, no dry season, hot summer",
                                       "Dfb: cold, no dry season, warm summer",
                                       "Dfc: cold, no dry season, cold summer",
                                       "Dsb: cold, dry season, warm summer"),cex=3.5,adj=0)
dev.off()

legend_size <- image_flatten(image_crop(KG,"320x100+120+3370"),"Modulate")

result <- image_composite(result,legend_size,offset='+100+2325')

# Export ------------------------------------------------------------------

image_write(result,"../Output/Koppen_Geiger_classification/Figure6_Study_area_and_KG_classification.jpg")
