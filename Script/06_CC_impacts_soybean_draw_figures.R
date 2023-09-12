# Purpose: Use single plots obtained in script 06 to create complete maps showing the impact of climate change on soybean performances
# Author: M. Marteau-Bazouni
# Date: 18/07/2023

# Packages ----------------------------------------------------------------
library(magick);library(plotrix)


# Map options and drawing functions -------------------------------------------------------------

# Palettes
palPink <- c("white",'#feedf6','#fcdbed','#f5c4e1','#eda8d0','#e285b8','#d6599d','#c82882','#ac1069','#8e0152')
palGreen <- c("white","#f1f9e4","#e1f3c8","#c7e89f","#abd976","#8cc450","#6eae36","#539724","#3c7d1d","#276419")
palGrey <- c("white","#f6f6f6","#ededed","#e0e0e0","#d3d3d3","#c3c3c3","#b0b0b0","#9a9a9a","#868686","#737373")
palBlue<-c("#C6DBEF","#9ECAE1","#6BAED6","#4292C6","#2171B5","#084594","#08306B","#081A3A")

font_size=5.5

modif_map1 <- function(n,pal,num,cat){ # Resize and add a legend for the maps of the first row
  m <- image_read(n)
  m<- image_crop(m,"1300x1570+0+0")
  m <- image_border(m, "white", "5x5")
  
  m <- image_draw(m)
  rect(0,0,1310,110, col="white",border=NA)
  rect(1040,20,1310,940, col="white",border=NA)
  text(1180,200, c("% of \nselected \npapers"), family = "Helvetica", cex = 5,adj=0.5)
  gradient.rect(1100,800,1160,350, col=rev(pal),gradient="y")
  text(1200,seq(350,800,90), c("0","20","40","60","80","100"), family = "Helvetica", cex = 5,adj=0.2)
  rect(1100,910,1160,860,col='#fbf8f3')
  text(1200,885, "NA", family = "Helvetica", cex = 5,adj=0.2)
  text(50, 190, num, family = "Helvetica", cex = 6,adj=0)
  dev.off()
  
  if(cat=="first" & grepl("_pos",n)){
    m <- image_border(m, "white", "110x0")
    m<- image_crop(m, "1420x1580+0+0")
  }
  return(m)
}

modif_map <- function(n,num,cat){ # Resize the maps of the other rows
  m <- image_read(n)
  if(cat=="last"){
    m<- image_crop(m,"1300x1570+0+70")
  }
  else{
    m<- image_crop(m,"1300x1500+0+70")
  }
  
  m <- image_border(m, "white", "5x5")
  
  m <- image_draw(m)
  text(50, 100, num, family = "Helvetica", cex = 6,adj=0)
  dev.off()
  
  if(grepl("_pos",n)){
    m <- image_border(m, "white", "110x0")
    m<- image_crop(m, "1420x1580+0+0")
  }
  return(m)
}

draw_row <- function(name,legend,n,cat){ # Return a row of three maps
  n1 <- paste("../Output/CC_impact/",name,"_pos.jpg",sep="")
  n2 <- paste("../Output/CC_impact/",name,"_neu.jpg",sep="")
  n3 <- paste("../Output/CC_impact/",name,"_neg.jpg",sep="")
  if(cat %in% c("first","single")){
    m1 <- modif_map1(n1,palGreen,n[1],cat)
    m2 <- modif_map1(n2,palGrey,n[2],cat)
    m3 <- modif_map1(n3,palPink,n[3],cat)
  }
  else{
    m1 <- modif_map(n1,n[1],cat)
    m2 <- modif_map(n2,n[2],cat)
    m3 <- modif_map(n3,n[3],cat)
  }
  
  fig <- image_append(c(m1,m2,m3))
  fig <- image_draw(fig)
  if(cat=="single"){text(c(650,1950,3250), 45, c("Share of papers simulating a positive impact","neutral impact","negative impact"), family = "Helvetica", cex = font_size,adj=0.5)}
  if(cat=="first"){text(c(650,1950,3250)+110, 45, c("Share of papers simulating a positive impact","neutral impact","negative impact"), family = "Helvetica", cex = font_size,adj=0.5)}
  if(cat!="single"){text(45, 785, legend, family = "Helvetica", cex = font_size,adj=0.5,srt = 90)}
  dev.off()
  return(fig)
}

# Draw climate change impact maps ----------------------------------------------------

# Full dataset
r <- draw_row("full","",n=c("a)","b)","c)"),cat="single")
image_write(r,path="../Output/CC_impact/Figure4_Impact_CC_full_data_impact.jpg")

# Rainfed soybean only
r <- draw_row("rainfed","Rainfed soybean",n=c("a)","b)","c)"),cat="first")
image_write(r,path="../Output/CC_impact/FigureS11_Impact_CC_rainfed_impact.jpg")

# Near and far future
r1 <- draw_row("near","Near future",n=c("a)","b)","c)"),cat="first")
r2 <- draw_row("far","Far future",n=c("d)","e)","f)"),cat="last")
image_write(image_append(c(r1,r2),stack=TRUE),path="../Output/CC_impact/Figure5_Impact_CC_time_impact.jpg")

# Studies with and without CO2
r1 <- draw_row("no_CO2","Without CO2",n=c("a)","b)","c)"),cat="first")
r2 <- draw_row("CO2","With CO2",n=c("d)","e)","f)"),cat="last")
image_write(image_append(c(r1,r2),stack=TRUE),path="../Output/CC_impact/FigureS9_Impact_CC_CO2_impact.jpg")

# Type of model
r1 <- draw_row("process","Process-based models",n=c("a)","b)","c)"),cat="first")
r2 <- draw_row("niche","Niche models",n=c("d)","e)","f)"),cat="mid")
r3 <- draw_row("statistical","Statistical models",n=c("g)","h)","i)"),cat="last")
image_write(image_append(c(r1,r2,r3),stack=TRUE),path="../Output/CC_impact/FigureS7_Impact_CC_type_of_model_impact.jpg")

# Scenario
r1 <- draw_row("RCP2.6","RCP2.6",n=c("a)","b)","c)"),cat="first")
r2 <- draw_row("RCP4.5","RCP4.5",n=c("d)","e)","f)"),cat="mid")
r3 <- draw_row("RCP6.0","RCP6.0",n=c("g)","h)","i)"),cat="mid")
r4 <- draw_row("RCP8.5","RCP8.5",n=c("j)","k)","l)"),cat="last")
image_write(image_append(c(r1,r2,r3,r4),stack=TRUE),path="../Output/CC_impact/FigureS5_Impact_CC_scenario_impact.jpg")

# Draw number of papers----------------------------------------------------

add_nb_legend <- function(name,num){ # Add the legend
  n <- paste("../Output/CC_impact/",name,"_nb.jpg",sep="")
  m <- image_read(n)
  
  m <- image_crop(m,"1300x1500+0+70")
  m <- image_draw(m)
  rect(1040,0,1310,830, col="white",border=NA)
  text(1175,70, c("Number \nof papers"), family = "Helvetica", cex = 5,adj=0.5)
  gradient.rect(1100,700,1160,180, col=rev(palBlue),gradient="y")
  text(1180,seq(245,700,62), c("1","3","5","7","9","11","13","15"), family = "Helvetica", cex = 5,adj=0)
  rect(1100,790,1160,740,col='#fbf8f3')
  text(1180,755, "NA", family = "Helvetica", cex = 5,adj=0)
  text(50, 140, num, family = "Helvetica", cex = 6,adj=0)
  dev.off()
  m <- image_border(m, "white", "5x5")
  return(m)
}

resize_nb <- function(name,num){ # Resize the maps
  n <- paste("../Output/CC_impact/",name,"_nb.jpg",sep="")
  m <- image_read(n)
  
  m <- image_crop(m,"1300x1500+0+70")
  m <- image_draw(m)
  text(50, 140, num, family = "Helvetica", cex = 6,adj=0)
  dev.off()
  m <- image_border(m, "white", "5x5")
  return(m)
}

# Full dataset
nb <- add_nb_legend("full","")
image_write(nb,path="../Output/CC_impact/Figure3_Impact_CC_full_data_nb.jpg")

# Rainfed soybean
nb <- add_nb_legend("rainfed","")
image_write(nb,path="../Output/CC_impact/FigureS10_Impact_CC_rainfed_nb.jpg")

# Near and far future
nb1 <- resize_nb("near","a) Near future")
nb2 <- add_nb_legend("far","b) Far future")
image_write(image_append(c(nb1,nb2)),path="../Output/CC_impact/FigureS3_Impact_CC_time_nb.jpg")

# Studies with and without CO2
nb1 <- resize_nb("no_CO2","a) Without CO2")
nb2 <- add_nb_legend("CO2","b) With CO2")
image_write(image_append(c(nb1,nb2)),path="../Output/CC_impact/FigureS8_Impact_CC_CO2_nb.jpg")

# Type of model
nb1 <- resize_nb("process","a) Process-based models")
nb2 <- resize_nb("niche","b) Niche models")
nb3 <- add_nb_legend("statistical","c) Statistical models")
image_write(image_append(c(nb1,nb2,nb3)),path="../Output/CC_impact/FigureS6_Impact_CC_type_of_model_nb.jpg")

# Scenarios
nb1 <- resize_nb("RCP2.6","a) RCP2.6")
nb2 <- resize_nb("RCP4.5","b) RCP4.5")
nb3 <- resize_nb("RCP6.0","c) RCP6.0")
nb4 <- add_nb_legend("RCP8.5","d) RCP8.5")
image_write(image_append(c(image_append(c(nb1,nb2)),image_append(c(nb3,nb4))),stack=TRUE),path="../Output/CC_impact/FigureS4_Impact_CC_scenario_nb.jpg")
