# Purpose: Quantify the effect of different adaptation strategies on crop yield.
# For each paper, calculate the average effect over all time periods, climate scenarios, climate models and locations considered in the paper.
# Author: M. Marteau-Bazouni
# Date: 18/07/2023


# Packages ----------------------------------------------------------------
library(dplyr);library(ggplot2);library(ggforce);library(tidyr);library(gridExtra)

# Input data --------------------------------------------------------------
data <- read.csv2("../Input/Data_review.csv",fileEncoding="latin1",dec=',')


# Rename adaptation strategies -----------------------------------------------

adpt <- data %>% filter(Cat!="Impact" & !is.na(Adapt_impact) & Adapt_type!="Rotation") %>%
  mutate(Adapt_type = case_match(Adapt_type,"Cultivar"~"Cultivar choice","Virtual cultivar"~"Virtual genotype",c("Residue incorporation","Fertilization")~"Residue management \n or fertilization",c("Sowing date x virtual cultivar","Sowing date x cultivar")~"Combination of crop \n management options",c("Residue incorporation x fertilization","Virtual cultivar x Reduced tillage","Fertilization x irrigation")~"Combination of soil \n management options",.default=Adapt_type))%>%
  mutate(Crop=case_match(Crop,"Soybean"~"Soybean",.default="Others"))

# Order strategies
adpt$Adapt_type <- factor(adpt$Adapt_type, levels = rev(c("Cultivar choice","Virtual genotype","Sowing date","Sowing density","Combination of crop \n management options","Irrigation","Reduced tillage","Residue management \n or fertilization","Cover crop","Combination of soil \n management options")))

# Identify papers included in the figure
t <- adpt %>% distinct(Doi)

# Graph parameters --------------------------------------------------------

brks <- c(-20,-0.05,0,0.05, 0.1, 0.25, 0.5, 1, 20)
scale_x <- seq(-4,10,2)

plot_graph <- function(x){
  x$Crop<-factor(x$Crop, levels = c("Soybean","Others"))
  pos <- x %>% spread(Group,Nb,fill=0) %>% dplyr::select(-Negative)
  pos$Class<-factor(pos$Class, levels = rev(levels(pos$Class)))
  neg <- x %>% spread(Group,Nb,fill=0) %>% dplyr::select(-Positive)
  
  labels <- c("Soybean","Other grain leg.")
  names(labels) <- c("Soybean","Others")
  
  neg$Adapt_family <- case_match(neg$Adapt_type,c("Cultivar choice","Virtual genotype","Sowing date","Sowing density","Combination of crop \n management options")~"Crop management",.default = "Water and soil management")
  pos$Adapt_family <- case_match(neg$Adapt_type,c("Cultivar choice","Virtual genotype","Sowing date","Sowing density","Combination of crop \n management options")~"Crop management",.default = "Water and soil management")
  
  p <- ggplot(data=x, aes(fill=Class, y=Adapt_type)) +
    geom_bar(data = neg, stat="identity",position="stack", aes(x=-Negative))+ geom_bar(data = pos, stat="identity",position="stack", aes(x=Positive))+
    scale_fill_manual(values=c("#dfc27d","#f6e8c3","#e5f5f9","#c7eae5","#80cdc1","#35978f","#01665e","#003c30"),labels=c("<-5%","-5% - 0%", "0% - 5%","5% - 10%","10% - 25%","25% - 50%","50% - 100%", ">100%"))+
    scale_x_continuous(breaks=scale_x,labels=abs(scale_x))+
    labs(title=element_blank(), x= "Number of papers", y = element_blank(),fill="Effect of \nadaptation")+
    geom_vline(data=x, aes(xintercept = 0),color="black",linetype="dashed")+
    theme_bw()+ facet_grid(Adapt_family ~ Crop,labeller = labeller(Crop = labels),scales="free_y")
}


# Average adaptation effect over all time periods, climate scenarios, climate models and locations -----------------------------------------------

mean <- adpt %>% group_by(Doi,Crop,Adapt_type) %>%
  mutate(Adapt_impact=mean(Adapt_impact),Nb=1) %>%
  distinct(Adapt_impact,Nb) %>%
  mutate(Class = cut(Adapt_impact, breaks = brks,include.lowest = T, right = F)) %>% # Put adaptation effects into categories
  mutate(Group = ifelse(Class %in%c("[-20,-0.05)","[-0.05,0)"),"Negative","Positive")) # Separate positive effect from negative effect 


# Export ------------------------------------------------------------------

plot_graph(mean)
ggsave(filename = "../Output/Adaptation/Figure8_Adaptation_effect.jpg",width=6.5,height=4.5)