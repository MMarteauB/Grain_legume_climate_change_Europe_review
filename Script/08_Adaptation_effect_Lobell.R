# Purpose: Quantify the effect of different adaptation strategies on crop yield, adopting the definition of Lobell (2014)
# Author: M. Marteau-Bazouni
# Date: 18/07/2023

# Packages ----------------------------------------------------------------
library(dplyr);library(ggplot2);library(ggforce);library(tidyr)

# Input data --------------------------------------------------------------
data <- read.csv2("../Input/Data_review.csv",fileEncoding="latin1",dec=',')

# Calculate adaptation effect --------------------------------------------------------------------

adapt <- data %>%
  filter(Cat!="Impact" & !is.na(Baseline_no_adapt) & !is.na(Baseline_adapt) & !is.na(CC_adapt)) %>% # Keep only papers with a complete dataset
  mutate(Baseline_no_adapt = as.numeric(str_replace(Baseline_no_adapt,",",".")), CC_no_adapt = as.numeric(str_replace(CC_no_adapt,",","."))) %>%
  mutate(Adapt_lobell = ((CC_adapt-CC_no_adapt)-(Baseline_adapt-Baseline_no_adapt))/Baseline_no_adapt) # Calculate adaptation effect
  
# Order adaptation strategies
adapt <- adapt %>%
  mutate(Group = case_match(Adapt_type,"Cultivar"~1,"Virtual cultivar"~2,"Sowing date"~3,"Sowing density"~4,c("Sowing date x virtual cultivar","Sowing date x cultivar")~5,"Irrigation"~6,"Reduced tillage"~7,c("Residue incorporation","Fertilization")~8,"Cover crop"~9,c("Residue incorporation x fertilization","Virtual cultivar x Reduced tillage","Fertilization x irrigation")~10,.default=11))

legend <- c("Cultivar choice","Virtual genotype","Sowing date","Sowing density","Combination of crop \nmanagement options","Irrigation","Reduced tillage","Residue management \nor fertilization","Cover crop","Combination of soil \nmanagement options","Others")

# Calculate mean, min and max adaptation effect
adapt <- adapt %>%
  group_by(Doi,Crop,Adapt_type) %>%
  mutate(Mean = mean(Adapt_lobell),Max=max(Adapt_lobell),Min=min(Adapt_lobell)) %>% # Calculate mean, min and max effect
  distinct(Doi,Crop,Adapt_type,Mean,Max,Min,Group) %>% ungroup() %>% arrange(Group,Crop)

# Plot --------------------------------------------------------------------

# Calculate Y coordinates for each point
adapt <- adapt %>% group_by(Group) %>% mutate(y=Group+(row_number()-1)/n()/2)

ggplot()+
  geom_errorbar(data=adapt,aes(y=-y,xmin=Min,xmax=pmin(Max,1)))+
  geom_point(data = adapt, aes(y=-y,x=Mean,shape=Crop),size=2.5,fill="#e0e0e0")+
  labs(title=element_blank(), x ="Adaptation effect as defined by Lobell (2014), \nexpressed in % of baseline yield without adaptation",y =element_blank())+
  theme_minimal() +
  scale_y_continuous(labels = legend,breaks=-seq(1,11)) +
  scale_x_continuous(labels = c("-100%","-50%","0%","+50%","+100%"),breaks=seq(-1,1,0.5)) +
  scale_shape_manual(values=c(21,22,23,16),name="Crop species")+
  theme(panel.grid.minor.x = element_blank(),panel.grid.major.y = element_blank())

# Export
ggsave(filename = "../Output/Adaptation/FigureS12_Adaptation_effect_Lobell.jpg",width = 6,height = 5)
