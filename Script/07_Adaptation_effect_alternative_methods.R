# Purpose: Quantify the effect of different adaptation strategies on crop yield.
# Test alternative methods to aggregate results
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

# Count the number of papers included in the figure
adpt %>% count(n_distinct(Doi))

# Graph parameters --------------------------------------------------------

brks <- c(-20,-0.05,0,0.05, 0.1, 0.25, 0.5, 1, 20)
scale_x <- seq(-4,10,2)

plot_graph <- function(x,t){
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
    labs(title=t, x= ifelse(t %in% c("a)","b)"),"","Number of papers"), y = element_blank(),fill="Effect of \nadaptation")+
    geom_vline(data=x, aes(xintercept = 0),color="black",linetype="dashed")+
    theme_bw()+
    theme(text = element_text(size = 50))+
    facet_grid(Adapt_family ~ Crop,labeller = labeller(Crop = labels),scales="free_y")+
    if(t %in% c("b)","d)")){theme(axis.text.y=element_blank())} else {theme(legend.position="none")}
  return(p)
}


# Other methods for aggregating results -----------------------------------

# a) unweighted results (one paper with 20 combinations of time period x climate scenario x crop model x GCM x location x replicate counts for 20)

no_weight <- adpt %>%
  mutate(Class = cut(Adapt_impact, breaks = brks,include.lowest = T, right = F)) %>% mutate(Group = ifelse(Class %in%c("[-20,-0.05)","[-0.05,0)"),"Negative","Positive")) %>% add_count(Adapt_type,Class,Crop,name="Nb") %>% distinct(Crop,Adapt_type,Class,Group,Nb)

scale_x <- seq(-25,200,25)
p1 <- plot_graph(no_weight,"a)")

# b) weighted results (one paper with 20 combinations of time period x climate scenario x crop model x GCM x location x replicate counts for 1, each combination being given a weight of 1/20)

equal_weight <- adpt %>%
  mutate(Class = cut(Adapt_impact, breaks = brks,include.lowest = T, right = F)) %>%
  add_count(Doi,Crop,Adapt_type,name="Weight") %>% mutate(Weight=1/Weight) %>%
  mutate(Group = ifelse(Class %in%c("[-20,-0.05)","[-0.05,0)"),"Negative","Positive")) %>%
  group_by(Adapt_type,Class,Crop) %>% mutate(Nb=sum(Weight)) %>% distinct(Crop,Adapt_type,Class,Group,Nb)

scale_x <- seq(-4,10,2)
p2 <- plot_graph(equal_weight,"b)")

# c) results averaged on GCM, location and replicate, unweighted (one paper with 3 combinations of time period x climate scenario x crop model counts for 3)

mean2_unw <- adpt %>% group_by(Doi,Crop,Adapt_type,Future_midpoint,Scenario,Model_name) %>%
  mutate(Adapt_impact=mean(Adapt_impact)) %>% distinct(Doi,Crop,Adapt_impact,.keep_all = TRUE) %>%
  mutate(Class = cut(Adapt_impact, breaks = brks,include.lowest = T, right = F)) %>%
  mutate(Group = ifelse(Class %in%c("[-20,-0.05)","[-0.05,0)"),"Negative","Positive")) %>%
  ungroup() %>% add_count(Adapt_type,Class,Crop,name="Nb") %>%
  distinct(Crop,Adapt_type,Class,Group,Nb)

scale_x <- seq(-15,35,5)
p3 <- plot_graph(mean2_unw,"c)")

# d) results averaged on GCM, location and replicate, weighted (one paper with 3 combinations of time period x climate scenario x crop model counts for 1, each combination being given a weight of 1/3)

mean2_w <- adpt %>% group_by(Doi,Crop,Adapt_type,Future_midpoint,Scenario,Model_name) %>%
  mutate(Adapt_impact=mean(Adapt_impact)) %>% distinct(Doi,Crop,Adapt_impact,.keep_all = TRUE) %>%
  ungroup() %>% add_count(Doi,Crop,Adapt_type,name="Weight") %>% mutate(Weight=1/Weight) %>%
  mutate(Class = cut(Adapt_impact, breaks = brks,include.lowest = T, right = F)) %>%
  mutate(Group = ifelse(Class %in%c("[-20,-0.05)","[-0.05,0)"),"Negative","Positive")) %>%
  group_by(Adapt_type,Class,Crop) %>% mutate(Nb=sum(Weight)) %>% distinct(Crop,Adapt_type,Class,Group,Nb)

scale_x <- seq(-4,10,2)
p4 <- plot_graph(mean2_w,"d)")

# Export
jpeg("../Output/Adaptation/FigureS1_Adaptation_effect_alternative_methods.jpg",width=3000,height=3000)
grid.arrange(p1, p2, p3, p4, ncol=2)
dev.off()