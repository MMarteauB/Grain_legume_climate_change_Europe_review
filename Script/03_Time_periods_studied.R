# Purpose: Plot the number of papers per time period studied 
# Author: M. Marteau-Bazouni
# Date: 18/07/2023

# Packages
library(dplyr);library(ggplot2);library(ggh4x)

# Input data
data <- read.csv2("../Input/Data_review.csv",fileEncoding="latin1",dec=',')

# Extracting time slices studied
impact <- data %>% filter(Cat!="Adapt") %>% mutate(Cat="Impact")
adapt <- data %>% filter(Cat!="Impact") %>% mutate(Cat="Adaptation")
time <- rbind(impact,adapt) %>% mutate(Crop=case_match(Crop,"Soybean"~"Soybean",.default="Others")) %>%
  distinct(Doi,Cat,Crop,Future_time_slice1,Future_time_slice2) %>% rowwise() %>%
  mutate(Full_slice=list(c(Future_time_slice1:Future_time_slice2)))

unlist_time <- function(x,cat,crop) {
  y <- x %>% filter(Crop==crop & Cat==cat)
  t <- data.frame(Cat=cat,Crop=crop,Year = unlist(y$Full_slice))
  return(t)
}

time_table <- rbind(unlist_time(time,"Impact","Soybean"),unlist_time(time,"Adaptation","Soybean"),unlist_time(time,"Impact","Others"),unlist_time(time,"Adaptation","Others"))

# Plot
time_table %>% filter(Year<=2100 & Year >= 2020) %>%
  ggplot(aes(x=Year,fill=Crop))+
  geom_bar(stat="bin",position = "dodge",binwidth=1)+
  labs(title=element_blank(), y ="Number of papers",x ="Time period studied",fill=NULL)+
  theme_bw()+
  theme(axis.text.x=element_text(hjust = 0.5,size=9),legend.position = c(0.37,0.87),text=element_text(size=13, family = "Helvetica"))+
  scale_x_continuous(breaks = seq(2020, 2110, by = 10)) +
  scale_y_continuous(breaks = seq(0,28,by=2)) +
  scale_fill_manual(breaks=c("Soybean","Others"),labels=c("Soybean","Other grain leg."),values=c('#80cdc1',"#8c510a")) +
  facet_grid2(. ~ factor(Cat,levels=c("Impact","Adaptation"),labels=c("a) 'Impact' corpus","b) 'Adaptation' corpus")),axes="all")

# Output
ggsave(filename = "../Output/Corpus_description/Figure2_Time_periods_studied.jpg",width=8,height=4)
