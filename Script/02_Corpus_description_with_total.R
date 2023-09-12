# Purpose: Summarize study characteristics and count the number of papers per spatial scale, climate scenario, CC factors assessed, indicators investigated 
# Author: M. Marteau-Bazouni
# Date: 18/07/2023


# Packages
library(dplyr);library(tidyr);library(forcats);library(stringr);library(kableExtra);library(extrafont);library(formattable)

# Input data
input <- read.csv2("../Input/Data_review.csv",fileEncoding="latin1",dec=',')

# Split cell content
data <- input %>%
  separate_longer_delim(Cat,";") %>%
  separate_longer_delim(Scenario,";") %>%
  separate_longer_delim(Climate_factors,";") %>%
  separate_longer_delim(Model_cat,";") %>%
  separate_longer_delim(Param_assessed,";")

# Separate soybean from other grain legumes
data$Crop <- case_match(data$Crop,"Soybean"~"1_Soybean",.default="2_Others")

# Add a "total" group
total_group <- data %>% mutate(Crop="3_Total")
data <- rbind(data,total_group)

# Regroup variables
data$Param_assessed <- case_match(data$Param_assessed,
                                  c("WUE","IWUE","Water productivity","Water balance")~"Water balance and water efficiency",
                                  c("Yield variability","Risk of crop failure","Likelihood of yield loss hotspots")~"Yield variability or risk of crop failure",
                                  c("Biomass","Biological N Fixation")~"Biomass and biological N fixation",
                                  c("Economic indicators","Cost & benefits of adaptation","Gross margin","Global prices")~"Economic indicators",
                                  c("GHG emissions","N2O emissions")~"GHG emissions",
                                  c("Runoff","Sediment yield","SOC","Nutrient budget","N budget")~"SOC, nutrient balance and erosion",
                                  .default=data$Param_assessed)

data$Scenario <- case_match(data$Scenario,c("RCP2.6","SSP1-2.6")~"RCP2.6 / SSP1-2.6",
                            c("RCP4.5","SSP2-4.5")~"RCP4.5 / SSP2-4.5","RCP7.0"~"SSP3-7.0",
                            c("RCP8.5","SSP5-8.5")~"RCP8.5 / SSP5-8.5",.default=data$Scenario)

# Remove duplicates
data <- data %>% distinct(Doi,Cat,Crop,Irrigation,Scale,Climate_factors,Scenario,Param_assessed,Model_cat)

# Count the number of papers depending on a variable x
count_me <- function(data,x,legend) {
  if (missing(x)){
    y <- data %>%
      group_by(Cat,Crop) %>%
      mutate(x_name = legend, x = legend, Nb=n_distinct(Doi))
  }
  else{
    y <- data %>%
      group_by(Cat,Crop,{{x}}) %>%
      mutate(x_name = legend, x = {{x}}, Nb=n_distinct(Doi))
  }
  y <- y %>%
    ungroup() %>%
    distinct(Cat,Crop,x_name,x,Nb) %>%
    spread(Cat,Nb,fill=0) %>%
    pivot_wider(names_from = Crop, values_from = c(Impact,Adapt))
  y[is.na(y)]<-0
  return(y)
}

scale <- count_me(data,Scale,"Spatial_scale")
mod <- count_me(data,Model_cat,"Type_of_model")
sc <- count_me(data,Scenario,"Climate_scenario")
clim <- count_me(data,Climate_factors,"Climate_factors")
param <- count_me(data,Param_assessed,"Performance_indicators")
total <- count_me(data,legend="Total") # Add a "total" row

# Order tables
scale <- scale %>%
  mutate(x = factor(x,levels=c("World","Europe","Country","Region","Spot"),labels=c("Global scale with data for Europe","European scale","Country scale","Regional scale","Site-based studies"))) %>%
  arrange(x)

sc <- sc %>%
  mutate(x = factor(x,levels=c("A1B","A2","B1","RCP2.6 / SSP1-2.6","RCP4.5 / SSP2-4.5","RCP6.0","SSP3-7.0","RCP8.5 / SSP5-8.5","+1.5C World","+2C World"))) %>%
  arrange(x)

mod <- mod %>% arrange(desc(Impact_1_Soybean))
clim <- clim %>% filter(!grepl("tress",x)) %>% arrange(desc(Impact_1_Soybean))
param <- param %>% arrange(desc(Impact_3_Total),desc(Adapt_3_Total))

# Merge tables
table <- rbind(scale,mod,sc,clim,param,total)

# Display
size = 14
font = "Helvetica"
table %>% dplyr::select(-x_name) %>%
  kbl(escape = F,col.names = c(" ","Soybean","Others","Total","Soybean","Others","Total"),align=c('l','l',rep('c',6))) %>%
  kable_styling(bootstrap_options = c("condensed"), full_width = F, position = "center", html_font = font, font_size = size) %>%
  add_header_above(c(" " = 1, "Impact" = 3, "Adaptation" = 3)) %>%
  pack_rows(index = table(fct_inorder(table$x_name))) %>%
  cat(., file = "../Output/Corpus_description/Table2_Corpus_description_with_total.html")
