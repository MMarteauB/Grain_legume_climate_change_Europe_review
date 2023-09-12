# Purpose: Count the number of papers per crop species
# Author: M. Marteau-Bazouni
# Date: 18/07/2023


# Packages
library(dplyr);library(tidyr);library(stringr);library(kableExtra);library(extrafont);library(formattable)

# Input data
input <- read.csv2("../Input/Data_review.csv",fileEncoding="latin1",dec=',')

# Duplicate papers belonging to both "impact" and "adaptation" corpus
data <- input %>% separate_longer_delim(Cat,";")

# Count the number of papers depending on the crop species
c <- data %>% group_by(Cat,Crop) %>% mutate(Nb=n_distinct(Doi)) %>% ungroup() %>%
  group_by(Cat) %>% mutate(Total=n_distinct(Doi)) %>% # Add a "total" column
  distinct(Cat,Crop,Nb,Total) %>% mutate(Irrigation="") %>% ungroup()

# Count the number of papers depending on the crop species and irrigation
irri <- data %>% group_by(Cat,Crop,Irrigation) %>% mutate(Nb=n_distinct(Doi)) %>% ungroup() %>%
  group_by(Cat,Irrigation) %>% mutate(Total=n_distinct(Doi)) %>% # Add a "total" column
  distinct(Cat,Crop,Nb,Total,Irrigation) %>% ungroup()

# Merge tables
table <- rbind(c,irri) %>% spread(Crop,Nb,fill=0) %>%
  filter(Cat=="Impact" | Irrigation=="") %>% # Display only information on irrigation for the "impact" corpus
  mutate(Cat=ifelse(Irrigation=="",Cat,""))

# Order column names
table <- table[c(1,3,11,4,10,7,5,6,8,9,2)]

# Order row names
table$Irrigation <- factor(table$Irrigation,levels=c("","Rainfed","Irrigated","Undifferentiated","Unclear"))
table$Cat <- factor(table$Cat,levels=c("Impact","","Adapt"),labels=c("'Impact' corpus","", "'Adaptation' corpus"))
table <- table %>% arrange(Cat,Irrigation)

# Display
size = 14
font = "Helvetica"
table %>%
  kbl(escape = F,caption = "Number of papers per crop species:",
      col.names = c("","","Soybean","Bean","Pea","Faba bean","Chickpea","Cow pea","Lentil","Lupin","Total"),
      align=c('l','l',rep('c',9))) %>%
  kable_styling(bootstrap_options = c("condensed"), full_width = F, position = "center", html_font = font, font_size = size) %>%
  pack_rows("of which:", 2, 5) %>%
  row_spec(2:5, italic = T) %>%
  cat(., file = "../Output/Corpus_description/Table1_Nb_papers_per_crop_species.html")
