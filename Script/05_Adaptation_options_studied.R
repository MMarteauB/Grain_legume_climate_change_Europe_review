# Purpose: Plot the number of papers per crop and adaptation strategy studied
# Author: M. Marteau-Bazouni
# Date: 18/07/2023

# Packages
library(dplyr);library(tidyr);library(ggplot2);library(ggforce);
library(magick);library(plotrix)

# Input data
data <- read.csv2("../Input/Data_review.csv",fileEncoding="latin1",dec=',')

# Select only data from the "adaptation" corpus
adapt <- data %>% filter(Cat!="Impact") %>% separate_longer_delim(Adapt_tested,";")

# Regroup adaptation options
adapt$Adapt_tested <- case_match(adapt$Adapt_tested,"Cultivar"~"Cultivar choice",
                                 "Virtual cultivar"~"Virtual genotype",
                             c("Residue incorporation","Residue management","Fertilization")~"Residue management \n or fertilization",
                             c("Sowing date x Virtual cultivar","Sowing date x Cultivar")~"Combination of crop \n management options",
                             c("Residue incorporation x fertilization","Virtual cultivar x Reduced tillage","Fertilization x Irrigation")~"Combination of soil and \n water management options",
                             c("Irrigation x Sowing date x Cultivar","Virtual cultivar x Reduced tillage","Irrigation x Cultivar")~"Others",
                             .default=adapt$Adapt_tested)
adapt$Continent <- case_match(adapt$Continent,"Europe"~"Europe",.default="Others")

# Count papers par crop, location and adaptation tested
adapt <- adapt %>%
  group_by(Crop,Continent,Adapt_tested) %>%
  mutate(Nb=n_distinct(Doi)) %>%
  distinct(Crop,Continent,Adapt_tested,Nb) %>% 
  spread(Continent,Nb,fill=0) %>% # Add a total
  mutate(Total=Others+Europe) %>% dplyr::select(-Others) %>%
  gather(Continent,Nb,Europe,Total)

# Arrange results
adapt$Continent <- case_match(adapt$Continent,"Europe"~"2",.default="1") # Draw "total" circle behind "Europe" circle

adapt$Type <- case_match(adapt$Adapt_tested,c("Cultivar choice","Virtual genotype","Sowing date","Sowing density","Double cropping")~1, "Combination of crop \n management options" ~ 2,"Combination of soil and \n water management options" ~ 4,"Others" ~5,.default=3)
adapt <- adapt %>% ungroup() %>% add_row(Crop="Soybean",Continent="1",Type=2,Nb=0,Adapt_tested="") %>% # A blank row is added to harmonize plot scales.
  arrange(Type,desc(Nb),desc(Crop)) %>% # number of papers arranged by descending order.
  add_row(Crop="Soybean",Continent="1",Type=0,Nb=c(0,1,2,5,10,15,20),Adapt_tested=c("0","1","2","5","10","15","20")) # Legend

# Calculate circle radius and coordinates
adapt$r <- sqrt(adapt$Nb / pi)

xlab <- unique(adapt$Crop)
xmax <- length(xlab) ; xval <- c(1:xmax)*max(adapt$r)*2.5
adapt$x0 <- xval[match(adapt$Crop, xlab)]+adapt$r

ylab <- unique(adapt$Adapt_tested)
ymax <- length(ylab) ; yval <-rev(c(1:ymax)*max(adapt$r)*2)
adapt$y0 <- yval[match(adapt$Adapt_tested, ylab)]


# Plot
size = 16
adapt$Type <- case_match(adapt$Type, c(1:2) ~ "Crop management",0~" Legend",.default="Water and soil management")

ggplot(adapt) +
  geom_circle(aes(x0 = x0, y0 = y0, r = r, fill = Continent))+
  theme_bw()+
  theme(aspect.ratio = 1,axis.text.x=element_text(angle=45,hjust=1),text=element_text(size=size, family = "Helvetica"))+
  scale_x_continuous(name="Crop species", breaks=xval, labels=xlab)+
  scale_y_continuous(name="Adaptation strategy", breaks=yval, labels=ylab)+
  scale_fill_manual(breaks=c("1","2"),values = c("#bababa","#4d4d4d"), labels = c("Total number \n of studies","Studies \n in Europe"),"")+
  labs(title=element_blank())+
  facet_grid(Type~.,scale="free_y")

# Output
ggsave(filename = "../Output/Corpus_description/Figure7_Adaptation_stategies_studied.jpg", width=9, height=9)


# Add legend
image <- image_crop(image_read("../Output/Corpus_description/Figure7_Adaptation_stategies_studied.jpg"),"2150x1900+250+820")
legend <- image_crop(image_read("../Output/Corpus_description/Figure7_Adaptation_stategies_studied.jpg"),"300x640+900+160")

image <- image_draw(image)
rect(0,0,120,1900, col="white",border=NA)
text(75, 900, "Adaptation strategy", family = "Helvetica", cex = 6,adj=0.5,srt = 90)
rect(1700,0,2150,1900, col="white",border=NA)
draw.circle(1730,1150,40,col="#bababa",border="black")
draw.circle(1730,1300,40,col="#4d4d4d",border="black")
text(1800,c(1150,1300), c("Total number \nof papers","In Europe"), family = "Helvetica", cex = 4.5,adj=0)
text(1900,200, c("Number \nof papers"), family = "Helvetica", cex = 5,adj=0.5)
dev.off()

result <- image_composite(image,legend, offset = "+1720+315")
image_write(result,path="../Output/Corpus_description/Figure7_Adaptation_stategies_studied.jpg")

# Simplified table (for main text)
table <- adapt %>% filter(Continent=="1") %>%
  group_by(Adapt_tested) %>% mutate(Total = round(sum(Nb)/53,2)) %>%
  distinct(Adapt_tested,Total)
