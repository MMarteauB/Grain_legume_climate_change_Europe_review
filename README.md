Supplementary data to:

Grain legume response to future climate and adaptation strategies in Europe: a review of simulation studies
=======

# General information

This folder contains all data and R scripts needed to reproduce the figures included in the article. A brief content description is available below.

# Content description

## Input

CroplandArea2000_Geotiff: Folder containing the dataset by Ramankutty et al. (2008) displaying croplands and pastures circa 2000. Downloaded in August 2023 from http://www.earthstat.org/cropland-pasture-area-2000/

Eurostat: Folder containing data for soybean yield, area harvested, and crop production aggregated at a national scale (2010-2021) and NUTS3 scale (2014-2023). Downloaded in June 2023 from https://www.fao.org/faostat/en/#data/QCL and https://ec.europa.eu/eurostat/databrowser/view/APRO_CPSHR/default/table

Map-KG_Global: Folder containing the Köppen-Geiger climate classification (1986-2010) re-analyzed by Kottek et al. (2006) and Rubel et al. (2017). Downloaded in May 2023 from https://koeppen-geiger.vu-wien.ac.at/Rcode/Map_KG-Global.zip

Data_review.csv: CSV file containing all the data collected from the selected papers

Data_review_description.pdf: PDF file describing the data contained in the CSV file


## Script

01_Nb_papers_per_crop_species.R: R script used to count the number of papers per corpus and crop species. The output of this script is available in Output/Corpus_description.

02_Corpus_description_with_total.R: R script used to count the number of papers per spatial scale of the analysis, type of model used, climate change scenario, biotic and abiotic parameters considered, and indicators assessed. The output of this script is available in Output/Corpus_description.

03_Time_periods_studied.R: R script used to count the number of papers per time period studied. The output of this script is available in Output/Corpus_description.

04_Koppen_Geiger_classification_study_area.R: R script used to create the map of the study area and show the geographical distribution of the papers selected in the "adaptation" corpus. The output of this script is available in Output/Koppen_Geiger_classification.

04_Koppen_Geiger_classification_study_area_draw_figure.R: R script used to lay out the outputs of the previous script and combine them into a single figure. The output of this script is available in Output/Koppen_Geiger_classification.

05_Adaptation_options_studied.R: R script used to count the number of papers per crop species and adaptation strategy. The output of this script is available in Output/Corpus_description.

06_CC_impacts_soybean.R: R script used to map the number of papers per geographical area and the simulated impact of climate change on soybean performances. The output of this script is available in Output/CC_impact.

06_CC_impacts_soybean_draw_figures.R: R script used to lay out the outputs of the previous script and combine them into a single figure. The output of this script is available in Output/CC_impact.

07_Adaptation_effect.R: R script used to summarize the average effect of different adaptation strategies on crop yields. The output of this script is available in Output/Adaptation.

07_Adaptation_effect_alternative_methods.R: R script used to summarize the effect of different adaptation strategies on crop yields. Several methods for averaging and aggregating results are tested. The output of this script is available in Output/Adaptation.

08_Adaptation_effect_Lobell.R: R script used to summarize the effect of different adaptation strategies on crop yields, using Lobell's (2014) definition of adaptation. The output of this script is available in Output/Adaptation.

09_Soybean_prod_Eurostat.R: R script used to map soybean yield and production in Europe between 2019 and 2021. The output of this script is available in Output/Soybean_production_Eurostat.