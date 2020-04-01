library(readxl)
library(tidyverse)

setwd('C:\\Users\\komatsuk\\Dropbox (Smithsonian)\\USDA_drought\\USDA_DxG_EEA_data') #desktop
setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\USDA_drought\\USDA_DxG_EEA_data') #laptop


###read in data
soilMoisture <- read.csv('Komatsu_USDA_DxG_EEA_soilmoisture_2019data.csv')

absorbance <- read_excel('Komatsu_USDA_DxG_EEA_absorbance_2019data.xlsx')
fluorescence <- read_excel('Komatsu_USDA_DxG_EEA_fluorescence_2019data.xlsx')

#quick look --- needs to change soil weight data once it is dried and weighed
fluorescenceActivity <- fluorescence%>%
  filter(!is.na(standard_control))%>%
  mutate(quench=((quench_control-soil_control)/standard_control), emmission=(standard_control/0.5), activity=((soil_assay*125)/(emmission*0.2*time_hr*EEA_weight)))

#identify outliers across all plates and within plates

#data exploration
ggplot(data=fluorescenceActivity, aes(x=enzyme, y=activity, fill=site)) +
  geom_boxplot()

