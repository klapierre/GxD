library(tidyverse)

setwd('C:\\Users\\komatsuk\\Dropbox (Smithsonian)\\USDA_drought\\USDA_DxG_EEA_data') #desktop


###read in data
soilMoisture <- read.csv('Komatsu_USDA_DxG_EEA_soilmoisture_2019data.csv')

absorbance <- read_excel('Komatsu_USDA_DxG_EEA_absorbance_2019data.xlsx')
fluorescence <- read_excel('Komatsu_USDA_DxG_EEA_fluorescence_2019data.xlsx')

