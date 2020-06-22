library(readxl)
library(tidyverse)

setwd('C:\\Users\\komatsuk\\Dropbox (Smithsonian)\\USDA_drought\\USDA_DxG_EEA_data') #desktop
setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\USDA_drought\\USDA_DxG_EEA_data') #laptop

'%notin%' <- negate('%in%')

###bar graph summary statistics function
#barGraphStats(data=, variable="", byFactorNames=c(""))
barGraphStats <- function(data, variable, byFactorNames) {
  count <- length(byFactorNames)
  N <- aggregate(data[[variable]], data[byFactorNames], FUN=length)
  names(N)[1:count] <- byFactorNames
  names(N) <- sub("^x$", "N", names(N))
  mean <- aggregate(data[[variable]], data[byFactorNames], FUN=mean)
  names(mean)[1:count] <- byFactorNames
  names(mean) <- sub("^x$", "mean", names(mean))
  sd <- aggregate(data[[variable]], data[byFactorNames], FUN=sd)
  names(sd)[1:count] <- byFactorNames
  names(sd) <- sub("^x$", "sd", names(sd))
  preSummaryStats <- merge(N, mean, by=byFactorNames)
  finalSummaryStats <- merge(preSummaryStats, sd, by=byFactorNames)
  finalSummaryStats$se <- finalSummaryStats$sd / sqrt(finalSummaryStats$N)
  return(finalSummaryStats)
}  

###read in data
plate <- read_excel('06212020\\Komatsu_USDA_DxG_EEA_plate setup.xlsx')

#need to fix to have dry weight conversions
soilWeight <- plate%>%
  mutate(plate_position2=paste('soil_weight',plate_position, sep='_'))%>%
  select(processing_date, plate_num, plate_position2, EEA_weight)%>%
  spread(key=plate_position2, value=EEA_weight)

fluorescence <- read_excel('06212020\\Komatsu_USDA_DxG_EEA_fluorescence data.xlsx')%>%
  left_join(read_excel('06212020\\Komatsu_USDA_DxG_EEA_plate time.xlsx'))%>%
  left_join(soilWeight)

ggplot(data=subset(fluorescence, !is.na(standard_control)), aes(x=as.numeric(standard_control)/100000)) + geom_histogram() + facet_wrap(~enzyme)

redosHigh <- fluorescence%>%
  filter(standard_control!='#SAT'&!is.na(standard_control))%>%
  mutate(standard_control2=as.numeric(as.character(standard_control)))%>%
  filter(standard_control2>400000000)%>%
  group_by(processing_date, plate_num)%>%
  summarise(redo=length(standard_control))%>%
  ungroup()%>%
  left_join(plate)

redosLow <- fluorescence%>%
  filter(standard_control!='#SAT'&!is.na(standard_control))%>%
  mutate(standard_control2=as.numeric(as.character(standard_control)))%>%
  filter(standard_control2<1000000)%>%
  group_by(processing_date, plate_num)%>%
  summarise(redo=length(standard_control))%>%
  ungroup()%>%
  left_join(plate)



###processing notes: 4/19 used too high a concentration of MUB, need to divide MUB standard wells by 100

#quick look --- needs to change soil weight data once it is dried and weighed
fluorescenceActivity <- fluorescence%>%
  filter(!is.na(standard_control)&standard_control!='#SAT',
         !is.na(quench_control_1)&quench_control_1!='#SAT',
         !is.na(quench_control_2)&quench_control_2!='#SAT',
         !is.na(quench_control_3)&quench_control_3!='#SAT')%>%
  mutate(standard_control=as.numeric(as.character(standard_control)),
         quench_control_1=as.numeric(as.character(quench_control_1)),
         quench_control_2=as.numeric(as.character(quench_control_2)),
         quench_control_3=as.numeric(as.character(quench_control_3)))%>%
  # filter(standard_control<400000000)%>%
  mutate(#activity 1
         quench1=((quench_control_1-soil_control_1)/standard_control), 
         emmission1=(standard_control/0.5),
         net_fluor1=(((soil_assay_1-soil_control_1)/quench1)-substrate_control),
         activity1=((net_fluor1*125)/(emmission1*0.2*time_hr*soil_weight_1)),
         #activity 2
         quench2=((quench_control_2-soil_control_2)/standard_control), 
         emmission2=(standard_control/0.5),
         net_fluor2=(((soil_assay_2-soil_control_2)/quench2)-substrate_control),
         activity2=((net_fluor2*125)/(emmission2*0.2*time_hr*soil_weight_2)),
         #activity 3
         quench3=((quench_control_3-soil_control_3)/standard_control), 
         emmission3=(standard_control/0.5),
         net_fluor3=(((soil_assay_3-soil_control_3)/quench3)-substrate_control),
         activity3=((net_fluor3*125)/(emmission3*0.2*time_hr*soil_weight_3)))%>%
  group_by(processing_date, enzyme, plate_num)%>%
  summarise(activity1=mean(activity1), activity2=mean(activity2), activity3=mean(activity3))%>%
  ungroup()%>%
  gather(key='position', value='activity', activity1:activity3)%>%
  mutate(date=as.Date(processing_date))



ggplot(data=subset(fluorescenceActivity), aes(x=activity)) +
  geom_histogram() +
  facet_wrap(~enzyme)

ggplot(data=barGraphStats(data=subset(fluorescenceActivity, activity<5000&activity>0), variable="activity", byFactorNames=c("date","enzyme")), aes(x=date, y=mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se)) +
  scale_x_date(date_breaks = "1 week", date_labels = "%W") +
  facet_wrap(~enzyme)

data=barGraphStats(data=subset(fluorescenceActivity, activity<658&activity>0), variable="activity", byFactorNames=c("date","enzyme"))




#identify outliers across all plates and within plates


#data exploration
ggplot(data=fluorescenceActivity, aes(x=enzyme, y=activity, fill=site)) +
  geom_boxplot()










#old code to transform data
# # soilMoisture <- read.csv('Komatsu_USDA_DxG_EEA_soilmoisture_2019data.csv')
# 
# 
# envelope <- read.csv('Komatsu_USDA_DxG_EEA_envelope.csv')
# 
# absorbanceSoils <- read_excel('Komatsu_USDA_DxG_EEA_absorbance.xlsx')%>%
#   select(processing_date, site, sample_year, sample_month, plot, wet_weight, EEA_weight, plate_no_pos)%>%
#   separate(plate_no_pos, c('slurry','plate_num','plate_position'), sep='-')%>%
#   unique()%>%
#   left_join(envelope)
# 
# write.csv(absorbanceSoils, 'Komatsu_USDA_DxG_EEA_plate data.csv')
# 
# 
# fluorescenceTimes <- read_excel('Komatsu_USDA_DxG_EEA_fluorescence.xlsx')%>%
#   select(processing_date, plate_no_pos, enzyme, start_time, end_time, assay_time, time_hr)%>%
#   separate(plate_no_pos, c('slurry','plate_num','plate_position'), sep='-')%>%
#   unique()
# 
# absorbanceTimes <- read_excel('Komatsu_USDA_DxG_EEA_absorbance.xlsx')%>%
#   select(processing_date, plate_no_pos, enzyme, start_time, end_time, assay_time, time_hr)%>%
#   separate(plate_no_pos, c('slurry','plate_num','plate_position'), sep='-')%>%
#   unique()%>%
#   rbind(fluorescenceTimes)
# 
# write.csv(absorbanceTimes, 'Komatsu_USDA_DxG_EEA_plate time.csv')
# 
# 
# 
# absorbanceCtl <- as.data.frame(read_excel('Komatsu_USDA_DxG_EEA_absorbance.xlsx'))%>%
#   select(processing_date, plate_no_pos, enzyme, well, blank, substrate_control1, substrate_control2)%>%
#   separate(plate_no_pos, c('slurry','plate_num','plate_position'), sep='-')%>%
#   filter(!is.na(enzyme))%>%
#   select(-slurry, -plate_position)%>%
#   unique()
# 
# absorbance1 <- as.data.frame(read_excel('Komatsu_USDA_DxG_EEA_absorbance.xlsx'))%>%
#   filter(!is.na(enzyme))%>%
#   select(processing_date, plate_no_pos, enzyme, well, soil_control)%>%
#   separate(plate_no_pos, c('slurry','plate_num','plate_position'), sep='-')%>%
#   select(-slurry)%>%
#   spread(key=plate_position, value=soil_control)%>%
#   rename(soil_control_1=5, soil_control_2=6, soil_control_3=7)
# 
# absorbance2 <- as.data.frame(read_excel('Komatsu_USDA_DxG_EEA_absorbance.xlsx'))%>%
#   filter(!is.na(enzyme))%>%
#   select(processing_date, plate_no_pos, enzyme, well, soil_assay1)%>%
#   separate(plate_no_pos, c('slurry','plate_num','plate_position'), sep='-')%>%
#   select(-slurry)%>%
#   spread(key=plate_position, value=soil_assay1)%>%
#   rename(soil_assay_a_1=5, soil_assay_a_2=6, soil_assay_a_3=7)
# 
# absorbance3 <- as.data.frame(read_excel('Komatsu_USDA_DxG_EEA_absorbance.xlsx'))%>%
#   filter(!is.na(enzyme))%>%
#   select(processing_date, plate_no_pos, enzyme, well, soil_assay2)%>%
#   separate(plate_no_pos, c('slurry','plate_num','plate_position'), sep='-')%>%
#   select(-slurry)%>%
#   spread(key=plate_position, value=soil_assay2)%>%
#   rename(soil_assay_b_1=5, soil_assay_b_2=6, soil_assay_b_3=7)%>%
#   cbind(absorbanceCtl, absorbance1, absorbance2)
# 
# write.csv(absorbance3, 'Komatsu_USDA_DxG_EEA_absorbance data.csv')
# 
# 
# 
# 
# 
# 
# 
# 
# 
# fluCtl <- as.data.frame(read_excel('Komatsu_USDA_DxG_EEA_fluorescence.xlsx'))%>%
#   select(processing_date, plate_no_pos, enzyme, well, blank, standard_control, substrate_control)%>%
#   separate(plate_no_pos, c('slurry','plate_num','plate_position'), sep='-')%>%
#   filter(!is.na(enzyme))%>%
#   select(-slurry, -plate_position)%>%
#   unique()
# 
# flu1 <- as.data.frame(read_excel('Komatsu_USDA_DxG_EEA_fluorescence.xlsx'))%>%
#   filter(!is.na(enzyme))%>%
#   select(processing_date, plate_no_pos, enzyme, well, soil_control)%>%
#   separate(plate_no_pos, c('slurry','plate_num','plate_position'), sep='-')%>%
#   select(-slurry)%>%
#   spread(key=plate_position, value=soil_control)%>%
#   rename(soil_control_1=5, soil_control_2=6, soil_control_3=7)
# 
# flu2 <- as.data.frame(read_excel('Komatsu_USDA_DxG_EEA_fluorescence.xlsx'))%>%
#   filter(!is.na(enzyme))%>%
#   select(processing_date, plate_no_pos, enzyme, well, quench_control)%>%
#   separate(plate_no_pos, c('slurry','plate_num','plate_position'), sep='-')%>%
#   select(-slurry)%>%
#   spread(key=plate_position, value=quench_control)%>%
#   rename(quench_control_1=5, quench_control_2=6, quench_control_3=7)
# 
# flu3 <- as.data.frame(read_excel('Komatsu_USDA_DxG_EEA_fluorescence.xlsx'))%>%
#   filter(!is.na(enzyme))%>%
#   select(processing_date, plate_no_pos, enzyme, well, soil_assay)%>%
#   separate(plate_no_pos, c('slurry','plate_num','plate_position'), sep='-')%>%
#   select(-slurry)%>%
#   spread(key=plate_position, value=soil_assay)%>%
#   rename(soil_assay_1=5, soil_assay_2=6, soil_assay_3=7)%>%
#   cbind(fluCtl, flu1, flu2)
# 
# write.csv(flu3, 'Komatsu_USDA_DxG_EEA_fluorescence data.csv')
# 
# 
# plateTime <- read.csv('Komatsu_USDA_DxG_EEA_plate time.csv')%>%
#   select(-slurry, -plate_position)%>%
#   unique()
# 
# write.csv(plateTime, 'Komatsu_USDA_DxG_EEA_plate time.csv')