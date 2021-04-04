
library(dplyr)
library(fmsb) #Rothman KJ (2012) Epidemiology: An Introduction. 2nd Ed., Oxford University Press, Oxford.: http://minato.sip21c.org/msb/ 

# option + command + c > copy file path to clipboard
data <-  read.csv(file = '/Users/safa/Documents/Safa/UBC/RA/thesis/Datasets/ECLIPSE_MOHSEN.csv')

data_year1 <- (data %>% filter(YRCAT == "Year 1" & OBSTIME > 360 )) # 212 patients had OBSTIME <= 360 in year 1
data_year2 <- (data %>% filter(YRCAT == "Year 2" & OBSTIME > 360 )) # 153 patients had OBSTIME <= 360 in year 2
joined_data <- inner_join(data_year1, data_year2, by = (c("ID" = "ID")), suffix = c(".y1", ".y2"))

#-----------------------------------------
# condition : probability of >= 1 mod/sev in year 1
year1_1exac <- joined_data %>% filter(obsExac.y1 >= 1)

#probability of exacerbation in year 2 | condition
prob_condition1 <- nrow( joined_data %>% filter(ID %in% year1_1exac$ID & obsExac.y2 > 0) ) / nrow(year1_1exac)
prob_severe1 <- nrow( joined_data %>% filter(ID %in% year1_1exac$ID & obsExac_severe.y2 > 0) ) / nrow(year1_1exac)

RR_exac_1hist <- prob_condition1 
cat(">=1 mod/sev: ", RR_exac_1hist, " ,")
#2.039587

model_1exac <- glm(year1_1exac$obsExac.y2 ~ year1_1exac$obsExac.y1, family = "poisson")

summary(model_1exac)

#-----------------------------------------
year1_0exac <- joined_data %>% filter(obsExac.y1 == 0)
 
#probability of exacerbation in year 2 | 0 exac in year 1
prob_condition0 <- nrow( joined_data %>% filter(ID %in% year1_0exac$ID & obsExac.y2 > 0) ) / nrow(year1_0exac)
prob_severe0 <- nrow( joined_data %>% filter(ID %in% year1_0exac$ID & obsExac_severe.y2 > 0) ) / nrow(year1_0exac)

RR_exac_0hist <- prob_condition0 
cat("0 mod/sev: ", RR_exac_0hist, " ,")
#0.4669951

model_0exac <- glm(year1_0exac$obsExac.y2 ~ year1_0exac$obsExac.y1, family = "poisson")

summary(model_0exac)
#-----------------------------------------
# condition : probability of >= 2 mod OR >= 1sev in year 1
year1_2mod1sev <- joined_data %>% filter(obsExac_moderate.y1 >=2 | obsExac_severe.y1 >=1)

#probability of exacerbation in year 2 | condition
prob_condition3 <- nrow( joined_data %>% filter(ID %in% year1_2mod1sev$ID & obsExac.y2 > 0) ) / nrow(year1_2mod1sev)
prob_severe3 <- nrow( joined_data %>% filter(ID %in% year1_2mod1sev$ID & obsExac_severe.y2 > 0) ) / nrow(year1_2mod1sev)

RR_exac_3hist <- prob_condition3 
cat(">=2 mod or >= 1sev: ", RR_exac_3hist, " ,")
#1.927891

model_3exac <- glm(year1_2mod1sev$obsExac.y2 ~ year1_2mod1sev$obsExac.y1, family = "poisson")

summary(model_3exac)
#-----------------------------------------
# condition : probability of >= 2 mod/sev in year 1
year1_2exac <- joined_data %>% filter(obsExac.y1 >=2 )

#probability of exacerbation in year 2 | condition
prob_condition2 <- nrow( joined_data %>% filter(ID %in% year1_2exac$ID & obsExac.y2 > 0) ) / nrow(year1_2exac)
prob_severe2 <- nrow( joined_data %>% filter(ID %in% year1_2exac$ID & obsExac_severe.y2 > 0) ) / nrow(year1_2exac)

RR_exac_2hist <- prob_condition2 
cat(">=2 mod/sev: ", RR_exac_2hist, " ,")
#2.334975
model_2exac <- glm(year1_2exac$obsExac.y2 ~ year1_2exac$obsExac.y1, family = "poisson")

summary(model_2exac)

#-----------------------------------------

print(RR_exac_0hist/RR_exac_1hist)
print(RR_exac_3hist/RR_exac_1hist)
print(RR_exac_2hist/RR_exac_1hist)

mean(year1_0exac$obsExac.y2)/mean(year1_1exac$obsExac.y2)
mean(year1_2mod1sev$obsExac.y2)/mean(year1_1exac$obsExac.y2)
mean(year1_2exac$obsExac.y2)/mean(year1_1exac$obsExac.y2)

rateratio(sum(year1_0exac$obsExac.y2), sum(year1_1exac$obsExac.y2),nrow(year1_0exac) , nrow(year1_1exac), conf.level=0.95) 
# 0.16 ( 0.14-0.17)
rateratio(sum(year1_2mod1sev$obsExac.y2), sum(year1_1exac$obsExac.y2),nrow(year1_2mod1sev) , nrow(year1_1exac), conf.level=0.95) 
# 1.25 ( 1.16-1.33)
rateratio(sum(year1_2exac$obsExac.y2), sum(year1_1exac$obsExac.y2),nrow(year1_2exac) , nrow(year1_1exac), conf.level=0.95) 
# 1.3 (1.2 - 1.4)

# rateratio(a,b,PT1, PT0, conf.level):
# a	
# The number of disease occurence among exposed cohort.
# b	
# The number of disease occurence among non-exposed cohort.
# PT1	
# The observed person-time of the exposed cohort.
# PT0	
# The observed person-time of the unexposed cohort.
# conf.level	
# Probability for confidence intervals. Default is 0.95.









