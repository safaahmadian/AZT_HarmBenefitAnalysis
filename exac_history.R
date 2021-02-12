
library(dplyr)

# option + command + c > copy file path to clipboard
data <-  read.csv(file = '/Users/safa/Documents/Safa/UBC/RA/thesis/Datasets/ECLIPSE_MOHSEN.csv')

data_year1 <- (data %>% filter(YRCAT == "Year 1" & OBSTIME > 360 )) # 212 patients had OBSTIME <= 360 in year 1
data_year2 <- (data %>% filter(YRCAT == "Year 2" & OBSTIME > 360 )) # 153 patients had OBSTIME <= 360 in year 2
joined_data <- inner_join(data_year1, data_year2, by = (c("ID" = "ID")), suffix = c(".y1", ".y2"))


#probability of exacerbation in year 2
prob_exac_year2 <- nrow(joined_data %>% filter(obsExac.y2 > 0)) / nrow(joined_data)

#-----------------------------------------
year1_0exac <- joined_data %>% filter(obsExac.y1 == 0)
 
#probability of exacerbation in year 2 | 0 exac in year 1
prob_condition0 <- nrow( joined_data %>% filter(ID %in% year1_0exac$ID & obsExac.y2 > 0) ) / nrow(year1_0exac)

RR_exac_0hist <- prob_condition0 / prob_exac_year2
cat("0 mod/sev: ", RR_exac_0hist, " ,")
#0.4669951

#-----------------------------------------
# condition : probability of >= 1 mod/sev in year 1
year1_1exac <- joined_data %>% filter(obsExac.y1 >= 1)

#probability of exacerbation in year 2 | condition
prob_condition1 <- nrow( joined_data %>% filter(ID %in% year1_1exac$ID & obsExac.y2 > 0) ) / nrow(year1_1exac)

RR_exac_1hist <- prob_condition1 / prob_exac_year2
cat(">=1 mod/sev: ", RR_exac_1hist, " ,")
#2.039587

#-----------------------------------------
# condition : probability of >= 2 mod OR >= 1sev in year 1
year1_2mod1sev <- joined_data %>% filter(obsExac_moderate.y1 >=2 | obsExac_severe.y1 >=1)

#probability of exacerbation in year 2 | condition
prob_condition3 <- nrow( joined_data %>% filter(ID %in% year1_2mod1sev$ID & obsExac.y2 > 0) ) / nrow(year1_2mod1sev)

RR_exac_3hist <- prob_condition3 / prob_exac_year2
cat(">=2 mod or >= 1sev: ", RR_exac_3hist, " ,")
#1.927891

#-----------------------------------------
# condition : probability of >= 2 mod/sev in year 1
year1_2exac <- joined_data %>% filter(obsExac.y1 >=2 )

#probability of exacerbation in year 2 | condition
prob_condition2 <- nrow( joined_data %>% filter(ID %in% year1_2exac$ID & obsExac.y2 > 0) ) / nrow(year1_2exac)

RR_exac_2hist <- prob_condition2 / prob_exac_year2
cat(">=2 mod/sev: ", RR_exac_2hist, " ,")
#2.334975


print(RR_exac_0hist/RR_exac_1hist)
print(RR_exac_2hist/RR_exac_1hist)
print(RR_exac_3hist/RR_exac_1hist)













