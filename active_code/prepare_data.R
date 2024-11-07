##load packages

require(tidyverse)
require(dplyr)

##load data and clean
tmp_data <- readxl::read_excel(file.path("data", "NIRUDAK_over5yrs_raw_data_.xlsx"))
data_tmp1 <- tmp_data %>% rename(id="Study ID") %>% rename(admit_date="Admit Date") %>% 
		rename(admit_time="Admit Time") %>% mutate(male=as.integer(Sex=="Male")) %>%
		select(-c(Sex, sex1)) %>% rename(age=Age) %>% rename(admit_wt="Admit Weight") %>%
		rename(IV_fluid_prior_to_admit_wt="IV Fluid Prior to Getting Admit Weight")
fu_names_to_change <- names(data_tmp1)[7:76]
colnames(data_tmp1)[7:76] <- gsub("Fluid", "fluid", gsub("Time", "time", gsub("Date", "date", 
		gsub("Patient_Weight", "wt", gsub(" ", "_", gsub("FU ", "fu", gsub("Form [6-8]::", "", fu_names_to_change)))))))
colnames(data_tmp1) <- gsub("Discharge Weight", "discharge_wt", gsub("Discharge Time", "discharge_time", 
					gsub("Discharge Date", "discharge_date", gsub("Form [8-9]::", "", colnames(data_tmp1)))))
colnames(data_tmp1) <- gsub("Date", "date", gsub("Weight", "wt", gsub("Time", "time", gsub("Return ", "return_", colnames(data_tmp1)))))
data_tmp2 <- data_tmp1 %>% rename(final_wt="Final wt") %>% rename(stable_wt="Stable wt") %>% 
			rename(actual_percent_dehydrat="Actual Percent Dehdyration") %>%
			rename(actual_dehydrat_cat="Actual Dehydration Category") %>%
 			rename(nirudak_percent_dehydrat="Model 6 Percent Dehydration") %>% 
			rename(nirudak_dehydrat_cat="Model 6 Dehydration Category") %>% 
			rename(nirudak_volume_deficit="Model 6 Volume Deficit (L)") %>% 
			rename(who_dehydrat_cat="WHO Dehydration Category") %>% 
			rename(who_volume_deficit="WHO Volume Deficit (L)") %>% 
			rename(who_severe_dehydrat="WHO severe dehydration") %>% 
			rename(who_some_dehydrat="WHO some dehydration")

extra_data_tmp <- readxl::read_excel(file.path("data", "NIRUDAK_additional_demographic_information.xlsx"))
extra_data <- extra_data_tmp %>% rename(monthly_income='Form 5::Monthly Income') %>% rename(id = 'Study ID')
extra_data_use <- extra_data$monthly_income
data_tmp2[, 'monthly_income'] = c(extra_data_use, NA)

##Construct Needed Variables
data_tmp3 <- data_tmp2 %>% rename(mental_status='Form 2::Mental Status 1') %>%
			rename(eye_level="Form 2::Eyes 1") %>% rename(thirst='Form 2::Thirst 1') %>% 
			rename(skin_pinch="Form 2::Skin Pinch 1")


data <- data_tmp3 %>% mutate(admit_date_time=as.POSIXct(paste(as.Date(admit_date), 
							format(admit_time, "%H:%M:%S")), tz="UTC", format="%Y-%m-%d %H:%M:%S")) %>%
		mutate(discharge_date_time=as.POSIXct(paste(as.Date(discharge_date), 
							format(discharge_time, "%H:%M:%S")), tz="UTC", format="%Y-%m-%d %H:%M:%S")) %>%
		mutate(hosp_los = difftime(discharge_date_time, admit_date_time, units='hours', 2))

for (g in 1:14){

	tmp_vec_IV <- as.numeric(unlist(data.frame(data[, paste(paste("fu", g, sep=''), "IV_fluid", sep="_")])))
	new_vec_IV <- ifelse(is.na(tmp_vec_IV), 0, tmp_vec_IV)
	data[, paste(paste("fu", g, sep=''), "IV_fluid", sep="_")] = new_vec_IV
	tmp_vec_ORS <- as.numeric(unlist(data.frame(data[, paste(paste("fu", g, sep=''), "ORS", sep="_")])))
	new_vec_ORS <- ifelse(is.na(tmp_vec_ORS), 0, tmp_vec_ORS)
	data[, paste(paste("fu", g, sep=''), "ORS", sep="_")] = new_vec_ORS
}

##Drop those with missing true dehydration status
data_use_tmp <- data[-which(is.na(data$actual_dehydrat_cat)), ]  
data_use <- data_use_tmp[-which(is.na(data_use_tmp$nirudak_dehydrat_cat)), ]  

# changing 
data_use$who_volume_deficit <- as.numeric(data_use$who_volume_deficit)

##Construct/Identify hospital LOS, wage rate, and ORS and IV ml of fluid
#data_use %>% colnames()

data_use$daily_wage = data_use$monthly_income/30
data_use$hosp_los_days = as.numeric(data_use$hosp_los)/24
data_use$wage_lost_hosp_stay = with(data_use, daily_wage * hosp_los_days)
data_use$total_ORS_fluid <- rowSums(as.data.frame(data_use[, colnames(data_use)[grep('ORS', colnames(data_use))]]))
data_use$total_IV_fluid <- rowSums(as.data.frame(data_use[, colnames(data_use)[grep('IV', colnames(data_use))][-1]]))

data_orig <- data_use 




rm(list=setdiff(ls(), c('user', 'jonah_work_dir', 'anagha_work_dir', 'data_orig')))
