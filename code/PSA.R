user = 'Jonah'

##load packages
library(tidyverse)


##Set working directory
jonah_work_dir <- "C:/Users/Jonah/Desktop/NIRUDAK_CEA"

if (user=='Jonah'){
	setwd(jonah_work_dir)	
}


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
data_use <- data[-which(is.na(data$actual_dehydrat_cat)), ]  


##Make Tables & Cross-tabulation
truth <- factor(data_use$actual_dehydrat_cat, levels=levels(factor(data_use$actual_dehydrat_cat))[c(1, 3, 2)], ordered=T)
who <- factor(data_use$who_dehydrat_cat, levels=levels(factor(data_use$who_dehydrat_cat))[c(1, 3, 2)], ordered=T)
nirudak <- factor(data_use$nirudak_dehydrat_cat, levels=levels(factor(data_use$nirudak_dehydrat_cat))[c(1, 3, 2)], ordered=T)

#Figure out the missingness
these_missing_nirudak <- which(is.na(nirudak)) 
these_missing_who <- which(is.na(who))
#no missing who
these_missing_who
#7 missing nirduak
length(these_missing_nirudak)

truth_props <- round(prop.table(table(truth, useNA='always')), 3)
who_props <- round(prop.table(table(who, useNA='always')), 3)
nirudak_props <- round(prop.table(table(nirudak, useNA='always')), 3)
props_table <- rbind(truth_props, who_props, nirudak_props)
rownames(props_table) <- c("truth", "who", "nirudak") 
 
nirudak_cond_props <- round(prop.table(table(truth, nirudak, useNA='ifany'), margin=1), 3)
who_cond_props <- round(prop.table(table(truth, who, useNA='ifany'), margin=1), 3)
nirudak_v_who_cond_probs <- round(prop.table(table(who, nirudak, useNA='ifany'), margin=1), 3)

all_table <- round(prop.table(table(who, nirudak, truth, useNA='ifany'), margin=3), 3)
nirudak_v_who_true_no <- all_table[, , 1]
nirudak_v_who_true_some <- all_table[, , 2]
nirudak_v_who_true_severe <- all_table[, , 3]

severe_true <- as.integer(truth=='Severe')
severe_who <- as.integer(who=='Severe')  
severe_nirudak <- as.integer(nirudak=='Severe')  

who_binary_table_cond_truth <- round(prop.table(table(severe_true, severe_who), margin=1), 3)
nirudak_binary_table_cond_truth <- round(prop.table(table(severe_true, severe_nirudak), margin=1), 3)
who_binary_table_cond_dx <- round(prop.table(table(severe_true, severe_who), margin=2), 3)
nirudak_binary_table_cond_dx <- round(prop.table(table(severe_true, severe_nirudak), margin=2), 3)

#calculate sensitivity and specificity and PPV and NPV wrt severe dehydration
sens_vec <- c(who_binary_table_cond_truth[2, 2], nirudak_binary_table_cond_truth[2, 2])
names(sens_vec) <- c("who", "nirudak")
spec_vec <- c(who_binary_table_cond_truth[1, 1], nirudak_binary_table_cond_truth[1, 1])
names(spec_vec) <- names(sens_vec)
PPV_vec <- c(who_binary_table_cond_dx[2, 2], nirudak_binary_table_cond_dx[2, 2])
NPV_vec <- c(who_binary_table_cond_dx[1, 1], nirudak_binary_table_cond_dx[1, 1])
severe_dx <- cbind(sens_vec, spec_vec, PPV_vec, NPV_vec)
colnames(severe_dx) <- c("sensitivity", "specificity", "PPV", "NPV")


##View Table Results

#proportion in each category of dehydration by standard  
props_table

#diagnostic characteristics of each instrument wrt severe dehydration
severe_dx

#conditional proportion in each predicted dehyrdation category gvn truth for each instrument 
nirudak_cond_props
who_cond_props

#conditional proportion in each predicted dehydration category according to nirudak gvn who classification
nirudak_v_who_cond_probs

#cross tabulation of nirudak and who gvn each level of true dehydration category 
nirudak_v_who_true_no  
nirudak_v_who_true_some
nirudak_v_who_true_severe

##classify everyone as treated appropriately or an error (6 possible errors) for each instrument
 #treat missing as error

who_status <- rep('problem', length(truth))
who_status[which(truth==who)] = 'treated appropriately'
who_status[which(truth=='Severe' & who=='Some')] = 'severe dehydration treated with OR fluids'
who_status[which(truth=='Severe' & who=='No')] = 'severe dehydration untreated'
who_status[which(truth=='Some' & who=='Severe')] = 'some dehydration treated with IV fluids'
who_status[which(truth=='Some' & who=='No')] = 'some dehydration untreated'
who_status[which(truth=='No' & who=='Severe')] = 'no dehydration treated with IV fluids'
who_status[which(truth=='No' & who=='Some')] = 'no dehydration treated with OR fluids'

who_status_coarse <- who_status
who_status_coarse[which(who_status %in% c('severe dehydration treated with OR fluids', 
			'severe dehydration untreated'))] = 'undertreated severe dehydration'
who_status_coarse[which(who_status %in% c('some dehydration treated with IV fluids', 
			'no dehydration treated with IV fluids'))] = 'non-severe dehydration treated with IV fluids'

nirudak_status <- rep('problem', length(truth))
nirudak_status[which(truth==nirudak)] = 'treated appropriately'
nirudak_status[which(truth=='Severe' & nirudak=='Some')] = 'severe dehydration treated with OR fluids'
nirudak_status[which(truth=='Severe' & nirudak=='No')] = 'severe dehydration untreated'
nirudak_status[which(truth=='Some' & nirudak=='Severe')] = 'some dehydration treated with IV fluids'
nirudak_status[which(truth=='Some' & nirudak=='No')] = 'some dehydration untreated'
nirudak_status[which(truth=='No' & nirudak=='Severe')] = 'no dehydration treated with IV fluids'
nirudak_status[which(truth=='No' & nirudak=='Some')] = 'no dehydration treated with OR fluids'
nirudak_status[which(truth=='Some' & is.na(nirudak))] = 'some dehydration untreated b/c missing'
nirudak_status[which(truth=='Severe' & is.na(nirudak))] = 'severe dehydration untreated b/c missing'

nirudak_status_coarse <- nirudak_status
nirudak_status_coarse[which(nirudak_status %in% c('severe dehydration treated with OR fluids', 
			'severe dehydration untreated', 'severe dehydration untreated b/c missing'))] = 
			'undertreated severe dehydration'
nirudak_status_coarse[which(nirudak_status %in% c('some dehydration treated with IV fluids', 
			'no dehydration treated with IV fluids'))] = 'non-severe dehydration treated with IV fluids'
nirudak_status_coarse[which(nirudak_status %in% c('some dehydration untreated b/c missing', 
			'some dehydration untreated'))] = 'some dehydration untreated'

age <- data_use$age
age_undrtrt_severe_who <- age[which(who_status_coarse=='undertreated severe dehydration')]
age_undrtrt_severe_nirudak <- age[which(nirudak_status_coarse=='undertreated severe dehydration')]
age_ovrtrt_severe_who <- age[which(who_status_coarse=='non-severe dehydration treated with IV fluids')]
age_ovrtrt_severe_nirudak <- age[which(nirudak_status_coarse=='non-severe dehydration treated with IV fluids')]

sample_age_stats <- round(c(mean(age), sd(age), as.numeric(quantile(age, probs=c(0.25, 0.5, 0.75))), range(age)))
undrtrt_who_age_stats <- round(c(mean(age_undrtrt_severe_who), sd(age_undrtrt_severe_who), 
			as.numeric(quantile(age_undrtrt_severe_who, probs=c(0.25, 0.5, 0.75))), range(age_undrtrt_severe_who)))
undrtrt_nirudak_age_stats <- round(c(mean(age_undrtrt_severe_nirudak), sd(age_undrtrt_severe_nirudak), 
			as.numeric(quantile(age_undrtrt_severe_nirudak, probs=c(0.25, 0.5, 0.75))), range(age_undrtrt_severe_nirudak)))
ovrtrt_who_age_stats <- round(c(mean(age_ovrtrt_severe_who), sd(age_ovrtrt_severe_who), 
			as.numeric(quantile(age_ovrtrt_severe_who, probs=c(0.25, 0.5, 0.75))), range(age_ovrtrt_severe_who)))
ovrtrt_nirudak_age_stats <- round(c(mean(age_ovrtrt_severe_nirudak), sd(age_ovrtrt_severe_nirudak), 
			as.numeric(quantile(age_ovrtrt_severe_nirudak, probs=c(0.25, 0.5, 0.75))), range(age_ovrtrt_severe_nirudak)))
age_table <- rbind(sample_age_stats, undrtrt_who_age_stats, undrtrt_nirudak_age_stats, ovrtrt_who_age_stats, ovrtrt_nirudak_age_stats)
rownames(age_table) <- c("sample", "who undertreated severe",
	 "nirudak undertreated severe", "who overtreated severe", "nirudak overtreated severe")
colnames(age_table) <- c("mean", "sd", "25th percentile", "median", "75th percentile", "min", "max")
Ns <- c(length(age), length(age_undrtrt_severe_who), length(age_undrtrt_severe_nirudak), 
		length(age_ovrtrt_severe_who), length(age_ovrtrt_severe_nirudak))
age_table <- cbind(Ns, age_table)
colnames(age_table)[1] = 'N'

#test if age distributions have different locations. They do for both undertrt and overtrt of severe
t.test(age_undrtrt_severe_who, age_undrtrt_severe_nirudak)
wilcox.test(age_undrtrt_severe_who, age_undrtrt_severe_nirudak, conf.int=T)
t.test(age_ovrtrt_severe_who, age_ovrtrt_severe_nirudak)
wilcox.test(age_ovrtrt_severe_who, age_ovrtrt_severe_nirudak, conf.int=T)


####Summary & Notes

##the gold standard category was derived from categorizing actual percent dehydration calculated 
 #as 100 * (final_wt - admission_wt)/final_wt
 #severe dehydration was >9%, some dehydration was 3-9%, and no dehydration was <3%
##the Nirudak model was a logistic ordinal regression model based on age and sex and
 #16 clinical predictors measured at admission. 

#both who and nirudak (but more so nirudak) tend to missclassify some dehyrdation
 #as severe and, to a lesser extent, no dehydration.
#nirudak was more likely to pick up severe dehydration at the exense, however, of being more likely to incorrectly classify
 #non-severe dehydration as dehydration.
#who had a roughly 53% sensitivity wrt severe dehyrdation (47% false negative rate)
 #while nirudak had a roughly 79% sensitivity wrt severe dehydration (21% false negative rate)
#who had a roughly 68% specificity wrt severe dehydration (with a roughly 32% false positive rate)
 #while nirudak had a roughly 63% specificity wrt to severe dehydration (with a roughly 37% false positive rate) 
#who has a roughly 20% PPV and 91% NPV while Nirudak has a roughly 24% PPV and 95% NPV.
 #meaning only roughly 1/5 and 1/4 patients classified as severely dehydrated by who and nirudak, respectively, 
 #are actually severely dehydrated. Although Nirudak is slightly better here, it classifies more as severely dehydrated
 #so it incorreclty classifies more patients as severely dehyrdated.
 #However, only 1/10 and 1/20 patients classified as not severely dehydrated by who and nirudak, respectively, are actually
  #severely dehyrdated. Thus Nirudak is better at ruling out severe dehydration.
#Nirudak was more accurate than WHO among those without dehdyrdation, with WHO tending to overdiagnose more
 #in terms of 'some' and 'severe'. So Who would lead to more overtreatment among those with no actual dehydration.
#Who was somewhat more accurate among those with some dehydration, with NIRUDAK both under ('no') and 
 #overdiagnosing ('severe') more than who. So, NIRUDAK would both undertreat and overtreat patients
 #with some dehydration more often than (by a modest degree) WHO.
#Nirudak was more accurate with severe dehydration than WHO
#among the severely dehydrated patients that Nirudak and Who underdiagnosed (missed), 
 #patients tended to be older (p<0.001) in the Nirudak sample compared with the Who
 #with a 9-year difference in median age and a 13-year difference in mean age (95% CI: 6-20)
#among the non-severely dehydrated patients that Nirudak and Who overdiagnosed (as severe)
 #patients tended to be younger in the Nirudak group (p<0.0001), with a 21-year difference in median age 
 #and a 10-year difference in mean age (95% CI: 7-12)


   
- figure out incorporating uncertainty into distribution of cross-tables for PSA 
- write out assumptions for penalties
- review your PSA code



problem areas:
treating those with severe dehyrdation as if they had none
treating those with severe dehydration as if they had some 

treating those with None as if they had severe
treating those with some as if they had severe

treating those with some as if they had no dehydration


####Assumptions

##Trt
 #patients treated according to who guidelines. So, none is no treatment is sent home, 
 #some is oral fluids and monitoring and severe is IV and hospitalization.

##DALY Penalties associated with Appropriate Treatment
 #no DALY penalty for treating accurately (if instrument classifies correctly)
   #this may be wrong: do some with severe dehydration die even if treated appropriately?
   #if they did, this would diminish the benefit of getting things right.
##DALY penalities associated with getting things wrong:
  #assume treating those with no dehydration as if they had some dehydration carries no DALY penalty, only a cost 




####Unit Costs

##IV and ORS Costs in BDT
IV_tube_and_solution_price <- 14.58
pair_gloves_price <- 6.86
butterfly_needle_price <- 11.15
IV_fixed_unit_cost <- IV_tube_and_solution_price + pair_gloves_price + butterfly_needle_price
IV_fluid_cost_per_ml <- 0.104
ORS_fluid_cost_per_ml <- 0.0054
 

##Hospital Costs in BDT
hosp_cost_per_hour <- 2573.1/24




####Calculate Costs

##Actual
IV_fluid_prior_to_admit_wt

data <- 

#calculate total fluid for IV and 

data %>% select(str_subset(names(data), "IV")) %>% rowSums()




data$iv_fluid_prior_to_admit_wt %>% summary()

names(data)












