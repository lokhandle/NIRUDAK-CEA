# user = 'Jonah'
user = 'Anagha'

##load packages
library(tidyverse)


##Set working directory
jonah_work_dir <- "C:/Users/Jonah/Desktop/NIRUDAK_CEA"

anagha_work_dir <- "/Users/anaghalokhande/Desktop/Research/Levine-NIRUDAK-CEA"

if (user=='Jonah'){
	setwd(jonah_work_dir)	
}

if (user=='Anagha'){
  setwd(anagha_work_dir)	
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
data_use_tmp <- data[-which(is.na(data$actual_dehydrat_cat)), ]  
data_use <- data_use_tmp[-which(is.na(data_use_tmp$nirudak_dehydrat_cat)), ]  


joint_prob_vec_nirudak_orig <- joint_prob_vec_who_orig <- rep(NA, 9) 
names(joint_prob_vec_nirudak_orig) <- names(joint_prob_vec_who_orig) <- c(
	c("true none, model none", "true none, model some", "true none, model severe"),
	c("true some, model none", "true some, model some", "true some, model severe"),
	c("true severe, model none", "true severe, model some", "true severe, model severe"))

#original data
joint_prob_vec_nirudak_orig[1] <- mean(with(data_use, actual_dehydrat_cat == 'No' & nirudak_dehydrat_cat == 'No'))
joint_prob_vec_nirudak_orig[2] <- mean(with(data_use, actual_dehydrat_cat == 'No' & nirudak_dehydrat_cat == 'Some'))
joint_prob_vec_nirudak_orig[3] <- mean(with(data_use, actual_dehydrat_cat == 'No' & nirudak_dehydrat_cat == 'Severe'))
joint_prob_vec_nirudak_orig[4] <- mean(with(data_use, actual_dehydrat_cat == 'Some' & nirudak_dehydrat_cat == 'No'))
joint_prob_vec_nirudak_orig[5] <- mean(with(data_use, actual_dehydrat_cat == 'Some' & nirudak_dehydrat_cat == 'Some'))
joint_prob_vec_nirudak_orig[6] <- mean(with(data_use, actual_dehydrat_cat == 'Some' & nirudak_dehydrat_cat == 'Severe'))
joint_prob_vec_nirudak_orig[7] <- mean(with(data_use, actual_dehydrat_cat == 'Severe' & nirudak_dehydrat_cat == 'No'))
joint_prob_vec_nirudak_orig[8] <- mean(with(data_use, actual_dehydrat_cat == 'Severe' & nirudak_dehydrat_cat == 'Some'))
joint_prob_vec_nirudak_orig[9] <- mean(with(data_use, actual_dehydrat_cat == 'Severe' & nirudak_dehydrat_cat == 'Severe'))

joint_prob_vec_who_orig[1] <- mean(with(data_use, actual_dehydrat_cat == 'No' & who_dehydrat_cat == 'No'))
joint_prob_vec_who_orig[2] <- mean(with(data_use, actual_dehydrat_cat == 'No' & who_dehydrat_cat == 'Some'))
joint_prob_vec_who_orig[3] <- mean(with(data_use, actual_dehydrat_cat == 'No' & who_dehydrat_cat == 'Severe'))
joint_prob_vec_who_orig[4] <- mean(with(data_use, actual_dehydrat_cat == 'Some' & who_dehydrat_cat == 'No'))
joint_prob_vec_who_orig[5] <- mean(with(data_use, actual_dehydrat_cat == 'Some' & who_dehydrat_cat == 'Some'))
joint_prob_vec_who_orig[6] <- mean(with(data_use, actual_dehydrat_cat == 'Some' & who_dehydrat_cat == 'Severe'))
joint_prob_vec_who_orig[7] <- mean(with(data_use, actual_dehydrat_cat == 'Severe' & who_dehydrat_cat == 'No'))
joint_prob_vec_who_orig[8] <- mean(with(data_use, actual_dehydrat_cat == 'Severe' & who_dehydrat_cat == 'Some'))
joint_prob_vec_who_orig[9] <- mean(with(data_use, actual_dehydrat_cat == 'Severe' & who_dehydrat_cat == 'Severe'))


B <- 50 #number of bootstrap samples 
N <- nrow(data_use) #number of obsevations per bootstrap dataset
joint_prob_vec_who_boot_mat <- joint_prob_vec_nirudak_boot_mat <- matrix(NA, nrow=B, ncol=9)
row_indices <- 1:N

for (i in 1:B){

test_sample <- sample.int(N, size = N, replace = TRUE)
data_tmp <- data_use[test_sample, ]


joint_prob_vec_nirudak_tmp <- joint_prob_vec_who_tmp <- rep(NA, 9) 
names(joint_prob_vec_nirudak_tmp) <- names(joint_prob_vec_who_tmp) <- c(
	c("true none, model none", "true none, model some", "true none, model severe"),
	c("true some, model none", "true some, model some", "true some, model severe"),
	c("true severe, model none", "true severe, model some", "true severe, model severe"))

joint_prob_vec_nirudak_tmp[1] <- mean(with(data_tmp, actual_dehydrat_cat == 'No' & nirudak_dehydrat_cat == 'No'))
joint_prob_vec_nirudak_tmp[2] <- mean(with(data_tmp, actual_dehydrat_cat == 'No' & nirudak_dehydrat_cat == 'Some'))
joint_prob_vec_nirudak_tmp[3] <- mean(with(data_tmp, actual_dehydrat_cat == 'No' & nirudak_dehydrat_cat == 'Severe'))
joint_prob_vec_nirudak_tmp[4] <- mean(with(data_tmp, actual_dehydrat_cat == 'Some' & nirudak_dehydrat_cat == 'No'))
joint_prob_vec_nirudak_tmp[5] <- mean(with(data_tmp, actual_dehydrat_cat == 'Some' & nirudak_dehydrat_cat == 'Some'))
joint_prob_vec_nirudak_tmp[6] <- mean(with(data_tmp, actual_dehydrat_cat == 'Some' & nirudak_dehydrat_cat == 'Severe'))
joint_prob_vec_nirudak_tmp[7] <- mean(with(data_tmp, actual_dehydrat_cat == 'Severe' & nirudak_dehydrat_cat == 'No'))
joint_prob_vec_nirudak_tmp[8] <- mean(with(data_tmp, actual_dehydrat_cat == 'Severe' & nirudak_dehydrat_cat == 'Some'))
joint_prob_vec_nirudak_tmp[9] <- mean(with(data_tmp, actual_dehydrat_cat == 'Severe' & nirudak_dehydrat_cat == 'Severe'))

joint_prob_vec_who_tmp[1] <- mean(with(data_tmp, actual_dehydrat_cat == 'No' & who_dehydrat_cat == 'No'))
joint_prob_vec_who_tmp[2] <- mean(with(data_tmp, actual_dehydrat_cat == 'No' & who_dehydrat_cat == 'Some'))
joint_prob_vec_who_tmp[3] <- mean(with(data_tmp, actual_dehydrat_cat == 'No' & who_dehydrat_cat == 'Severe'))
joint_prob_vec_who_tmp[4] <- mean(with(data_tmp, actual_dehydrat_cat == 'Some' & who_dehydrat_cat == 'No'))
joint_prob_vec_who_tmp[5] <- mean(with(data_tmp, actual_dehydrat_cat == 'Some' & who_dehydrat_cat == 'Some'))
joint_prob_vec_who_tmp[6] <- mean(with(data_tmp, actual_dehydrat_cat == 'Some' & who_dehydrat_cat == 'Severe'))
joint_prob_vec_who_tmp[7] <- mean(with(data_tmp, actual_dehydrat_cat == 'Severe' & who_dehydrat_cat == 'No'))
joint_prob_vec_who_tmp[8] <- mean(with(data_tmp, actual_dehydrat_cat == 'Severe' & who_dehydrat_cat == 'Some'))
joint_prob_vec_who_tmp[9] <- mean(with(data_tmp, actual_dehydrat_cat == 'Severe' & who_dehydrat_cat == 'Severe'))

joint_prob_vec_who_boot_mat[i, ] <- joint_prob_vec_who_tmp
joint_prob_vec_nirudak_boot_mat[i, ] <- joint_prob_vec_nirudak_tmp
}

colnames(joint_prob_vec_who_boot_mat) <- colnames(joint_prob_vec_nirudak_boot_mat) <- names(joint_prob_vec_nirudak_tmp)



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


# define net heatlh benefit function
ICER_fxn <- function(death_gvn_overtreat_prob, death_gvn_undertreat_prob) {
  
  
  # defining all expected costs and all expected DALYs
  # WHO prob
  branch_a <- 0.06812325
  branch_b <- 0.23498925
  branch_c <- 0.0393875
  branch_d <- 0.05125032
  branch_e <- 0.30279966
  branch_f <- 0.07735002
  branch_g <- 0.00988057
  branch_h <- 0.13161281
  branch_i <- 0.08460662
  
  # NIRUDAK
  branch_j <- 0.10285635
  branch_k <- 0.28938165
  branch_l <- 0.032262
  branch_m <- 0.02196501
  branch_n <- 0.21924172
  branch_o <- 0.05049327
  branch_p <- 0.0046827
  branch_q <- 0.16037538
  branch_r <- 0.1187703
  
  # WHO costs
  branch_a_cost <- 97.04
  branch_b_cost <-100.36
  branch_c_cost <-102.37
  branch_d_cost <-27.91
  branch_e_cost <-26.65
  branch_f_cost <-17.19
  branch_g_cost <-9.92
  branch_h_cost <-12.87
  branch_i_cost <-14.37
  
  # NIRUDAK costs
  branch_j_cost <-96.82
  branch_k_cost <-93.91
  branch_l_cost <-96.02
  branch_m_cost <-26.09
  branch_n_cost <-27.27
  branch_o_cost <-28.92
  branch_p_cost <-16.53
  branch_q_cost <-14.17
  branch_r_cost <-6.88
  
  # listing mean DALYs lost conditional on dying; doesn't vary w/ sensitivity analysis
  # WHO
  # death from overtreatment
  mean_DALYs_lost_if_die_branch_b <- 44.97
  mean_DALYs_lost_if_die_branch_c <- 35.09
  # death from undertreatment
  mean_DALYs_lost_if_die_branch_d <- 58.99
  mean_DALYs_lost_if_die_branch_g <- 64.02
  
  # NIRUDAK
  # death from overtreatment
  mean_DALYs_lost_if_die_branch_k <- 52.41
  mean_DALYs_lost_if_die_branch_l <- 49.18
  # death from undertreatment
  mean_DALYs_lost_if_die_branch_m <- 48.32
  mean_DALYs_lost_if_die_branch_p <- 49.92
  
  # expected DALYs lost conditional on making it to that branch endpoint
  # WHO
  branch_a_DALY <- 0
  branch_b_DALY <- death_gvn_overtreat_prob*mean_DALYs_lost_if_die_branch_b
  branch_c_DALY <- death_gvn_overtreat_prob*mean_DALYs_lost_if_die_branch_c
  branch_d_DALY <- death_gvn_undertreat_prob*mean_DALYs_lost_if_die_branch_d
  branch_e_DALY <- 0
  branch_f_DALY <- 0
  branch_g_DALY <- death_gvn_undertreat_prob*mean_DALYs_lost_if_die_branch_g
  branch_h_DALY <- 0
  branch_i_DALY <- 0
  
  # NIRUDAK
  branch_j_DALY <- 0
  branch_k_DALY <- death_gvn_overtreat_prob*mean_DALYs_lost_if_die_branch_k
  branch_l_DALY <- death_gvn_overtreat_prob*mean_DALYs_lost_if_die_branch_l
  branch_m_DALY <- death_gvn_undertreat_prob*mean_DALYs_lost_if_die_branch_m
  branch_n_DALY <- 0
  branch_o_DALY <- 0
  branch_p_DALY <- death_gvn_undertreat_prob*mean_DALYs_lost_if_die_branch_p
  branch_q_DALY <- 0
  branch_r_DALY <- 0
  
  # calculating expected costs
  # WHO
  branch_a_expected_cost <- branch_a_cost*branch_a
  branch_b_expected_cost <- branch_b_cost*branch_b
  branch_c_expected_cost <- branch_c_cost*branch_c
  branch_d_expected_cost <- branch_d_cost*branch_d
  branch_e_expected_cost <- branch_e_cost*branch_e
  branch_f_expected_cost <- branch_f_cost*branch_f
  branch_g_expected_cost <- branch_g_cost*branch_g
  branch_h_expected_cost <- branch_h_cost*branch_h
  branch_i_expected_cost <- branch_i_cost*branch_i
  
  #NIRUDAK
  branch_j_expected_cost <- branch_j_cost*branch_j
  branch_k_expected_cost <- branch_k_cost*branch_k
  branch_l_expected_cost <- branch_l_cost*branch_l
  branch_m_expected_cost <- branch_m_cost*branch_m
  branch_n_expected_cost <- branch_n_cost*branch_n
  branch_o_expected_cost <- branch_o_cost*branch_o
  branch_p_expected_cost <- branch_p_cost*branch_p
  branch_q_expected_cost <- branch_q_cost*branch_q
  branch_r_expected_cost <- branch_r_cost*branch_r
  
  # calculating expected DALYs
  # WHO
  branch_a_expected_DALY <- branch_a_DALY*branch_a
  branch_b_expected_DALY <- branch_b_DALY*branch_b
  branch_c_expected_DALY <- branch_c_DALY*branch_c
  branch_d_expected_DALY <- branch_d_DALY*branch_d
  branch_e_expected_DALY <- branch_e_DALY*branch_e
  branch_f_expected_DALY <- branch_f_DALY*branch_f
  branch_g_expected_DALY <- branch_g_DALY*branch_g
  branch_h_expected_DALY <- branch_h_DALY*branch_h
  branch_i_expected_DALY <- branch_i_DALY*branch_i
  
  # NIRUDAK
  branch_j_expected_DALY <- branch_j_DALY*branch_j
  branch_k_expected_DALY <- branch_k_DALY*branch_k
  branch_l_expected_DALY <- branch_l_DALY*branch_l
  branch_m_expected_DALY <- branch_m_DALY*branch_m
  branch_n_expected_DALY <- branch_n_DALY*branch_n
  branch_o_expected_DALY <- branch_o_DALY*branch_o
  branch_p_expected_DALY <- branch_p_DALY*branch_p
  branch_q_expected_DALY <- branch_q_DALY*branch_q
  branch_r_expected_DALY <- branch_r_DALY*branch_r
  
  # calculating ICER
  # total expected costs
  WHO_total_expected_cost <- sum(branch_a_expected_cost, branch_b_expected_cost, branch_c_expected_cost, branch_d_expected_cost,
                                 branch_e_expected_cost, branch_f_expected_cost, branch_g_expected_cost, branch_h_expected_cost,
                                 branch_i_expected_cost)
  
  NIRUDAK_total_expected_cost <- sum(branch_j_expected_cost, branch_k_expected_cost, branch_l_expected_cost, branch_m_expected_cost,
                                     branch_n_expected_cost, branch_o_expected_cost, branch_p_expected_cost, branch_q_expected_cost,
                                     branch_r_expected_cost)
  
  # total expected DALYs
  WHO_total_expected_DALYs <- sum(branch_a_expected_DALY, branch_b_expected_DALY, branch_c_expected_DALY, branch_d_expected_DALY,
                                  branch_e_expected_DALY, branch_f_expected_DALY, branch_g_expected_DALY, branch_h_expected_DALY,
                                  branch_i_expected_DALY)
  
  NIRUDAK_total_expected_DALYs <-sum(branch_j_expected_DALY, branch_k_expected_DALY, branch_l_expected_DALY, branch_m_expected_DALY,
                                     branch_n_expected_DALY, branch_o_expected_DALY, branch_p_expected_DALY, branch_q_expected_DALY,
                                     branch_r_expected_DALY)
  # incremental costs and DALYs
  incremental_costs <- NIRUDAK_total_expected_cost-WHO_total_expected_cost
  incremental_DALYs <-NIRUDAK_total_expected_DALYs-WHO_total_expected_DALYs
  # negative sign preserves usual interpretation of ICER
  incremental_DALYs_averted <- -incremental_DALYs
  
  # ICER
  ICER = (incremental_costs)/(incremental_DALYs_averted)
  
  results_vec <- c(ICER, incremental_costs, incremental_DALYs_averted, incremental_DALYs, NIRUDAK_total_expected_cost, WHO_total_expected_cost, NIRUDAK_total_expected_DALYs, WHO_total_expected_DALYs)
  
  names(results_vec) <- c("ICER", "Incremental Costs", "Incremental DALYs Averted", "Incremental DALYs", "NIRUDAK Total Expected Costs", "WHO Total Expected Costs", "NIRUDAK Total Expected DALYs", "WHO Total Expected DALYs")
  
  return(results_vec)
  
}

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

############################ Stata code ########################################
* SET UP CODE
* CALCULATING HYPOTHETICAL YEARS OF LIFE LOST FOR ALL PATIENTS (i.e. if they died at their current age)
gen years_lost_male = 0
replace years_lost_male = 81.41 if Sex == "Male" & Age < 5
replace years_lost_male = 76.63 if Sex == "Male" & 5 <= Age & Age < 10
replace years_lost_male = 71.66 if Sex == "Male" & 10 <= Age & Age < 15
replace years_lost_male = 66.69 if Sex == "Male" & 15 <= Age & Age < 20
replace years_lost_male = 61.77 if Sex == "Male" & 20 <= Age & Age < 25
replace years_lost_male = 56.91 if Sex == "Male" & 25 <= Age & Age < 30
replace years_lost_male = 52.03 if Sex == "Male" & 30 <= Age & Age < 35
replace years_lost_male = 47.18 if (Sex == "Male") & 35 <= Age & Age < 40
replace years_lost_male = 42.35 if Sex == "Male" & 40 <= Age & Age < 45
replace years_lost_male = 37.57 if Sex == "Male" & 45 <= Age & Age < 50
replace years_lost_male = 32.89 if Sex == "Male" & 50 <= Age & Age < 55
replace years_lost_male = 28.34 if Sex == "Male" & 55 <= Age & Age < 60
replace years_lost_male = 23.97 if Sex == "Male" & 60 <= Age & Age < 65
replace years_lost_male = 19.83 if Sex == "Male" & 65 <= Age & Age < 70
replace years_lost_male = 15.96 if Sex == "Male" & 70 <= Age & Age < 75
replace years_lost_male = 12.41 if Sex == "Male" & 75 <= Age & Age < 80
replace years_lost_male = 9.18 if Sex == "Male" & 80 <= Age & Age < 85
replace years_lost_male = 6.46 if Sex == "Male" & 85 <= Age & Age < 90
replace years_lost_male = 4.41 if Sex == "Male" & 90 <= Age


gen years_lost_female = 87.45 if Sex == "Female" & Age < 5
replace years_lost_female = 82.66 if Sex == "Female" & 5 <= Age & Age < 10
replace years_lost_female = 77.69 if Sex == "Female" & 10 <= Age & Age < 15
replace years_lost_female = 72.72 if Sex == "Female" & 15 <= Age & Age < 20
replace years_lost_female = 67.77 if Sex == "Female" & 20 <= Age & Age < 25
replace years_lost_female = 62.84 if Sex == "Female" & 25 <= Age & Age < 30
replace years_lost_female = 57.91 if Sex == "Female" & 30 <= Age & Age < 35
replace years_lost_female = 53 if Sex == "Female" & 35 <= Age & Age < 40
replace years_lost_female = 48.11 if Sex == "Female" & 40 <= Age & Age < 45
replace years_lost_female = 43.26 if Sex == "Female" & 45 <= Age & Age < 50
replace years_lost_female = 38.49 if Sex == "Female" & 50 <= Age & Age < 55
replace years_lost_female = 33.79 if Sex == "Female" & 55 <= Age & Age < 60
replace years_lost_female = 29.17 if Sex == "Female" & 60 <= Age & Age < 65
replace years_lost_female = 24.63 if Sex == "Female" & 65 <= Age & Age < 70
replace years_lost_female = 20.21 if Sex == "Female" & 70 <= Age & Age < 75
replace years_lost_female = 15.97 if Sex == "Female" & 75 <= Age & Age < 80
replace years_lost_female = 12.01 if Sex == "Female" & 80 <= Age & Age < 85
replace years_lost_female = 8.51 if Sex == "Female" & 85 <= Age & Age < 90
replace years_lost_female = 5.71 if Sex == "Female" & 90 <= Age


gen all_sex_years_lost_japan = years_lost_male if Sex == "Male"
replace all_sex_years_lost_japan = years_lost_female if Sex == "Female"


********************************************************************************
  * SETTING UP COST VARIABLES
* converting string date variables to formatted data types
gen admit_date = date(AdmitDate, "MDY")
format admit_date %td
gen discharge_date = date(Form8DischargeDate, "MDY")
format admit_date %td

* calculating length of stay
gen length_stay = discharge_date - admit_date

* daily wage
gen daily_wage = (MonthlyIncome/30)

* wage lost to hospital stay
gen wage_lost_hospital_stay = daily_wage * length_stay

* converting lost wages from BDT to USD using purchasing power parity (PPP) from the World Bank
gen wage_lost_hospital_stay_usd = wage_lost_hospital_stay * 0.031

* total cost variable: NIRUDAK_irr_cost_bdt2 & WHO_irr_cost_bdt2
* BDT to USD conversion using PPP (in 2019, 31.29 BT per USD)
* those NIRUDAK_irr_cost_bdt2 & WHO_irr_cost_bdt2 variables are from the file "nirudak_cost_analysis_stata_code.do" — i.e., the initial cost analysis
gen NIRUDAK_total_cost_USD = NIRUDAK_irr_cost_bdt2 * 0.031
gen WHO_total_cost_USD = WHO_irr_cost_bdt2 * 0.031

* initial recommended resuscitation (IRR) (in USD) — did not do this because there are no wages lost in IRR

* calculating total costs — full length of hospital stay (in USD)
gen WHO_cea_cost_total = WHO_total_cost_USD + wage_lost_hospital_stay_usd
gen NIRUDAK_cea_cost_total = NIRUDAK_total_cost_USD + wage_lost_hospital_stay_usd

############################ end Stata code ####################################












