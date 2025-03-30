#user = 'Jonah'
user = 'Anagha'

##load packages
library(tidyverse)
library(dplyr)

##Set working directory
jonah_work_dir <- "C:/Users/Jonah/Desktop/NIRUDAK-CEA"

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
    dplyr::select(-c(Sex, sex1)) %>% rename(age=Age) %>% rename(admit_wt="Admit Weight") %>%
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
data_use %>% colnames()

data_use$daily_wage = data_use$monthly_income/30
data_use$hosp_los_days = as.numeric(data_use$hosp_los)/24
data_use$wage_lost_hosp_stay = with(data_use, daily_wage * hosp_los_days)
data_use$total_ORS_fluid <- rowSums(as.data.frame(data_use[, colnames(data_use)[grep('ORS', colnames(data_use))]]))
data_use$total_IV_fluid <- rowSums(as.data.frame(data_use[, colnames(data_use)[grep('IV', colnames(data_use))][-1]]))


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


##Useful Functions
# implementing DALYs
five_yr_age_buckets <- seq(0, 90, 5)
resid_life_expect_at_age_male <- c(81.41, 76.63, 71.66, 66.69, 61.77, 56.91, 52.03, 47.18,
                                   42.35, 37.57, 32.89, 28.34, 23.97, 19.83, 15.96, 12.41, 9.18,
                                   6.46, 4.41)

resid_life_expect_at_age_female <- c(87.45, 82.66, 77.69, 72.72, 67.77, 62.84, 57.91,
                                     53, 48.11, 43.26, 38.49, 33.79, 29.17, 24.63, 20.21, 15.97,
                                     12.01, 8.51, 5.71)

resid_life_interp_fxn_male <- splinefun(five_yr_age_buckets, resid_life_expect_at_age_male)

resid_life_interp_fxn_female <- splinefun(five_yr_age_buckets, resid_life_expect_at_age_female)

##Setting Up Bootstrap

B <- 2000 #number of bootstrap samples 
N <- nrow(data_use) #number of observations per bootstrap dataset
joint_prob_vec_who_boot_mat <- joint_prob_vec_nirudak_boot_mat <- matrix(NA, nrow=B, ncol=9)
row_indices <- 1:N
mean_cost_boot_mat <- data.frame(matrix(NA, nrow=B, ncol=14))
colnames(mean_cost_boot_mat) <- c("mean_hosp_costs", "mean_hosp_costs_severe_only_nirudak", "mean_hosp_costs_severe_only_who", "mean_productivity_costs", 
					"mean_ORS_fluid_costs_actual", "mean_IV_fluid_costs_actual", 
					"mean_ORS_fluid_costs_who", "mean_ORS_fluid_costs_nirudak", 
					 "mean_IV_fluid_costs_who", "mean_IV_fluid_costs_nirudak", 
					"mean_ORS_fluid_costs_act_who", "mean_ORS_fluid_costs_act_nirudak",
					"mean_IV_fluid_costs_act_who", "mean_IV_fluid_costs_act_nirudak")
mean_dalys_boot_mat_all <- mean_dalys_boot_mat_specific <- data.frame(matrix(NA, nrow=B, ncol=2))
colnames(mean_dalys_boot_mat_all) <- colnames(mean_dalys_boot_mat_specific) <- c("who", "nirudak")
mean_YLL_boot_mat_all <- mean_YLL_boot_mat_specific <- data.frame(matrix(NA, nrow=B, ncol=2))
colnames(mean_YLL_boot_mat_all) <- colnames(mean_YLL_boot_mat_specific) <- c("who", "nirudak")

##Hospital Costs in BDT
##IV and ORS Costs in BDT
#fixed per-person fluid unit costs
IV_tube_and_solution_price <- 14.58
pair_gloves_price <- 6.86
butterfly_needle_price <- 11.15
IV_fixed_unit_cost <- IV_tube_and_solution_price + pair_gloves_price + butterfly_needle_price



################################################################ BEGIN BOOTSTRAP


for (i in 1:B){

test_sample <- sample.int(N, size = N, replace = TRUE)
data_tmp <- data_use[test_sample, ]

# cost_data_tmp = matrix of cost variables from 1 bootstrapped dataset
cost_data_tmp <- as.data.frame(data_tmp[, c("hosp_los", "wage_lost_hosp_stay", "total_ORS_fluid", "total_IV_fluid", 
			"nirudak_volume_deficit", "who_volume_deficit", "actual_dehydrat_cat")])

##Calculate Mean costs


# actual hospital costs - for each bootstrap dataset we calculate the mean
hosp_cost_per_hour <- 2573.1/24
hosp_los_hours_use <- cost_data_tmp$hosp_los
hosp_costs <- as.numeric(hosp_cost_per_hour * hosp_los_hours_use)
mean_cost_boot_mat[i, "mean_hosp_costs"] <- mean(hosp_costs, na.rm=T)
mean_cost_boot_mat[i, "mean_hosp_costs_severe_only_nirudak"] <- with(data_tmp, mean(ifelse(nirudak_dehydrat_cat == 'Severe', hosp_costs, 0)))
mean_cost_boot_mat[i, "mean_hosp_costs_severe_only_who"] <- with(data_tmp, mean(ifelse(who_dehydrat_cat == 'Severe', hosp_costs, 0)))
#productivity costs
# mean_cost_boot_mat[i, "mean_productivity_costs"] <- mean(cost_data_tmp$wage_lost_hosp_stay, na.rm=T)

# revist this
# people who die will incur productivity costs
# take the average yearly wage * remaining lifetime
# detailed approach: for anyone who died - mean income per year (mean(data_use$monthly_income))*12 - 
# every year of life they lose, they lose that amount
# assume that people with Severe dehydration lose work whether they're in the hospital or not, 
# so the model does not affect short term productivity costs

#ORS fluid variable costs
ORS_fluid_cost_per_ml <- 0.0054
total_ORS_variable_fluid_costs_actual <- cost_data_tmp$total_ORS_fluid * ORS_fluid_cost_per_ml 
total_ORS_fluid_costs_actual <- total_ORS_variable_fluid_costs_actual + 0
mean_cost_boot_mat[i, "mean_ORS_fluid_costs_actual"] <- mean(total_ORS_fluid_costs_actual, na.rm=T)
#IV fluid variable costs
IV_fluid_cost_per_ml <- 0.104
total_IV_variable_fluid_costs_actual <- cost_data_tmp$total_IV_fluid * IV_fluid_cost_per_ml 
total_IV_fluid_costs_actual <- total_IV_variable_fluid_costs_actual + IV_fixed_unit_cost*(total_IV_variable_fluid_costs_actual > 0)
mean_cost_boot_mat[i, "mean_IV_fluid_costs_actual"] <- mean(total_IV_fluid_costs_actual, na.rm=T)

#model predicted fluid
#convert to $ using ml cost and condition on having 'some' for ORS
# calculating costs over the entire sample based on the amount/type of fluid prescribed by WHO (for SOME)
who_predicted_ORS_costs <- as.numeric(data_tmp$who_volume_deficit) * 1000 * ORS_fluid_cost_per_ml * 
					as.integer(data_tmp$who_dehydrat_cat == 'Some')
mean_cost_boot_mat[i, "mean_ORS_fluid_costs_who"] <- mean(who_predicted_ORS_costs, na.rm=T)

# calculating costs over the entire sample based on the amount/type of fluid prescribed by NIRUDAK (for SOME)
nirudak_predicted_ORS_costs <- as.numeric(data_tmp$nirudak_volume_deficit) * 1000 * ORS_fluid_cost_per_ml * 
					as.integer(data_tmp$nirudak_dehydrat_cat == 'Some')
mean_cost_boot_mat[i, "mean_ORS_fluid_costs_nirudak"] <- mean(nirudak_predicted_ORS_costs, na.rm=T)

# calculating costs over the entire sample based on the amount/type of fluid prescribed by WHO (for SEVERE)
who_predicted_IV_costs <- (as.numeric(data_tmp$who_volume_deficit) * 1000 * IV_fluid_cost_per_ml + IV_fixed_unit_cost) *
					as.integer(data_tmp$who_dehydrat_cat == 'Severe')
mean_cost_boot_mat[i, "mean_IV_fluid_costs_who"] <- mean(who_predicted_IV_costs, na.rm=T)

# calculating costs over the entire sample based on the amount/type of fluid prescribed by NIRUDAK (for SEVERE)
nirudak_predicted_IV_costs <- (as.numeric(data_tmp$nirudak_volume_deficit) * 1000 * IV_fluid_cost_per_ml + IV_fixed_unit_cost) * 
					as.integer(data_tmp$nirudak_dehydrat_cat == 'Severe')
mean_cost_boot_mat[i, "mean_IV_fluid_costs_nirudak"] <- mean(nirudak_predicted_IV_costs, na.rm=T)

# calculating mean variable fluid cost over entire sample assuming that providers tailor the amount of fluid to the model predicted
# dehydration category and they give the patient the average amount of fluid that was actually given to all patients in the corresponding true
# dehydration category, with the exception that only patients identified by the model as severely dehydrated get IVF
# but note that all categories get ORS
# here (below), the model is used to identify dehydration category and hospitalization status (e.g., Severe goes to the hospital)
# and the type of fluid received (e.g., Severe gets IVF)
# then everyone in that model predicted dehydration category will be treated like what was observed in the real hospital in Bangladesh (with the exception that
# only patients in a model-predicted Severe dehydration category get IVF)

# ORS - WHO
mean_cost_boot_mat[i, c("mean_ORS_fluid_costs_act_who")] <-
				as.numeric(as.numeric(prop.table(table(data_tmp$who_dehydrat_cat))[c(1, 3, 2)]) %*% # proportion (according to WHO) in each dehydration category
				(ORS_fluid_cost_per_ml * as.numeric(with(cost_data_tmp, by(total_ORS_fluid, actual_dehydrat_cat, mean))[c(1, 3, 2)]))) # mean ORS fluid used in each true disease category
# ORS - NIRUDAK
mean_cost_boot_mat[i, c("mean_ORS_fluid_costs_act_nirudak")] <-
					as.numeric(as.numeric(prop.table(table(data_tmp$nirudak_dehydrat_cat))[c(1, 3, 2)]) %*%
				(ORS_fluid_cost_per_ml * as.numeric(with(cost_data_tmp, by(total_ORS_fluid, actual_dehydrat_cat, mean))[c(1, 3, 2)])))

# IVF - WHO
mean_cost_boot_mat[i, c("mean_IV_fluid_costs_act_who")] <-
					as.numeric(as.numeric(prop.table(table(data_tmp$who_dehydrat_cat))[c(2)]) *
				((IV_fluid_cost_per_ml * as.numeric(with(cost_data_tmp, by(total_IV_fluid, actual_dehydrat_cat, mean))[c(2)])) +
				with(cost_data_tmp, by(total_IV_fluid, actual_dehydrat_cat, function(x){mean(as.integer(x > 0))}))[2] * IV_fixed_unit_cost)
				)

# IVF - NIRUDAK
mean_cost_boot_mat[i, c("mean_IV_fluid_costs_act_nirudak")] <-
  as.numeric(as.numeric(prop.table(table(data_tmp$nirudak_dehydrat_cat))[c(2)]) *
               ((IV_fluid_cost_per_ml * as.numeric(with(cost_data_tmp, by(total_IV_fluid, actual_dehydrat_cat, mean))[c(2)])) +
                  with(cost_data_tmp, by(total_IV_fluid, actual_dehydrat_cat, function(x){mean(as.integer(x > 0))}))[2] * IV_fixed_unit_cost)
  )


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

## Implementing DALYs

# nirudak
mean_YLL_among_dead_nirudak_male <- mean(resid_life_interp_fxn_male(as.numeric(unlist(data_tmp[data_tmp$male == 1 & data_tmp$actual_dehydrat_cat == "Severe" &
                                                                                                 data_tmp$nirudak_dehydrat_cat %in% c("No", "Some"), "age"]))))

mean_YLL_among_dead_nirudak_female <- mean(resid_life_interp_fxn_female(as.numeric(unlist(data_tmp[data_tmp$male == 0 & data_tmp$actual_dehydrat_cat == "Severe" &
                                                                                                     data_tmp$nirudak_dehydrat_cat %in% c("No", "Some"), "age"]))))

# hypothetical overtreat NIRUDAK
mean_YLL_among_dead_nirudak_male_overtreat <- mean(resid_life_interp_fxn_male(as.numeric(unlist(data_tmp[data_tmp$male == 1 & data_tmp$nirudak_dehydrat_cat == "Severe" &
                                                                                                           data_tmp$actual_dehydrat_cat %in% c("No", "Some"), "age"]))))

mean_YLL_among_dead_nirudak_female_overtreat <- mean(resid_life_interp_fxn_female(as.numeric(unlist(data_tmp[data_tmp$male == 0 & data_tmp$nirudak_dehydrat_cat == "Severe" &
                                                                                                               data_tmp$actual_dehydrat_cat %in% c("No", "Some"), "age"]))))


# who
mean_YLL_among_dead_who_male <- mean(resid_life_interp_fxn_male(as.numeric(unlist(data_tmp[data_tmp$male == 1 & data_tmp$actual_dehydrat_cat == "Severe" &
                                                                                             data_tmp$who_dehydrat_cat %in% c("No", "Some"), "age"]))))

mean_YLL_among_dead_who_female <- mean(resid_life_interp_fxn_female(as.numeric(unlist(data_tmp[data_tmp$male == 0 & data_tmp$actual_dehydrat_cat == "Severe" & 
                                                                                                 data_tmp$who_dehydrat_cat %in% c("No", "Some"), "age"]))))


# WHO overtreat (hypothetical)
mean_YLL_among_dead_who_male_overtreat <- mean(resid_life_interp_fxn_male(as.numeric(unlist(data_tmp[data_tmp$male == 1 & data_tmp$who_dehydrat_cat == "Severe" &
                                                                                                       data_tmp$actual_dehydrat_cat %in% c("No", "Some"), "age"]))))

mean_YLL_among_dead_who_female_overtreat <- mean(resid_life_interp_fxn_female(as.numeric(unlist(data_tmp[data_tmp$male == 0 & data_tmp$who_dehydrat_cat == "Severe" &
                                                                                                           data_tmp$actual_dehydrat_cat %in% c("No", "Some"), "age"]))))

# nirudak & who 
total_YLL_among_dead_who <- (mean_YLL_among_dead_who_female + mean_YLL_among_dead_who_male)/2
total_YLL_among_dead_nirudak <- (mean_YLL_among_dead_nirudak_female + mean_YLL_among_dead_nirudak_male)/2

total_YLL_among_dead_who_overtreat <- (mean_YLL_among_dead_who_female_overtreat + mean_YLL_among_dead_who_male_overtreat)/2
total_YLL_among_dead_nirudak_overtreat <- (mean_YLL_among_dead_nirudak_female_overtreat + mean_YLL_among_dead_nirudak_male_overtreat)/2


nirudak_YLL_vec <- rep(0, 9)

nirudak_YLL_vec[7:8] <- total_YLL_among_dead_nirudak
nirudak_YLL_vec[c(3,6)] <- total_YLL_among_dead_nirudak_overtreat
names(nirudak_YLL_vec) <- names(joint_prob_vec_nirudak_orig)

who_YLL_vec <- rep(0, 9)
who_YLL_vec[7:8] <- total_YLL_among_dead_who
who_YLL_vec[c(3,6)] <- total_YLL_among_dead_who_overtreat
names(who_YLL_vec) <- names(joint_prob_vec_who_orig)

# whole sample
mean_YLL_among_dead_all_male <- mean(resid_life_interp_fxn_male(as.numeric(unlist(data_tmp[data_tmp$male == 1 & data_tmp$actual_dehydrat_cat == "Severe","age"]))))

mean_YLL_among_dead_all_female <- mean(resid_life_interp_fxn_female(as.numeric(unlist(data_tmp[data_tmp$male == 0 & data_tmp$actual_dehydrat_cat == "Severe", "age"]))))

total_YLL_among_dead_all <- (mean_YLL_among_dead_all_male + mean_YLL_among_dead_all_female)/2

all_YLL_vec <- rep(0, 9)
all_YLL_vec[7:8] <- total_YLL_among_dead_all
all_YLL_vec[c(3,6)] <- total_YLL_among_dead_all
names(all_YLL_vec) <- names(joint_prob_vec_nirudak_orig)

# disutility aka years lost to disability (YLD) — seizures from Sharifi paper
nirudak_YLD_vec <- rep(0, 9)
who_YLD_vec <- rep(0, 9)
names(nirudak_YLD_vec) <- names(joint_prob_vec_nirudak_orig)
names(who_YLD_vec) <- names(joint_prob_vec_who_orig)
# for now, assume no disutility
# morbidity (YLD) — setting to 0 for now
branch_morbidity_prob <- rep(0,9)
names(branch_morbidity_prob) <- names(joint_prob_vec_nirudak_orig)

branch_death_prob <- rep(0,9)
names(branch_death_prob) <- names(joint_prob_vec_nirudak_orig)
# can change the value below if we make the assumption less crazy
prob_die_undertreat <- 1
branch_death_prob[7:8] <- prob_die_undertreat

# using the mean life years lost calculated based on the age of people undertreated by each
# specific model
mean_YLL_nirudak_specific <- as.numeric((joint_prob_vec_nirudak_tmp * branch_death_prob) %*% nirudak_YLL_vec)
mean_YLL_who_specific <- as.numeric((joint_prob_vec_who_tmp * branch_death_prob) %*% who_YLL_vec)

# using the mean life years lost calculated based on the age of everyone who had Severe dehydration
# this gives us the same age distribution across both models (both arms)
mean_YLL_nirudak_all <- as.numeric((joint_prob_vec_nirudak_tmp * branch_death_prob) %*% all_YLL_vec)
mean_YLL_who_all <- as.numeric((joint_prob_vec_who_tmp * branch_death_prob) %*% all_YLL_vec)

# for YLD (years lost to disability)
mean_YLD_nirudak <- as.numeric((joint_prob_vec_nirudak_tmp * branch_morbidity_prob) %*% nirudak_YLD_vec)
mean_YLD_who <- as.numeric((joint_prob_vec_who_tmp * branch_morbidity_prob) %*% who_YLD_vec)

mean_DALYs_lost_nirudak_specific <- sum(mean_YLL_nirudak_specific, mean_YLD_nirudak)
mean_DALYs_lost_who_specific <- sum(mean_YLL_who_specific, mean_YLD_who)

mean_DALYs_lost_nirudak_all <- sum(mean_YLL_nirudak_all, mean_YLD_nirudak)
mean_DALYs_lost_who_all <- sum(mean_YLL_who_all, mean_YLD_who)

mean_dalys_boot_mat_all[i,] <- c(mean_DALYs_lost_who_all, mean_DALYs_lost_nirudak_all)
mean_dalys_boot_mat_specific[i,] <- c(mean_DALYs_lost_who_specific, mean_DALYs_lost_nirudak_specific)

# to calculate productivity costs
mean_YLL_boot_mat_all[i,] <- c(mean_YLL_who_all, mean_YLL_nirudak_all)
mean_YLL_boot_mat_specific[i,] <- c(mean_YLL_who_specific, mean_YLL_nirudak_specific)
}

################################################################## END BOOTSTRAP

colnames(joint_prob_vec_who_boot_mat) <- colnames(joint_prob_vec_nirudak_boot_mat) <- names(joint_prob_vec_nirudak_tmp)

####Calculate DALYs
# more DALYs are bad
# first arg = who, second arg = nirudak
incremental_mean_dalys_all <- mean_dalys_boot_mat_all[,2] - mean_dalys_boot_mat_all[,1]
incremental_mean_dalys_spec <- mean_dalys_boot_mat_specific[,2] - mean_dalys_boot_mat_specific[,1]

####Calculate Costs

##make vectors for summary stats

##Unit Costs

#i=1

# the lines below give us the total ORS or IVF costs for all arms (for each model)
# SCENARIO A
# assuming model predicted fluid usage (assuming use of only one type of fluid)
# NIRUDAK puts more people in the Severe category but predicts less fluid for those people than WHO (however, NIRUDAK's hospital
# costs will be higher b/c it puts more people in the Severe category)
ref_who_pred_total_ORS_mean_costs_vec <- mean_cost_boot_mat[, "mean_ORS_fluid_costs_who"]
ref_nirudak_pred_total_ORS_mean_costs_vec <- mean_cost_boot_mat[, "mean_ORS_fluid_costs_nirudak"]
ref_who_pred_total_IV_mean_costs_vec <- mean_cost_boot_mat[, "mean_IV_fluid_costs_who"]
ref_nirudak_pred_total_IV_mean_costs_vec <- mean_cost_boot_mat[, "mean_IV_fluid_costs_nirudak"]

# SCENARIO B
# assuming real-life fluid usage
# NIRUDAK more expensive here b/c gives more people IV fluid and in real life IVF usage is high
scen_who_pred_total_ORS_mean_costs_vec <- mean_cost_boot_mat[, c("mean_ORS_fluid_costs_act_who")]
scen_nirudak_pred_total_ORS_mean_costs_vec <- mean_cost_boot_mat[, c("mean_ORS_fluid_costs_act_nirudak")] 
scen_who_pred_total_IV_mean_costs_vec <- mean_cost_boot_mat[, c("mean_IV_fluid_costs_act_who")]
scen_nirudak_pred_total_IV_mean_costs_vec <- mean_cost_boot_mat[, c("mean_IV_fluid_costs_act_nirudak")] 

# only 1 scenario for hosp costs
ref_who_mean_hosp_costs_vec <- mean_cost_boot_mat[,"mean_hosp_costs_severe_only_who"]
ref_nirudak_mean_hosp_costs_vec <- mean_cost_boot_mat[,"mean_hosp_costs_severe_only_nirudak"]

# assume that people lose productive time while severely dehydrated but that would be equally distributed among arms
# but people would differentially lose productivity if they die (as a result of having Severe dehydration but being treated as Some or None)

# REPLACE WITH ACTUAL VALUE
mean_annual_wage <- 200000

# discounting will bring down the impact of productivity costs on total costs
# productivity costs are one form of health costs
productivity_costs_nirduak_specific_vec <- mean_YLL_boot_mat_specific[,2]*mean_annual_wage
productivity_costs_who_specific_vec <- mean_YLL_boot_mat_specific[,1]*mean_annual_wage

productivity_costs_nirduak_all_vec <- mean_YLL_boot_mat_all[,2]*mean_annual_wage
productivity_costs_who_all_vec <- mean_YLL_boot_mat_all[,1]*mean_annual_wage

# fluid has 2 scenarios
#### ref
#### scen
# hospital has 1 scenarios
# productivity has 2 scenarios
#### specific
#### all

# REF — societal perspective (meaning including productivity)
mean_total_cost_nirduak_ref_all_vec <- productivity_costs_nirduak_all_vec + ref_nirudak_mean_hosp_costs_vec +
  ref_nirudak_pred_total_ORS_mean_costs_vec + ref_nirudak_pred_total_IV_mean_costs_vec

mean_total_cost_nirduak_ref_spec_vec <- productivity_costs_nirduak_specific_vec + ref_nirudak_mean_hosp_costs_vec +
  ref_nirudak_pred_total_ORS_mean_costs_vec + ref_nirudak_pred_total_IV_mean_costs_vec

mean_total_cost_who_ref_all_vec <- productivity_costs_who_all_vec + ref_who_mean_hosp_costs_vec + 
  ref_who_pred_total_ORS_mean_costs_vec + ref_who_pred_total_IV_mean_costs_vec

mean_total_cost_who_ref_spec_vec <- productivity_costs_who_specific_vec + ref_who_mean_hosp_costs_vec + 
  ref_who_pred_total_ORS_mean_costs_vec + ref_who_pred_total_IV_mean_costs_vec


# REF - this is the healthcare perspective (meaning EXcluding productivity)
mean_total_cost_nirduak_ref_all_vec_hcs <-ref_nirudak_mean_hosp_costs_vec +
  ref_nirudak_pred_total_ORS_mean_costs_vec + ref_nirudak_pred_total_IV_mean_costs_vec

mean_total_cost_nirduak_ref_spec_vec_hcs <- ref_nirudak_mean_hosp_costs_vec +
  ref_nirudak_pred_total_ORS_mean_costs_vec + ref_nirudak_pred_total_IV_mean_costs_vec

mean_total_cost_who_ref_all_vec_hcs <- ref_who_mean_hosp_costs_vec + 
  ref_who_pred_total_ORS_mean_costs_vec + ref_who_pred_total_IV_mean_costs_vec

mean_total_cost_who_ref_spec_vec_hcs <- ref_who_mean_hosp_costs_vec + 
  ref_who_pred_total_ORS_mean_costs_vec + ref_who_pred_total_IV_mean_costs_vec

# incremental costs - societal (w/ productivity)
incremental_mean_total_cost_ref_all <- mean_total_cost_nirduak_ref_all_vec - mean_total_cost_who_ref_all_vec
  
incremental_mean_total_cost_ref_spec <- mean_total_cost_nirduak_ref_spec_vec - mean_total_cost_who_ref_spec_vec

# incremental costs - healthcare (w/out productivity)
incremental_mean_total_cost_ref_all_hcs <- mean_total_cost_nirduak_ref_all_vec_hcs - mean_total_cost_who_ref_all_vec_hcs

incremental_mean_total_cost_ref_spec_hcs <- mean_total_cost_nirduak_ref_spec_vec_hcs - mean_total_cost_who_ref_spec_vec_hcs

############

# SCEN - societal perspective (including productivity)
mean_total_cost_nirduak_scen_all_vec <- productivity_costs_nirduak_all_vec + ref_nirudak_mean_hosp_costs_vec +
  scen_nirudak_pred_total_ORS_mean_costs_vec + scen_nirudak_pred_total_IV_mean_costs_vec

mean_total_cost_nirduak_scen_spec_vec <- productivity_costs_nirduak_specific_vec + ref_nirudak_mean_hosp_costs_vec +
  scen_nirudak_pred_total_ORS_mean_costs_vec + scen_nirudak_pred_total_IV_mean_costs_vec

mean_total_cost_who_scen_all_vec <- productivity_costs_who_all_vec + ref_who_mean_hosp_costs_vec + 
  scen_who_pred_total_ORS_mean_costs_vec + scen_who_pred_total_IV_mean_costs_vec

mean_total_cost_who_scen_spec_vec <- productivity_costs_who_specific_vec + ref_who_mean_hosp_costs_vec + 
  scen_who_pred_total_ORS_mean_costs_vec + scen_who_pred_total_IV_mean_costs_vec

# SCEN - healthcare sector perspective (excluding productivity)
mean_total_cost_nirduak_scen_all_vec_hcs <- ref_nirudak_mean_hosp_costs_vec +
  scen_nirudak_pred_total_ORS_mean_costs_vec + scen_nirudak_pred_total_IV_mean_costs_vec

mean_total_cost_nirduak_scen_spec_vec_hcs <- ref_nirudak_mean_hosp_costs_vec +
  scen_nirudak_pred_total_ORS_mean_costs_vec + scen_nirudak_pred_total_IV_mean_costs_vec

mean_total_cost_who_scen_all_vec_hcs <- ref_who_mean_hosp_costs_vec + 
  scen_who_pred_total_ORS_mean_costs_vec + scen_who_pred_total_IV_mean_costs_vec

mean_total_cost_who_scen_spec_vec_hcs <- ref_who_mean_hosp_costs_vec + 
  scen_who_pred_total_ORS_mean_costs_vec + scen_who_pred_total_IV_mean_costs_vec


# incremental costs - societal
incremental_mean_total_cost_scen_all <- mean_total_cost_nirduak_scen_all_vec - mean_total_cost_who_scen_all_vec
  
incremental_mean_total_cost_scen_spec <- mean_total_cost_nirduak_scen_spec_vec - mean_total_cost_who_scen_spec_vec

# incremental costs - healthcare sector
incremental_mean_total_cost_scen_all_hcs <- mean_total_cost_nirduak_scen_all_vec_hcs - mean_total_cost_who_scen_all_vec_hcs

incremental_mean_total_cost_scen_spec_hcs <- mean_total_cost_nirduak_scen_spec_vec_hcs - mean_total_cost_who_scen_spec_vec_hcs


####Calculate Net Monetary Benefit 

# to-do: figure out value in Bangladesh currency
daly_wtp_vec <- seq(25000, 200000, by = 25000)

# societal perspective
increm_net_monetary_benefit_ref_all_mat <- matrix(data = NA, nrow = B, ncol = length(daly_wtp_vec))
increm_net_monetary_benefit_ref_spec_mat <- matrix(data = NA, nrow = B, ncol = length(daly_wtp_vec))
increm_net_monetary_benefit_scen_all_mat <- matrix(data = NA, nrow = B, ncol = length(daly_wtp_vec))
increm_net_monetary_benefit_scen_spec_mat <- matrix(data = NA, nrow = B, ncol = length(daly_wtp_vec))

# healthcare sector perspective
increm_net_monetary_benefit_ref_all_mat_hcs <- matrix(data = NA, nrow = B, ncol = length(daly_wtp_vec))
increm_net_monetary_benefit_ref_spec_mat_hcs <- matrix(data = NA, nrow = B, ncol = length(daly_wtp_vec))
increm_net_monetary_benefit_scen_all_mat_hcs <- matrix(data = NA, nrow = B, ncol = length(daly_wtp_vec))
increm_net_monetary_benefit_scen_spec_mat_hcs <- matrix(data = NA, nrow = B, ncol = length(daly_wtp_vec))

colnames(increm_net_monetary_benefit_ref_all_mat) <- 
  colnames(increm_net_monetary_benefit_ref_spec_mat) <- 
  colnames(increm_net_monetary_benefit_scen_all_mat) <- 
  colnames(increm_net_monetary_benefit_scen_spec_mat) <- 
  colnames(increm_net_monetary_benefit_ref_all_mat_hcs) <- 
  colnames(increm_net_monetary_benefit_ref_spec_mat_hcs) <- 
  colnames(increm_net_monetary_benefit_scen_all_mat_hcs) <- 
  colnames(increm_net_monetary_benefit_scen_spec_mat_hcs) <- paste('wtp', daly_wtp_vec, sep='=')


for (i in 1:length(daly_wtp_vec)){
  
temp_wtp <- daly_wtp_vec[i]

# dollar value of benefit minus dollar value of cost
# incremental DALYs and costs are defined as NIRUDAK minus WHO
## can either do this as opportunity cost of money or via the demand-side
## taking negative DALYs -> positive = more effective
## positive incremental costs = more expensive
## standardized for how QALYs are done

#### societal perspective (including productivity)
increm_net_monetary_benefit_ref_all_mat[,i] <- -incremental_mean_dalys_all*temp_wtp  - incremental_mean_total_cost_ref_all # taking the negative gives us incremental DALYs saved
increm_net_monetary_benefit_ref_spec_mat[,i] <- -incremental_mean_dalys_spec*temp_wtp - incremental_mean_total_cost_ref_spec

increm_net_monetary_benefit_scen_all_mat[,i] <- -incremental_mean_dalys_all*temp_wtp - incremental_mean_total_cost_scen_all
increm_net_monetary_benefit_scen_spec_mat[,i] <- -incremental_mean_dalys_spec*temp_wtp - incremental_mean_total_cost_scen_spec

#### healthcare perspective (excluding productivity)
increm_net_monetary_benefit_ref_all_mat_hcs[,i] <- -incremental_mean_dalys_all*temp_wtp  - incremental_mean_total_cost_ref_all_hcs # taking the negative gives us incremental DALYs saved
increm_net_monetary_benefit_ref_spec_mat_hcs[,i] <- -incremental_mean_dalys_spec*temp_wtp - incremental_mean_total_cost_ref_spec_hcs

increm_net_monetary_benefit_scen_all_mat_hcs[,i] <- -incremental_mean_dalys_all*temp_wtp - incremental_mean_total_cost_scen_all_hcs
increm_net_monetary_benefit_scen_spec_mat_hcs[,i] <- -incremental_mean_dalys_spec*temp_wtp - incremental_mean_total_cost_scen_spec_hcs


}

# calculated expected incremental net monetary benefit - societal perspective // nmb = net monetary benefit
expected_increm_nmb_ref_all <- colMeans(increm_net_monetary_benefit_ref_all_mat)
expected_increm_nmb_ref_spec <- colMeans(increm_net_monetary_benefit_ref_spec_mat)
expected_increm_nmb_scen_all <- colMeans(increm_net_monetary_benefit_scen_all_mat)
expected_increm_nmb_scen_spec <- colMeans(increm_net_monetary_benefit_scen_spec_mat)

# calculate probability optimal strategies - societal perspective
prob_nirudak_optimal_vec_ref_all <- colMeans(increm_net_monetary_benefit_ref_all_mat > 0)
prob_nirudak_optimal_vec_ref_spec <- colMeans(increm_net_monetary_benefit_ref_spec_mat > 0)
prob_nirudak_optimal_vec_scen_all <- colMeans(increm_net_monetary_benefit_scen_all_mat > 0)
prob_nirudak_optimal_vec_scen_spec <- colMeans(increm_net_monetary_benefit_scen_spec_mat > 0)

# calculated expected incremental net monetary benefit - healthcare perspective
# if the values on the left are positive, NIRUDAK is favored
expected_increm_nmb_ref_all_hcs <- colMeans(increm_net_monetary_benefit_ref_all_mat_hcs)
expected_increm_nmb_ref_spec_hcs <- colMeans(increm_net_monetary_benefit_ref_spec_mat_hcs)
expected_increm_nmb_scen_all_hcs <- colMeans(increm_net_monetary_benefit_scen_all_mat_hcs)
expected_increm_nmb_scen_spec_hcs <- colMeans(increm_net_monetary_benefit_scen_spec_mat_hcs)
# calculate probability optimal strategies - healthcare perspective
prob_nirudak_optimal_vec_ref_all_hcs <- colMeans(increm_net_monetary_benefit_ref_all_mat_hcs > 0)
prob_nirudak_optimal_vec_ref_spec_hcs <- colMeans(increm_net_monetary_benefit_ref_spec_mat_hcs > 0)
prob_nirudak_optimal_vec_scen_all_hcs <- colMeans(increm_net_monetary_benefit_scen_all_mat_hcs > 0)
prob_nirudak_optimal_vec_scen_spec_hcs <- colMeans(increm_net_monetary_benefit_scen_spec_mat_hcs > 0)

####Making the Plot
# plot cost effectiveness acceptability curve (CEAC) & cost-effectiveness frontier (CEAF)

#pdf(file = file.path(plots_dir, paste("reference_CEAF_discounted", "pdf", sep=".")), 
 #   onefile = T, height = 6.5, width = 7.3, fonts = 'Times')

# replace with PPP
bdt_usd_conversion <- 0.0084

daly_wtp_vec_usd <- daly_wtp_vec * bdt_usd_conversion

hcs_ref_all_nirudak_best_vec <- ifelse(expected_increm_nmb_ref_all_hcs > 0, 1, 0)

# version of analysis being plotted: healthcare perspective, ref (model determines type and amount of fluid), all (revist what this means)
plot(daly_wtp_vec_usd, prob_nirudak_optimal_vec_ref_all_hcs, ylim=c(0, 1), xlim=c(0, 2000), pch=20, cex=1.5, col='black',
     xlab="WTP ($)", ylab="Prob Strategy is Optimal", main='CEAC & CEAF for Cohort')
# the above shows at each WTP, which has the highest probability
lines(daly_wtp_vec_usd, prob_nirudak_optimal_vec_ref_all_hcs, lty=1, col='black')

# to be adapted
lines(c(275, 300), c(mean(tail(prob_nirudak_optimal_vec_ref_all_hcs, 2)), tail(MCI_societal_discounted_prob, 1)), lty=1, col='black')
points(daly_wtp_vec, (1-prob_nirudak_optimal_vec_ref_all_hcs), pch=17, cex=0.95, col='black')
lines(head(daly_wtp_vec, -1), head((1-MCI_societal_discounted_prob), -1), lty=1, col='black')
lines(c(250, 275), c((1-MCI_societal_discounted_prob)[5], mean((1-MCI_societal_discounted_prob)[5:6])), lty=1, col='black')
lines(c(275, 300), c(mean(tail(1-MCI_societal_discounted_prob, 2)), tail(1-MCI_societal_discounted_prob, 1)), lty=3, col='black')
points(daly_wtp_vec, MCI_HCS_discounted_prob, pch=20, cex=1.5, col='red')
lines(daly_wtp_vec, MCI_HCS_discounted_prob, lty=2, col='red')
points(daly_wtp_vec, (1-MCI_HCS_discounted_prob), pch=17, cex=0.95, col='red')
lines(daly_wtp_vec, (1-MCI_HCS_discounted_prob), lty=1, col='red')
legend('left', legend=c("Probability TAU is Optimal (Societal, r=3%)", 
                        "Probability Lecanemab is Optimal (Societal, r=3%)",
                        "Probability TAU is Optimal (HCS, r=3%)",
                        "Probability Lecanemab is Optimal (HCS, r=3%)"), 
       col=c(rep('black', 2), rep('red', 2)), pch=c(17, 20, 17, 20))

# also want to have a version of the cost without productivity (health care perspective)

# Jonah to make into function & add discounting










# next: incremental DALYs
# convert cost to health (instead of ICER)


# calculating fluid costs for NIRUDAK based on model predicted dehydration categories
# these next two items are just sanity checks
#NIRUDAK_fluid_cost = c(
#((data_use$nirudak_volume_deficit * 1000 * 0.104) + 32.59)*(data_use$nirudak_dehydrat_cat == "Severe"),
#(data_use$nirudak_volume_deficit * 1000 * 0.0054)*(data_use$nirudak_dehydrat_cat == "Some"),
#rep(0, length(which(data_use$nirudak_dehydrat_cat == "No"))))

#WHO_fluid_cost = c(
#  ((data_use$who_volume_deficit * 1000 * 0.104) + 32.59)*(data_use$who_dehydrat_cat == "Severe"),
#  (data_use$who_volume_deficit * 1000 * 0.0054)*(data_use$who_dehydrat_cat == "Some"),
#  rep(0, length(which(data_use$who_dehydrat_cat == "No"))))

################################################################################





# implementing DALYs with real data first

# nirudak
mean_YLL_among_dead_nirudak_male <- mean(resid_life_interp_fxn_male(as.numeric(unlist(data_use[data_use$male == 1 & data_use$actual_dehydrat_cat == "Severe" &
           data_use$nirudak_dehydrat_cat %in% c("No", "Some"), "age"]))))

mean_YLL_among_dead_nirudak_female <- mean(resid_life_interp_fxn_female(as.numeric(unlist(data_use[data_use$male == 0 & data_use$actual_dehydrat_cat == "Severe" &
                                                                                                 data_use$nirudak_dehydrat_cat %in% c("No", "Some"), "age"]))))

# hypothetical overtreat NIRUDAK
mean_YLL_among_dead_nirudak_male_overtreat <- mean(resid_life_interp_fxn_male(as.numeric(unlist(data_use[data_use$male == 1 & data_use$nirudak_dehydrat_cat == "Severe" &
                                                                                                 data_use$actual_dehydrat_cat %in% c("No", "Some"), "age"]))))

mean_YLL_among_dead_nirudak_female_overtreat <- mean(resid_life_interp_fxn_female(as.numeric(unlist(data_use[data_use$male == 0 & data_use$nirudak_dehydrat_cat == "Severe" &
                                                                                                     data_use$actual_dehydrat_cat %in% c("No", "Some"), "age"]))))


# who
mean_YLL_among_dead_who_male <- mean(resid_life_interp_fxn_male(as.numeric(unlist(data_use[data_use$male == 1 & data_use$actual_dehydrat_cat == "Severe" &
                                                                                                 data_use$who_dehydrat_cat %in% c("No", "Some"), "age"]))))

mean_YLL_among_dead_who_female <- mean(resid_life_interp_fxn_female(as.numeric(unlist(data_use[data_use$male == 0 & data_use$actual_dehydrat_cat == "Severe" & 
                                                                                                 data_use$who_dehydrat_cat %in% c("No", "Some"), "age"]))))


# WHO overtreat (hypothetical)
mean_YLL_among_dead_who_male_overtreat <- mean(resid_life_interp_fxn_male(as.numeric(unlist(data_use[data_use$male == 1 & data_use$who_dehydrat_cat == "Severe" &
                                                                                                 data_use$actual_dehydrat_cat %in% c("No", "Some"), "age"]))))

mean_YLL_among_dead_who_female_overtreat <- mean(resid_life_interp_fxn_female(as.numeric(unlist(data_use[data_use$male == 0 & data_use$who_dehydrat_cat == "Severe" &
                                                                                                     data_use$actual_dehydrat_cat %in% c("No", "Some"), "age"]))))

# nirudak & who 
total_YLL_among_dead_who <- (mean_YLL_among_dead_who_female + mean_YLL_among_dead_who_male)/2
total_YLL_among_dead_nirudak <- (mean_YLL_among_dead_nirudak_female + mean_YLL_among_dead_nirudak_male)/2

total_YLL_among_dead_who_overtreat <- (mean_YLL_among_dead_who_female_overtreat + mean_YLL_among_dead_who_male_overtreat)/2
total_YLL_among_dead_nirudak_overtreat <- (mean_YLL_among_dead_nirudak_female_overtreat + mean_YLL_among_dead_nirudak_male_overtreat)/2


nirudak_YLL_vec <- rep(0, 9)

nirudak_YLL_vec[7:8] <- total_YLL_among_dead_nirudak
nirudak_YLL_vec[c(3,6)] <- total_YLL_among_dead_nirudak_overtreat
names(nirudak_YLL_vec) <- names(joint_prob_vec_nirudak_orig)

who_YLL_vec <- rep(0, 9)
who_YLL_vec[7:8] <- total_YLL_among_dead_who
who_YLL_vec[c(3,6)] <- total_YLL_among_dead_who_overtreat
names(who_YLL_vec) <- names(joint_prob_vec_who_orig)

# whole sample
mean_YLL_among_dead_all_male <- mean(resid_life_interp_fxn_male(as.numeric(unlist(data_use[data_use$male == 1 & data_use$actual_dehydrat_cat == "Severe","age"]))))

mean_YLL_among_dead_all_female <- mean(resid_life_interp_fxn_female(as.numeric(unlist(data_use[data_use$male == 0 & data_use$actual_dehydrat_cat == "Severe", "age"]))))

total_YLL_among_dead_all <- (mean_YLL_among_dead_all_male + mean_YLL_among_dead_all_female)/2

all_YLL_vec <- rep(0, 9)
all_YLL_vec[7:8] <- total_YLL_among_dead_all
all_YLL_vec[c(3,6)] <- total_YLL_among_dead_all
names(all_YLL_vec) <- names(joint_prob_vec_nirudak_orig)

# disutility aka years lost to disability (YLD) — seizures from Sharifi paper
nirudak_YLD_vec <- rep(0, 9)
who_YLD_vec <- rep(0, 9)
names(nirudak_YLD_vec) <- names(joint_prob_vec_nirudak_orig)
names(who_YLD_vec) <- names(joint_prob_vec_who_orig)
# for now, assume no disutility
# morbidity (YLD) — setting to 0 for now
branch_morbidity_prob <- rep(0,9)
names(branch_morbidity_prob) <- names(joint_prob_vec_nirudak_orig)

branch_death_prob <- rep(0,9)
names(branch_death_prob) <- names(joint_prob_vec_nirudak_orig)
# can change the value below if we make the assumption less crazy
prob_die_undertreat <- 1
branch_death_prob[7:8] <- prob_die_undertreat

# using the mean life years lost calculated based on the age of people undertreated by each
# specific model
mean_YLL_nirudak_specific <- as.numeric((joint_prob_vec_nirudak_orig * branch_death_prob) %*% nirudak_YLL_vec)
mean_YLL_who_specific <- as.numeric((joint_prob_vec_who_orig * branch_death_prob) %*% who_YLL_vec)

# using the mean life years lost calculated based on the age of everyone who had Severe dehydration
# this gives us the same age distribution across both models (both arms)
mean_YLL_nirudak_all <- as.numeric((joint_prob_vec_nirudak_orig * branch_death_prob) %*% all_YLL_vec)
mean_YLL_who_all <- as.numeric((joint_prob_vec_who_orig * branch_death_prob) %*% all_YLL_vec)

# for YLD
mean_YLD_nirudak <- as.numeric((joint_prob_vec_nirudak_orig * branch_morbidity_prob) %*% nirudak_YLD_vec)
mean_YLD_who <- as.numeric((joint_prob_vec_who_orig * branch_morbidity_prob) %*% who_YLD_vec)

mean_DALYs_lost_nirudak_specific <- sum(mean_YLL_nirudak_specific, mean_YLD_nirudak)
mean_DALYs_lost_who_specific <- sum(mean_YLL_who_specific, mean_YLD_who)

mean_DALYs_lost_nirudak_all <- sum(mean_YLL_nirudak_all, mean_YLD_nirudak)
mean_DALYs_lost_who_all <- sum(mean_YLL_who_all, mean_YLD_who)

# will at some point need to go back and change ICER to be consistent new assumptions made here

# note in manuscript: YLL is the mean among all patients of a branch in which patients die
# e.g., true Severe dehydration but model-assigned "No" or "Some" dehydration
# check Jonah's code w/ statistical tests comparing the distribution of ages
# across branches between the two models
# he did find statistically sig. differences in age between branches (in exploratory_analysis_of_trial_data.R)
# can run the analysis both ways (with mean YLL across whole sample and with average age in each branch)

# YLD is based on patients in the Sharifi paper who were hypernatremic (high sodium levels)
# 234 patients got IV fluid, of those 24 

##Actual
IV_fluid_prior_to_admit_wt

data <- 

#calculate total fluid for IV and 

data %>% select(str_subset(names(data), "IV")) %>% rowSums()




data$iv_fluid_prior_to_admit_wt %>% summary()

names(data)




#productivity costs
no_dehyd_lost_hours = 3
some_dehyd_lost_hours = 9
severe_dehyrd_lost_hours_tmp = 3









