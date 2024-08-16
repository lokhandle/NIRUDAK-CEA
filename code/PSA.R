#user = 'Jonah'
user = 'Anagha'

##load packages
library(tidyverse)


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



B <- 50 #number of bootstrap samples 
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
# detailed approach: for anyone who died - mean income per year (mean(data_use$monthly_income))*12 - every year of life they lose, they lose that amount
# assume that people with Severe dehydration lose work whether they're in the hospital or not, so the model does not affect short term productivity costs

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

}

################################################################## END BOOTSTRAP

colnames(joint_prob_vec_who_boot_mat) <- colnames(joint_prob_vec_nirudak_boot_mat) <- names(joint_prob_vec_nirudak_tmp)


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

# will at some point need to go back and change ICER to be consistent new assumptions made here

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









