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
N <- nrow(data_use) #number of obsevations per bootstrap dataset
joint_prob_vec_who_boot_mat <- joint_prob_vec_nirudak_boot_mat <- matrix(NA, nrow=B, ncol=9)
row_indices <- 1:N
variable_mean_cost_boot_mat <- data.frame(matrix(NA, nrow=B, ncol=12))
colnames(variable_mean_cost_boot_mat) <- c("mean_hosp_costs", "mean_productivity_costs", 
					"mean_variable_ORS_fluid_costs_actual", "mean_variable_IV_fluid_costs_actual", 
					"mean_variable_ORS_fluid_costs_who", "mean_variable_ORS_fluid_costs_nirudak", 
					 "mean_variable_IV_fluid_costs_who", "mean_variable_IV_fluid_costs_nirudak", 
					"mean_variable_ORS_fluid_costs_act_who", "mean_variable_ORS_fluid_costs_act_nirudak",
					"mean_variable_IV_fluid_costs_act_who", "mean_variable_IV_fluid_costs_act_nirudak")

for (i in 1:B){

test_sample <- sample.int(N, size = N, replace = TRUE)
data_tmp <- data_use[test_sample, ]

cost_data_tmp <- as.data.frame(data_tmp[, c("hosp_los", "wage_lost_hosp_stay", "total_ORS_fluid", "total_IV_fluid", 
			"nirudak_volume_deficit", "who_volume_deficit", "actual_dehydrat_cat")])

##Calculate Mean costs

#hospital costs
hosp_cost_per_hour <- 2573.1/24
hosp_los_hours_use <- cost_data_tmp$hosp_los
hosp_total_costs_if_severe <- hosp_cost_per_hour * hosp_los_hours_use 
variable_mean_cost_boot_mat[i, "mean_hosp_costs"] <- mean(hosp_total_costs_if_severe, na.rm=T)
#productivity costs
variable_mean_cost_boot_mat[i, "mean_productivity_costs"] <- mean(cost_data_tmp$wage_lost_hosp_stay, na.rm=T)
#ORS fluid variable costs
ORS_fluid_cost_per_ml <- 0.0054
total_ORS_variable_fluid_costs_actual <- cost_data_tmp$total_ORS_fluid * ORS_fluid_cost_per_ml 
variable_mean_cost_boot_mat[i, "mean_variable_ORS_fluid_costs_actual"] <- mean(total_ORS_variable_fluid_costs_actual, na.rm=T)
#IV fluid variable costs
IV_fluid_cost_per_ml <- 0.104
total_IV_variable_fluid_costs_actual <- cost_data_tmp$total_IV_fluid * IV_fluid_cost_per_ml 
variable_mean_cost_boot_mat[i, "mean_variable_IV_fluid_costs_actual"] <- mean(total_IV_variable_fluid_costs_actual, na.rm=T)
#model predicted fluid
#convert to $ using ml cost and condition on having 'some' for ORS
# calculating costs over the entire sample based on the amount/type of fluid prescribed by WHO (for SOME)
who_predicted_ORS_costs <- as.numeric(data_tmp$who_volume_deficit) * 1000 * ORS_fluid_cost_per_ml * 
					as.integer(data_tmp$who_dehydrat_cat == 'Some')
variable_mean_cost_boot_mat[i, "mean_variable_ORS_fluid_costs_who"] <- mean(who_predicted_ORS_costs, na.rm=T)

# calculating costs over the entire sample based on the amount/type of fluid prescribed by NIRUDAK (for SOME)
nirudak_predicted_ORS_costs <- as.numeric(data_tmp$nirudak_volume_deficit) * 1000 * ORS_fluid_cost_per_ml * 
					as.integer(data_tmp$nirudak_dehydrat_cat == 'Some')
variable_mean_cost_boot_mat[i, "mean_variable_ORS_fluid_costs_nirudak"] <- mean(nirudak_predicted_ORS_costs, na.rm=T)

# calculating costs over the entire sample based on the amount/type of fluid prescribed by WHO (for SEVERE)
who_predicted_IV_costs <- as.numeric(data_tmp$who_volume_deficit) * 1000 * IV_fluid_cost_per_ml * 
					as.integer(data_tmp$who_dehydrat_cat == 'Severe')
variable_mean_cost_boot_mat[i, "mean_variable_IV_fluid_costs_who"] <- mean(who_predicted_IV_costs, na.rm=T)

# calculating costs over the entire sample based on the amount/type of fluid prescribed by NIRUDAK (for SEVERE)
nirudak_predicted_IV_costs <- as.numeric(data_tmp$nirudak_volume_deficit) * 1000 * IV_fluid_cost_per_ml * 
					as.integer(data_tmp$nirudak_dehydrat_cat == 'Severe')
variable_mean_cost_boot_mat[i, "mean_variable_IV_fluid_costs_nirudak"] <- mean(nirudak_predicted_IV_costs, na.rm=T)

# calculating mean variable fluid cost over entire sample assuming that providers tailor the amount of fluid to the model predicted
# dehyrdation category and they give the patient the average amount of fluid that was actually given to all patients in the corresponding true
# dehydration category, with the exception that only patients identified by the model as severely dehydrated get IVF
# here, the model is used to identify dehydration category and hospitalization status (e.g., Severe goes to the hospital)
# and the type of fluid received (e.g., Severe gets IVF)
# then everyone in that model predicted dehydration category will be treated like what was observed in the real hospital in Bangladesh (with the exception that
# only patients in a model-predicted Severe dehydration category get IVF)

# ORS - WHO
variable_mean_cost_boot_mat[i, c("mean_variable_ORS_fluid_costs_act_who")] <-
				as.numeric(as.numeric(prop.table(table(data_tmp$who_dehydrat_cat))[c(1, 3, 2)]) %*% # proportion (according to WHO) in each dehydration category
				(ORS_fluid_cost_per_ml * as.numeric(with(cost_data_tmp, by(total_ORS_fluid, actual_dehydrat_cat, mean))[c(1, 3, 2)]))) # mean ORS fluid used in each true disease category
# ORS - NIRUDAK
variable_mean_cost_boot_mat[i, c("mean_variable_ORS_fluid_costs_act_nirudak")] <-
					as.numeric(as.numeric(prop.table(table(data_tmp$nirudak_dehydrat_cat))[c(1, 3, 2)]) %*%
				(ORS_fluid_cost_per_ml * as.numeric(with(cost_data_tmp, by(total_ORS_fluid, actual_dehydrat_cat, mean))[c(1, 3, 2)])))

# IVF - WHO
variable_mean_cost_boot_mat[i, c("mean_variable_IV_fluid_costs_act_who")] <-
					as.numeric(as.numeric(prop.table(table(data_tmp$who_dehydrat_cat))[c(2)]) %*%
				(IV_fluid_cost_per_ml * as.numeric(with(cost_data_tmp, by(total_IV_fluid, actual_dehydrat_cat, mean))[c(2)])))

# IVF - NIRUDAK
variable_mean_cost_boot_mat[i, c("mean_variable_IV_fluid_costs_act_nirudak")] <-
					as.numeric(as.numeric(prop.table(table(data_tmp$nirudak_dehydrat_cat))[c(2)]) %*%
				(IV_fluid_cost_per_ml * as.numeric(with(cost_data_tmp, by(total_IV_fluid, actual_dehydrat_cat, mean))[c(2)])))

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


####Calculate Costs

##make vectors for summary stats

##Unit Costs

#i=1

##Hospital Costs in BDT
##IV and ORS Costs in BDT
#fixed per-person fluid unit costs
IV_tube_and_solution_price <- 14.58
pair_gloves_price <- 6.86
butterfly_needle_price <- 11.15
IV_fixed_unit_cost <- IV_tube_and_solution_price + pair_gloves_price + butterfly_needle_price

# assuming model predicted fluid usage (assuming use of only one type of fluid)
ref_who_pred_total_ORS_mean_costs_vec <- variable_mean_cost_boot_mat[, "mean_variable_ORS_fluid_costs_who"]
ref_nirudak_pred_total_ORS_mean_costs_vec <- variable_mean_cost_boot_mat[, "mean_variable_ORS_fluid_costs_nirudak"]
ref_who_pred_total_IV_mean_costs_vec <- variable_mean_cost_boot_mat[, "mean_variable_IV_fluid_costs_who"] + IV_fixed_unit_cost
ref_nirudak_pred_total_IV_mean_costs_vec <- variable_mean_cost_boot_mat[, "mean_variable_IV_fluid_costs_nirudak"] + IV_fixed_unit_cost

# assuming real-life fluid usage ()
scen_who_pred_total_ORS_mean_costs_vec <- variable_mean_cost_boot_mat[ c("mean_variable_ORS_fluid_costs_act_who")]
scen_nirudak_pred_total_ORS_mean_costs_vec <- variable_mean_cost_boot_mat[ c("mean_variable_ORS_fluid_costs_act_nirudak")] 
scen_who_pred_total_IV_mean_costs_vec <- variable_mean_cost_boot_mat[ c("mean_variable_IV_fluid_costs_act_who")] + IV_fixed_unit_cost
scen_nirudak_pred_total_IV_mean_costs_vec <- variable_mean_cost_boot_mat[ c("mean_variable_IV_fluid_costs_act_nirudak")] + IV_fixed_unit_cost

# JULY 23: first thing to do next meeting is compare the mean volume deficits among models (we would expect NIRUDAK to have a higher average volume
# deficit b/c it puts more people in the Severe dehydration category)


do hospital and productivity costs for each 



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









