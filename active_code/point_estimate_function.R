


#######construct vectors of probabilities of falling in each branch for each model

joint_prob_vec_nirudak_orig <- joint_prob_vec_who_orig <- rep(NA, 9) 
names(joint_prob_vec_nirudak_orig) <- names(joint_prob_vec_who_orig) <- c(
	c("true none, model none", "true none, model some", "true none, model severe"),
	c("true some, model none", "true some, model some", "true some, model severe"),
	c("true severe, model none", "true severe, model some", "true severe, model severe"))

joint_prob_vec_nirudak_orig[1] <- mean(with(data_orig, actual_dehydrat_cat == 'No' & nirudak_dehydrat_cat == 'No'))
joint_prob_vec_nirudak_orig[2] <- mean(with(data_orig, actual_dehydrat_cat == 'No' & nirudak_dehydrat_cat == 'Some'))
joint_prob_vec_nirudak_orig[3] <- mean(with(data_orig, actual_dehydrat_cat == 'No' & nirudak_dehydrat_cat == 'Severe'))
joint_prob_vec_nirudak_orig[4] <- mean(with(data_orig, actual_dehydrat_cat == 'Some' & nirudak_dehydrat_cat == 'No'))
joint_prob_vec_nirudak_orig[5] <- mean(with(data_orig, actual_dehydrat_cat == 'Some' & nirudak_dehydrat_cat == 'Some'))
joint_prob_vec_nirudak_orig[6] <- mean(with(data_orig, actual_dehydrat_cat == 'Some' & nirudak_dehydrat_cat == 'Severe'))
joint_prob_vec_nirudak_orig[7] <- mean(with(data_orig, actual_dehydrat_cat == 'Severe' & nirudak_dehydrat_cat == 'No'))
joint_prob_vec_nirudak_orig[8] <- mean(with(data_orig, actual_dehydrat_cat == 'Severe' & nirudak_dehydrat_cat == 'Some'))
joint_prob_vec_nirudak_orig[9] <- mean(with(data_orig, actual_dehydrat_cat == 'Severe' & nirudak_dehydrat_cat == 'Severe'))

joint_prob_vec_who_orig[1] <- mean(with(data_orig, actual_dehydrat_cat == 'No' & who_dehydrat_cat == 'No'))
joint_prob_vec_who_orig[2] <- mean(with(data_orig, actual_dehydrat_cat == 'No' & who_dehydrat_cat == 'Some'))
joint_prob_vec_who_orig[3] <- mean(with(data_orig, actual_dehydrat_cat == 'No' & who_dehydrat_cat == 'Severe'))
joint_prob_vec_who_orig[4] <- mean(with(data_orig, actual_dehydrat_cat == 'Some' & who_dehydrat_cat == 'No'))
joint_prob_vec_who_orig[5] <- mean(with(data_orig, actual_dehydrat_cat == 'Some' & who_dehydrat_cat == 'Some'))
joint_prob_vec_who_orig[6] <- mean(with(data_orig, actual_dehydrat_cat == 'Some' & who_dehydrat_cat == 'Severe'))
joint_prob_vec_who_orig[7] <- mean(with(data_orig, actual_dehydrat_cat == 'Severe' & who_dehydrat_cat == 'No'))
joint_prob_vec_who_orig[8] <- mean(with(data_orig, actual_dehydrat_cat == 'Severe' & who_dehydrat_cat == 'Some'))
joint_prob_vec_who_orig[9] <- mean(with(data_orig, actual_dehydrat_cat == 'Severe' & who_dehydrat_cat == 'Severe'))


#######Calculate DALYs

####calcualte mean YLL among those who die assuming sex-dependent Japenese LE

##set up function to assign remaining LE for a given age and sex
five_yr_age_buckets <- seq(0, 90, 5)
resid_life_expect_at_age_male <- c(81.41, 76.63, 71.66, 66.69, 61.77, 56.91, 52.03, 47.18,
                                   42.35, 37.57, 32.89, 28.34, 23.97, 19.83, 15.96, 12.41, 9.18,
                                   6.46, 4.41)

resid_life_expect_at_age_female <- c(87.45, 82.66, 77.69, 72.72, 67.77, 62.84, 57.91,
                                    53, 48.11, 43.26, 38.49, 33.79, 29.17, 24.63, 20.21, 15.97,
                                    12.01, 8.51, 5.71)
resid_life_interp_fxn_male <- splinefun(five_yr_age_buckets, resid_life_expect_at_age_male)
resid_life_interp_fxn_female <- splinefun(five_yr_age_buckets, resid_life_expect_at_age_female)

## NIRUDAK model
#among those who die from undertreatment 
mean_YLL_among_dead_nirudak_male_undertreat <- mean(resid_life_interp_fxn_male(as.numeric(unlist(data_orig[data_orig$male == 1 & 
		data_orig$actual_dehydrat_cat == "Severe" & data_orig$nirudak_dehydrat_cat %in% c("No", "Some"), "age"]))))
mean_YLL_among_dead_nirudak_female_undertreat <- mean(resid_life_interp_fxn_female(as.numeric(unlist(data_orig[data_orig$male == 0 & 
		data_orig$actual_dehydrat_cat == "Severe" & data_orig$nirudak_dehydrat_cat %in% c("No", "Some"), "age"]))))
#among those who die from overtreat
mean_YLL_among_dead_nirudak_male_overtreat <- mean(resid_life_interp_fxn_male(as.numeric(unlist(data_orig[data_orig$male == 1 &
		data_orig$nirudak_dehydrat_cat == "Severe" & data_orig$actual_dehydrat_cat %in% c("No", "Some"), "age"]))))
mean_YLL_among_dead_nirudak_female_overtreat <- mean(resid_life_interp_fxn_female(as.numeric(unlist(data_orig[data_orig$male == 0 &
		data_orig$nirudak_dehydrat_cat == "Severe" & data_orig$actual_dehydrat_cat %in% c("No", "Some"), "age"]))))
#sum across sexes
total_YLL_among_dead_nirudak_undertreat <- (mean_YLL_among_dead_nirudak_female_undertreat + mean_YLL_among_dead_nirudak_male_undertreat)/2
total_YLL_among_dead_nirudak_overtreat <- (mean_YLL_among_dead_nirudak_female_overtreat + mean_YLL_among_dead_nirudak_male_overtreat)/2

## WHO Model
#among those who die from undertreatment 
mean_YLL_among_dead_who_male_undertreat <- mean(resid_life_interp_fxn_male(as.numeric(unlist(data_orig[data_orig$male == 1 & 
		data_orig$actual_dehydrat_cat == "Severe" & data_orig$who_dehydrat_cat %in% c("No", "Some"), "age"]))))
mean_YLL_among_dead_who_female_undertreat <- mean(resid_life_interp_fxn_female(as.numeric(unlist(data_orig[data_orig$male == 0 & 
		data_orig$actual_dehydrat_cat == "Severe" & data_orig$who_dehydrat_cat %in% c("No", "Some"), "age"]))))
#among those who die from overtreat
mean_YLL_among_dead_who_male_overtreat <- mean(resid_life_interp_fxn_male(as.numeric(unlist(data_orig[data_orig$male == 1 & 
		data_orig$who_dehydrat_cat == "Severe" & data_orig$actual_dehydrat_cat %in% c("No", "Some"), "age"]))))
mean_YLL_among_dead_who_female_overtreat <- mean(resid_life_interp_fxn_female(as.numeric(unlist(data_orig[data_orig$male == 0 & 
		data_orig$who_dehydrat_cat == "Severe" & data_orig$actual_dehydrat_cat %in% c("No", "Some"), "age"]))))
#sum across sexes
total_YLL_among_dead_who_undertreat <- (mean_YLL_among_dead_who_female_undertreat + mean_YLL_among_dead_who_male_undertreat)/2
total_YLL_among_dead_who_overtreat <- (mean_YLL_among_dead_who_female_overtreat + mean_YLL_among_dead_who_male_overtreat)/2

##Use the whole sample so same penalty for both models
#among those who die from undertreatment: use the entire sample of severely dehyrdated patients (accroding to actual dehyrdration status) 
	#to calculate YLL 
mean_YLL_among_dead_all_male_undertreat <- mean(resid_life_interp_fxn_male(as.numeric(unlist(data_orig[data_orig$male == 1
	& data_orig$actual_dehydrat_cat == "Severe", "age"]))))
mean_YLL_among_dead_all_female_undertreat <- mean(resid_life_interp_fxn_female(as.numeric(unlist(data_orig[data_orig$male == 0 & 
	data_orig$actual_dehydrat_cat == "Severe", "age"]))))
#among those who die from overtreament: use the entire sample of patients with some or no dehyrdation (accroding to actual dehyrdration status) 
	#to calculate YLL 
mean_YLL_among_dead_all_male_overtreat <- mean(resid_life_interp_fxn_male(as.numeric(unlist(data_orig[data_orig$male == 1 & 
		 data_orig$actual_dehydrat_cat %in% c("No", "Some"), "age"]))))
mean_YLL_among_dead_all_female_overtreat <- mean(resid_life_interp_fxn_female(as.numeric(unlist(data_orig[data_orig$male == 0 & 
		data_orig$actual_dehydrat_cat %in% c("No", "Some"), "age"]))))
#sum across sexes
total_YLL_among_dead_all_undertreat <- (mean_YLL_among_dead_all_male_undertreat + mean_YLL_among_dead_all_female_undertreat)/2
total_YLL_among_dead_all_overtreat <- (mean_YLL_among_dead_all_male_overtreat+ mean_YLL_among_dead_all_female_overtreat)/2

####Construct vectors giving the YLL (mortality) among those who die for each branch, for both models

nirudak_YLL_vec <- rep(0, 9)
nirudak_YLL_vec[7:8] <- total_YLL_among_dead_nirudak_undertreat
nirudak_YLL_vec[c(3,6)] <- total_YLL_among_dead_nirudak_overtreat
names(nirudak_YLL_vec) <- names(joint_prob_vec_nirudak_orig)

who_YLL_vec <- rep(0, 9)
who_YLL_vec[7:8] <- total_YLL_among_dead_who_undertreat
who_YLL_vec[c(3,6)] <- total_YLL_among_dead_who_overtreat
names(who_YLL_vec) <- names(joint_prob_vec_who_orig)

all_YLL_vec <- rep(0, 9)
all_YLL_vec[7:8] <- total_YLL_among_dead_all_undertreat
all_YLL_vec[c(3,6)] <- total_YLL_among_dead_all_overtreat
names(all_YLL_vec) <- names(joint_prob_vec_nirudak_orig)


####Construct vectors giving the YLD (years lost to disability, i.e., morbidity) among those who die for each branch, for both models
	#from Sharifi paper
nirudak_YLD_vec <- rep(0, 9)
who_YLD_vec <- rep(0, 9)
names(nirudak_YLD_vec) <- names(joint_prob_vec_nirudak_orig)
names(who_YLD_vec) <- names(joint_prob_vec_who_orig)
#for now, assume no disutility (value of 0) for any branches (beyond mortality)

####Construct vectors giving the probability of suffering morbidity and of suffering mortality for each branch
#note these are the same for the two models as it depends only on which branch you end up in
#for now assume 0 prob of morbidity (and 0 penalty above) 
branch_morbidity_prob <- rep(0,9)
names(branch_morbidity_prob) <- names(joint_prob_vec_nirudak_orig)
branch_death_prob <- rep(0,9)
names(branch_death_prob) <- names(joint_prob_vec_nirudak_orig)
#for now assume only the severely dehyrdrated who are undertreated have a positive probability of death, set equal to 1 to start.
prob_die_undertreat <- 1
branch_death_prob[7:8] <- prob_die_undertreat


####Calculate mean YLL for each model under two assumptions
##calculating mean years of life lost based on the ages of people actually undertreated or overtreated by each model
mean_YLL_nirudak_model_specific_penalty <- as.numeric((joint_prob_vec_nirudak_orig * branch_death_prob) %*% nirudak_YLL_vec)
mean_YLL_who_model_specific_penalty <- as.numeric((joint_prob_vec_who_orig * branch_death_prob) %*% who_YLL_vec)

##calculating mean years of life lost based on the ages of all patients who were actually severely dehyrated (for undertreatment)
	or who actually had none or some dehydration (for overtreatment)
#note: this gives us the same age distribution across both models (both arms)
mean_YLL_nirudak_common_penalty <- as.numeric((joint_prob_vec_nirudak_orig * branch_death_prob) %*% all_YLL_vec)
mean_YLL_who_common_penalty <- as.numeric((joint_prob_vec_who_orig * branch_death_prob) %*% all_YLL_vec)

####Calculate mean YLD for each model
mean_YLD_nirudak <- as.numeric((joint_prob_vec_nirudak_orig * branch_morbidity_prob) %*% nirudak_YLD_vec)
mean_YLD_who <- as.numeric((joint_prob_vec_who_orig * branch_morbidity_prob) %*% who_YLD_vec)

####Calculate mean DALYS lost for each model under two assumptions
##assuming mortality penalties are claculated based on the ages of people actually undertreated or overtreated by each model
mean_DALYs_lost_nirudak_specific <- sum(mean_YLL_nirudak_model_specific_penalty , mean_YLD_nirudak)
mean_DALYs_lost_who_specific <- sum(mean_YLL_who_model_specific_penalty, mean_YLD_who)
##assuming common mortality penalties across models
mean_DALYs_lost_nirudak_all <- sum(mean_YLL_nirudak_common_penalty, mean_YLD_nirudak)
mean_DALYs_lost_who_all <- sum(mean_YLL_who_common_penalty, mean_YLD_who)



#######Calculate Costs


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
data_tmp <- data_orig[test_sample, ]

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
# detailed approach: for anyone who died - mean income per year (mean(data_orig$monthly_income))*12 - every year of life they lose, they lose that amount
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
#((data_orig$nirudak_volume_deficit * 1000 * 0.104) + 32.59)*(data_orig$nirudak_dehydrat_cat == "Severe"),
#(data_orig$nirudak_volume_deficit * 1000 * 0.0054)*(data_orig$nirudak_dehydrat_cat == "Some"),
#rep(0, length(which(data_orig$nirudak_dehydrat_cat == "No"))))

#WHO_fluid_cost = c(
#  ((data_orig$who_volume_deficit * 1000 * 0.104) + 32.59)*(data_orig$who_dehydrat_cat == "Severe"),
#  (data_orig$who_volume_deficit * 1000 * 0.0054)*(data_orig$who_dehydrat_cat == "Some"),
#  rep(0, length(which(data_orig$who_dehydrat_cat == "No"))))

