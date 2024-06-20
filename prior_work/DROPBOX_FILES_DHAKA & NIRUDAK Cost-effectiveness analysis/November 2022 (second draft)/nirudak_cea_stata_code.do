* SET UP CODE
* CALCULATING HYPOTHETICAL YEARS OF LIFE LOST FOR ALL PATIENTS (i.e. if they died at their current age)
gen years_lost_male = (71 - Age) if Sex == "Male"
replace years_lost_male = 0 if Age > 71 & Sex == "Male"
gen years_lost_female = (74 - Age) if Sex == "Female"
replace years_lost_female = 0 if Age > 74 & Sex == "Female"

gen all_sex_years_lost = years_lost_male if Sex == "Male"
replace all_sex_years_lost = years_lost_female if Sex == "Female"

gen years_lost_male_japan = (81 - Age) if Sex == "Male"
replace years_lost_male_japan = 0 if Age > 81 & Sex == "Male"
gen years_lost_female_japan = (87 - Age) if Sex == "Female"
replace years_lost_female_japan = 0 if Age > 87 & Sex == "Female"

gen all_sex_years_lost_japan = years_lost_male if Sex == "Male"
replace all_sex_years_lost_japan = years_lost_female if Sex == "Female"

********************************************************************************
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

* converting lost wages from BDT to USD using purchasing power parity (PPP)
gen wage_lost_hospital_stay_usd = wage_lost_hospital_stay * 0.031

* total cost variable: NIRUDAK_irr_cost_bdt2 & WHO_irr_cost_bdt2
* BDT to USD conversion using PPP (in 2019, 31.29 BT per USD)
gen NIRUDAK_total_cost_USD = NIRUDAK_irr_cost_bdt2 * 0.031
gen WHO_total_cost_USD = WHO_irr_cost_bdt2 * 0.031

* initial recommended resuscitation (IRR) (in USD) — Dr. Levine said to do this but will hold off on doing this because there are no wages lost in IRR and could be confusing for paper

* calculating total costs — full length of hospital stay (in USD)
gen WHO_cea_cost_total = WHO_total_cost_USD + wage_lost_hospital_stay_usd
gen NIRUDAK_cea_cost_total = NIRUDAK_total_cost_USD + wage_lost_hospital_stay_usd

********************************************************************************
* getting counts for probabilities
tab WHODehydrationCategory
tab Model6DehydrationCategory

********************************************************************************
* getting costs — swap out first string for Severe/Some/No and second string for Severe/Some/No to permute through all branches of the decision tree
preserve
keep if Model6DehydrationCategory == "No" & ActualDehydrationCategory == "No"
tabstat WHO_cea_cost_total, stat(mean)
restore


*****************************     WHO     **************************************
* BRANCH B
preserve
keep if WHODehydrationCategory == "Severe" & ActualDehydrationCategory == "Some"
* tab Sex to get a count
tab Sex
tabstat WHO_cea_cost_total, stat(mean)
restore

* BRANCH D
preserve
keep if WHODehydrationCategory == "Some" & ActualDehydrationCategory == "Severe"
tabstat all_sex_years_lost_japan, stat(mean)
tabstat WHO_cea_cost_total, stat(mean)
restore

* BRANCH G
preserve
keep if WHODehydrationCategory == "No" & ActualDehydrationCategory == "Severe"
tabstat all_sex_years_lost_japan, stat(mean)
tabstat WHO_cea_cost_total, stat(mean)
restore

*****************************    NIRUDAK   *************************************
* BRANCH K
preserve
keep if Model6DehydrationCategory == "Severe" & ActualDehydrationCategory == "Some"
* tab Sex to get a count
tab Sex
tabstat WHO_cea_cost_total, stat(mean)
restore

* BRANCH M
preserve
keep if Model6DehydrationCategory == "Some" & ActualDehydrationCategory == "Severe"
tabstat all_sex_years_lost_japan, stat(mean)
tabstat WHO_cea_cost_total, stat(mean)
restore

* BRANCH P
preserve
keep if Model6DehydrationCategory == "No" & ActualDehydrationCategory == "Severe"
tabstat all_sex_years_lost_japan, stat(mean)
tabstat WHO_cea_cost_total, stat(mean)
restore
