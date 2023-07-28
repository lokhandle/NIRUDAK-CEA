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

********************************************************************************
* getting counts for probabilities
tab WHODehydrationCategory
tab Model6DehydrationCategory

********************************************************************************
* GETTING COSTS FOR EACH BRANCH
* swap out first string for Severe/Some/No and second string for Severe/Some/No to permute through all branches of the decision tree

**WHO
preserve
keep if WHODehydrationCategory == "Some" & ActualDehydrationCategory == "Some"
tabstat WHO_cea_cost_total, stat(mean)
restore

**NIRUDAK
preserve
keep if Model6DehydrationCategory == "No" & ActualDehydrationCategory == "No"
tabstat NIRUDAK_cea_cost_total, stat(mean)
restore


*****************************     WHO     **************************************
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
* BRANCH M
preserve
keep if Model6DehydrationCategory == "Some" & ActualDehydrationCategory == "Severe"
tabstat all_sex_years_lost_japan, stat(mean)
tabstat NIRUDAK_cea_cost_total, stat(mean)
restore

* BRANCH P
preserve
keep if Model6DehydrationCategory == "No" & ActualDehydrationCategory == "Severe"
tabstat all_sex_years_lost_japan, stat(mean)
tabstat NIRUDAK_cea_cost_total, stat(mean)
restore
