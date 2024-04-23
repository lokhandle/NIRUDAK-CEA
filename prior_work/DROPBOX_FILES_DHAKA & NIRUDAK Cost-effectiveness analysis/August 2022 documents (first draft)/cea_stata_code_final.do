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

gen WHO_cea_cost =  WHO_irr_cost_usd
gen NIRUDAK_cea_cost = NIRUDAK_irr_cost_usd


* note to self: using csv nirudak_proc from July 2022
* COSTS & YEARS OF LIFE LOST FOR PATIENTS WITH TRUE SEVERE DEHYDRATION WHO WERE INCORRECTLY CLASSFIED
* WHO
preserve
keep if actualdehydrationcategory == "Severe" & (whodehydrationcategory == "Some" | whodehydrationcategory == "No")
tabstat who_irr_cost_usd, stat(sum)
tabstat all_sex_years_lost_japan, stat(sum)
restore

* NIRUDAK
preserve
keep if actualdehydrationcategory == "Severe" & (model6dehydrationcategory == "Some" | model6dehydrationcategory == "No")
tabstat nirudak_irr_cost_usd, stat(sum)
tabstat all_sex_years_lost_japan, stat(sum)
restore

* TO-DO: DISABILITY WEIGHT DECREMENTS

* TO-DO WAGES LOST
