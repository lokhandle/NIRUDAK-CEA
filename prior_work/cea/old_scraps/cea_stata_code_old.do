
*0 years lost for people older than life expectancy in Bangladesh
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

preserve
keep if ActualDehydrationCategory == "Severe" & (WHODehydrationCategory == "Some" | WHODehydrationCategory == "No")
tabstat WHO_irr_cost_usd, stat(sum)
tabstat all_sex_years_lost_japan, stat(sum)
restore

preserve
keep if ActualDehydrationCategory == "Severe" & (Model6DehydrationCategory == "Some" | Model6DehydrationCategory == "No")
tabstat NIRUDAK_irr_cost_usd, stat(sum)
tabstat all_sex_years_lost_japan, stat(sum)
restore





gen WHO_irr_cost_usd = .
replace WHO_irr_cost_usd = (whovolumedeficitl * 1000 * 0.00126) + 0.38 + 30 if whodehydrationcategory == "Severe"
replace WHO_irr_cost_usd = (whovolumedeficitl * 1000 * 0.00126) + 10 if whodehydrationcategory == "Some"
replace WHO_irr_cost_usd = 0 if whodehydrationcategory == "No"


preserve
keep if ActualDehydrationCategory == "Severe" & (WHODehydrationCategory == "Some" | WHODehydrationCategory == "No")
tabstat WHO_irr_cost_usd, stat(sum)
tabstat all_sex_years_lost_japan, stat(sum)
restore

* NIRUDAK
preserve
keep if ActualDehydrationCategory == "Severe" & (Model6DehydrationCategory == "Some" | Model6DehydrationCategory == "No")
tabstat NIRUDAK_irr_cost_usd, stat(sum)
tabstat all_sex_years_lost_japan, stat(sum)
restore
