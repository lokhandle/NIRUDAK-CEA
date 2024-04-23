drop WHO_irr_cost_usd
gen WHO_irr_cost_usd = .
replace WHO_irr_cost_usd = (WHOVolumeDeficitL * 1000 * 0.00126) + 0.38 + 30 if WHOVolumeDeficitL == "Severe"
replace WHO_irr_cost_usd = (WHOVolumeDeficitL * 1000 * 0.00126) + 10 if WHODehydrationCategory == "Some"
replace WHO_irr_cost_usd = 0 if WHODehydrationCategory == "No"



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
gen NIRUDAK_cea_cost = nirudak_irr_cost_usd

preserve
keep if ActualDehydrationCategory == "Severe" & (whodehydrationcategory == "Some" | whodehydrationcategory == "No")
drop if ((NIRUDAK_cea_cost == .) | (WHO_cea_cost == .))
tabstat WHO_cea_cost, stat(sum)
tabstat all_sex_years_lost_japan, stat(sum)
restore

preserve
keep if actualdehydrationcategory == "Severe" & (model6dehydrationcategory == "Some" | model6dehydrationcategory == "No")
drop if ((NIRUDAK_cea_cost == .) | (WHO_cea_cost == .))
tabstat NIRUDAK_cea_cost, stat(sum)
tabstat all_sex_years_lost_japan, stat(sum)
restore


preserve
keep if WHODehydrationCategory == "Severe" & who_undertreat == 1
tabstat WHO_irr_cost_usd, stat(sum)
tabstat all_sex_years_lost, stat(sum)
restore

preserve
keep if Model6DehydrationCategory == "Severe" & nirudak_undertreat == 1
tabstat NIRUDAK_irr_cost_usd, stat(sum)
tabstat all_sex_years_lost, stat(sum)
restore


drop who_overtreat
drop nirudak_overtreat
drop who_undertreat
drop nirudak_undertreat

gen newdummy_nirudak = 1 if actualfluiddef - nirudak < (.1 * var1)

gen WHO_irr_cost_usd = .
replace WHO_irr_cost_usd = (whovolumedeficitl * 1000 * 0.00126) + 0.38 + 30 if whodehydrationcategory == "Severe"
replace WHO_irr_cost_usd = (whovolumedeficitl * 1000 * 0.00126) + 10 if whodehydrationcategory == "Some"
replace WHO_irr_cost_usd = 0 if whodehydrationcategory == "No"
