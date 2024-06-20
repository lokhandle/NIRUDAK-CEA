/*
from Dr. Levine's email:
assume the following: 1 pair of gloves, 1 IV tubing/solution set, 1 IV needle for patients treated with IVF. For the butterfly needle, 24G is actually the smallest size, so we can assign that to children under five and 22G for children over five and adults. 

For hospital stay, in the initial exercise where we just figure out the actual costs for patients, we can use the actual length of stay (discharge date/time - admit date/time). Be careful to account for partial days by looking at the time of admit and discharge as well. It would be great to see the overall median cost and IQR for all patients in the NIRUDAK study, but then also separately for patients who truly had no, some or severe dehydration by our gold standard. It would also be good to break this down by those who only received ORS versus those who received IVF +/- ORS. Finally we can calculate the costs just for the first 6 hours of care to understand the cost of their initial resuscitation, again broken down by all the groups above.

The bigger question is how we handle costs for the hypothetical scenario of the patient treated using the NIRUDAK full model or WHO algorithm recommendation. I don't think there is a correct answer here, but here are a couple ways we can try it: 

1) For the NIRUDAK model, for patients diagnosed as severe dehydration, use the median costs of patients given IVF+/- ORS from the calculations above, for patients diagnosed with some dehydration use the median cost for patients only receiving ORS from the calculations above, and $0 for patients with no dehydration. For WHO, do the same.

2) For NIRUDAK, just look at the costs associated with initial recommended resuscitation. So for patients with severe dehydration predicted by the model, multiply the cost of IVF by the amount recommended by the NIRUDAK model + the cost of IV equipment above + 1 day of a hospital stay, for patients with some dehydration multiply the cost of ORS by the amount recommended by the NIRUDAK model + 8 hours of a hospital stay, and $0 for no dehydration. For WHO, do the same except use 100cc/kg for the amount of IVF for patients categorized as severe dehydration and 75cc/kg for the amount of ORS for patients categorized as some dehydration. [this is the same as the WHO variable in the spreadsheet] 

note: NIRUDAK & Model6 are used interchangeably in this code and refer to hypothetical costs from the NIRUDAK model
"actual" refers to the actual data or actual costs 

the data file associated with this code should contain all variables below. some unnecessary varibles (not indicated below) may be included; they are a by-product of the analysis and can be ignored */

/*************************************SETUP*************************************
—–—————————————————————————FIXED COST: IV EQUIPMENT—————————————————————————————
PATIENTS UNDER 5 (for future DHAKA analysis)
IV tubing/solution set + 1 pair of gloves + 1 24G butterfly needle
0.17 + 0.08 + 0.15 = $0.40
34.31 BDT

PATIENTS OVER 5 (current NIRUDAK analysis)
IV tubing/solution set + 1 pair of gloves + 1 22G butterfly needle
0.17 + 0.08 + 0.13 = $0.38
32.59 BDT

————————————————————————————VARIABLE COST: FLUIDS———————————————————————————————
$0.00006 (0.0054 BDT) per mL ORS
$0.00126 (0.104 BDT) per mL IVF

75 mL ORS costs $0.0045
100 mL IV costs $0.126

————————————————–————————VARIABLE COST: HOSPITAL STAY———————————————————————————
all patients admitted to short stay unit
$30 (2573.1 BDT) per 24 hours (1 day)
additional $30 for each day */

gen length_stay_days = Form8DischargeDate - AdmitDate
gen length_stay_hrs = Form8_Discharge_Time_adj - AdmitTime_adj
replace length_stay_hrs = (-1 * length_stay_hrs) + 1 if length_stay_hrs < 0

replace Form6FU1ORS = 0 if Form6FU1ORS == .
replace Form6FU2ORS = 0 if Form6FU2ORS == .
replace Form6FU3ORS = 0 if Form6FU3ORS == .
replace Form6FU4ORS = 0 if Form6FU4ORS == .
replace Form6FU5ORS = 0 if Form6FU5ORS == .
replace Form6FU6ORS = 0 if Form6FU6ORS == .
replace Form6FU7ORS = 0 if Form6FU7ORS == .
replace Form7FU8ORS = 0 if Form7FU8ORS == .
replace Form7FU9ORS = 0 if Form7FU9ORS == .
replace Form7FU10ORS = 0 if Form7FU10ORS == .
replace Form7FU11ORS = 0 if Form7FU11ORS == .
replace Form7FU12ORS = 0 if Form7FU12ORS == .
replace Form7FU13ORS = 0 if Form7FU13ORS == .
replace Form7FU14ORS = 0 if Form7FU14ORS == .

gen total_ORS = Form6FU1ORS + Form6FU2ORS + Form6FU3ORS + Form6FU4ORS + Form6FU5ORS + Form6FU6ORS + Form6FU7ORS + Form7FU8ORS + Form7FU9ORS + Form7FU10ORS + Form7FU11ORS + Form7FU12ORS + Form7FU13ORS + Form7FU14ORS

replace Form6FU1IVFluid = 0 if Form6FU1IVFluid == .
replace Form6FU2IVFluid = 0 if Form6FU2IVFluid == .
replace Form6FU3IVFluid = 0 if Form6FU3IVFluid == .
replace Form6FU4IVFluid = 0 if Form6FU4IVFluid == .
replace Form6FU5IVFluid = 0 if Form6FU5IVFluid == .
replace Form6FU6IVFluid = 0 if Form6FU6IVFluid == .
replace Form6FU7IVFluid = 0 if Form6FU7IVFluid == .
replace Form7FU8IVFluid = 0 if Form7FU8IVFluid == .
replace Form7FU9IVFluid = 0 if Form7FU9IVFluid == .
replace Form7FU10IVFluid = 0 if Form7FU10IVFluid == .
replace Form7FU11IVFluid = 0 if Form7FU11IVFluid == .
replace Form7FU12IVFluid = 0 if Form7FU12IVFluid == .
replace Form7FU13IVFluid = 0 if Form7FU13IVFluid == .
replace Form7FU14IVFluid = 0 if Form7FU14IVFluid == .
replace IVFluidPriortoGettingAdmit = 0 if IVFluidPriortoGettingAdmit == .

gen total_IVF = Form6FU1IVFluid + Form6FU2IVFluid + Form6FU3IVFluid + Form6FU4IVFluid + Form6FU5IVFluid + Form6FU6IVFluid + Form6FU7IVFluid + Form7FU8IVFluid + Form7FU9IVFluid + Form7FU10IVFluid + Form7FU11IVFluid + Form7FU12IVFluid + Form7FU13IVFluid + Form7FU14IVFluid + IVFluidPriortoGettingAdmit

gen hospital_cost_usd = length_stay_hrs * 30
gen hospital_cost_bdt = length_stay_hrs * 2573.1

replace hospital_cost_usd = 0 if hospital_cost_usd == .
replace hospital_cost_bdt = 0 if hospital_cost_bdt == .

gen actual_total_ORS_cost_usd = total_ORS * 0.00006
gen actual_total_ORS_cost_bdt = total_ORS * 0.0054
gen actual_total_IVF_cost_usd = (total_IVF * 0.00126) + 0.38
gen actual_total_IVF_cost_bdt = (total_IVF * 0.104) + 32.59

gen actual_total_cost_usd = actual_total_IVF_cost_usd + actual_total_ORS_cost_usd + hospital_cost_usd
gen actual_total_cost_bdt = actual_total_IVF_cost_bdt + actual_total_ORS_cost_bdt + hospital_cost_bdt

*———————————————————————————————-ACTUAL TOTAL————————————————————————————
*OVERALL COST — ALL PATIENTS
sum actual_total_cost_usd, detail
sum actual_total_cost_bdt, detail
tabstat actual_total_cost_usd, stat(sum)
tabstat actual_total_cost_bdt, stat(sum)

*OVERALL COST — NO DEHYDRATION DIAGNOSIS
preserve
keep if ActualDehydrationCategory == "No"
sum actual_total_cost_usd, detail
sum actual_total_cost_bdt, detail
tabstat actual_total_cost_usd, stat(sum)
tabstat actual_total_cost_bdt, stat(sum)
restore

*OVERALL COST — SOME DEHYDRATION DIAGNOSIS
preserve
keep if ActualDehydrationCategory == "Some"
sum actual_total_cost_usd, detail
sum actual_total_cost_bdt, detail
tabstat actual_total_cost_usd, stat(sum)
tabstat actual_total_cost_bdt, stat(sum)
restore

*OVERALL COST — SEVERE DEHYDRATION DIAGNOSIS
preserve
keep if ActualDehydrationCategory == "Severe"
sum actual_total_cost_usd, detail
sum actual_total_cost_bdt, detail
tabstat actual_total_cost_usd, stat(sum)
tabstat actual_total_cost_bdt, stat(sum)
restore

*OVERALL COST — ORS PATIENTS ONLY
preserve
drop if total_IVF > 0 | total_ORS == 0
sum actual_total_cost_usd, detail
sum actual_total_cost_bdt, detail
tabstat actual_total_cost_usd, stat(sum)
tabstat actual_total_cost_bdt, stat(sum)
restore

*OVERALL COST — IV PATIENTS ONLY
preserve
drop if total_ORS > 0 | total_IVF == 0
sum actual_total_cost_usd, detail
sum actual_total_cost_bdt, detail
tabstat actual_total_cost_usd, stat(sum)
tabstat actual_total_cost_bdt, stat(sum)
restore

gen total_fluid = total_IVF + total_ORS
gen total_fluid_cost_usd = actual_total_IVF_cost_usd + actual_total_ORS_cost_usd
gen total_fluid_cost_bdt = actual_total_IVF_cost_bdt + actual_total_ORS_cost_bdt

*OVERALL COST — ORS & IV COMBO PATIENTS ONLY
preserve
drop if total_ORS == 0 | total_IVF == 0
sum actual_total_cost_usd, detail
sum actual_total_cost_bdt, detail
tabstat actual_total_cost_usd, stat(sum)
tabstat actual_total_cost_bdt, stat(sum)
restore

*———————————————————————————————-MODEL6 TOTAL———————————————————————————————————
/*based on the PLOS Neglected Tropical diseases paper; asumes "Severe" patients must be hospitalized while "Some" patients can be managed outpatient and don't need hospitalization 

SEVERE: (Model6VolumeDeficitL * 1000 * IVF cost per mL) + IV equipment + actual cost of hospital stay
SOME: (Model6VolumeDeficitL * 1000 * ORS cost per mL) */

gen actual_cost_usd = .
replace actual_cost_usd = (Model6VolumeDeficitL * 1000 * 0.00126) + 0.38 + hospital_cost_usd if Model6DehydrationCategory == "Severe"
replace actual_cost_usd = (Model6VolumeDeficitL * 1000 * 0.00126) if Model6DehydrationCategory == "Some"
replace actual_cost_usd = 0 if Model6DehydrationCategory == "No"

gen actual_cost_bdt = .
replace actual_cost_bdt = (Model6VolumeDeficitL * 1000 * 0.104) + 32.59 + hospital_cost_bdt if Model6DehydrationCategory == "Severe"
replace actual_cost_bdt = (Model6VolumeDeficitL * 1000 * 0.104) if Model6DehydrationCategory == "Some"
replace actual_cost_bdt = 0 if Model6DehydrationCategory == "No"

*NIRUDAK MODEL COSTS
*ALL
preserve
drop if Model6DehydrationCategory == ""
sum actual_cost_usd, detail
sum actual_cost_bdt, detail
tabstat actual_cost_usd, stat(sum)
tabstat actual_cost_bdt, stat(sum)
restore

*NO
preserve
keep if Model6DehydrationCategory == "No"
sum actual_cost_usd, detail
sum actual_cost_bdt, detail
tabstat actual_cost_usd, stat(sum)
tabstat actual_cost_bdt, stat(sum)
restore

*SEVERE
preserve
keep if Model6DehydrationCategory == "Severe"
sum actual_cost_usd, detail
sum actual_cost_bdt, detail
tabstat actual_cost_usd, stat(sum)
tabstat actual_cost_bdt, stat(sum)
restore

*SOME
preserve
keep if Model6DehydrationCategory == "Some"
sum actual_cost_usd, detail
sum actual_cost_bdt, detail
tabstat actual_cost_usd, stat(sum)
tabstat actual_cost_bdt, stat(sum)
restore

*—————————————————————————————————-WHO TOTAL ———————————————————————————————————
/*total cost variables — based on the PLOS Neglected Tropical diseases paper
*asumes "Severe" patients must be hospitalized while "Some" patients can be managed in the community
and don't need hospitalization 
SEVERE: (WHOVolumeDeficitL * 1000 * IVF cost per mL) + IV equipment + actual cost of hospital stay
SOME: (WHOVolumeDeficitL * 1000 * ORS cost per mL) */

gen WHO_cost_usd = .
replace WHO_cost_usd = (WHOVolumeDeficitL * 1000 * 0.00126) + 0.38 + hospital_cost_usd if WHODehydrationCategory == "Severe"
replace WHO_cost_usd = (WHOVolumeDeficitL * 1000 * 0.00126) if WHODehydrationCategory == "Some"
replace WHO_cost_usd = 0 if WHODehydrationCategory == "No"

gen WHO_cost_bdt = .
replace WHO_cost_bdt = (WHOVolumeDeficitL * 1000 * 0.104) + 32.59 + hospital_cost_bdt if WHODehydrationCategory == "Severe"
replace WHO_cost_bdt = (WHOVolumeDeficitL * 1000 * 0.104) if WHODehydrationCategory == "Some"
replace WHO_cost_bdt = 0 if WHODehydrationCategory == "No"

*WHO ALL
preserve
drop if WHODehydrationCategory == ""
sum WHO_cost_usd, detail
sum WHO_cost_bdt, detail
tabstat WHO_cost_usd, stat(sum)
tabstat WHO_cost_bdt, stat(sum)
restore

*WHO SEVERE
preserve
keep if WHODehydrationCategory == "Severe"
sum WHO_cost_usd, detail
sum WHO_cost_bdt, detail
tabstat WHO_cost_usd, stat(sum)
tabstat WHO_cost_bdt, stat(sum)
restore

*WHO SOME
preserve
keep if WHODehydrationCategory == "Some"
sum WHO_cost_usd, detail
sum WHO_cost_bdt, detail
tabstat WHO_cost_usd, stat(sum)
tabstat WHO_cost_bdt, stat(sum)
restore

************************************FLUID ONLY**********************************
*————————————————————————————-ACTUAL FLUID ONLY———————————————————————————
*FLUID ONLY COST — ALL PATIENTS
preserve
sum total_fluid_cost_usd, detail
sum total_fluid_cost_bdt, detail
tabstat total_fluid_cost_usd, stat(sum)
tabstat total_fluid_cost_bdt, stat(sum)
restore

*FLUID ONLY COST — NO DEHYDRATION DIAGNOSIS
preserve
keep if ActualDehydrationCategory == "No"
sum total_fluid_cost_usd, detail
sum total_fluid_cost_bdt, detail
tabstat total_fluid_cost_usd, stat(sum)
tabstat total_fluid_cost_bdt, stat(sum)
restore

*FLUID ONLY COST — SOME DEHYDRATION DIAGNOSIS
preserve
keep if ActualDehydrationCategory == "Some"
sum total_fluid_cost_usd, detail
sum total_fluid_cost_bdt, detail
tabstat total_fluid_cost_usd, stat(sum)
tabstat total_fluid_cost_bdt, stat(sum)
restore

*FLUID ONLY COST — SEVERE DEHYDRATION DIAGNOSIS
preserve
keep if ActualDehydrationCategory == "Severe"
sum total_fluid_cost_usd, detail
sum total_fluid_cost_bdt, detail
tabstat total_fluid_cost_usd, stat(sum)
tabstat total_fluid_cost_bdt, stat(sum)
restore

/*————————————————————————————-MODEL6 FLUID ONLY—————————————————————————————————
SEVERE: (Model6VolumeDeficitL * 1000 * IVF cost per mL) + IV equipment
SOME: (Model6VolumeDeficitL * 1000 * ORS cost per mL) */
gen NIRUDAK_fluid_only_cost_usd = .
replace NIRUDAK_fluid_only_cost_usd = (Model6VolumeDeficitL * 1000 * 0.00126) + 0.38 if Model6DehydrationCategory == "Severe"
replace NIRUDAK_fluid_only_cost_usd = (Model6VolumeDeficitL * 1000 * 0.00126) if Model6DehydrationCategory == "Some"
replace NIRUDAK_fluid_only_cost_usd = 0 if Model6DehydrationCategory == "No"

gen NIRUDAK_fluid_only_cost_bdt = .
replace NIRUDAK_fluid_only_cost_bdt = (Model6VolumeDeficitL * 1000 * 0.104) + 32.59 if Model6DehydrationCategory == "Severe"
replace NIRUDAK_fluid_only_cost_bdt = (Model6VolumeDeficitL * 1000 * 0.104) if Model6DehydrationCategory == "Some"
replace NIRUDAK_fluid_only_cost_bdt = 0 if Model6DehydrationCategory == "No"

*ALL
preserve
drop if Model6DehydrationCategory == ""
sum NIRUDAK_fluid_only_cost_usd, detail
sum NIRUDAK_fluid_only_cost_bdt, detail
tabstat NIRUDAK_fluid_only_cost_usd, stat(sum)
tabstat NIRUDAK_fluid_only_cost_bdt, stat(sum)
restore

*SOME
preserve
keep if Model6DehydrationCategory == "Some"
sum NIRUDAK_fluid_only_cost_usd, detail
sum NIRUDAK_fluid_only_cost_bdt, detail
tabstat NIRUDAK_fluid_only_cost_usd, stat(sum)
tabstat NIRUDAK_fluid_only_cost_bdt, stat(sum)
restore

*SEVERE
preserve
keep if Model6DehydrationCategory == "Severe"
sum NIRUDAK_fluid_only_cost_usd, detail
sum NIRUDAK_fluid_only_cost_bdt, detail
tabstat NIRUDAK_fluid_only_cost_usd, stat(sum)
tabstat NIRUDAK_fluid_only_cost_bdt, stat(sum)
restore

/*——————————————————————————————-WHO FLUID ONLY—————————————————————————————————
SEVERE: (Model6VolumeDeficitL * 1000 * IVF cost per mL) + IV equipment
SOME: (Model6VolumeDeficitL * 1000 * ORS cost per mL) */
gen WHO_fluid_only_cost_usd = .
replace WHO_fluid_only_cost_usd = (WHOVolumeDeficitL * 1000 * 0.00126) + 0.38 if WHODehydrationCategory == "Severe"
replace WHO_fluid_only_cost_usd = (WHOVolumeDeficitL * 1000 * 0.00126) if WHODehydrationCategory == "Some"
replace WHO_fluid_only_cost_usd = 0 if WHODehydrationCategory == "No"

gen WHO_fluid_only_cost_bdt = .
replace WHO_fluid_only_cost_bdt = (WHOVolumeDeficitL * 1000 * 0.104) + 32.59 if WHODehydrationCategory == "Severe"
replace WHO_fluid_only_cost_bdt = (WHOVolumeDeficitL * 1000 * 0.104) if WHODehydrationCategory == "Some"
replace WHO_fluid_only_cost_bdt = 0 if WHODehydrationCategory == "No"


*WHO FLUID ONLY COSTS
*ALL
preserve
drop if WHODehydrationCategory == ""
sum WHO_fluid_only_cost_usd, detail
sum WHO_fluid_only_cost_bdt, detail
tabstat WHO_fluid_only_cost_usd, stat(sum)
tabstat WHO_fluid_only_cost_bdt, stat(sum)
restore

*NO
preserve
keep if WHODehydrationCategory == "No"
sum WHO_fluid_only_cost_usd, detail
sum WHO_fluid_only_cost_bdt, detail
tabstat WHO_fluid_only_cost_usd, stat(sum)
tabstat WHO_fluid_only_cost_bdt, stat(sum)
restore

*SOME
preserve
keep if WHODehydrationCategory == "Some"
sum WHO_fluid_only_cost_usd, detail
sum WHO_fluid_only_cost_bdt, detail
tabstat WHO_fluid_only_cost_usd, stat(sum)
tabstat WHO_fluid_only_cost_bdt, stat(sum)
restore

*SEVERE
preserve
keep if WHODehydrationCategory == "Severe"
sum WHO_fluid_only_cost_usd, detail
sum WHO_fluid_only_cost_bdt, detail
tabstat WHO_fluid_only_cost_usd, stat(sum)
tabstat WHO_fluid_only_cost_bdt, stat(sum)
restore

gen WHO_cost_usd
replace WHO_cost_usd = (WHOVolumeDeficitL * 1000 * 0.00126) + 0.38 + 30 if WHODehydrationCategory == "Severe"
replace WHO_cost_usd = (WHOVolumeDeficitL * 1000 * 0.00126) + 10 if WHODehydrationCategory == "Some"
replace WHO_cost_usd = 0 if WHODehydrationCategory == "No"

gen WHO_cost_bdt
replace WHO_cost_bdt = (WHOVolumeDeficitL * 1000 * 0.104) + 32.59 + 2573.1 if WHODehydrationCategory == "Severe"
replace WHO_cost_bdt = (WHOVolumeDeficitL * 1000 * 0.104) + 857.7 if WHODehydrationCategory == "Some"
replace WHO_cost_bdt = 0 if WHODehydrationCategory == "No"

*************************************6 HOUR*************************************
gen sixhr_thresholdfu1 = Form6_FU1_Time_adj - AdmitTime_adj
replace sixhr_thresholdfu1 = (-1 * sixhr_thresholdfu1) if sixhr_thresholdfu1 < 0
replace sixhr_thresholdfu1 = 0 if sixhr_thresholdfu1 == .

gen sixhr_thresholdfu2 = Form6_FU2_Time_adj - AdmitTime_adj
replace sixhr_thresholdfu2 = (-1 * sixhr_thresholdfu2) if sixhr_thresholdfu2 < 0
replace sixhr_thresholdfu2 = 0 if sixhr_thresholdfu2 == .

gen sixhr_thresholdfu3 = Form6_FU3_Time_adj - AdmitTime_adj
replace sixhr_thresholdfu3 = (-1 * sixhr_thresholdfu3) if sixhr_thresholdfu3 < 0
replace sixhr_thresholdfu3 = 0 if sixhr_thresholdfu3 == .

drop sixhr_threshold
gen sixhr_threshold = sixhr_thresholdfu1 + sixhr_thresholdfu2 + sixhr_thresholdfu3

/* note: follow ups are not at exactly 6 hours, per the above variables

the indicator variable below tells us whether we should use total fluid from follow up 1 or follow up 2
sixhr_fu2_dummy = 1 means total fluid used in the calculation should be all the fluid given up 
to and including the fluid given in follow up 2 (fluid given prior to admission, fluid given in follow up 1, fluid given in follow up 2)
sixhr_fu2_dummy = 0 means total fluid used in the calculation should be all the fluid given up 
to and including the fluid given in follow up 1 (fluid given prior to admission & fluid given in follow up 1) */

gen sixhr_fu2_dummy = 0
replace sixhr_fu2_dummy = 1 if sixhr_thresholdfu1 < 0.25 & sixhr_thresholdfu2 >= 0.25

preserve
drop if sixhr_threshold == 0
sum sixhr_threshold, detail
restore

*below, we are calculating (IVF over ~6hrs * cost/mL) + cost of IV equipment
gen sixhr_IVF_fu1 = Form6FU1IVFluid + IVFluidPriortoGettingAdmit
gen sixhr_IVF_fu2 = Form6FU1IVFluid + IVFluidPriortoGettingAdmit + Form6FU2IVFluid

gen sixhr_IVF_cost_usd = 0
replace sixhr_IVF_cost_usd = (sixhr_IVF_fu1 * 0.00126) + 0.38 if sixhr_fu2_dummy == 0
replace sixhr_IVF_cost_usd = (sixhr_IVF_fu2 * 0.00126) + 0.38 if sixhr_fu2_dummy == 1

gen sixhr_IVF_cost_bdt = 0
replace sixhr_IVF_cost_bdt = (sixhr_IVF_fu1 * 0.104) + 32.59 if sixhr_fu2_dummy == 0
replace sixhr_IVF_cost_bdt = (sixhr_IVF_fu2 *  0.104) + 32.59 if sixhr_fu2_dummy == 1

*here, we are calculating (ORS over ~6hrs * cost/mL)
gen sixhr_ORS_fu1 = Form6FU1ORS
gen sixhr_ORS_fu2 = Form6FU1ORS + Form6FU2ORS

gen sixhr_ORS_cost_usd = 0
replace sixhr_ORS_cost_usd = (sixhr_ORS_fu1 * 0.00006) if sixhr_fu2_dummy == 0
replace sixhr_ORS_cost_usd = (sixhr_ORS_fu2 * 0.00006) if sixhr_fu2_dummy == 1

gen sixhr_ORS_cost_bdt = 0
replace sixhr_ORS_cost_bdt = (sixhr_ORS_fu1 * 0.0054) if sixhr_fu2_dummy == 0
replace sixhr_ORS_cost_bdt = (sixhr_ORS_fu2 * 0.0054) if sixhr_fu2_dummy == 1

*hospital cost fixed at 0.25*30=$6 or 0.25*2573.1=643.28 BDT
gen actual_sixhr_cost_usd = sixhr_IVF_cost_usd + sixhr_ORS_cost_usd + 6
gen actual_sixhr_cost_bdt = sixhr_IVF_cost_bdt + sixhr_ORS_cost_bdt + 643.28

*—–——————————————————————————————6 HOUR ACTUAL——————————————————————————————————
*FIRST 6 HOURS OF CARE — ALL PATIENTS
sum actual_sixhr_cost_usd, detail
sum actual_sixhr_cost_bdt, detail
tabstat actual_sixhr_cost_usd, stat(sum)
tabstat actual_sixhr_cost_bdt, stat(sum)

*FIRST 6 HOURS OF CARE COST — NO DEHYDRATION DIAGNOSIS
preserve
keep if ActualDehydrationCategory == "No"
sum actual_sixhr_cost_usd, detail
sum actual_sixhr_cost_bdt, detail
tabstat actual_sixhr_cost_usd, stat(sum)
tabstat actual_sixhr_cost_bdt, stat(sum)
restore

*FIRST 6 HOURS OF CARE COST — SOME DEHYDRATION DIAGNOSIS
preserve
keep if ActualDehydrationCategory == "Some"
sum actual_sixhr_cost_usd, detail
sum actual_sixhr_cost_bdt, detail
tabstat actual_sixhr_cost_usd, stat(sum)
tabstat actual_sixhr_cost_bdt, stat(sum)
restore

*FIRST 6 HOURS OF CARE COST — SEVERE DEHYDRATION DIAGNOSIS
preserve
keep if ActualDehydrationCategory == "Severe"
sum actual_sixhr_cost_usd, detail
sum actual_sixhr_cost_bdt, detail
tabstat actual_sixhr_cost_usd, stat(sum)
tabstat actual_sixhr_cost_bdt, stat(sum)
restore

*FIRST 6 HOURS OF CARE COST — ORS PATIENTS ONLY
preserve
drop if total_IVF > 0 | total_ORS == 0 
sum actual_sixhr_cost_usd, detail
sum actual_sixhr_cost_bdt, detail
tabstat actual_sixhr_cost_usd, stat(sum)
tabstat actual_sixhr_cost_bdt, stat(sum)
restore

*FIRST 6 HOURS OF CARE COST — IV PATIENTS ONLY
preserve
drop if total_ORS > 0 | total_IVF == 0
sum actual_sixhr_cost_usd, detail
sum actual_sixhr_cost_bdt, detail
tabstat actual_sixhr_cost_usd, stat(sum)
tabstat actual_sixhr_cost_bdt, stat(sum)
restore

*FIRST 6 HOURS OF CARE COST — IV & ORS COMBO PATIENTS ONLY
preserve
drop if total_ORS == 0 | total_IVF == 0
sum actual_sixhr_cost_usd, detail
sum actual_sixhr_cost_bdt, detail
tabstat actual_sixhr_cost_usd, stat(sum)
tabstat actual_sixhr_cost_bdt, stat(sum)
restore

*FIRST 6 HOURS OF CARE COST — NO FLUID PATIENTS
preserve
drop if total_fluid > 0
sum actual_sixhr_cost_usd, detail
sum actual_sixhr_cost_bdt, detail
tabstat actual_sixhr_cost_usd, stat(sum)
tabstat actual_sixhr_cost_bdt, stat(sum)
restore

/*********************INITIAL RECOMMENEDED RESCUSITATION (IRR)******************
——————————————————————————————————MODEL6 IRR————————————————————————————————————
SEVERE: (Model6VolumeDeficitL * 1000 * IVF cost per mL) + IV equipment + 1 day hospital stay
SOME: (Model6VolumeDeficitL * 1000 * ORS cost per mL) + 8 hrs hospital stay */
gen NIRUDAK_irr_cost_usd = .
replace NIRUDAK_irr_cost_usd = (Model6VolumeDeficitL * 1000 * 0.00126) + 0.38 + 30 if Model6DehydrationCategory == "Severe"
replace NIRUDAK_irr_cost_usd = (Model6VolumeDeficitL * 1000 * 0.00126) + 10 if Model6DehydrationCategory == "Some"
replace NIRUDAK_irr_cost_usd = 0 if Model6DehydrationCategory == "No"

gen NIRUDAK_irr_cost_bdt = .
replace NIRUDAK_irr_cost_bdt = (Model6VolumeDeficitL * 1000 * 0.104) + 32.59 + 2573.1 if Model6DehydrationCategory == "Severe"
replace NIRUDAK_irr_cost_bdt = (Model6VolumeDeficitL * 1000 * 0.104) + 857.7 if Model6DehydrationCategory == "Some"
replace NIRUDAK_irr_cost_bdt = 0 if Model6DehydrationCategory == "No"

*MODEL6 ALL
preserve
drop if Model6DehydrationCategory == ""
sum NIRUDAK_irr_cost_usd, detail
sum NIRUDAK_irr_cost_bdt, detail
tabstat NIRUDAK_irr_cost_usd, stat(sum)
tabstat NIRUDAK_irr_cost_bdt, stat(sum)
restore

*MODEL6 NO
preserve
keep if Model6DehydrationCategory == "No"
sum NIRUDAK_irr_cost_usd, detail
sum NIRUDAK_irr_cost_bdt, detail
tabstat NIRUDAK_irr_cost_usd, stat(sum)
tabstat NIRUDAK_irr_cost_bdt, stat(sum)
restore

*MODEL6 SOME
preserve
keep if Model6DehydrationCategory == "Some"
sum NIRUDAK_irr_cost_usd, detail
sum NIRUDAK_irr_cost_bdt, detail
tabstat NIRUDAK_irr_cost_usd, stat(sum)
tabstat NIRUDAK_irr_cost_bdt, stat(sum)
restore

*MODEL6 SEVERE
preserve
keep if Model6DehydrationCategory == "Severe"
sum NIRUDAK_irr_cost_usd, detail
sum NIRUDAK_irr_cost_bdt, detail
tabstat NIRUDAK_irr_cost_usd, stat(sum)
tabstat NIRUDAK_irr_cost_bdt, stat(sum)
restore

*———————————————————————————————————WHO IRR—————————————————————————————————————
drop WHO_irr_cost_usd
gen WHO_irr_cost_usd = .
replace WHO_irr_cost_usd = (WHOVolumeDeficitL * 1000 * 0.00126) + 0.38 + 30 if WHODehydrationCategory == "Severe"
replace WHO_irr_cost_usd = (WHOVolumeDeficitL * 1000 * 0.00126) + 10 if WHODehydrationCategory == "Some"
replace WHO_irr_cost_usd = 0 if WHODehydrationCategory == "No"

gen WHO_irr_cost_bdt = .
replace WHO_irr_cost_bdt = (WHOVolumeDeficitL * 1000 * 0.104) + 32.59 + 2573.1 if WHODehydrationCategory == "Severe"
replace WHO_irr_cost_bdt = (WHOVolumeDeficitL * 1000 * 0.104) + 857.7 if WHODehydrationCategory == "Some"
replace WHO_irr_cost_bdt = 0 if WHODehydrationCategory == "No"

*WHO ALL
preserve
drop if WHODehydrationCategory == ""
sum WHO_irr_cost_usd, detail
sum WHO_irr_cost_bdt, detail
tabstat WHO_irr_cost_usd, stat(sum)
tabstat WHO_irr_cost_bdt, stat(sum)
restore

*WHO SEVERE
preserve
keep if WHODehydrationCategory == "Severe"
sum WHO_irr_cost_usd, detail
sum WHO_irr_cost_bdt, detail
tabstat WHO_irr_cost_usd, stat(sum)
tabstat WHO_irr_cost_bdt, stat(sum)
restore

*WHO SOME
preserve
keep if WHODehydrationCategory == "Some"
sum WHO_irr_cost_usd, detail
sum WHO_irr_cost_bdt, detail
tabstat WHO_irr_cost_usd, stat(sum)
tabstat WHO_irr_cost_bdt, stat(sum)
restore

*WHO NO
preserve
keep if WHODehydrationCategory == "No"
sum WHO_irr_cost_usd, detail
sum WHO_irr_cost_bdt, detail
tabstat WHO_irr_cost_usd, stat(sum)
tabstat WHO_irr_cost_bdt, stat(sum)
restore

*——————————————————————————————————ACTUAL IRR———————————————————————————————————
gen actual_irr_cost_usd = .
replace actual_irr_cost_usd = total_fluid_cost_usd + 30 if ActualDehydrationCategory == "Severe"
replace actual_irr_cost_usd = total_fluid_cost_usd + 10 if ActualDehydrationCategory == "Some"
replace actual_irr_cost_usd = total_fluid_cost_usd if ActualDehydrationCategory == "No"

gen actual_irr_cost_bdt = .
replace actual_irr_cost_bdt = total_fluid_cost_bdt + 2573.1 if ActualDehydrationCategory == "Severe"
replace actual_irr_cost_bdt = total_fluid_cost_bdt + 857.7 if ActualDehydrationCategory == "Some"
replace actual_irr_cost_bdt = total_fluid_cost_bdt if ActualDehydrationCategory == "No"

*ACTUAL IRR: ALL PATIENTS
preserve
sum actual_irr_cost_usd, detail
sum actual_irr_cost_bdt, detail
tabstat actual_irr_cost_usd, stat(sum)
tabstat actual_irr_cost_bdt, stat(sum)
restore

*ACTUAL SEVERE
preserve
keep if ActualDehydrationCategory == "Severe"
sum actual_irr_cost_usd, detail
sum actual_irr_cost_bdt, detail
tabstat actual_irr_cost_usd, stat(sum)
tabstat actual_irr_cost_bdt, stat(sum)
restore

*ACTUAL SOME
preserve
keep if ActualDehydrationCategory == "Some"
sum actual_irr_cost_usd, detail
sum actual_irr_cost_bdt, detail
tabstat actual_irr_cost_usd, stat(sum)
tabstat actual_irr_cost_bdt, stat(sum)
restore

*ACTUAL NO
preserve
keep if ActualDehydrationCategory == "No"
sum actual_irr_cost_usd, detail
sum actual_irr_cost_bdt, detail
tabstat actual_irr_cost_usd, stat(sum)
tabstat actual_irr_cost_bdt, stat(sum)
restore
