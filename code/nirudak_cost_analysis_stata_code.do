/*note: 'NIRUDAK' & 'Model6' are used interchangeably in this code and refer to hypothetical costs from the NIRUDAK model

the data file associated with this code contains some unnecessary variables and some variables from Draft 1 that are not relevant for this revised/consolidated new analysis. these variables can be ignored.

the revised analysis focuses on 2 comparisons: (1) comparison of total costs of initial recommended resuscitation (2) comparison of fluid costs associated with initial recommended resuscitation */

/*—–—————————————————————————FIXED COST: IV EQUIPMENT———————————————————————————
PATIENTS OVER 5 (NIRUDAK analysis)
IV tubing/solution set + 1 pair of gloves + 1 22G butterfly needle
0.17 + 0.08 + 0.13 = $0.38
32.59 BDT

————————————————————————————VARIABLE COST: FLUIDS———————————————————————————————
$0.00006 (0.0054 BDT) per mL ORS
$0.00126 (0.104 BDT) per mL IVF

————————————————–————————VARIABLE COST: HOSPITAL STAY———————————————————————————
all patients admitted to short stay unit
$30 (2573.1 BDT) per 24 hours (1 day)
$1.25 (107 BDT) per hour
additional $30 for each day */
gen length_stay_days = Form8DischargeDate - AdmitDate
gen length_stay_hrs_initial = Form8_Discharge_Time_adj - AdmitTime_adj + length_stay_days

gen length_stay_hrs = length_stay_hrs_initial
replace length_stay_hrs = length_stay_hrs_initial + 1 if length_stay_hrs_initial < 0

gen hospital_cost_usd = length_stay_hrs * 30
gen hospital_cost_bdt = length_stay_hrs * 2573.1

********************************************************************************
*******(1) COMPARISON OF TOTAL COSTS OF INTIAL RECOMMENDED RESUSCITATION********
/*————————————————————————–————————ACTUAL TOTAL——————————–————————————————————*/
*ACTUAL ALL
preserve
drop if hospital_cost_usd == .
drop if length_stay_days == -60
sum actual_total_cost_usd, detail
sum actual_total_cost_bdt, detail
tabstat actual_total_cost_usd, stat(sum)
tabstat actual_total_cost_bdt, stat(sum)
restore

*ACTUAL — SEVERE
preserve
keep if ActualDehydrationCategory == "Severe"
drop if hospital_cost_usd == .
drop if length_stay_days == -60
sum actual_total_cost_usd, detail
sum actual_total_cost_bdt, detail
tabstat actual_total_cost_usd, stat(sum)
tabstat actual_total_cost_bdt, stat(sum)
restore

*ACTUAL — SOME
preserve
keep if ActualDehydrationCategory == "Some"
drop if hospital_cost_usd == .
drop if length_stay_days == -60
sum actual_total_cost_usd, detail
sum actual_total_cost_bdt, detail
tabstat actual_total_cost_usd, stat(sum)
tabstat actual_total_cost_bdt, stat(sum)
restore

*ACTUAL — NO
preserve
keep if ActualDehydrationCategory == "No"
drop if hospital_cost_usd == .
drop if length_stay_days == -60
sum actual_total_cost_usd, detail
sum actual_total_cost_bdt, detail
tabstat actual_total_cost_usd, stat(sum)
tabstat actual_total_cost_bdt, stat(sum)
restore

/*—————————————————————————————————WHO TOTAL————————————————————————————————————
SEVERE DEHYDRATION: total hospital stay + fluid required by WHO model (IVF only; assumed to be for the first 
	6 hours) + IV equipment + actual/gold std fluid received after first 6 hours (3rd follow-up and onward)
SOME DEHYDRATION: cost of 4 hours hospital stay + fluid required by WHO model (ORS only)
	cost of 4 hours of hospital stay: $5 (429 BDT)
NO DEHYDRATION: $0 */
gen sixhr_thresholdfu1 = Form6_FU1_Time_adj - AdmitTime_adj
replace sixhr_thresholdfu1 = (1 + sixhr_thresholdfu1) if sixhr_thresholdfu1 < 0
sum sixhr_thresholdfu1, detail

gen sixhr_thresholdfu2 = Form6_FU2_Time_adj - AdmitTime_adj
replace sixhr_thresholdfu2 = (1 + sixhr_thresholdfu2) if sixhr_thresholdfu2 < 0
sum sixhr_thresholdfu2, detail

*kept sixhr_thresholdfu3 just for reference, not used in analysis
gen sixhr_thresholdfu3 = Form6_FU3_Time_adj - AdmitTime_adj
replace sixhr_thresholdfu3 = (1 + sixhr_thresholdfu3) if sixhr_thresholdfu3 < 0
sum sixhr_thresholdfu3, detail

gen sixhr_threshold = .
replace sixhr_threshold = sixhr_thresholdfu2
replace sixhr_threshold = sixhr_thresholdfu1 if sixhr_thresholdfu1 >= 0.25 | sixhr_thresholdfu2 == .

*assuming 2 follow-ups = 6 hours
gen post_6hr_IVF = Form6FU3IVFluid + Form6FU4IVFluid + Form6FU5IVFluid + Form6FU6IVFluid + Form6FU7IVFluid + Form7FU8IVFluid + Form7FU9IVFluid + Form7FU10IVFluid + Form7FU11IVFluid + Form7FU12IVFluid + Form7FU13IVFluid + Form7FU14IVFluid
replace post_6hr_IVF = Form6FU2IVFluid+ Form6FU3IVFluid + Form6FU4IVFluid + Form6FU5IVFluid + Form6FU6IVFluid + Form6FU7IVFluid + Form7FU8IVFluid + Form7FU9IVFluid + Form7FU10IVFluid + Form7FU11IVFluid + Form7FU12IVFluid + Form7FU13IVFluid + Form7FU14IVFluid if sixhr_thresholdfu1 >= 0.25

gen post_6hr_ORS = Form6FU3ORS + Form6FU4ORS + Form6FU5ORS + Form6FU6ORS + Form6FU7ORS + Form7FU8ORS + Form7FU9ORS + Form7FU10ORS + Form7FU11ORS + Form7FU12ORS + Form7FU13ORS + Form7FU14ORS
replace post_6hr_ORS = Form6FU2ORS + Form6FU3ORS + Form6FU4ORS + Form6FU5ORS + Form6FU6ORS + Form6FU7ORS + Form7FU8ORS + Form7FU9ORS + Form7FU10ORS + Form7FU11ORS + Form7FU12ORS + Form7FU13ORS + Form7FU14ORS if sixhr_thresholdfu1 >= 0.25

*this IVF cost variable does not include IV equipment costs
gen post_6hr_IVF_cost_usd = post_6hr_IVF * 0.00126
gen post_6hr_IVF_cost_bdt = post_6hr_IVF * 0.104

gen post_6hr_ORS_cost_usd = post_6hr_ORS * 0.00006
gen post_6hr_ORS_cost_bdt = post_6hr_ORS * 0.0054

gen WHO_irr_cost_usd2 = .
replace WHO_irr_cost_usd2 = hospital_cost_usd + (WHOVolumeDeficitL * 1000 * 0.00126) + 0.38 + post_6hr_IVF_cost_usd if WHODehydrationCategory == "Severe"
replace WHO_irr_cost_usd2 = 5 + (WHOVolumeDeficitL * 1000 * 0.00006) if WHODehydrationCategory == "Some"
replace WHO_irr_cost_usd2 = 0 if WHODehydrationCategory == "No"

gen WHO_irr_cost_bdt2 = .
replace WHO_irr_cost_bdt2 = hospital_cost_bdt + (WHOVolumeDeficitL * 1000 * 0.104) + 32.59 + post_6hr_IVF_cost_bdt if WHODehydrationCategory == "Severe"
replace WHO_irr_cost_bdt2 = 429 + (WHOVolumeDeficitL * 1000 * 0.0054) if WHODehydrationCategory == "Some"
replace WHO_irr_cost_bdt2 = 0 if WHODehydrationCategory == "No"

*WHO — ALL
preserve
drop if WHODehydrationCategory == ""
drop if hospital_cost_usd == .
drop if length_stay_days == -60
sum WHO_irr_cost_usd2, detail
sum WHO_irr_cost_bdt2, detail
tabstat WHO_irr_cost_usd2, stat(sum)
tabstat WHO_irr_cost_bdt2, stat(sum)
restore

*WHO — SEVERE
preserve
keep if WHODehydrationCategory == "Severe"
drop if hospital_cost_usd == .
drop if length_stay_days == -60
sum WHO_irr_cost_usd2, detail
sum WHO_irr_cost_bdt2, detail
tabstat WHO_irr_cost_usd2, stat(sum)
tabstat WHO_irr_cost_bdt2, stat(sum)
restore

*WHO — SOME
preserve
keep if WHODehydrationCategory == "Some"
drop if hospital_cost_usd == .
drop if length_stay_days == -60
sum WHO_irr_cost_usd2, detail
sum WHO_irr_cost_bdt2, detail
tabstat WHO_irr_cost_usd2, stat(sum)
tabstat WHO_irr_cost_bdt2, stat(sum)
restore

*WHO — NO
preserve
keep if WHODehydrationCategory == "No"
drop if hospital_cost_usd == .
drop if length_stay_days == -60
sum WHO_irr_cost_usd2, detail
sum WHO_irr_cost_bdt2, detail
tabstat WHO_irr_cost_usd2, stat(sum)
tabstat WHO_irr_cost_bdt2, stat(sum)
restore

/*—————————————————————————————NIRUDAK/MODEL6 TOTAL————————————————————————————
SEVERE DEHYDRATION: total hospital stay + fluid required by NIRUDAK model (IVF only; assumed to be for the 	
	first 6 hours) + IV equipment + actual/gold std fluid received after first 6 hours (3rd follow-up and 	
	onward)
SOME DEHYDRATION: cost of 4 hours hospital stay + fluid required by NIRUDAK model (ORS only)
	cost of 4 hours of hospital stay: $5 (429 BDT)
NO DEHYDRATION: $0 */ 

gen NIRUDAK_irr_cost_usd2 = .
replace NIRUDAK_irr_cost_usd2 = hospital_cost_usd + (Model6VolumeDeficitL * 1000 * 0.00126) + 0.38 + post_6hr_IVF_cost_usd if Model6DehydrationCategory == "Severe"
replace NIRUDAK_irr_cost_usd2 = 5 + (Model6VolumeDeficitL * 1000 * 0.00006) if Model6DehydrationCategory == "Some"
replace NIRUDAK_irr_cost_usd2 = 0 if Model6DehydrationCategory == "No"

gen NIRUDAK_irr_cost_bdt2 = .
replace NIRUDAK_irr_cost_bdt2 = hospital_cost_bdt + (Model6VolumeDeficitL * 1000 * 0.104) + 32.59 + post_6hr_IVF_cost_bdt if Model6DehydrationCategory == "Severe"
replace NIRUDAK_irr_cost_bdt2 = 429 + (Model6VolumeDeficitL * 1000 * 0.0054) if Model6DehydrationCategory == "Some"
replace NIRUDAK_irr_cost_bdt2 = 0 if Model6DehydrationCategory == "No"

*NIRUDAK ALL
preserve
drop if Model6DehydrationCategory == ""
drop if hospital_cost_usd == .
drop if length_stay_days == -60
sum NIRUDAK_irr_cost_usd2, detail
sum NIRUDAK_irr_cost_bdt2, detail
tabstat NIRUDAK_irr_cost_usd2, stat(sum)
tabstat NIRUDAK_irr_cost_bdt2, stat(sum)
restore

*NIRUDAK SEVERE
preserve
keep if Model6DehydrationCategory == "Severe"
drop if hospital_cost_usd == .
drop if length_stay_days == -60
sum NIRUDAK_irr_cost_usd2, detail
sum NIRUDAK_irr_cost_bdt2, detail
tabstat NIRUDAK_irr_cost_usd2, stat(sum)
tabstat NIRUDAK_irr_cost_bdt2, stat(sum)
restore

*NIRUDAK SOME
preserve
keep if Model6DehydrationCategory == "Some"
drop if hospital_cost_usd == .
drop if length_stay_days == -60
sum NIRUDAK_irr_cost_usd2, detail
sum NIRUDAK_irr_cost_bdt2, detail
tabstat NIRUDAK_irr_cost_usd2, stat(sum)
tabstat NIRUDAK_irr_cost_bdt2, stat(sum)
restore

*NIRUDAK NO
preserve
keep if Model6DehydrationCategory == "No"
drop if hospital_cost_usd == .
drop if length_stay_days == -60
sum NIRUDAK_irr_cost_usd2, detail
sum NIRUDAK_irr_cost_bdt2, detail
tabstat NIRUDAK_irr_cost_usd2, stat(sum)
tabstat NIRUDAK_irr_cost_bdt2, stat(sum)
restore
********************************************************************************
********************************************************************************


********************************************************************************
*******(2) COMPARISON OF FLUID COSTS OF INTIAL RECOMMENDED RESUSCITATION********

*—————————————————————————ACTUAL/GOLD STD FLUID ONLY————————————————————————————
*IVF
gen sixhr_actual_IVF = .
replace sixhr_actual_IVF = Form6FU1IVFluid + IVFluidPriortoGettingAdmit + Form6FU2IVFluid
replace sixhr_actual_IVF = Form6FU1IVFluid + IVFluidPriortoGettingAdmit if sixhr_thresholdfu1 >= 0.25

gen sixhr_IVF_cost_usd = (sixhr_actual_IVF * 0.00126) + 0.38
gen sixhr_IVF_cost_bdt = (sixhr_actual_IVF * 0.104) + 32.59

*ORS
gen sixhr_actual_ORS = .
replace sixhr_actual_ORS = Form6FU1ORS + Form6FU2ORS
replace sixhr_actual_ORS = Form6FU1ORS if sixhr_thresholdfu1 >= 0.25

gen sixhr_ORS_cost_usd = sixhr_actual_ORS * 0.00006
gen sixhr_ORS_cost_bdt = sixhr_actual_ORS * 0.0054

*IVF + ORS
gen total6hr_fluid_cost_usd = sixhr_ORS_cost_usd + sixhr_IVF_cost_usd
gen total6hr_fluid_cost_bdt = sixhr_ORS_cost_bdt + sixhr_IVF_cost_bdt


*ACTUAL — ALL
preserve
sum total6hr_fluid_cost_usd, detail
sum total6hr_fluid_cost_bdt, detail
tabstat total6hr_fluid_cost_usd, stat(sum)
tabstat total6hr_fluid_cost_bdt, stat(sum)
restore

*ACTUAL — NO
preserve
keep if ActualDehydrationCategory == "No"
sum total6hr_fluid_cost_usd, detail
sum total6hr_fluid_cost_bdt, detail
tabstat total6hr_fluid_cost_usd, stat(sum)
tabstat total6hr_fluid_cost_bdt, stat(sum)
restore

*ACTUAL — SOME
preserve
keep if ActualDehydrationCategory == "Some"
sum total6hr_fluid_cost_usd, detail
sum total6hr_fluid_cost_bdt, detail
tabstat total6hr_fluid_cost_usd, stat(sum)
tabstat total6hr_fluid_cost_bdt, stat(sum)
restore

*ACTUAL — SEVERE
preserve
keep if ActualDehydrationCategory == "Severe"
sum total6hr_fluid_cost_usd, detail
sum total6hr_fluid_cost_bdt, detail
tabstat total6hr_fluid_cost_usd, stat(sum)
tabstat total6hr_fluid_cost_bdt, stat(sum)
restore

/*———————————————————————————————WHO FLUID ONLY—————————————————————————————————
SEVERE DEHYDRATION:fluid required by WHO model (IVF only; assumed to be for the first 6 hours) + IV equipment 
SOME DEHYDRATION:fluid required by WHO model (ORS only)
NO DEHYDRATION: $0
*/
*ALL
preserve
drop if WHODehydrationCategory == ""
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

*SOME
preserve
keep if WHODehydrationCategory == "Some"
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


/*——————————————————————————NIRUDAK/MODEL6 FLUID ONLY———————————————————————————
SEVERE DEHYDRATION:fluid required by NIRUDAK model (IVF only; assumed to be for the first 6 hours) + IV equipment 
SOME DEHYDRATION:fluid required by NIRUDAK model (ORS only)
NO DEHYDRATION: $0 */

*ALL
preserve
drop if Model6DehydrationCategory == ""
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

*SOME
preserve
keep if Model6DehydrationCategory == "Some"
sum NIRUDAK_fluid_only_cost_usd, detail
sum NIRUDAK_fluid_only_cost_bdt, detail
tabstat NIRUDAK_fluid_only_cost_usd, stat(sum)
tabstat NIRUDAK_fluid_only_cost_bdt, stat(sum)
restore

*NO
preserve
keep if Model6DehydrationCategory == "No"
sum NIRUDAK_fluid_only_cost_usd, detail
sum NIRUDAK_fluid_only_cost_bdt, detail
tabstat NIRUDAK_fluid_only_cost_usd, stat(sum)
tabstat NIRUDAK_fluid_only_cost_bdt, stat(sum)
restore

