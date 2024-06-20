/*—–—————————————————————————FIXED COST: IV EQUIPMENT———————————————————————————
PATIENTS UNDER 5 (DHAKA analysis)
IV tubing/solution set + 1 pair of gloves + 1 24G butterfly needle
0.17 + 0.08 + 0.15 = $0.40
34.31 BDT

————————————————————————————VARIABLE COST: FLUIDS———————————————————————————————
$0.00006 (0.0054 BDT) per mL ORS
$0.00126 (0.104 BDT) per mL IVF */

********************************************************************************

*—————————————————————————ACTUAL/GOLD STD FLUID ONLY————————————————————————————
*IVF
gen total_IVF = ivprior + ivtotal
gen total_IVF_cost_usd = (total_IVF * 0.00126) + 0.4
gen total_IVF_cost_bdt = (total_IVF * 0.104) + 34.31

*ORS
gen total_ORS_cost_usd = orstotal * 0.00006
gen total_ORS_cost_bdt = orstotal * 0.0054

*IVF + ORS (all fluid administered)
gen total_fluid = orstotal + total_IVF

gen total_fluid_cost_usd = total_ORS_cost_usd + total_IVF_cost_usd
gen total_fluid_cost_bdt = total_ORS_cost_bdt + total_IVF_cost_bdt


*ACTUAL/GOLD STD — ALL
preserve
drop if total_fluid_cost_usd == . | total_ORS_cost_usd == . | total_IVF_cost_usd == .
sum total_fluid_cost_usd, detail
sum total_fluid_cost_bdt, detail
tabstat total_fluid_cost_usd, stat(sum)
tabstat total_fluid_cost_bdt, stat(sum)
restore

*ACTUAL/GOLD STD — NO
preserve
keep if GoldStandardCategory == "No"
drop if total_fluid_cost_usd == . | total_ORS_cost_usd == . | total_IVF_cost_usd == .
sum total_fluid_cost_usd, detail
sum total_fluid_cost_bdt, detail
tabstat total_fluid_cost_usd, stat(sum)
tabstat total_fluid_cost_bdt, stat(sum)
restore

*ACTUAL/GOLD STD — SOME
preserve
keep if GoldStandardCategory == "Some"
drop if total_fluid_cost_usd == . | total_ORS_cost_usd == . | total_IVF_cost_usd == .
sum total_fluid_cost_usd, detail
sum total_fluid_cost_bdt, detail
tabstat total_fluid_cost_usd, stat(sum)
tabstat total_fluid_cost_bdt, stat(sum)
restore

*ACTUAL/GOLD STD — SEVERE
preserve
keep if GoldStandardCategory == "Severe"
drop if total_fluid_cost_usd == . | total_ORS_cost_usd == . | total_IVF_cost_usd == .
sum total_fluid_cost_usd, detail
sum total_fluid_cost_bdt, detail
tabstat total_fluid_cost_usd, stat(sum)
tabstat total_fluid_cost_bdt, stat(sum)
restore

/*———————————————————————————————WHO FLUID ONLY—————————————————————————————————
SEVERE DEHYDRATION:fluid required by WHO model (IVF only) + IV equipment 
SOME DEHYDRATION: fluid required by WHO model (ORS only)
NO DEHYDRATION: $0
*/

gen WHO_fluid_only_cost_usd = .
replace WHO_fluid_only_cost_usd = (WHOVolumeDeficitL * 1000 * 0.00126) + 0.38 if WHODehydrationCategory == 		"Severe"
replace WHO_fluid_only_cost_usd = (WHOVolumeDeficitL * 1000 * 0.00006) if WHODehydrationCategory == "Some"
replace WHO_fluid_only_cost_usd = 0 if WHODehydrationCategory == "No"

gen WHO_fluid_only_cost_bdt = .
replace WHO_fluid_only_cost_bdt = (WHOVolumeDeficitL * 1000 * 0.104) + 34.31 if WHODehydrationCategory == 		"Severe"
replace WHO_fluid_only_cost_bdt = (WHOVolumeDeficitL * 1000 * 0.0054) if WHODehydrationCategory == "Some"
replace WHO_fluid_only_cost_bdt = 0 if WHODehydrationCategory == "No"

*ALL
preserve
drop if WHODehydrationCategory == "" | WHO_fluid_only_cost_usd == .
sum WHO_fluid_only_cost_usd, detail
sum WHO_fluid_only_cost_bdt, detail
tabstat WHO_fluid_only_cost_usd, stat(sum)
tabstat WHO_fluid_only_cost_bdt, stat(sum)
restore

*SEVERE
preserve
drop if WHODehydrationCategory == "" | WHO_fluid_only_cost_usd == .
keep if WHODehydrationCategory == "Severe"
sum WHO_fluid_only_cost_usd, detail
sum WHO_fluid_only_cost_bdt, detail
tabstat WHO_fluid_only_cost_usd, stat(sum)
tabstat WHO_fluid_only_cost_bdt, stat(sum)
restore

*SOME
preserve
keep if WHODehydrationCategory == "Some"
drop if WHODehydrationCategory == "" | WHO_fluid_only_cost_usd == .
sum WHO_fluid_only_cost_usd, detail
sum WHO_fluid_only_cost_bdt, detail
tabstat WHO_fluid_only_cost_usd, stat(sum)
tabstat WHO_fluid_only_cost_bdt, stat(sum)
restore

*NO
preserve
keep if WHODehydrationCategory == "No"
drop if WHODehydrationCategory == "" | WHO_fluid_only_cost_usd == .
sum WHO_fluid_only_cost_usd, detail
sum WHO_fluid_only_cost_bdt, detail
tabstat WHO_fluid_only_cost_usd, stat(sum)
tabstat WHO_fluid_only_cost_bdt, stat(sum)
restore


/*——————————————————————————————DHAKA FLUID ONLY————————————————————————————————
SEVERE DEHYDRATION:fluid required by DHAKA model (IVF only) + IV 
	equipment 
SOME DEHYDRATION: fluid required by DHAKA model (ORS only)
NO DEHYDRATION: $0 */

gen DHAKA_fluid_only_cost_usd = .
replace DHAKA_fluid_only_cost_usd = (DHAKAVolumeDeficitL * 1000 * 0.00126) + 0.38 if DHAKADehydrationCategory 	== "Severe"
replace DHAKA_fluid_only_cost_usd = (DHAKAVolumeDeficitL * 1000 * 0.00006) if DHAKADehydrationCategory == 		"Some"
replace DHAKA_fluid_only_cost_usd = 0 if DHAKADehydrationCategory == "No"

gen DHAKA_fluid_only_cost_bdt = .
replace DHAKA_fluid_only_cost_bdt = (DHAKAVolumeDeficitL * 1000 * 0.104) + 34.31 if DHAKADehydrationCategory 	== "Severe"
replace DHAKA_fluid_only_cost_bdt = (DHAKAVolumeDeficitL * 1000 * 0.0054) if DHAKADehydrationCategory == "Some"
replace DHAKA_fluid_only_cost_bdt = 0 if DHAKADehydrationCategory == "No"

*ALL
preserve
drop if DHAKADehydrationCategory == "" | DHAKA_fluid_only_cost_usd == .
sum DHAKA_fluid_only_cost_usd, detail
sum DHAKA_fluid_only_cost_bdt, detail
tabstat DHAKA_fluid_only_cost_usd, stat(sum)
tabstat DHAKA_fluid_only_cost_bdt, stat(sum)
restore

*SEVERE
preserve
keep if DHAKADehydrationCategory == "Severe"
drop if DHAKA_fluid_only_cost_usd == .
sum DHAKA_fluid_only_cost_usd, detail
sum DHAKA_fluid_only_cost_bdt, detail
tabstat DHAKA_fluid_only_cost_usd, stat(sum)
tabstat DHAKA_fluid_only_cost_bdt, stat(sum)
restore

*SOME
preserve
keep if DHAKADehydrationCategory == "Some"
drop if DHAKA_fluid_only_cost_usd == .
sum DHAKA_fluid_only_cost_usd, detail
sum DHAKA_fluid_only_cost_bdt, detail
tabstat DHAKA_fluid_only_cost_usd, stat(sum)
tabstat DHAKA_fluid_only_cost_bdt, stat(sum)
restore

*NO
preserve
keep if DHAKADehydrationCategory == "No"
drop if DHAKA_fluid_only_cost_usd == .
sum DHAKA_fluid_only_cost_usd, detail
sum DHAKA_fluid_only_cost_bdt, detail
tabstat DHAKA_fluid_only_cost_usd, stat(sum)
tabstat DHAKA_fluid_only_cost_bdt, stat(sum)
restore
