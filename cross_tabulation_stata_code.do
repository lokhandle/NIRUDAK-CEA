********************************************************************************
**********************************NIRUDAK***************************************

********************************** SEVERE
*TRUE SEVERE/NIRUDAK SEVERE
preserve
keep if ActualDehydrationCategory == "Severe"
keep if Model6DehydrationCategory == "Severe"
summarize
restore

*TRUE SEVERE/NIRUDAK SOME
preserve
keep if ActualDehydrationCategory == "Severe"
keep if Model6DehydrationCategory == "Some"
summarize
restore

*TRUE SEVERE/NIRUDAK NO
preserve
keep if ActualDehydrationCategory == "Severe"
keep if Model6DehydrationCategory == "No"
summarize
restore

********************************** SOME
*TRUE SOME/NIRUDAK SEVERE
preserve
keep if ActualDehydrationCategory == "Some"
keep if Model6DehydrationCategory == "Severe"
summarize
restore

*TRUE SOME/NIRUDAK SOME
preserve
keep if ActualDehydrationCategory == "Some"
keep if Model6DehydrationCategory == "Some"
summarize
restore

*TRUE SOME/NIRUDAK NO
preserve
keep if ActualDehydrationCategory == "Some"
keep if Model6DehydrationCategory == "No"
summarize
restore

********************************** NO
*TRUE NO/NIRUDAK SEVERE
preserve
keep if ActualDehydrationCategory == "No"
keep if Model6DehydrationCategory == "Severe"
summarize
restore

*TRUE NO/NIRUDAK SOME
preserve
keep if ActualDehydrationCategory == "No"
keep if Model6DehydrationCategory == "Some"
summarize
restore

*TRUE NO/NIRUDAK NO
preserve
keep if ActualDehydrationCategory == "No"
keep if Model6DehydrationCategory == "No"
summarize
restore


********************************************************************************
**********************************WHO*******************************************

********************************** SEVERE
*TRUE SEVERE/WHO SEVERE
preserve
keep if ActualDehydrationCategory == "Severe"
keep if WHODehydrationCategory == "Severe"
summarize
restore

*TRUE SEVERE/WHO SOME
preserve
keep if ActualDehydrationCategory == "Severe"
keep if WHODehydrationCategory == "Some"
summarize
restore

*TRUE SEVERE/WHO NO
preserve
keep if ActualDehydrationCategory == "Severe"
keep if WHODehydrationCategory == "No"
summarize
restore

********************************** SOME
*TRUE SOME/WHO SEVERE
preserve
keep if ActualDehydrationCategory == "Some"
keep if WHODehydrationCategory == "Severe"
summarize
restore

*TRUE SOME/WHO SOME
preserve
keep if ActualDehydrationCategory == "Some"
keep if WHODehydrationCategory == "Some"
summarize
restore

*TRUE SOME/WHO NO
preserve
keep if ActualDehydrationCategory == "Some"
keep if WHODehydrationCategory == "No"
summarize
restore

********************************** NO
*TRUE NO/WHO SEVERE
preserve
keep if ActualDehydrationCategory == "No"
keep if WHODehydrationCategory == "Severe"
summarize
restore

*TRUE NO/WHO SOME
preserve
keep if ActualDehydrationCategory == "No"
keep if WHODehydrationCategory == "Some"
summarize
restore

*TRUE NO/WHO NO
preserve
keep if ActualDehydrationCategory == "No"
keep if WHODehydrationCategory == "No"
summarize
restore
