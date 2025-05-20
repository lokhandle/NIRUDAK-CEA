library(data.table)
library(gtsummary)

data <- fread("/Users/anaghalokhande/Desktop/research/Levine_NIRUDAK_CEA/old_code/table2_patient_demographics/table2_data.csv")

# creating age_category variable
data$age_category <- 'blank'

# setting age categories based on WHO guidelines
data[, age_category := fcase(age<20, 'children',
                             age< 60, 'adults',
                             default = 'elderly')]

# setting education based on past NIRUDAK paper (Levine et al. in PLOS Neglected Tropical Diseases)
# for children, their # of years of "education" is their mom's # years of education
data[, education := fifelse(age <17, mom_education, education)]

# reference code to change variable name in case the final table gets made from this R code
# var_label(data$age_category) <- "Age Category"

# converting monthly income from BDT to USD  (using World Bank PPP)
data$monthly_income <- data$monthly_income/31.29

final_data <- data[,-4]

# generating table
final_data %>%
  tbl_summary(by = age_category) %>%
  add_overall()

