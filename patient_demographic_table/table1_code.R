library(data.table)
library(gtsummary)

data <- fread("/Users/anaghalokhande/Documents/research_levine/NIRUDAK-CEA_github/patient_demographic_table/table1_data.csv")

# creating age_category variable
data$age_category <- 'children'

# 
data[, age_category := fifelse(age<20, 'children', 'adults')]

# 
data[, age_category := fcase(age<20, 'children',
                             age< 60, 'adults',
                             default = 'elderly')]

# changing education variable: for children, their "education" is their mom's educations
data[, education := fifelse(age_category == 'children', mom_education, education)]

# reference code to change variable name in case the final table gets made from this R code
# var_label(data$age_category) <- "Age Category"

# generating table
data %>%
  tbl_summary(by = age_category) %>%
  add_overall()

