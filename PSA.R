user = 'Jonah'


#load packages
library(tidyverse)


#Set working directory
jonah_work_dir <- "C:/Users/Jonah/Dropbox (Brown)/Shared Stuff/DHAKA & NIRUDAK Cost-effectiveness analysis"

if (user=='Jonah'){
	setwd(jonah_work_dir)	
}

#load data
tmp_data <- readxl::read_excel(file.path("previous work", "NIRUDAK_over5yrs_raw_data_.xlsx"))
names(tmp_data)
data <- tmp_data %>% rename(id="Study ID") %>% rename(admit_date="Admit Date") %>% 
		rename(admit_time="Admit Time") %>% mutate(male=as.integer(Sex=="Male")) %>%
		select(-c(Sex, sex1)) %>% rename(age=Age) %>% rename(admit_wt="Admit Weight") %>%
		rename(

with(tmp_data, Age)


tmp_data %>% names()




?drop





