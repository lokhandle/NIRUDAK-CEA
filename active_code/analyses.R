user = 'Jonah'
#user = 'Anagha'

##Set working directory
jonah_work_dir <- "C:/Users/Jonah/Desktop/NIRUDAK-CEA"

anagha_work_dir <- "/Users/anaghalokhande/Desktop/Research/Levine-NIRUDAK-CEA"

if (user=='Jonah'){
	setwd(jonah_work_dir)	
}

if (user=='Anagha'){
  setwd(anagha_work_dir)	
}


##run the prepare_data script
source('active_code/prepare_data.r')

ls()

