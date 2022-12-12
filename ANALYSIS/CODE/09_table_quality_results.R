# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot vocal mimicry
# Date started: 12-12-2022
# Date last modified: 12-12-2022
# Author: Simeon Q. Smeele
# Description: Making table with overview quality model results.   
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Set-up ----

# Loading libraries
libraries = c('tidyverse', 'rethinking', 'cmdstanr')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list = ls()) 

# Paths
path_models_quality = 'ANALYSIS/RESULTS/models_quality.RData'
path_overview_results_quality = 'ANALYSIS/RESULTS/table quality.csv'

# Load models
load(path_models_quality)

# Create data frame with results
pi_long = post_long$b_long |> PI() |> round(2)
pi_soc = post_soc$cont_greg |> PI() |> round(2)
pi_brain = post_brain$b_brain |> PI() |> round(2)
pi_body = post_body$b_body |> PI() |> round(2)
pi_hab_1 = (post_hab$a_hab[,1] - post_hab$a_hab[,2]) |> PI() |> round(2)
pi_hab_2 = (post_hab$a_hab[,1] - post_hab$a_hab[,3]) |> PI() |> round(2)
pi_hab_3 = (post_hab$a_hab[,2] - post_hab$a_hab[,3]) |> PI() |> round(2)
pi_temp = (post_temp$a_temp[,1] - post_temp$a_temp[,2]) |> PI() |> round(2)
pi_cont = post_cont$sigma_cont |> PI() |> round(2)

d = data.frame(effect = c('total effect longevity (beta)',
                          'total effect sociality (contrast)',
                          'total effect relative brain size (beta)',
                          'total effect body size (beta)',
                          'contrast closed vs mixed',
                          'contrast closed vs open',
                          'contrast mixed vs open',
                          'total effect template (contrast)',
                          'sigma effect context'),
               mean = c(post_long$b_long |> mean() |> round(2),
                        post_soc$cont_greg |> mean() |> round(2),
                        post_brain$b_brain |> mean() |> round(2),
                        post_body$b_body |> mean() |> round(2),
                        (post_hab$a_hab[,1] - post_hab$a_hab[,2]) |> mean() |> round(2),
                        (post_hab$a_hab[,1] - post_hab$a_hab[,3]) |> mean() |> round(2),
                        (post_hab$a_hab[,2] - post_hab$a_hab[,3]) |> mean() |> round(2),
                        (post_temp$a_temp[,1] - post_temp$a_temp[,2]) |> mean() |> round(2),
                        post_cont$sigma_cont |> mean() |> round(2)),
               `PI` = c(sprintf('%s - %s', pi_long[1], pi_long[2]),
                        sprintf('%s - %s', pi_soc[1], pi_soc[2]),
                        sprintf('%s - %s', pi_brain[1], pi_brain[2]),
                        sprintf('%s - %s', pi_body[1], pi_body[2]),
                        sprintf('%s - %s', pi_hab_1[1], pi_hab_1[2]),
                        sprintf('%s - %s', pi_hab_2[1], pi_hab_2[2]),
                        sprintf('%s - %s', pi_hab_3[1], pi_hab_3[2]),
                        sprintf('%s - %s', pi_temp[1], pi_temp[2]),
                        sprintf('%s - %s', pi_cont[1], pi_cont[2])))

# Save to file
write.csv2(d, path_overview_results_quality, row.names = F)

