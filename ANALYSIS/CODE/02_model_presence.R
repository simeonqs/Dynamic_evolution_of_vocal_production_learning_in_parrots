# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot vocal mimicry
# Date started: 10-11-2022
# Date last modified: 10-11-2022
# Author: Simeon Q. Smeele
# Description: Modelling the presence of mimicry. 
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
path_cleaned_data_long = 'ANALYSIS/RESULTS/cleaned_data_long.RData'
path_cleaned_data = 'ANALYSIS/RESULTS/cleaned_data.RData'

# Load data
load(path_cleaned_data_long)
load(path_cleaned_data)

# Model longevity ----

# Subset data
cc_dat = dat_long[!is.na(dat_long$context) &
                    !is.na(dat_long$template) &
                    !is.na(dat_long$gregar),]
cc_dat_short = dat[dat$species %in% cc_dat$species,]
if(length(cc_dat_short$species) != length(unique(cc_dat$species))) stop('Species do not match!')

# Translate species and genus
species_trans = 1:length(unique(cc_dat$species))
names(species_trans) = unique(cc_dat$species)
cc_dat$species_index = species_trans[cc_dat$species]
cc_dat_short$species_index = species_trans[cc_dat_short$species]
genus_trans = 1:length(unique(cc_dat$genus))
names(genus_trans) = unique(cc_dat$genus)
cc_dat$genus_index = genus_trans[cc_dat$genus]
cc_dat_short$genus_index = genus_trans[cc_dat_short$genus]

# Order short dat to match species index
cc_dat_short = cc_dat_short[order(cc_dat_short$species_index),]

# Compile data
clean_dat = list(N_obs = nrow(cc_dat),
                 N_species = max(cc_dat$species_index),
                 N_genera = max(cc_dat_short$genus_index),
                 mimic = c(yes = 1, no = 2, No = 2)[cc_dat$`vocal learning`] %>% as.integer,
                 species = cc_dat$species_index, 
                 context = ifelse(str_detect(cc_dat$context, 'interaction with human'), 1L, 2L),
                 temp = c(yes = 1, no = 2)[cc_dat$template] %>% as.integer,
                 
                 stand_long = as.numeric(scale(cc_dat_short$log_mean_life_exp)),
                 stand_body = as.numeric(scale(cc_dat_short$log_mean_body_weight)),
                 stand_brain = as.numeric(scale(cc_dat_short$log_mean_brain_size)),
                 gregar = cc_dat_short$gregar + 1L,
                 genus = cc_dat_short$genus_index,
                 
                 alpha = c(2, 2, 2))

# Prepare missing data
clean_dat$brain_missidx = which(is.na(cc_dat_short$log_mean_brain_size))
clean_dat$N_miss_brain = length(which(is.na(cc_dat_short$log_mean_brain_size)))
clean_dat$stand_brain[clean_dat$brain_missidx] = 999

clean_dat$long_missidx = which(is.na(cc_dat_short$log_mean_life_exp))
clean_dat$N_miss_long = length(which(is.na(cc_dat_short$log_mean_life_exp)))
clean_dat$stand_long[clean_dat$long_missidx] = 999

print(str(clean_dat))

model = cmdstan_model('ANALYSIS/CODE/models')
fit = model$sample(data = clean_dat, 
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4,
                   refresh = 500,
                   adapt_delta = 0.95) 
fit_nice = fit$output_files() %>%
  rstan::read_stan_csv()
precis(fit_nice)

# Plot results
post = extract.samples(fit_nice)
pdf('ANALYSIS/RESULTS/long - quality - total + imputation + context.pdf', 5, 5)
par(mar = c(3, 7, 1, 1))
params = list(post$sigma_species, post$sigma_genus, post$b_long, post$b_body, post$b_brain,
              post$a_greg[,2] - post$a_greg[,1], post$a_cont[,2] - post$a_cont[,1])
plot(sapply(params, mean), 1:length(params),
     xlim = c(-1.5, 1.5), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:length(params)) lines(PI(params[[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:length(params), 
     labels = c('sigma_species', 'sigma_genus', 'b_long', 'b_body', 'b_brain', 'sociality', 'context'), 
     las = 1)  
dev.off()





