# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot vocal mimicry
# Date started: 11-11-2022
# Date last modified: 10-12-2022
# Author: Simeon Q. Smeele
# Description: Modelling the quality.  
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
path_models_quality = 'ANALYSIS/RESULTS/models_quality.RData'

# Load data
load(path_cleaned_data_long)
load(path_cleaned_data)

# Model total effect longevity ----

# Subset data
cc_dat = dat_long[!is.na(dat_long$mimicquality) &
                    !is.na(dat_long$log_mean_body_weight) &
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
                 quality = c(low = 1, moderate = 2, high = 3)[cc_dat$mimicquality],
                 species = cc_dat$species_index, 

                 stand_long = as.numeric(scale(cc_dat_short$log_mean_life_exp)),
                 stand_body = as.numeric(scale(cc_dat_short$log_mean_body_weight)),
                 stand_brain = as.numeric(scale(cc_dat_short$log_mean_brain_size)),
                 gregar = cc_dat_short$gregar + 1L,
                 genus = cc_dat_short$genus_index,
                 alpha = rep(2, 3))

# Prepare missing data
clean_dat$brain_missidx = which(is.na(cc_dat_short$log_mean_brain_size))
clean_dat$N_miss_brain = length(which(is.na(cc_dat_short$log_mean_brain_size)))
clean_dat$stand_brain[clean_dat$brain_missidx] = 999

clean_dat$long_missidx = which(is.na(cc_dat_short$log_mean_life_exp))
clean_dat$N_miss_long = length(which(is.na(cc_dat_short$log_mean_life_exp)))
clean_dat$stand_long[clean_dat$long_missidx] = 999

str(clean_dat)

model = cmdstan_model('ANALYSIS/CODE/models/longevity_to_quality.stan')
fit = model$sample(data = clean_dat, 
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4,
                   refresh = 500,
                   adapt_delta = 0.99,
                   max_treedepth = 15) 
fit_nice_long = fit$output_files() |>
  rstan::read_stan_csv()
precis(fit_nice_long)

post_long = extract.samples(fit_nice_long)

# Model total effect brain size ----

# Subset data
cc_dat = dat_long[!is.na(dat_long$mimicquality) &
                    !is.na(dat_long$log_mean_body_weight) &
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
                 quality = c(low = 1, moderate = 2, high = 3)[cc_dat$mimicquality],
                 species = cc_dat$species_index, 
                 
                 stand_body = as.numeric(scale(cc_dat_short$log_mean_body_weight)),
                 stand_brain = as.numeric(scale(cc_dat_short$log_mean_brain_size)),
                 gregar = cc_dat_short$gregar + 1L,
                 genus = cc_dat_short$genus_index,
                 alpha = rep(2, 3))

# Prepare missing data
clean_dat$brain_missidx = which(is.na(cc_dat_short$log_mean_brain_size))
clean_dat$N_miss_brain = length(which(is.na(cc_dat_short$log_mean_brain_size)))
clean_dat$stand_brain[clean_dat$brain_missidx] = 999

print(str(clean_dat))

model = cmdstan_model('ANALYSIS/CODE/models/brain_to_quality.stan')
fit = model$sample(data = clean_dat, 
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4,
                   refresh = 500,
                   adapt_delta = 0.99,
                   max_treedepth = 15) 
fit_nice_brain = fit$output_files() |>
  rstan::read_stan_csv()
precis(fit_nice_brain)

post_brain = extract.samples(fit_nice_brain)

# Model total effect sociality ----

# Subset data
cc_dat = dat_long[!is.na(dat_long$mimicquality) &
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
                 quality = c(low = 1, moderate = 2, high = 3)[cc_dat$mimicquality],
                 species = cc_dat$species_index, 
                 
                 gregar = cc_dat_short$gregar + 1L,
                 genus = cc_dat_short$genus_index,
                 alpha = rep(2, 3))

print(str(clean_dat))

model = cmdstan_model('ANALYSIS/CODE/models/sociality_to_quality.stan')
fit = model$sample(data = clean_dat, 
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4,
                   refresh = 500,
                   adapt_delta = 0.99,
                   max_treedepth = 15) 
fit_nice_soc = fit$output_files() |>
  rstan::read_stan_csv()
precis(fit_nice_soc)

post_soc = extract.samples(fit_nice_soc)

# Model total effect body size ----

# Subset data
cc_dat = dat_long[!is.na(dat_long$mimicquality) &
                    !is.na(dat_long$log_mean_body_weight),]
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
                 quality = c(low = 1, moderate = 2, high = 3)[cc_dat$mimicquality],
                 species = cc_dat$species_index, 
                 
                 stand_body = as.numeric(scale(cc_dat_short$log_mean_body_weight)),
                 genus = cc_dat_short$genus_index, 
                 alpha = rep(2, 3))

print(str(clean_dat))

model = cmdstan_model('ANALYSIS/CODE/models/body_to_quality.stan')
fit = model$sample(data = clean_dat, 
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4,
                   refresh = 500,
                   adapt_delta = 0.99,
                   max_treedepth = 15) 
fit_nice_body = fit$output_files() |>
  rstan::read_stan_csv()
precis(fit_nice_body)

post_body = extract.samples(fit_nice_body)

# Model total effect habitat ----

# Subset data
cc_dat = dat_long[!is.na(dat_long$mimicquality),]
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
                 quality = as.numeric(c(low = 1, moderate = 2, high = 3)[cc_dat$mimicquality]),
                 species = cc_dat$species_index, 
                 
                 habitat = as.numeric(c(closed = 1, mixed = 2, open = 3)[cc_dat_short$habitat]),
                 genus = cc_dat_short$genus_index, 
                 alpha = rep(2, 3))

str(clean_dat)

model = cmdstan_model('ANALYSIS/CODE/models/habitat_to_quality.stan')
fit = model$sample(data = clean_dat, 
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4,
                   refresh = 500,
                   adapt_delta = 0.99,
                   max_treedepth = 15) 
fit_nice_hab = fit$output_files() |>
  rstan::read_stan_csv()
precis(fit_nice_hab)

post_hab = extract.samples(fit_nice_hab)

# Model total effect template ----

# Subset data
cc_dat = dat_long[!is.na(dat_long$mimicquality) &
                    !is.na(dat_long$template),]
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
                 quality = as.numeric(c(low = 1, moderate = 2, high = 3)[cc_dat$mimicquality]),
                 temp = as.numeric(c(yes = 1, no = 2)[cc_dat$template]),
                 species = cc_dat$species_index, 
                 
                 genus = cc_dat_short$genus_index, 
                 alpha = rep(2, 3))
str(clean_dat)

model = cmdstan_model('ANALYSIS/CODE/models/template_to_quality.stan')
fit = model$sample(data = clean_dat, 
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4,
                   refresh = 500,
                   adapt_delta = 0.99,
                   max_treedepth = 15) 
fit_nice_temp = fit$output_files() |>
  rstan::read_stan_csv()
precis(fit_nice_temp)

post_temp= extract.samples(fit_nice_temp)

# Model total effect context ----

# Fix typos and simplify
dat_long$context = ifelse(dat_long$context == 'Alone', 'alone',
                          ifelse(dat_long$context == 'alone and interaction with human', 
                                 'interaction with human', 
                                 dat_long$context))
# dat_long$context[dat_long$context == 'non human allospecifics',] = NA # remove single instance

# Subset data
cc_dat = dat_long[!is.na(dat_long$mimicquality) &
                    !is.na(dat_long$context),]
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
                 quality = as.numeric(c(low = 1, moderate = 2, high = 3)[cc_dat$mimicquality]),
                 context = as.numeric(as.factor(cc_dat$context)),
                 species = cc_dat$species_index, 
                 
                 genus = cc_dat_short$genus_index, 
                 alpha = rep(2, 3))
str(clean_dat)

model = cmdstan_model('ANALYSIS/CODE/models/context_to_quality.stan')
fit = model$sample(data = clean_dat, 
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4,
                   refresh = 500,
                   adapt_delta = 0.99,
                   max_treedepth = 15) 
fit_nice_cont = fit$output_files() |>
  rstan::read_stan_csv()
precis(fit_nice_cont)

post_cont = extract.samples(fit_nice_cont)

# Save ----
save(post_long, post_brain, post_soc, post_body, post_hab, post_temp, post_cont,
     fit_nice_long, fit_nice_brain, fit_nice_soc, fit_nice_body, fit_nice_hab, fit_nice_temp, fit_nice_cont,
     file = path_models_quality)
