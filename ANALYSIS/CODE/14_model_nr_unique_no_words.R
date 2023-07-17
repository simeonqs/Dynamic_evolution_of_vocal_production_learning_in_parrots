# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot vocal mimicry
# Date started: 17-07-2023
# Date last modified: 17-07-2023
# Author: Simeon Q. Smeele
# Description: Modelling the number of unique mimics, excluding words.
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
path_models_nr_unique = 'ANALYSIS/RESULTS/models_nr_unique_no_words.RData'

# Load data
load(path_cleaned_data_long)
load(path_cleaned_data)

# Remove words
dat_long$n_im = dat_long$n_im - dat_long$distinctwords

# Model total effect longevity ----

# Subset data
cc_dat = dat_long[!is.na(dat_long$n_im) &
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
                 n_mimic = cc_dat$n_im,
                 species = cc_dat$species_index, 

                 stand_long = as.numeric(scale(cc_dat_short$log_mean_life_exp)),
                 stand_body = as.numeric(scale(cc_dat_short$log_mean_body_weight)),
                 stand_brain = as.numeric(scale(cc_dat_short$log_mean_brain_size)),
                 gregar = cc_dat_short$gregar + 1L,
                 genus = cc_dat_short$genus_index)

# Prepare missing data
clean_dat$brain_missidx = which(is.na(cc_dat_short$log_mean_brain_size))
clean_dat$N_miss_brain = length(which(is.na(cc_dat_short$log_mean_brain_size)))
clean_dat$stand_brain[clean_dat$brain_missidx] = 999

clean_dat$long_missidx = which(is.na(cc_dat_short$log_mean_life_exp))
clean_dat$N_miss_long = length(which(is.na(cc_dat_short$log_mean_life_exp)))
clean_dat$stand_long[clean_dat$long_missidx] = 999

print(str(clean_dat))

model = cmdstan_model('ANALYSIS/CODE/models/longevity_to_nr_unique.stan')
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
cc_dat = dat_long[!is.na(dat_long$n_im) &
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
                 n_mimic = cc_dat$n_im,
                 species = cc_dat$species_index, 
                 
                 stand_body = as.numeric(scale(cc_dat_short$log_mean_body_weight)),
                 stand_brain = as.numeric(scale(cc_dat_short$log_mean_brain_size)),
                 gregar = cc_dat_short$gregar + 1L,
                 genus = cc_dat_short$genus_index)

# Prepare missing data
clean_dat$brain_missidx = which(is.na(cc_dat_short$log_mean_brain_size))
clean_dat$N_miss_brain = length(which(is.na(cc_dat_short$log_mean_brain_size)))
clean_dat$stand_brain[clean_dat$brain_missidx] = 999

print(str(clean_dat))

model = cmdstan_model('ANALYSIS/CODE/models/brain_to_nr_unique.stan')
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
cc_dat = dat_long[!is.na(dat_long$n_im) &
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
                 n_mimic = cc_dat$n_im,
                 species = cc_dat$species_index, 
                 
                 gregar = cc_dat_short$gregar + 1L,
                 genus = cc_dat_short$genus_index)

print(str(clean_dat))

model = cmdstan_model('ANALYSIS/CODE/models/sociality_to_nr_unique.stan')
fit = model$sample(data = clean_dat, 
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4,
                   iter_warmup = 2000,
                   iter_sampling = 2000,
                   refresh = 500,
                   adapt_delta = 0.99,
                   max_treedepth = 15) 
fit_nice_soc = fit$output_files() |>
  rstan::read_stan_csv()
precis(fit_nice_soc)

post_soc = extract.samples(fit_nice_soc)

# Model total effect body size ----

# Subset data
cc_dat = dat_long[!is.na(dat_long$n_im) &
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
                 n_mimic = cc_dat$n_im,
                 species = cc_dat$species_index, 
                 
                 stand_body = as.numeric(scale(cc_dat_short$log_mean_body_weight)),
                 genus = cc_dat_short$genus_index)

print(str(clean_dat))

model = cmdstan_model('ANALYSIS/CODE/models/body_to_nr_unique.stan')
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
cc_dat = dat_long[!is.na(dat_long$n_im),]
cc_dat_short = dat[dat$species %in% cc_dat$species & 
                     !is.na(dat$habitat),]
cc_dat = cc_dat[cc_dat$species %in% cc_dat_short$species,]
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
                 n_mimic = cc_dat$n_im,
                 species = cc_dat$species_index, 
                 
                 habitat = c(closed = 1, mixed = 2, open = 3)[cc_dat_short$habitat] |> as.numeric(),
                 genus = cc_dat_short$genus_index)

str(clean_dat)

model = cmdstan_model('ANALYSIS/CODE/models/habitat_to_nr_unique.stan')
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

# Save ----
save(post_long, post_brain, post_soc, post_body, post_hab,
     fit_nice_long, fit_nice_brain, fit_nice_soc, fit_nice_body, fit_nice_hab,
     file = path_models_nr_unique)
