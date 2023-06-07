# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot vocal mimicry
# Date started: 06-06-2023
# Date last modified: 06-06-2023
# Author: Simeon Q. Smeele
# Description: Modelling the presence of mimicry. 
# This version includes species where we have at least one video. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Set-up ----

# Loading libraries
libraries = c('tidyverse', 'rethinking', 'cmdstanr', 'ape')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list = ls()) 

# Paths
path_cleaned_data = 'ANALYSIS/RESULTS/cleaned_data.RData'
path_models_presence = 'ANALYSIS/RESULTS/models_presence_video_only.RData'

# Load data
load(path_cleaned_data)

# Subset
keep = which(dat$video == 1)
dat = dat[keep,]

# Model longevity ----

# Subset data
cc_dat = dat[!is.na(dat$gregar) &
               !is.na(dat$log_mean_body_weight),]

# Translate species and genus
genus_trans = 1:length(unique(cc_dat$genus))
names(genus_trans) = unique(cc_dat$genus)
cc_dat$genus_index = genus_trans[cc_dat$genus]

# Compile data
clean_dat = list(N_obs = nrow(cc_dat),
                 N_genera = max(cc_dat$genus_index),
                 mimic = cc_dat$vocal,
                 stand_long = as.numeric(scale(cc_dat$log_mean_life_exp)),
                 stand_body = as.numeric(scale(cc_dat$log_mean_body_weight)),
                 stand_brain = as.numeric(scale(cc_dat$log_mean_brain_size)),
                 gregar = cc_dat$gregar + 1L,
                 genus = cc_dat$genus_index)

# Prepare missing data
clean_dat$brain_missidx = which(is.na(cc_dat$log_mean_brain_size))
clean_dat$N_miss_brain = length(which(is.na(cc_dat$log_mean_brain_size)))
clean_dat$stand_brain[clean_dat$brain_missidx] = 999

clean_dat$long_missidx = which(is.na(cc_dat$log_mean_life_exp))
clean_dat$N_miss_long = length(which(is.na(cc_dat$log_mean_life_exp)))
clean_dat$stand_long[clean_dat$long_missidx] = 999

print(str(clean_dat))

model = cmdstan_model('ANALYSIS/CODE/models/longevity_to_presence.stan')
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

# Model brain ----

# Subset data
cc_dat = dat[!is.na(dat$gregar) &
               !is.na(dat$log_mean_body_weight),]

# Translate species and genus
genus_trans = 1:length(unique(cc_dat$genus))
names(genus_trans) = unique(cc_dat$genus)
cc_dat$genus_index = genus_trans[cc_dat$genus]

# Compile data
clean_dat = list(N_obs = nrow(cc_dat),
                 N_genera = max(cc_dat$genus_index),
                 mimic = cc_dat$vocal,
                 stand_body = as.numeric(scale(cc_dat$log_mean_body_weight)),
                 stand_brain = as.numeric(scale(cc_dat$log_mean_brain_size)),
                 gregar = cc_dat$gregar + 1L,
                 genus = cc_dat$genus_index)

# Prepare missing data
clean_dat$brain_missidx = which(is.na(cc_dat$log_mean_brain_size))
clean_dat$N_miss_brain = length(which(is.na(cc_dat$log_mean_brain_size)))
clean_dat$stand_brain[clean_dat$brain_missidx] = 999

print(str(clean_dat))

model = cmdstan_model('ANALYSIS/CODE/models/brain_to_presence.stan')
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

# Model sociality ----

# Subset data
cc_dat = dat[!is.na(dat$gregar),]

# Translate species and genus
genus_trans = 1:length(unique(cc_dat$genus))
names(genus_trans) = unique(cc_dat$genus)
cc_dat$genus_index = genus_trans[cc_dat$genus]

# Compile data
clean_dat = list(N_obs = nrow(cc_dat),
                 N_genera = max(cc_dat$genus_index),
                 mimic = cc_dat$vocal,
                 gregar = cc_dat$gregar + 1L,
                 genus = cc_dat$genus_index)

print(str(clean_dat))

model = cmdstan_model('ANALYSIS/CODE/models/sociality_to_presence.stan')
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

# Model brain ----

# Subset data
cc_dat = dat[!is.na(dat$log_mean_body_weight),]

# Translate species and genus
genus_trans = 1:length(unique(cc_dat$genus))
names(genus_trans) = unique(cc_dat$genus)
cc_dat$genus_index = genus_trans[cc_dat$genus]

# Compile data
clean_dat = list(N_obs = nrow(cc_dat),
                 N_genera = max(cc_dat$genus_index),
                 mimic = cc_dat$vocal,
                 stand_body = as.numeric(scale(cc_dat$log_mean_body_weight)),
                 genus = cc_dat$genus_index)

print(str(clean_dat))

model = cmdstan_model('ANALYSIS/CODE/models/body_to_presence.stan')
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

# Phylogenetic model ----

drop_tips = tree$tip.label[!tree$tip.label %in% dat$scinam]
message('Dropping: ', paste(drop_tips, collapse = ', '), ' from tree.')
tree = drop.tip(tree, drop_tips)

clean_dat = list(mimic = dat$vocal,
                 dmat = cophenetic(tree)/max(cophenetic(tree)),
                 N_species = nrow(dat))
model = cmdstan_model('ANALYSIS/CODE/models/phylo_signal.stan')
fit = model$sample(data = clean_dat, 
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4,
                   refresh = 100,
                   iter_warmup = 1500,
                   iter_sampling = 1500,
                   adapt_delta = 0.99,
                   max_treedepth = 15) 

fit_nice_phylo = fit$output_files() |>
  rstan::read_stan_csv()
precis(fit_nice_phylo)
post_phylo = extract.samples(fit_nice_phylo)

# Model habitat ----

# Subset data
cc_dat = dat[!is.na(dat$habitat),]

# Translate species and genus
genus_trans = 1:length(unique(cc_dat$genus))
names(genus_trans) = unique(cc_dat$genus)
cc_dat$genus_index = genus_trans[cc_dat$genus]

# Compile data
clean_dat = list(N_obs = nrow(cc_dat),
                 N_genera = max(cc_dat$genus_index),
                 mimic = cc_dat$vocal,
                 habitat = c(closed = 1, mixed = 2, open = 3)[cc_dat$habitat] |> as.numeric(),
                 genus = cc_dat$genus_index)

str(clean_dat)

model = cmdstan_model('ANALYSIS/CODE/models/habitat_to_presence.stan')
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
save(post_long, post_brain, post_soc, post_body, post_phylo, post_hab, 
     fit_nice_long, fit_nice_brain, fit_nice_soc, fit_nice_body, fit_nice_phylo, fit_nice_hab,
     file = path_models_presence)
