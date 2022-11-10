# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot vocal mimicry
# Date started: 12-09-2022
# Date last modified: 19-09-2022
# Author: Simeon Q. Smeele
# Description: Running models for the extended dataset. 
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

# Load
load('ANALYSIS/RESULTS/cleaned_data_long.RData')
load('ANALYSIS/RESULTS/cleaned_data.RData')

# Translate species and genus
species_trans = 1:length(unique(dat_long$species))
names(species_trans) = unique(dat_long$species)
dat_long$species_index = species_trans[dat_long$species]
genus_trans = 1:length(unique(dat_long$genus))
names(genus_trans) = unique(dat_long$genus)
dat_long$genus_index = genus_trans[dat_long$genus]

# Subset to known imitation repertoire
dat_long = dat_long[!is.na(dat_long$`total number of imitations`),]

# Simple species model ----

# Compile data
clean_dat = list(N_obs = length(dat_long$n_im),
                 N_species = max(dat_long$species_index),
                 n = dat_long$n_im,
                 species = dat_long$species_index)

model = cmdstan_model('ANALYSIS/CODE/models extended/species.stan')
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
pdf('ANALYSIS/RESULTS/long - species.pdf', 5, 5)
par(mar = c(3, 7, 1, 1))
params = list(post$sigma_species)
plot(sapply(params, mean), 1:length(params),
     xlim = c(-1, 2), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:length(params)) lines(PI(params[[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:length(params), 
     labels = c('sigma_species'), las = 1)  
dev.off()

# Simple species + genus model ----

# Subset data
cc_dat = dat_long[!is.na(dat_long$`total number of imitations`),]
cc_dat_short = dat[dat$species %in% cc_dat$species,]
if(length(cc_dat_short$species) != length(unique(cc_dat$species))) stop('Species do not match!')

# Translate species and genus
species_trans = 1:length(unique(cc_dat$species))
names(species_trans) = unique(cc_dat$species)
cc_dat$species_index = species_trans[cc_dat$species]
cc_dat_short$species_index = species_trans[cc_dat_short$species]
genus_trans = 1:length(unique(cc_dat_short$genus))
names(genus_trans) = unique(cc_dat_short$genus)
cc_dat_short$genus_index = genus_trans[cc_dat_short$genus]

# Order short dat to match species index
cc_dat_short = cc_dat_short[order(cc_dat_short$species_index),]

# Compile data
clean_dat = list(N_obs = length(cc_dat$n_im),
                 N_species = max(cc_dat$species_index),
                 N_genera = max(cc_dat_short$genus_index),
                 n = cc_dat$n_im,
                 species = cc_dat$species_index,
                 genus = cc_dat_short$genus_index)

model = cmdstan_model('ANALYSIS/CODE/models extended/species_genus.stan')
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
pdf('ANALYSIS/RESULTS/long - species + genus.pdf', 5, 5)
par(mar = c(3, 7, 1, 1))
params = list(post$sigma_species, post$sigma_genus)
plot(sapply(params, mean), 1:length(params),
     xlim = c(-1, 2), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:length(params)) lines(PI(params[[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:length(params), 
     labels = c('sigma_species', 'sigma_genus'), las = 1)  
dev.off()

# Simple species + genus model + time ----

# Compile data
clean_dat = list(N_obs = length(dat_long$n_im),
                 N_species = max(dat_long$species_index),
                 N_genera = max(dat_long$genus_index),
                 n = dat_long$n_im,
                 species = dat_long$species_index,
                 genus = dat_long$genus_index,
                 log_time = log(dat_long$`time bird is visible (seconds)`))

model = cmdstan_model('ANALYSIS/CODE/models extended/species_genus_time.stan')
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
pdf('ANALYSIS/RESULTS/long - species + genus + time.pdf', 5, 5)
par(mar = c(3, 7, 1, 1))
params = list(post$sigma_species, post$sigma_genus, post$b_time)
plot(sapply(params, mean), 1:length(params),
     xlim = c(-1, 2), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:length(params)) lines(PI(params[[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:length(params), 
     labels = c('sigma_species', 'sigma_genus', 'b_time'), las = 1)  
dev.off()

# Simple species + genus model + longevity ----

# Subset data
cc_dat = dat_long[!is.na(dat_long$log_mean_life_exp),]

# Compile data
clean_dat = list(N_obs = length(cc_dat$n_im),
                 N_species = max(cc_dat$species_index),
                 N_genera = max(cc_dat$genus_index),
                 n = cc_dat$n_im,
                 species = cc_dat$species_index,
                 genus = cc_dat$genus_index,
                 stand_long = as.numeric(scale(cc_dat$log_mean_life_exp)))

model = cmdstan_model('ANALYSIS/CODE/models extended/species_genus_longevity.stan')
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
pdf('ANALYSIS/RESULTS/long - species + genus + longevity.pdf', 5, 5)
par(mar = c(3, 7, 1, 1))
params = list(post$sigma_species, post$sigma_genus, post$b_long)
plot(sapply(params, mean), 1:length(params),
     xlim = c(-1, 2), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:length(params)) lines(PI(params[[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:length(params), 
     labels = c('sigma_species', 'sigma_genus', 'b_long'), las = 1)  
dev.off()

# Simple species + genus model + longevity + brain size + body size ----

# Subset data
cc_dat = dat_long[!is.na(dat_long$log_mean_life_exp) & 
                    !is.na(dat_long$log_mean_body_weight) &
                    !is.na(dat_long$log_mean_brain_size),]
cc_dat_short = dat[!is.na(dat$log_mean_life_exp) & 
                     !is.na(dat$log_mean_body_weight) &
                     !is.na(dat$log_mean_brain_size) &
                     dat$species %in% cc_dat$species,]
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
clean_dat = list(N_obs = length(cc_dat$n_im),
                 N_species = max(cc_dat$species_index),
                 N_genera = max(cc_dat$genus_index),
                 n = cc_dat$n_im,
                 species = cc_dat$species_index,
                 genus = cc_dat$genus_index,
                 stand_long = as.numeric(scale(cc_dat$log_mean_life_exp)),
                 stand_body = as.numeric(scale(cc_dat$log_mean_body_weight)),
                 
                 stand_body_short = as.numeric(scale(cc_dat_short$log_mean_body_weight)),
                 stand_brain_short = as.numeric(scale(cc_dat_short$log_mean_brain_size)),
                 genus_short = cc_dat_short$genus_index)

model = cmdstan_model('ANALYSIS/CODE/models extended/species_genus_longevity_body_brain.stan')
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
pdf('ANALYSIS/RESULTS/long - species + genus + longevity + body + brain.pdf', 5, 5)
par(mar = c(3, 7, 1, 1))
params = list(post$sigma_species, post$sigma_genus, post$b_long, post$b_body, post$b_brain)
plot(sapply(params, mean), 1:length(params),
     xlim = c(-1, 2), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:length(params)) lines(PI(params[[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:length(params), 
     labels = c('sigma_species', 'sigma_genus', 'b_long', 'b_body', 'b_brain'), las = 1)  
dev.off()

# Complete species + genus + longevity + brain size ----

# Subset data
cc_dat = dat_long[!is.na(dat_long$`total number of imitations`) &
                    !is.na(dat_long$log_mean_body_weight) &
                    !is.na(dat_long$gregar),]
cc_dat_short = dat[!is.na(dat$log_mean_body_weight) &
                     dat$species %in% cc_dat$species,]
if(length(cc_dat_short$species) != length(unique(cc_dat$species))) stop('Species do not match!')

# Translate species and genus
species_trans = 1:length(unique(cc_dat$species))
names(species_trans) = unique(cc_dat$species)
cc_dat$species_index = species_trans[cc_dat$species]
cc_dat_short$species_index = species_trans[cc_dat_short$species]
genus_trans = 1:length(unique(cc_dat_short$genus))
names(genus_trans) = unique(cc_dat_short$genus)
cc_dat_short$genus_index = genus_trans[cc_dat_short$genus]

# Order short dat to match species index
cc_dat_short = cc_dat_short[order(cc_dat_short$species_index),]

# Compile data
clean_dat = list(N_obs = length(cc_dat$n_im),
                 N_species = max(cc_dat$species_index),
                 N_genera = max(cc_dat_short$genus_index),
                 n = cc_dat$n_im,
                 species = cc_dat$species_index,
                 gregar = cc_dat$gregar + 1L,
                 
                 stand_long = as.numeric(scale(cc_dat_short$log_mean_life_exp)),
                 stand_body = as.numeric(scale(cc_dat_short$log_mean_body_weight)),
                 stand_body = as.numeric(scale(cc_dat_short$log_mean_body_weight)),
                 stand_brain = as.numeric(scale(cc_dat_short$log_mean_brain_size)),
                 genus = cc_dat_short$genus_index)

# Prepare missing data
clean_dat$brain_missidx = which(is.na(cc_dat_short$log_mean_brain_size))
clean_dat$N_miss_brain = length(which(is.na(cc_dat_short$log_mean_brain_size)))
clean_dat$stand_brain[clean_dat$brain_missidx] = 999

clean_dat$long_missidx = which(is.na(cc_dat_short$log_mean_life_exp))
clean_dat$N_miss_long = length(which(is.na(cc_dat_short$log_mean_life_exp)))
clean_dat$stand_long[clean_dat$long_missidx] = 999

model = cmdstan_model('ANALYSIS/CODE/models extended/complete_species_genus_longevity_sociality_brain.stan')
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
pdf('ANALYSIS/RESULTS/long complete - species + genus + longevity + brain + sociality.pdf', 5, 5)
par(mar = c(3, 7, 1, 1))
params = list(post$sigma_species, post$sigma_genus, post$b_long, post$b_brain, 
              post$a_greg[,2] - post$a_greg[,1])
plot(sapply(params, mean), 1:length(params),
     xlim = c(-1.5, 1.5), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:length(params)) lines(PI(params[[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:length(params), 
     labels = c('sigma_species', 'sigma_genus', 'b_long', 'b_brain', 'sociality'), las = 1)  
dev.off()

# Complete species + genus + longevity + brain size + body size ----

# Subset data
cc_dat = dat_long[!is.na(dat_long$`total number of imitations`) &
                    !is.na(dat_long$log_mean_body_weight) &
                    !is.na(dat_long$gregar),]
cc_dat_short = dat[!is.na(dat$log_mean_body_weight) &
                     dat$species %in% cc_dat$species,]
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
clean_dat = list(N_obs = length(cc_dat$n_im),
                 N_species = max(cc_dat$species_index),
                 N_genera = max(cc_dat_short$genus_index),
                 n = cc_dat$n_im,
                 species = cc_dat$species_index,
                 
                 stand_long = as.numeric(scale(cc_dat_short$log_mean_life_exp)),
                 stand_body = as.numeric(scale(cc_dat_short$log_mean_body_weight)),
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

model = 
  cmdstan_model('ANALYSIS/CODE/models extended/complete_species_genus_longevity_sociality_brain_body.stan')
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
pdf('ANALYSIS/RESULTS/long complete - species + genus + longevity + brain + sociality + body.pdf', 5, 5)
par(mar = c(3, 7, 1, 1))
params = list(post$sigma_species, post$sigma_genus, post$b_long, post$b_brain, post$b_body,
              post$a_greg[,2] - post$a_greg[,1])
plot(sapply(params, mean), 1:length(params),
     xlim = c(-1.5, 1.5), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:length(params)) lines(PI(params[[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:length(params), 
     labels = c('sigma_species', 'sigma_genus', 'b_long', 'b_brain', 'b_body', 'sociality'), las = 1)  
dev.off()

# Complete species + genus + brain size + sociality ----

# Subset data
cc_dat = dat_long[!is.na(dat_long$`total number of imitations`) &
                    !is.na(dat_long$log_mean_body_weight) &
                    !is.na(dat_long$gregar),]
cc_dat_short = dat[!is.na(dat$log_mean_body_weight) &
                     dat$species %in% cc_dat$species,]
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
clean_dat = list(N_obs = length(cc_dat$n_im),
                 N_species = max(cc_dat$species_index),
                 N_genera = max(cc_dat_short$genus_index),
                 n = cc_dat$n_im,
                 species = cc_dat$species_index,

                 stand_body = as.numeric(scale(cc_dat_short$log_mean_body_weight)),
                 stand_body = as.numeric(scale(cc_dat_short$log_mean_body_weight)),
                 stand_brain = as.numeric(scale(cc_dat_short$log_mean_brain_size)),
                 gregar = cc_dat_short$gregar + 1L,
                 genus = cc_dat_short$genus_index)

# Prepare missing data
clean_dat$brain_missidx = which(is.na(cc_dat_short$log_mean_brain_size))
clean_dat$N_miss_brain = length(which(is.na(cc_dat_short$log_mean_brain_size)))
clean_dat$stand_brain[clean_dat$brain_missidx] = 999

model = cmdstan_model('ANALYSIS/CODE/models extended/complete_species_genus_sociality_brain.stan')
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
pdf('ANALYSIS/RESULTS/long complete - species + genus + brain.pdf', 5, 5)
par(mar = c(3, 7, 1, 1))
params = list(post$sigma_species, post$sigma_genus, post$b_brain, 
              post$a_greg[,2] - post$a_greg[,1])
plot(sapply(params, mean), 1:length(params),
     xlim = c(-1.5, 1.5), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:length(params)) lines(PI(params[[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:length(params), 
     labels = c('sigma_species', 'sigma_genus', 'b_brain', 'sociality'), las = 1)  
dev.off()

# Model quality - species ----

cc_dat = dat_long[!is.na(dat_long$`quality of speech mimicry`),]

# Compile data
clean_dat = list(N_obs = length(cc_dat$`quality of speech mimicry`),
                 N_species = max(cc_dat$species_index),
                 quality = as.integer(c(low = 1, moderate = 2, high = 3)[
                   cc_dat$`quality of speech mimicry`]),
                 species = cc_dat$species_index, 
                 alpha = c(2, 2, 2))

model = cmdstan_model('ANALYSIS/CODE/models extended/quality_species.stan')
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
pdf('ANALYSIS/RESULTS/long - quality - species.pdf', 5, 5)
par(mar = c(3, 7, 1, 1))
params = list(post$sigma_species)
plot(sapply(params, mean), 1:length(params),
     xlim = c(-1, 2), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:length(params)) lines(PI(params[[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:length(params), 
     labels = c('sigma_species'), las = 1)  
dev.off()

# Model quality - species + genus ----

cc_dat = dat_long[!is.na(dat_long$`quality of speech mimicry`),]

# Compile data
clean_dat = list(N_obs = length(cc_dat$`quality of speech mimicry`),
                 N_species = max(cc_dat$species_index),
                 N_genera = max(cc_dat$genus_index),
                 quality = as.integer(c(low = 1, moderate = 2, high = 3)[
                   cc_dat$`quality of speech mimicry`]),
                 species = cc_dat$species_index, 
                 genus = cc_dat$genus_index,
                 alpha = c(2, 2, 2))

model = cmdstan_model('ANALYSIS/CODE/models extended/quality_species_genus.stan')
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
pdf('ANALYSIS/RESULTS/long - quality - species + genus.pdf', 5, 5)
par(mar = c(3, 7, 1, 1))
params = list(post$sigma_species, post$sigma_genus)
plot(sapply(params, mean), 1:length(params),
     xlim = c(-1, 2), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:length(params)) lines(PI(params[[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:length(params), 
     labels = c('sigma_species', 'sigma_genus'), las = 1)  
dev.off()

# Model quality - species + genus + body ----

cc_dat = dat_long[!is.na(dat_long$`quality of speech mimicry`),]

species_trans = 1:length(unique(cc_dat$species))
names(species_trans) = unique(cc_dat$species)
cc_dat$species_index = species_trans[cc_dat$species]

genus_trans = 1:length(unique(cc_dat$genus))
names(genus_trans) = unique(cc_dat$genus)
cc_dat$genus_index = genus_trans[cc_dat$genus]

# Compile data
clean_dat = list(N_obs = length(cc_dat$`quality of speech mimicry`),
                 N_species = max(cc_dat$species_index),
                 N_genera = max(cc_dat$genus_index),
                 quality = as.integer(c(low = 1, moderate = 2, high = 3)[
                   cc_dat$`quality of speech mimicry`]),
                 species = cc_dat$species_index, 
                 genus = cc_dat$genus_index,
                 stand_body = as.numeric(scale(cc_dat$log_mean_body_weight)),
                 alpha = c(2, 2, 2))

model = cmdstan_model('ANALYSIS/CODE/models extended/quality_species_genus_body.stan')
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
pdf('ANALYSIS/RESULTS/long - quality - species + genus + body.pdf', 5, 5)
par(mar = c(3, 7, 1, 1))
params = list(post$sigma_species, post$sigma_genus, post$b_body)
plot(sapply(params, mean), 1:length(params),
     xlim = c(-1, 2), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:length(params)) lines(PI(params[[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:length(params), 
     labels = c('sigma_species', 'sigma_genus', 'b_body'), las = 1)  
dev.off()

# Model quality - species + genus + body + brain + longevity ----

# Subset data
cc_dat = dat_long[!is.na(dat_long$log_mean_life_exp) & 
                    !is.na(dat_long$`quality of speech mimicry`) &
                    !is.na(dat_long$log_mean_body_weight) &
                    !is.na(dat_long$log_mean_brain_size),]
cc_dat_short = dat[!is.na(dat$log_mean_life_exp) & 
                     !is.na(dat$log_mean_body_weight) &
                     !is.na(dat$log_mean_brain_size) &
                     dat$species %in% cc_dat$species,]
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
clean_dat = list(N_obs = length(cc_dat$`quality of speech mimicry`),
                 N_species = max(cc_dat$species_index),
                 N_genera = max(cc_dat$genus_index),
                 quality = as.integer(c(low = 1, moderate = 2, high = 3)[
                   cc_dat$`quality of speech mimicry`]),
                 species = cc_dat$species_index, 
                 genus = cc_dat$genus_index,
                 stand_long = as.numeric(scale(cc_dat$log_mean_life_exp)),
                 stand_body = as.numeric(scale(cc_dat$log_mean_body_weight)),
                 
                 stand_body_short = as.numeric(scale(cc_dat_short$log_mean_body_weight)),
                 stand_brain_short = as.numeric(scale(cc_dat_short$log_mean_brain_size)),
                 genus_short = cc_dat_short$genus_index,
                 
                 alpha = c(2, 2, 2))

model = cmdstan_model('ANALYSIS/CODE/models extended/quality_species_genus_body_brain_longevity.stan')
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
pdf('ANALYSIS/RESULTS/long - quality - species + genus + body + brain + longevity.pdf', 5, 5)
par(mar = c(3, 7, 1, 1))
params = list(post$sigma_species, post$sigma_genus, post$b_long, post$b_body, post$b_brain)
plot(sapply(params, mean), 1:length(params),
     xlim = c(-1, 2), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:length(params)) lines(PI(params[[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:length(params), 
     labels = c('sigma_species', 'sigma_genus', 'b_long', 'b_body', 'b_brain'), las = 1)  
dev.off()

# Check results with simple residuals
clean_dat$res_brain = clean_dat$stand_brain_short - 
  fitted(lm(clean_dat$stand_brain_short ~ clean_dat$stand_body_short))
plot(clean_dat$res_brain[clean_dat$species], clean_dat$quality)

# Model quality - species + genus + brain ----

# Subset data
cc_dat = dat_long[!is.na(dat_long$log_mean_life_exp) & 
                    !is.na(dat_long$`quality of speech mimicry`) &
                    !is.na(dat_long$log_mean_body_weight) &
                    !is.na(dat_long$log_mean_brain_size),]
cc_dat_short = dat[!is.na(dat$log_mean_life_exp) & 
                     !is.na(dat$log_mean_body_weight) &
                     !is.na(dat$log_mean_brain_size) &
                     dat$species %in% cc_dat$species,]
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
clean_dat = list(N_obs = length(cc_dat$`quality of speech mimicry`),
                 N_species = max(cc_dat$species_index),
                 N_genera = max(cc_dat$genus_index),
                 quality = as.integer(c(low = 1, moderate = 2, high = 3)[
                   cc_dat$`quality of speech mimicry`]),
                 species = cc_dat$species_index, 
                 genus = cc_dat$genus_index,
                 stand_long = as.numeric(scale(cc_dat$log_mean_life_exp)),
                 stand_body = as.numeric(scale(cc_dat$log_mean_body_weight)),
                 
                 stand_body_short = as.numeric(scale(cc_dat_short$log_mean_body_weight)),
                 stand_brain_short = as.numeric(scale(cc_dat_short$log_mean_brain_size)),
                 genus_short = cc_dat_short$genus_index,
                 
                 alpha = c(2, 2, 2))

model = cmdstan_model('ANALYSIS/CODE/models extended/quality_species_genus_brain.stan')
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
pdf('ANALYSIS/RESULTS/long - quality - species + genus + brain.pdf', 5, 5)
par(mar = c(3, 7, 1, 1))
params = list(post$sigma_species, post$sigma_genus, post$b_brain)
plot(sapply(params, mean), 1:length(params),
     xlim = c(-1, 2), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:length(params)) lines(PI(params[[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:length(params), 
     labels = c('sigma_species', 'sigma_genus', 'b_brain'), las = 1)  
dev.off()

# Check results with simple residuals
clean_dat$res_brain = clean_dat$stand_brain_short - 
  fitted(lm(clean_dat$stand_brain_short ~ clean_dat$stand_body_short))
plot(clean_dat$res_brain[clean_dat$species], clean_dat$quality)

# Model quality - species + genus + template ----

cc_dat = dat_long[!is.na(dat_long$`quality of speech mimicry`),]

# Compile data
clean_dat = list(N_obs = length(cc_dat$`quality of speech mimicry`),
                 N_species = max(cc_dat$species_index),
                 N_genera = max(cc_dat$genus_index),
                 quality = as.integer(c(low = 1, moderate = 2, high = 3)[
                   cc_dat$`quality of speech mimicry`]),
                 species = cc_dat$species_index, 
                 genus = cc_dat$genus_index,
                 temp = as.integer(c(no = 1, yes = 2, possibly = 2, `yes (NO?)` = 2)[cc_dat$template]),
                 alpha = c(2, 2, 2))

model = cmdstan_model('ANALYSIS/CODE/models extended/quality_species_genus_template.stan')
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
pdf('ANALYSIS/RESULTS/long - quality - species + genus + template.pdf', 5, 5)
par(mar = c(3, 7, 1, 1))
params = list(post$sigma_species, post$sigma_genus, post$a_temp[,2] - post$a_temp[,1])
plot(sapply(params, mean), 1:length(params),
     xlim = c(-1, 2), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:length(params)) lines(PI(params[[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:length(params), 
     labels = c('sigma_species', 'sigma_genus', 'template'), las = 1)  
dev.off()

# Model quality - species + genus + template ----

cc_dat = dat_long[!is.na(dat_long$`quality of speech mimicry`),]

# Compile data
clean_dat = list(N_obs = length(cc_dat$`quality of speech mimicry`),
                 N_species = max(cc_dat$species_index),
                 N_genera = max(cc_dat$genus_index),
                 quality = as.integer(c(low = 1, moderate = 2, high = 3)[
                   cc_dat$`quality of speech mimicry`]),
                 species = cc_dat$species_index, 
                 genus = cc_dat$genus_index,
                 temp = ifelse(str_detect(cc_dat$context, 'interaction with human'), 1L, 2L),
                 alpha = c(2, 2, 2))

model = cmdstan_model('ANALYSIS/CODE/models extended/quality_species_genus_template.stan')
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
pdf('ANALYSIS/RESULTS/long - quality - species + genus + context.pdf', 5, 5)
par(mar = c(3, 7, 1, 1))
params = list(post$sigma_species, post$sigma_genus, post$a_temp[,1] - post$a_temp[,2])
plot(sapply(params, mean), 1:length(params),
     xlim = c(-1, 2), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:length(params)) lines(PI(params[[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:length(params), 
     labels = c('sigma_species', 'sigma_genus', 'context'), las = 1)  
dev.off()

# Model quality - species + genus + template + context ----

cc_dat = dat_long[!is.na(dat_long$`quality of speech mimicry`),]

# Compile data
clean_dat = list(N_obs = length(cc_dat$`quality of speech mimicry`),
                 N_species = max(cc_dat$species_index),
                 N_genera = max(cc_dat$genus_index),
                 quality = as.integer(c(low = 1, moderate = 2, high = 3)[
                   cc_dat$`quality of speech mimicry`]),
                 species = cc_dat$species_index, 
                 genus = cc_dat$genus_index,
                 temp = as.integer(c(no = 1, yes = 2, possibly = 2, `yes (NO?)` = 2)[cc_dat$template]),
                 context = ifelse(str_detect(cc_dat$context, 'interaction with human'), 1L, 2L),
                 alpha = c(2, 2, 2))

model = cmdstan_model('ANALYSIS/CODE/models extended/quality_species_genus_template_context.stan')
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
pdf('ANALYSIS/RESULTS/long - quality - species + genus + template + context.pdf', 5, 5)
par(mar = c(3, 7, 1, 1))
params = list(post$sigma_species, post$sigma_genus, post$a_temp[,2] - post$a_temp[,1],
              post$a_cont[,1] - post$a_cont[,2])
plot(sapply(params, mean), 1:length(params),
     xlim = c(-1, 2), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:length(params)) lines(PI(params[[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:length(params), 
     labels = c('sigma_species', 'sigma_genus', 'template', 'context'), las = 1)  
dev.off()

# Model quality - species + genus + template + context + body ----

# Subset data
cc_dat = dat_long[!is.na(dat_long$`quality of speech mimicry`) &
                    !is.na(dat_long$log_mean_body_weight),]
cc_dat_short = dat[!is.na(dat$log_mean_body_weight) &
                     dat$species %in% cc_dat$species,]
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
clean_dat = list(N_obs = length(cc_dat$`quality of speech mimicry`),
                 N_species = max(cc_dat$species_index),
                 N_genera = max(cc_dat_short$genus_index),
                 quality = as.integer(c(low = 1, moderate = 2, high = 3)[
                   cc_dat$`quality of speech mimicry`]),
                 species = cc_dat$species_index, 
                 temp = as.integer(c(no = 1, yes = 2, possibly = 2, `yes (NO?)` = 2)[cc_dat$template]),
                 context = ifelse(str_detect(cc_dat$context, 'interaction with human'), 1L, 2L),
                 
                 stand_body = as.numeric(scale(cc_dat_short$log_mean_body_weight)),
                 genus = cc_dat_short$genus_index,
                 
                 alpha = c(2, 2, 2))

# Run model
model = cmdstan_model('ANALYSIS/CODE/models extended/quality_species_genus_template_context_body.stan')
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
pdf('ANALYSIS/RESULTS/long - quality - species + genus + template + context + body.pdf', 5, 5)
par(mar = c(3, 7, 1, 1))
params = list(post$sigma_species, post$sigma_genus, post$a_temp[,2] - post$a_temp[,1],
              post$a_cont[,1] - post$a_cont[,2], post$b_body)
plot(sapply(params, mean), 1:length(params),
     xlim = c(-1, 2), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:length(params)) lines(PI(params[[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:length(params), 
     labels = c('sigma_species', 'sigma_genus', 'template', 'context', 'b_body'), las = 1)  
dev.off()

# Model quality - species + genus + body + brain + longevity + sociality ----

# Subset data
cc_dat = dat_long[!is.na(dat_long$log_mean_life_exp) & 
                    !is.na(dat_long$`quality of speech mimicry`) &
                    !is.na(dat_long$log_mean_body_weight) &
                    !is.na(dat_long$log_mean_brain_size) &
                    !is.na(dat_long$gregar),]
cc_dat_short = dat[!is.na(dat$log_mean_life_exp) & 
                     !is.na(dat$log_mean_body_weight) &
                     !is.na(dat$log_mean_brain_size) &
                     dat$species %in% cc_dat$species,]
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
clean_dat = list(N_obs = length(cc_dat$`quality of speech mimicry`),
                 N_species = max(cc_dat$species_index),
                 N_genera = max(cc_dat_short$genus_index),
                 quality = as.integer(c(low = 1, moderate = 2, high = 3)[
                   cc_dat$`quality of speech mimicry`]),
                 species = cc_dat$species_index, 
                 
                 stand_long = as.numeric(scale(cc_dat_short$log_mean_life_exp)),
                 stand_body = as.numeric(scale(cc_dat_short$log_mean_body_weight)),
                 stand_brain = as.numeric(scale(cc_dat_short$log_mean_brain_size)),
                 gregar = cc_dat_short$gregar + 1L,
                 genus = cc_dat_short$genus_index,
                 
                 alpha = c(2, 2, 2))

model = cmdstan_model('ANALYSIS/CODE/models extended/quality_species_genus_body_brain_longevity_gregar.stan')
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
pdf('ANALYSIS/RESULTS/long - quality - species + genus + body + brain + longevity + gregar.pdf', 5, 5)
par(mar = c(3, 7, 1, 1))
params = list(post$sigma_species, post$sigma_genus, post$b_long, post$b_body, post$b_brain,
              post$a_greg[,2] - post$a_greg[,1])
plot(sapply(params, mean), 1:length(params),
     xlim = c(-1, 2), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:length(params)) lines(PI(params[[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:length(params), 
     labels = c('sigma_species', 'sigma_genus', 'b_long', 'b_body', 'b_brain', 'sociality'), las = 1)  
dev.off()

# Model quality - brain + sociality + imputation ----

# Subset data
cc_dat = dat_long[!is.na(dat_long$`quality of speech mimicry`) &
                    !is.na(dat_long$gregar),]
cc_dat_short = dat[!is.na(dat$gregar) &
                     !is.na(dat$log_mean_body_weight) &
                     dat$species %in% cc_dat$species,]
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
clean_dat = list(N_obs = length(cc_dat$`quality of speech mimicry`),
                 N_species = max(cc_dat$species_index),
                 N_genera = max(cc_dat_short$genus_index),
                 quality = as.integer(c(low = 1, moderate = 2, high = 3)[
                   cc_dat$`quality of speech mimicry`]),
                 species = cc_dat$species_index, 
                 
                 stand_body = as.numeric(scale(cc_dat_short$log_mean_body_weight)),
                 stand_brain = as.numeric(scale(cc_dat_short$log_mean_brain_size)),
                 gregar = cc_dat_short$gregar + 1L,
                 genus = cc_dat_short$genus_index,
                 
                 alpha = c(2, 2, 2))

# Prepare missing data
clean_dat$brain_missidx = which(is.na(cc_dat_short$log_mean_brain_size))
clean_dat$N_miss_brain = length(which(is.na(cc_dat_short$log_mean_brain_size)))
clean_dat$stand_brain[clean_dat$brain_missidx] = 999

model = cmdstan_model('ANALYSIS/CODE/models extended/quality_brain_gregar_imputation.stan')
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
pdf('ANALYSIS/RESULTS/long - quality - species + genus + brain + gregar + imputation.pdf', 5, 5)
par(mar = c(3, 7, 1, 1))
params = list(post$sigma_species, post$sigma_genus, post$b_brain,
              post$a_greg[,2] - post$a_greg[,1])
plot(sapply(params, mean), 1:length(params),
     xlim = c(-1.5, 1.5), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:length(params)) lines(PI(params[[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:length(params), 
     labels = c('sigma_species', 'sigma_genus', 'b_brain', 'sociality'), 
     las = 1)  
dev.off()

# Model quality - total + imputation ----

# Subset data
cc_dat = dat_long[!is.na(dat_long$`quality of speech mimicry`) &
                    !is.na(dat_long$log_mean_body_weight) &
                    !is.na(dat_long$gregar),]
cc_dat_short = dat[!is.na(dat$gregar) &
                     !is.na(dat$log_mean_body_weight) &
                     dat$species %in% cc_dat$species,]
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
clean_dat = list(N_obs = length(cc_dat$`quality of speech mimicry`),
                 N_species = max(cc_dat$species_index),
                 N_genera = max(cc_dat_short$genus_index),
                 quality = as.integer(c(low = 1, moderate = 2, high = 3)[
                   cc_dat$`quality of speech mimicry`]),
                 species = cc_dat$species_index, 
                 
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

model = cmdstan_model('ANALYSIS/CODE/models extended/quality_total_imputation.stan')
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
pdf('ANALYSIS/RESULTS/long - quality - total + imputation.pdf', 5, 5)
par(mar = c(3, 7, 1, 1))
params = list(post$sigma_species, post$sigma_genus, post$b_long, post$b_body, post$b_brain,
              post$a_greg[,2] - post$a_greg[,1])
plot(sapply(params, mean), 1:length(params),
     xlim = c(-1.5, 1.5), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:length(params)) lines(PI(params[[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:length(params), 
     labels = c('sigma_species', 'sigma_genus', 'b_long', 'b_body', 'b_brain', 'sociality'), 
     las = 1)  
dev.off()

# Model quality - total + imputation + context ----

# Subset data
cc_dat = dat_long[!is.na(dat_long$`quality of speech mimicry`) &
                    !is.na(dat_long$context) &
                    !is.na(dat_long$log_mean_body_weight) &
                    !is.na(dat_long$gregar),]
cc_dat_short = dat[!is.na(dat$gregar) &
                     !is.na(dat$log_mean_body_weight) &
                     dat$species %in% cc_dat$species,]
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
clean_dat = list(N_obs = length(cc_dat$`quality of speech mimicry`),
                 N_species = max(cc_dat$species_index),
                 N_genera = max(cc_dat_short$genus_index),
                 quality = as.integer(c(low = 1, moderate = 2, high = 3)[
                   cc_dat$`quality of speech mimicry`]),
                 species = cc_dat$species_index, 
                 context = ifelse(str_detect(cc_dat$context, 'interaction with human'), 1L, 2L),
                 
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

model = cmdstan_model('ANALYSIS/CODE/models extended/quality_total_imputation_context.stan')
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
