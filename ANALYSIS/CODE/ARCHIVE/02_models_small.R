# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot vocal mimicry
# Date started: 29-08-2022
# Date last modified: 18-09-2022
# Author: Simeon Q. Smeele
# Description: Running all kinds of Bayesian models.
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
path_cleaned_data = 'ANALYSIS/RESULTS/cleaned_data.RData'

path_model_phylo = 'ANALYSIS/CODE/models/phylo_signal.stan'
path_model_genus = 'ANALYSIS/CODE/models/genus.stan'
path_model_genus_family = 'ANALYSIS/CODE/models/genus_family.stan'
path_model_genus_longevity = 'ANALYSIS/CODE/models/genus_longevity.stan'
path_model_genus_longevity_imp = 'ANALYSIS/CODE/models/genus_longevity_impute.stan'
path_model_genus_gregar = 'ANALYSIS/CODE/models/genus_gregar.stan'
path_model_genus_longevity_brain = 'ANALYSIS/CODE/models/genus_longevity_brain.stan'
path_model_genus_proper_brain = 'ANALYSIS/CODE/models/genus_proper_brain.stan'
path_model_genus_longevity_brain_afr = 'ANALYSIS/CODE/models/genus_longevity_brain_afr.stan'
path_model_genus_longevity_brain_body = 'ANALYSIS/CODE/models/genus_longevity_brain_body.stan'
path_model_total = 'ANALYSIS/CODE/models/total.stan'
path_model_total_proper_brain = 'ANALYSIS/CODE/models/total_proper_brain.stan'
path_model_total_all_whistles = 'ANALYSIS/CODE/models/total_all_whistles.stan'

path_pdf_genus_results = 'ANALYSIS/RESULTS/genus results.pdf'
path_pdf_phylo = 'ANALYSIS/RESULTS/phylogenetic results.pdf'
path_pdf_genus_family_results = 'ANALYSIS/RESULTS/genus and family results.pdf'
path_pdf_genus_longevity_results = 'ANALYSIS/RESULTS/genus and longevity results.pdf'
path_pdf_genus_longevity_imp_results = 'ANALYSIS/RESULTS/genus and longevity results - impute.pdf'
path_pdf_genus_gregar_results = 'ANALYSIS/RESULTS/genus and gregariousness results.pdf'
path_pdf_genus_longevity_brain_results = 'ANALYSIS/RESULTS/genus, longevity and brain results.pdf'
path_pdf_genus_proper_brain_results = 'ANALYSIS/RESULTS/genus and proper brain results.pdf'
path_pdf_genus_longevity_brain_afr_results = 'ANALYSIS/RESULTS/genus, longevity, brain and afr results.pdf'
path_pdf_genus_longevity_brain_body_results = 'ANALYSIS/RESULTS/genus, longevity, brain and body results.pdf'
path_pdf_total_results = 'ANALYSIS/RESULTS/total results.pdf'
path_pdf_total_proper_brain_results = 'ANALYSIS/RESULTS/total results - proper brain.pdf'
path_pdf_total_all_whistles_results = 'ANALYSIS/RESULTS/total results - all whistles.pdf'
path_pdf_total_all_whistles_sub_results = 'ANALYSIS/RESULTS/total results - all whistles - subsetted.pdf'

# Load data
load(path_cleaned_data)

# Run phylogenetic model ----
clean_dat = list(mimic = dat$vocal,
                 dmat = cophenetic(tree)/max(cophenetic(tree)),
                 N_species = nrow(dat))
model = cmdstan_model(path_model_phylo)
fit = model$sample(data = clean_dat, 
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4,
                   refresh = 100,
                   adapt_delta = 0.95) 

fit_nice = fit$output_files() %>%
  rstan::read_stan_csv()
precis(fit_nice)

# Plot
post = extract.samples(fit_nice)
pdf(path_pdf_phylo)
plot(NULL, xlab = '', ylab = '',
     xlim = c(0, 1) , ylim = c(0, 4), main = 'phylogenetic signal', cex = 2, cex.axis = 1.25, cex.main = 1.5)
mtext('normalised phylogenetic distance', 1, 3, cex = 1)
mtext('covariance', 2, 3, cex = 1)
x_seq <- seq(from = 0, to = 1, length.out = 1000)
# for(i in 1:20) curve( rexp(1, 2)*exp(-rexp(1, 0.1)*x^2) , add = TRUE ,
#                       col = alpha('grey', 0.4), lwd = 4)
for(i in 1:20){
  curve( post$etasq[i]*exp(-post$rhosq[i]*x^2) , add = TRUE ,
         col = alpha('black', 0.4), lwd = 4)
}
dev.off()

# m_1 = ulam(
#   alist(
#     # mimic ~ binomial(p),
#     # vector[N]:logit(p) <- multi_normal(0 , SIGMA),
#     mimic <- multi_normal(0 , SIGMA),
#     matrix[N_species, N_species]: SIGMA <- cov_GPL2(dmat, etasq, rhosq, 0.01),
#     etasq ~ dexp(2),
#     rhosq ~ dexp(0.1),
#   ), data = clean_dat, chains = 1, cores = 1, sample = F)
# stancode(m_1)

# Run simple model with genus as predictor ----
genus_trans = 1:length(unique(dat$genus))
names(genus_trans) = sort(unique(dat$genus), decreasing = T)
clean_dat = list(mimic = dat$vocal,
                 genus = genus_trans[dat$genus],
                 N_species = nrow(dat), 
                 N_genus = length(unique(dat$genus)))
model = cmdstan_model(path_model_genus)
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
pdf(path_pdf_genus_results, 7, 21)
genus_averages = sapply(genus_trans, function(i) 
  sapply(1:length(post$a_bar), function(j)
    post$a_bar[j] + post$z[j, i] * post$sigma_genus[j]))
par(mar = c(3, 10, 2, 2))
plot(inv_logit(apply(genus_averages, 2, mean)), genus_trans,
     xlim = c(-0, 1), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:max(genus_trans)) lines(inv_logit(PI(genus_averages[,i])), rep(i, 2))
abline(v = 0.5, lty = 2)  
axis(2, genus_trans, labels = names(genus_trans), las = 1)  
dev.off()

# Run simple model with genus and family as predictor ----
genus_trans = 1:length(unique(dat$genus))
names(genus_trans) = sort(unique(dat$genus), decreasing = T)
family_trans = 1:length(unique(dat$family))
names(family_trans) = sort(unique(dat$family), decreasing = T)
clean_dat = list(mimic = dat$vocal,
                 genus = genus_trans[dat$genus],
                 family = family_trans[dat$family],
                 N_species = nrow(dat), 
                 N_genus = length(unique(dat$genus)),
                 N_family = length(unique(dat$family)))
model = cmdstan_model(path_model_genus_family)
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
pdf(path_pdf_genus_family_results, 7, 21)

genus_averages = sapply(genus_trans, function(i) 
  sapply(1:length(post$a_bar), function(j)
    post$a_bar[j] + post$z_genus[j, i] * post$sigma_genus[j]))
par(mar = c(3, 10, 2, 2))
plot(inv_logit(apply(genus_averages, 2, mean)), genus_trans,
     xlim = c(-0, 1), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:max(genus_trans)) lines(inv_logit(PI(genus_averages[,i])), rep(i, 2))
abline(v = 0.5, lty = 2)  
axis(2, genus_trans, labels = names(genus_trans), las = 1)  

family_averages = sapply(family_trans, function(i) 
  sapply(1:length(post$a_bar), function(j)
    post$a_bar[j] + post$z_family[j, i] * post$sigma_family[j]))
par(mar = c(30, 10, 2, 2))
plot(inv_logit(apply(family_averages, 2, mean)), family_trans,
     xlim = c(-0, 1), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:max(family_trans)) lines(inv_logit(PI(family_averages[,i])), rep(i, 2))
abline(v = 0.5, lty = 2)  
axis(2, family_trans, labels = names(family_trans), las = 1)  

dev.off()

# Run simple model with genus and longevity as predictor ----
cc_dat = dat[!is.na(dat$log_mean_life_exp),]
genus_trans = 1:length(unique(cc_dat$genus))
names(genus_trans) = sort(unique(cc_dat$genus), decreasing = T)
clean_dat = list(mimic = cc_dat$vocal,
                 genus = as.integer(genus_trans[cc_dat$genus]),
                 stand_long = as.numeric(scale(cc_dat$log_mean_life_exp)),
                 N_species = nrow(cc_dat), 
                 N_genus = length(unique(cc_dat$genus)))
model = cmdstan_model(path_model_genus_longevity)
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
pdf(path_pdf_genus_longevity_results, 5, 5)
par(mar = c(3, 6, 1, 1))
plot(sapply(post[c('a_bar', 'b', 'sigma_genus')], mean), 1:3,
     xlim = c(-1, 2), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:3) lines(PI(post[c('a_bar', 'b', 'sigma_genus')][[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:3, labels = c('a_bar', 'b', 'sigma_genus'), las = 1)  
dev.off()

# Run simple model with genus and longevity as predictor - impute ----
genus_trans = 1:length(unique(dat$genus))
names(genus_trans) = sort(unique(dat$genus), decreasing = T)
clean_dat = list(mimic = dat$vocal,
                 genus = as.integer(genus_trans[dat$genus]),
                 stand_long = as.numeric(scale(dat$log_mean_life_exp)),
                 N_species = nrow(dat), 
                 N_genus = length(unique(dat$genus)))
clean_dat$long_missidx = which(is.na(dat$log_mean_life_exp))
clean_dat$N_miss_long = length(which(is.na(dat$log_mean_life_exp)))
clean_dat$stand_long[clean_dat$long_missidx] = 999
model = cmdstan_model(path_model_genus_longevity_imp)
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
pdf(path_pdf_genus_longevity_imp_results, 5, 5)
par(mar = c(3, 6, 1, 1))
plot(sapply(post[c('a_bar', 'b', 'sigma_genus')], mean), 1:3,
     xlim = c(-1, 2), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:3) lines(PI(post[c('a_bar', 'b', 'sigma_genus')][[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:3, labels = c('a_bar', 'b', 'sigma_genus'), las = 1)  
dev.off()

# Run simple model with genus and gregariousness as predictor ----
cc_dat = dat[!is.na(dat$log_mean_life_exp) & !is.na(dat$gregar),]
genus_trans = 1:length(unique(cc_dat$genus))
names(genus_trans) = sort(unique(cc_dat$genus), decreasing = T)
clean_dat = list(mimic = cc_dat$vocal,
                 genus = as.integer(genus_trans[cc_dat$genus]),
                 gregar = as.integer(cc_dat$gregar) + 1L, # 1 = not, 2 = yes
                 N_species = nrow(cc_dat), 
                 N_genus = length(unique(cc_dat$genus)))
model = cmdstan_model(path_model_genus_gregar)
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
pdf(path_pdf_genus_gregar_results, 5, 5)
par(mar = c(3, 6, 1, 1))
cont_greg = sapply(1:length(post$a_bar), function(i) 
  (post$z_greg[i,2] * post$sigma_greg[i]) - (post$z_greg[i,1] * post$sigma_greg[i]))
plot(c(sapply(post[c('a_bar', 'sigma_genus', 'sigma_greg')], mean), mean(cont_greg)), 1:4,
     xlim = c(-1, 2), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:3) lines(PI(post[c('a_bar', 'sigma_genus', 'sigma_greg')][[i]]), rep(i, 2))
lines(PI(cont_greg), rep(4, 2))
abline(v = 0, lty = 2)  
axis(2, 1:4, labels = c('a_bar', 'sigma_genus', 'sigma_greg', 'cont_greg'), las = 1)  
dev.off()

# Run simple model with genus, longevity and residual brain size as predictor ----
cc_dat = dat[!is.na(dat$log_mean_life_exp) & !is.na(dat$res_brain),]
genus_trans = 1:length(unique(cc_dat$genus))
names(genus_trans) = sort(unique(cc_dat$genus), decreasing = T)
clean_dat = list(mimic = cc_dat$vocal,
                 genus = as.integer(genus_trans[cc_dat$genus]),
                 stand_long = as.numeric(scale(cc_dat$log_mean_life_exp)),
                 stand_brain = as.numeric(scale(cc_dat$res_brain)),
                 N_species = nrow(cc_dat), 
                 N_genus = length(unique(cc_dat$genus)))
model = cmdstan_model(path_model_genus_longevity_brain)
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
pdf(path_pdf_genus_longevity_brain_results, 5, 5)
par(mar = c(3, 6, 1, 1))
params = c('a_bar', 'b_long', 'b_brain', 'sigma_genus')
plot(sapply(post[params], mean), 1:length(params),
     xlim = c(-1, 2), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:length(params)) lines(PI(post[params][[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:length(params), labels = params, las = 1)  
dev.off()

# Run simple model with genus and proper brain size as predictor ----
cc_dat = dat[!is.na(dat$log_mean_life_exp) & !is.na(dat$log_mean_body_weight) &
               !is.na(dat$log_mean_brain_size),]
genus_trans = 1:length(unique(cc_dat$genus))
names(genus_trans) = sort(unique(cc_dat$genus), decreasing = T)
clean_dat = list(mimic = cc_dat$vocal,
                 genus = as.integer(genus_trans[cc_dat$genus]),
                 stand_body = as.numeric(scale(cc_dat$log_mean_body_weight)),
                 stand_brain = as.numeric(scale(cc_dat$log_mean_brain_size)),
                 N_species = nrow(cc_dat), 
                 N_genus = length(unique(cc_dat$genus)))
model = cmdstan_model(path_model_genus_proper_brain)
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
pdf(path_pdf_genus_proper_brain_results, 5, 5)
par(mar = c(3, 6, 1, 1))
params = c('a_bar', 'b_brain', 'sigma_genus')
plot(sapply(post[params], mean), 1:length(params),
     xlim = c(-1, 2), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:length(params)) lines(PI(post[params][[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:length(params), labels = params, las = 1)  
dev.off()

# Run simple model with genus, longevity, residual brain size and AFR as predictor ----
cc_dat = dat[!is.na(dat$log_mean_life_exp) & !is.na(dat$res_brain) & !is.na(dat$afr_5th_percentile),]
genus_trans = 1:length(unique(cc_dat$genus))
names(genus_trans) = sort(unique(cc_dat$genus), decreasing = T)
clean_dat = list(mimic = cc_dat$vocal,
                 genus = as.integer(genus_trans[cc_dat$genus]),
                 stand_long = as.numeric(scale(cc_dat$log_mean_life_exp)),
                 stand_brain = as.numeric(scale(cc_dat$res_brain)),
                 stand_afr = as.numeric(scale(log(cc_dat$afr_5th_percentile))),
                 N_species = nrow(cc_dat), 
                 N_genus = length(unique(cc_dat$genus)))
model = cmdstan_model(path_model_genus_longevity_brain_afr)
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
pdf(path_pdf_genus_longevity_brain_afr_results, 5, 5)
par(mar = c(3, 6, 1, 1))
params = c('a_bar', 'b_long', 'b_brain', 'b_afr', 'sigma_genus')
plot(sapply(post[params], mean), 1:length(params),
     xlim = c(-1, 2), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:length(params)) lines(PI(post[params][[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:length(params), labels = params, las = 1)  
dev.off()

# Run simple model with genus, longevity, residual brain size and body size as predictor ----
cc_dat = dat[!is.na(dat$log_mean_life_exp) & !is.na(dat$res_brain) & !is.na(dat$log_mean_body_weight),]
genus_trans = 1:length(unique(cc_dat$genus))
names(genus_trans) = sort(unique(cc_dat$genus), decreasing = T)
clean_dat = list(mimic = cc_dat$vocal,
                 genus = as.integer(genus_trans[cc_dat$genus]),
                 stand_long = as.numeric(scale(cc_dat$log_mean_life_exp)),
                 stand_brain = as.numeric(scale(cc_dat$res_brain)),
                 stand_body = as.numeric(scale(log(cc_dat$log_mean_body_weight))),
                 N_species = nrow(cc_dat), 
                 N_genus = length(unique(cc_dat$genus)))
model = cmdstan_model(path_model_genus_proper_brain)
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
pdf(path_pdf_genus_longevity_brain_body_results, 5, 5)
par(mar = c(3, 6, 1, 1))
params = c('a_bar', 'b_long', 'b_brain', 'b_body', 'sigma_genus')
plot(sapply(post[params], mean), 1:length(params),
     xlim = c(-1, 2), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:length(params)) lines(PI(post[params][[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:length(params), labels = params, las = 1)  
dev.off()

# Run simple total model ----
cc_dat = dat[!is.na(dat$log_mean_life_exp) & !is.na(dat$res_brain) & !is.na(dat$log_mean_body_weight) &
               !is.na(dat$afr_5th_percentile),]
genus_trans = 1:length(unique(cc_dat$genus))
names(genus_trans) = sort(unique(cc_dat$genus), decreasing = T)
clean_dat = list(mimic = cc_dat$vocal,
                 genus = as.integer(genus_trans[cc_dat$genus]),
                 stand_long = as.numeric(scale(cc_dat$log_mean_life_exp)),
                 stand_brain = as.numeric(scale(cc_dat$res_brain)),
                 stand_body = as.numeric(scale(log(cc_dat$log_mean_body_weight))),
                 stand_afr = as.numeric(scale(log(cc_dat$afr_5th_percentile))),
                 N_species = nrow(cc_dat), 
                 N_genus = length(unique(cc_dat$genus)))
model = cmdstan_model(path_model_total)
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
pdf(path_pdf_total_results, 5, 5)
par(mar = c(3, 6, 1, 1))
params = c('a_bar', 'b_long', 'b_brain', 'b_body', 'b_afr', 'sigma_genus')
plot(sapply(post[params], mean), 1:length(params),
     xlim = c(-1, 2), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:length(params)) lines(PI(post[params][[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:length(params), labels = params, las = 1)  
dev.off()

# Run simple total model - proper brain ----
cc_dat = dat[!is.na(dat$log_mean_life_exp) & !is.na(dat$log_mean_brain_size) & 
               !is.na(dat$log_mean_body_weight) &
               !is.na(dat$afr_5th_percentile),]
genus_trans = 1:length(unique(cc_dat$genus))
names(genus_trans) = sort(unique(cc_dat$genus), decreasing = T)
clean_dat = list(mimic = cc_dat$vocal,
                 genus = as.integer(genus_trans[cc_dat$genus]),
                 stand_long = as.numeric(scale(cc_dat$log_mean_life_exp)),
                 stand_brain = as.numeric(scale(cc_dat$log_mean_brain_size)),
                 stand_body = as.numeric(scale(log(cc_dat$log_mean_body_weight))),
                 stand_afr = as.numeric(scale(log(cc_dat$afr_5th_percentile))),
                 N_species = nrow(cc_dat), 
                 N_genus = length(unique(cc_dat$genus)))
model = cmdstan_model(path_model_total_proper_brain)
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
pdf(path_pdf_total_proper_brain_results, 5, 5)
par(mar = c(3, 6, 1, 1))
params = c('a_bar', 'b_long', 'b_brain', 'b_body', 'b_afr', 'sigma_genus')
plot(sapply(post[params], mean), 1:length(params),
     xlim = c(-1, 2), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:length(params)) lines(PI(post[params][[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:length(params), labels = params, las = 1)  
dev.off()

# Run simple total model - all whistles ----
stop('Check that body is not imputed.')

cc_dat = dat[!is.na(dat$gregar),]
genus_trans = 1:length(unique(cc_dat$genus))
names(genus_trans) = sort(unique(cc_dat$genus), decreasing = T)

# Prepare brain size
sd_brain = sd(cc_dat$log_mean_brain_size, na.rm = T)
mean_brain = mean(cc_dat$log_mean_brain_size, na.rm = T)
stand_brain = as.numeric((cc_dat$log_mean_brain_size - mean_brain) / sd_brain)
se_brain = cc_dat$log_SE_brain_size
se_brain = as.numeric(se_brain / sd_brain)

# Prepare body size
sd_body = sd(cc_dat$log_mean_body_weight, na.rm = T)
mean_body = mean(cc_dat$log_mean_body_weight, na.rm = T)
stand_body = as.numeric((cc_dat$log_mean_body_weight - mean_body) / sd_body)
se_body = cc_dat$log_mean_body_weight
se_body = as.numeric(se_body / sd_body)

# Prepare longevity
sd_long = sd(cc_dat$log_mean_life_exp, na.rm = T)
mean_long = mean(cc_dat$log_mean_life_exp, na.rm = T)
stand_long = as.numeric((cc_dat$log_mean_life_exp - mean_long) / sd_long)
se_long = cc_dat$log_SE_life_exp
se_long = as.numeric(se_long / sd_long)

# Compile data
clean_dat = list(mimic = cc_dat$vocal,
                 genus = as.integer(genus_trans[cc_dat$genus]),
                 stand_long = stand_long,
                 se_long = se_long,
                 stand_brain = stand_brain,
                 se_brain = se_brain,
                 stand_body = stand_body,
                 se_body = se_body,
                 stand_afr = as.numeric(scale(log(cc_dat$afr_5th_percentile))),
                 sociality = cc_dat$gregar + 1,
                 N_species = nrow(cc_dat), 
                 N_genus = length(unique(cc_dat$genus)))

# Prepare missing data
clean_dat$brain_missidx = which(is.na(cc_dat$log_mean_brain_size))
clean_dat$N_miss_brain = length(which(is.na(cc_dat$log_mean_brain_size)))
clean_dat$stand_brain[clean_dat$brain_missidx] = 999
clean_dat$se_brain[clean_dat$brain_missidx] = 0.5

clean_dat$long_missidx = which(is.na(cc_dat$log_mean_life_exp))
clean_dat$N_miss_long = length(which(is.na(cc_dat$log_mean_life_exp)))
clean_dat$stand_long[clean_dat$long_missidx] = 999
clean_dat$se_long[clean_dat$long_missidx] = 0.5

clean_dat$afr_missidx = which(is.na(cc_dat$afr_5th_percentile))
clean_dat$N_miss_afr = length(which(is.na(cc_dat$afr_5th_percentile)))
clean_dat$stand_afr[clean_dat$afr_missidx] = 999

model = cmdstan_model(path_model_total_all_whistles)
fit = model$sample(data = clean_dat, 
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4,
                   refresh = 500,
                   adapt_delta = 0.95) 
fit_nice = fit$output_files() %>%
  rstan::read_stan_csv()
precis(fit_nice)
traceplot(fit_nice, pars = c('a_brain', 'b_body_brain', 'a_long'))

# Plot results
post = extract.samples(fit_nice)
pdf(path_pdf_total_all_whistles_results, 5, 5)
par(mar = c(3, 6, 1, 1))
contrast = sapply(1:length(post$a_bar), function(i) 
  (post$z_soc[i,2] * post$sigma_soc) - (post$z_soc[,1] * post$sigma_soc))
params = list(post$a_bar, contrast, post$b_long, post$b_brain, post$b_body, post$b_afr, post$sigma_genus)
plot(sapply(params, mean), 1:length(params),
     xlim = c(-1, 2), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:length(params)) lines(PI(params[[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:length(params), 
     labels = c('a_bar', 'sociality', 'b_long', 'b_brain', 'b_body', 'b_afr', 'sigma_genus'), las = 1)  
dev.off()

# Run simple total model - all whistles - subsetted ----
sub_dat = dat[which(dat$video == 1 & dat$pet == 1),]
genus_trans = 1:length(unique(sub_dat$genus))
names(genus_trans) = sort(unique(sub_dat$genus), decreasing = T)

# Prepare brain size
sd_brain = sd(sub_dat$log_mean_brain_size, na.rm = T)
mean_brain = mean(sub_dat$log_mean_brain_size, na.rm = T)
stand_brain = as.numeric((sub_dat$log_mean_brain_size - mean_brain) / sd_brain)
se_brain = sub_dat$log_SE_brain_size
se_brain = as.numeric(se_brain / sd_brain)

# Prepare body size
sd_body = sd(sub_dat$log_mean_body_weight, na.rm = T)
mean_body = mean(sub_dat$log_mean_body_weight, na.rm = T)
stand_body = as.numeric((sub_dat$log_mean_body_weight - mean_body) / sd_body)
se_body = sub_dat$log_mean_body_weight
se_body = as.numeric(se_body / sd_body)

# Prepare longevity
sd_long = sd(sub_dat$log_mean_life_exp, na.rm = T)
mean_long = mean(sub_dat$log_mean_life_exp, na.rm = T)
stand_long = as.numeric((sub_dat$log_mean_life_exp - mean_long) / sd_long)
se_long = sub_dat$log_SE_life_exp
se_long = as.numeric(se_long / sd_long)

# Compile sub_data
clean_sub_dat = list(mimic = sub_dat$vocal,
                     genus = as.integer(genus_trans[sub_dat$genus]),
                     stand_long = stand_long,
                     se_long = se_long,
                     stand_brain = stand_brain,
                     se_brain = se_brain,
                     stand_body = stand_body,
                     se_body = se_body,
                     stand_afr = as.numeric(scale(log(sub_dat$afr_5th_percentile))),
                     N_species = nrow(sub_dat), 
                     N_genus = length(unique(sub_dat$genus)))

# Prepare missing sub_data
clean_sub_dat$brain_missidx = which(is.na(sub_dat$log_mean_brain_size))
clean_sub_dat$N_miss_brain = length(which(is.na(sub_dat$log_mean_brain_size)))
clean_sub_dat$stand_brain[clean_sub_dat$brain_missidx] = 999
clean_sub_dat$se_brain[clean_sub_dat$brain_missidx] = 0.5

clean_sub_dat$long_missidx = which(is.na(sub_dat$log_mean_life_exp))
clean_sub_dat$N_miss_long = length(which(is.na(sub_dat$log_mean_life_exp)))
clean_sub_dat$stand_long[clean_sub_dat$long_missidx] = 999
clean_sub_dat$se_long[clean_sub_dat$long_missidx] = 0.5

clean_sub_dat$afr_missidx = which(is.na(sub_dat$afr_5th_percentile))
clean_sub_dat$N_miss_afr = length(which(is.na(sub_dat$afr_5th_percentile)))
clean_sub_dat$stand_afr[clean_sub_dat$afr_missidx] = 999

model = cmdstan_model(path_model_total_all_whistles)
fit = model$sample(data = clean_sub_dat, 
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4,
                   refresh = 500,
                   adapt_delta = 0.95) 
fit_nice = fit$output_files() %>%
  rstan::read_stan_csv()
precis(fit_nice)
traceplot(fit_nice, pars = c('a_brain', 'b_body_brain', 'a_long'))

# Plot results
post = extract.samples(fit_nice)
pdf(path_pdf_total_all_whistles_sub_results, 5, 5)
par(mar = c(3, 6, 1, 1))
params = c('a_bar', 'b_long', 'b_brain', 'b_body', 'b_afr', 'sigma_genus')
plot(sapply(post[params], mean), 1:length(params),
     xlim = c(-1, 2), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:length(params)) lines(PI(post[params][[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:length(params), labels = params, las = 1)  
dev.off()

# Run simple total model - all whistles - no afr ----
cc_dat = dat[!is.na(dat$gregar),]
genus_trans = 1:length(unique(cc_dat$genus))
names(genus_trans) = sort(unique(cc_dat$genus), decreasing = T)

# Prepare brain size
sd_brain = sd(cc_dat$log_mean_brain_size, na.rm = T)
mean_brain = mean(cc_dat$log_mean_brain_size, na.rm = T)
stand_brain = as.numeric((cc_dat$log_mean_brain_size - mean_brain) / sd_brain)
se_brain = cc_dat$log_SE_brain_size
se_brain = as.numeric(se_brain / sd_brain)

# Prepare body size
sd_body = sd(cc_dat$log_mean_body_weight, na.rm = T)
mean_body = mean(cc_dat$log_mean_body_weight, na.rm = T)
stand_body = as.numeric((cc_dat$log_mean_body_weight - mean_body) / sd_body)
se_body = cc_dat$log_mean_body_weight
se_body = as.numeric(se_body / sd_body)

# Prepare longevity
sd_long = sd(cc_dat$log_mean_life_exp, na.rm = T)
mean_long = mean(cc_dat$log_mean_life_exp, na.rm = T)
stand_long = as.numeric((cc_dat$log_mean_life_exp - mean_long) / sd_long)
se_long = cc_dat$log_SE_life_exp
se_long = as.numeric(se_long / sd_long)

# Compile data
clean_dat = list(mimic = cc_dat$vocal,
                 genus = as.integer(genus_trans[cc_dat$genus]),
                 stand_long = stand_long,
                 stand_brain = stand_brain,
                 stand_body = stand_body,
                 sociality = cc_dat$gregar + 1,
                 N_species = nrow(cc_dat), 
                 N_genus = length(unique(cc_dat$genus)))

# Prepare missing data
clean_dat$brain_missidx = which(is.na(cc_dat$log_mean_brain_size))
clean_dat$N_miss_brain = length(which(is.na(cc_dat$log_mean_brain_size)))
clean_dat$stand_brain[clean_dat$brain_missidx] = 999

clean_dat$long_missidx = which(is.na(cc_dat$log_mean_life_exp))
clean_dat$N_miss_long = length(which(is.na(cc_dat$log_mean_life_exp)))
clean_dat$stand_long[clean_dat$long_missidx] = 999

model = cmdstan_model('ANALYSIS/CODE/models/total_all_whistles_no_afr.stan')
fit = model$sample(data = clean_dat, 
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4,
                   refresh = 500,
                   adapt_delta = 0.95) 
fit_nice = fit$output_files() %>%
  rstan::read_stan_csv()
precis(fit_nice)
traceplot(fit_nice, pars = c('a_brain', 'b_body_brain', 'a_long'))

# Plot results
post = extract.samples(fit_nice)
pdf('ANALYSIS/RESULTS/total results - all whistles - no afr.pdf', 5, 5)
par(mar = c(3, 6, 1, 1))
contrast = sapply(1:length(post$a_bar), function(i) 
  (post$z_soc[i,2] * post$sigma_soc) - (post$z_soc[,1] * post$sigma_soc))
params = list(post$a_bar, contrast, post$b_long, post$b_brain, post$b_body, post$sigma_genus)
plot(sapply(params, mean), 1:length(params),
     xlim = c(-1, 2), xlab = '', yaxt = 'n', ylab = '')
for(i in 1:length(params)) lines(PI(params[[i]]), rep(i, 2))
abline(v = 0, lty = 2)  
axis(2, 1:length(params), 
     labels = c('a_bar', 'sociality', 'b_long', 'b_brain', 'b_body', 'sigma_genus'), las = 1)  
dev.off()

# Eye pinning ----

# Compile data
clean_dat = data.frame(mimic = dat$vocal,
                       eye = dat$eyepin,
                       genus = as.integer(genus_trans[dat$genus]),
                       stand_long = dat$log_mean_life_exp,
                       stand_body = dat$log_mean_body_weight,
                       sociality = dat$gregar + 1)
plot(clean_dat)

# Ground ----

# Compile data
clean_dat = data.frame(mimic = dat$vocal,
                       ground = dat$ground,
                       genus = as.integer(genus_trans[dat$genus]),
                       stand_long = dat$log_mean_life_exp,
                       stand_body = dat$log_mean_body_weight,
                       sociality = dat$gregar + 1)
plot(clean_dat)

