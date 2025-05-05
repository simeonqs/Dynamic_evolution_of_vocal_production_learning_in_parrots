# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot vocal mimicry
# Date started: 10-11-2022
# Date last modified: 24-07-2023
# Author: Simeon Q. Smeele
# Description: Modelling the VPL total repertoire size.  
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('tidyverse', 'rethinking', 'cmdstanr', 'vioplot')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
# rm(list = ls()) 

# Colours 
cols = c(red = '#d11141', green = '#00b159', blue = '#00aedb', orange = '#f37735', yellow = '#ffc425')

# Paths
path_cleaned_data_long = 'ANALYSIS/RESULTS/cleaned_data_long.RData'
path_cleaned_data = 'ANALYSIS/RESULTS/cleaned_data.RData'
path_pdf = 'ANALYSIS/RESULTS/results VPL total repertoire size.pdf'
path_models_nr_unique = 'ANALYSIS/RESULTS/models_nr_unique.RData'

# Load data
load(path_cleaned_data_long)
load(path_cleaned_data)
load(path_models_nr_unique)

# Open PDF
pdf(path_pdf, 6, 5)

# Set plot layout
layout(mat = matrix(c(1, 5, 5, 6, 6,
                      2, 5, 5, 6, 6,
                      3, 7, 7, 8, 8,
                      4, 7, 7, 8, 8), nrow = 4, ncol = 5, byrow = T))
par(mar = rep(0.5, 4), oma = c(4.0, 0.5, 0.5, 0.5))

# Densities
effects = list(post_long$b_long, post_brain$b_brain, post_soc$cont_greg, post_body$b_body)
for(i in 1:4){
  message(sprintf('overlap 0: %s', round(length(which(effects[[i]]>0))/length(effects[[i]]), 2)))
  ylims = c(0, 7)
  plot(NULL, xlim = c(-1, 1), ylim = ylims, main = '', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
  lines(c(0, 0), ylims, lty = 2, col = alpha(1, 0.5), lwd = 5)
  lines(density(effects[[i]]),
        col = alpha(cols[i], 0.8), lwd = 5)
}
axis(1, c(-0.75, 0.75), c(-0.75, 0.75))
mtext('effect', 1, 2.5, cex = 0.75)

# Scatters
## longevity
par(mar = c(2.5, 4.5, 0.5, 0.5))
var = dat_long$log_mean_life_exp
mean_var = mean(var, na.rm = T)
sd_var = sd(var, na.rm = T)
plot((var - mean_var) / sd_var, log10(dat_long$n_im + 1),
     pch = 16, col = alpha(1, 0.5), xaxt = 'n', yaxt = 'n',
     xlab = '', ylab = '')
axis(1, c(-2, 0, 2), round(exp(c(-2, 0, 2) * sd_var + mean_var)))
axis(2, c(0, log10(2), log10(11)), c(0, 1, 10))
lims = c(min((var - mean_var) / sd_var, na.rm = T), max((var - mean_var) / sd_var, na.rm = T))
for(i in 1:20) lines(lims, 
                     log10(exp(sample(post_long$a_bar, 1) + sample(post_long$b_long, 1) * lims) + 1),
                     col = alpha(cols[1], 0.8), lwd = 5)
mtext('life expectancy [y]', 1, 2.5, cex = 0.75)
mtext('VPL total repertoire size', 2, 2.5, cex = 0.75)
## brain size
var = post_brain$rel_brain %>% apply(2, mean)
mean_var = mean(var, na.rm = T)
sd_var = sd(var, na.rm = T)
plot((var - mean_var) / sd_var, log10(dat_long[!is.na(dat_long$n_im) &
                                                 !is.na(dat_long$log_mean_body_weight) &
                                                 !is.na(dat_long$gregar),]$n_im + 1),
     pch = 16, col = alpha(1, 0.5), xaxt = 'n', yaxt = 'n',
     xlab = '', ylab = '')
axis(1, c(-2, 0, 2), c(-2, 0, 2))
axis(2, c(0, log10(2), log10(11)), c(0, 1, 10))
lims = c(min((var - mean_var) / sd_var, na.rm = T), max((var - mean_var) / sd_var, na.rm = T))
for(i in 1:20) lines(lims, 
                     log10(exp(sample(post_brain$a_bar, 1) + sample(post_brain$b_brain, 1) * lims) + 1),
                     col = alpha(cols[2], 0.8), lwd = 5)
mtext('relative brain size [sd]', 1, 2.5, cex = 0.75)
mtext('VPL total repertoire size', 2, 2.5, cex = 0.75)
## sociality
par(mar = c(0.5, 4.5, 2.5, 0.5))
var = dat_long$gregar 
mean_var = mean(var, na.rm = T)
sd_var = sd(var, na.rm = T)
plot(var + rnorm(nrow(dat_long), 0, 0.1), log10(dat_long$n_im + 1),
     pch = 16, col = alpha(1, 0.5), xaxt = 'n', yaxt = 'n',
     xlab = '', ylab = '')
axis(1, c(0, 1), c('non-gregarious', 'gregarious'))
axis(2, c(0, log10(2), log10(11)), c(0, 1, 10))
lims = c(0, 1)
for(i in 1:20) lines(lims, 
                     log10(exp(c(sample(post_soc$a_greg[,1], 1), sample(post_soc$a_greg[,2], 1))) + 1),
                     col = alpha(cols[3], 0.8), lwd = 5)
mtext('sociality', 1, 2.5, cex = 0.75)
mtext('VPL total repertoire size', 2, 2.5, cex = 0.75)
## body
var = dat_long$log_mean_body_weight
mean_var = mean(var, na.rm = T)
sd_var = sd(var, na.rm = T)
plot((var - mean_var) / sd_var, log10(dat_long$n_im + 1),
     pch = 16, col = alpha(1, 0.5), xaxt = 'n', yaxt = 'n',
     xlab = '', ylab = '')
axis(1, c(-2, 0, 2), round(exp(c(-2, 0, 2) * sd_var + mean_var)))
axis(2, c(0, log10(2), log10(11)), c(0, 1, 10))
lims = c(min((var - mean_var) / sd_var, na.rm = T), max((var - mean_var) / sd_var, na.rm = T))
for(i in 1:20) lines(lims, 
                     log10(exp(sample(post_body$a_bar, 1) + sample(post_body$b_body, 1) * lims) + 1),
                     col = alpha(cols[4], 0.8), lwd = 5)
mtext('body size [g]', 1, 2.5, cex = 0.75)
mtext('VPL total repertoire size', 2, 2.5, cex = 0.75)

# Close PDF
dev.off()

# Habitat output
pdf('ANALYSIS/RESULTS/habitat results - nr unique mimics.pdf', 5, 5)
a_hab_means = post_hab$a_hab |> apply(2, mean) |> exp()
a_hab_pis = post_hab$a_hab |> apply(2, PI) |> exp()
plot(a_hab_means, 1:3, xlim = c(0, 5), xlab = 'n unique mimics', yaxt = 'n', ylab = '')
axis(2, 1:3, c('closed', 'mixed', 'open'))
for(i in 1:3) lines(a_hab_pis[,i], rep(i, 2))
cont_cl_mi = post_hab$a_hab[,1] - post_hab$a_hab[,2]
cont_cl_op = post_hab$a_hab[,1] - post_hab$a_hab[,3]
cont_mi_op = post_hab$a_hab[,2] - post_hab$a_hab[,3]
plot(lapply(list(cont_cl_mi, cont_cl_op, cont_mi_op), mean), 1:3, 
     xlim = c(-1, 1), xlab = 'contrast [log-scale]', yaxt = 'n', ylab = '')
for(i in 1:3) lines(lapply(list(cont_cl_mi, cont_cl_op, cont_mi_op), PI)[[i]], rep(i, 2))
abline(v = 0, lty = 2)
axis(2, 1:3, c('closed vs mixed', 'closed vs open', 'mixed vs open'))
dev.off()

# Create PDF with raw variation in 10 most common species
tab_species = dat_long$species |> table()
enough_species = tab_species[tab_species >= 10]
set.seed(1)
ten_species = enough_species |> sample(10) |> names() |> sort(decreasing = FALSE)
trans_species = seq_along(ten_species)
names(trans_species) = ten_species
pdf('ANALYSIS/RESULTS/within species variation.pdf', 7, 10)
par(mar = c(4, 12, 1, 1))
sub = dat_long[dat_long$species %in% ten_species,]
plot(as.integer(sub$distinctmimic), trans_species[sub$species] + rnorm(nrow(sub), 0, 0.05),
     yaxt = 'n', xlab = 'VPL total repertoire size', ylab = '')
for(i in unique(trans_species[sub$species])) 
  vioplot(as.integer(sub$distinctmimic)[trans_species[sub$species] == i], at = i,
          horizontal = TRUE, add = TRUE, col = alpha(1, 0.5), pchMed = 3)
axis(2, trans_species, names(trans_species), las = 2)
dev.off()

