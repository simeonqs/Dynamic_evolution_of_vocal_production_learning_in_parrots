# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot vocal mimicry
# Date started: 11-11-2022
# Date last modified: 05-06-2023
# Author: Simeon Q. Smeele
# Description: Plotting the outcomes of the presence models.  
# This version is for pets only.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Set-up ----

# Loading libraries
libraries = c('tidyverse', 'rethinking', 'cmdstanr')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
# rm(list = ls()) 

# Colours 
cols = c(red = '#d11141', green = '#00b159', blue = '#00aedb', orange = '#f37735', yellow = '#ffc425')

# Paths
path_cleaned_data = 'ANALYSIS/RESULTS/cleaned_data.RData'
path_pdf = 'ANALYSIS/RESULTS/results presence mimicry - only pets.pdf'
path_models_presence = 'ANALYSIS/RESULTS/models_presence_pets_only.RData'
path_pdf_phylo = 'ANALYSIS/RESULTS/results phylogenetic signal - only pets.pdf'

# Load data
load(path_cleaned_data)
load(path_models_presence)

# Subset
keep = which(dat$CITEStrade != 'no' | dat$WPTpet != 'no')
dat = dat[keep,]

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
  ylims = c(0, 2.5)
  plot(NULL, xlim = c(-2.5, 2.5), ylim = ylims, main = '', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
  lines(c(0, 0), ylims, lty = 2, col = alpha(1, 0.5), lwd = 5)
  lines(density(effects[[i]]),
        col = alpha(cols[i], 0.8), lwd = 5)
}
axis(1, c(-1.5, 1.5), c(-1.5, 1.5))
mtext('effect', 1, 2.5, cex = 0.75)

# Scatters
## longevity
par(mar = c(2.5, 4.5, 0.5, 0.5))
var = dat$log_mean_life_exp
mean_var = mean(var, na.rm = T)
sd_var = sd(var, na.rm = T)
plot((var - mean_var) / sd_var, dat$vocal,
     pch = 16, col = alpha(1, 0.5), xaxt = 'n', yaxt = 'n',
     xlab = '', ylab = '')
axis(1, c(-2, 0, 2), round(exp(c(-2, 0, 2) * sd_var + mean_var)))
axis(2, c(0, 1), c(0, 1))
lims = seq(min((var - mean_var) / sd_var, na.rm = T), max((var - mean_var) / sd_var, na.rm = T), 0.1)
for(i in 1:20) lines(lims, 
                     inv_logit(sample(post_long$a_bar, 1) + sample(post_long$b_long, 1) * lims),
                     col = alpha(cols[1], 0.8), lwd = 5)
mtext('life expectancy [y]', 1, 2.5, cex = 0.75)
mtext('probability of VPL', 2, 2.5, cex = 0.75)
## brain size
var = post_brain$rel_brain %>% apply(2, mean)
mean_var = mean(var, na.rm = T)
sd_var = sd(var, na.rm = T)
plot((var - mean_var) / sd_var, dat[!is.na(dat$vocal) &
                                                 !is.na(dat$log_mean_body_weight) &
                                                 !is.na(dat$gregar),]$vocal,
     pch = 16, col = alpha(1, 0.5), xaxt = 'n', yaxt = 'n',
     xlab = '', ylab = '')
axis(1, c(-2, 0, 2), c(-2, 0, 2))
axis(2, c(0, 1), c(0, 1))
lims = seq(min((var - mean_var) / sd_var, na.rm = T), max((var - mean_var) / sd_var, na.rm = T), 0.1)
for(i in 1:20) lines(lims, 
                     inv_logit(sample(post_brain$a_bar, 1) + sample(post_brain$b_brain, 1) * lims),
                     col = alpha(cols[2], 0.8), lwd = 5)
mtext('relative brain size [sd]', 1, 2.5, cex = 0.75)
mtext('probability of VPL', 2, 2.5, cex = 0.75)
## sociality
par(mar = c(0.5, 4.5, 2.5, 0.5))
var = dat$gregar 
mean_var = mean(var, na.rm = T)
sd_var = sd(var, na.rm = T)
plot(var + rnorm(nrow(dat), 0, 0.1), dat$vocal,
     pch = 16, col = alpha(1, 0.5), xaxt = 'n', yaxt = 'n',
     xlab = '', ylab = '', xlim = c(-0.4, 1.4))
axis(1, c(0, 1), c('no', 'yes'))
axis(2, c(0, 1), c(0, 1))
lims = c(0, 1)
for(i in 1:20) lines(lims, 
                     inv_logit(c(sample(post_soc$a_greg[,1], 1), sample(post_soc$a_greg[,2], 1))),
                     col = alpha(cols[3], 0.8), lwd = 5)
mtext('gregarious', 1, 2.5, cex = 0.75)
mtext('probability of VPL', 2, 2.5, cex = 0.75)
## body
var = dat$log_mean_body_weight
mean_var = mean(var, na.rm = T)
sd_var = sd(var, na.rm = T)
plot((var - mean_var) / sd_var, dat$vocal,
     pch = 16, col = alpha(1, 0.5), xaxt = 'n', yaxt = 'n',
     xlab = '', ylab = '')
axis(1, c(-2, 0, 2), round(exp(c(-2, 0, 2) * sd_var + mean_var)))
axis(2, c(0, 1), c(0, 1))
lims = seq(min((var - mean_var) / sd_var, na.rm = T), max((var - mean_var) / sd_var, na.rm = T), 0.1)
for(i in 1:20) lines(lims, 
                     inv_logit(sample(post_body$a_bar, 1) + sample(post_body$b_body, 1) * lims),
                     col = alpha(cols[4], 0.8), lwd = 5)
mtext('body size [g]', 1, 2.5, cex = 0.75)
mtext('probability of VPL', 2, 2.5, cex = 0.75)


# Close PDF
dev.off()

# Phylogenetic model
pdf(path_pdf_phylo, 5, 5)
plot(NULL, xlab = '', ylab = '',
     xlim = c(0, 1) , ylim = c(0, 4), main = 'phylogenetic signal', cex = 2, cex.axis = 1.25, cex.main = 1.5)
mtext('normalised phylogenetic distance', 1, 3, cex = 1)
mtext('covariance', 2, 3, cex = 1)
x_seq <- seq(from = 0, to = 1, length.out = 1000)
# for(i in 1:20) curve( rexp(1, 2)*exp(-rexp(1, 0.1)*x^2) , add = TRUE ,
#                       col = alpha('grey', 0.4), lwd = 4)
for(i in 1:20){
  curve( post_phylo$etasq[i]*exp(-post_phylo$rhosq[i]*x^2) , add = TRUE ,
         col = alpha('black', 0.4), lwd = 4)
}
dev.off()

# Habitat output
pdf('ANALYSIS/RESULTS/habitat results.pdf', 5, 5)
a_hab_means = post_hab$a_hab |> apply(2, mean) |> inv_logit()
a_hab_pis = post_hab$a_hab |> apply(2, PI) |> inv_logit()
plot(a_hab_means, 1:3, xlim = c(0, 1), xlab = 'probability of VPL', yaxt = 'n', ylab = '')
axis(2, 1:3, c('closed', 'mixed', 'open'))
for(i in 1:3) lines(a_hab_pis[,i], rep(i, 2))
cont_cl_mi = post_hab$a_hab[,1] - post_hab$a_hab[,2]
cont_cl_op = post_hab$a_hab[,1] - post_hab$a_hab[,3]
cont_mi_op = post_hab$a_hab[,2] - post_hab$a_hab[,3]
plot(lapply(list(cont_cl_mi, cont_cl_op, cont_mi_op), mean), 1:3, 
     xlim = c(-2, 2), xlab = 'contrast [log-odds]', yaxt = 'n', ylab = '')
for(i in 1:3) lines(lapply(list(cont_cl_mi, cont_cl_op, cont_mi_op), PI)[[i]], rep(i, 2))
abline(v = 0, lty = 2)
axis(2, 1:3, c('closed vs mixed', 'closed vs open', 'mixed vs open'))
dev.off()
