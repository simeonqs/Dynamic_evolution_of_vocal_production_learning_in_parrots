# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot vocal mimicry
# Date started: 28-11-2022
# Date last modified: 16-12-2022
# Author: Simeon Q. Smeele
# Description: Checking Rhat in all posts.    
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('tidyverse', 'rethinking', 'cmdstanr')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list = ls()) 

# Paths
path_models_presence = 'ANALYSIS/RESULTS/models_presence.RData'
path_models_nr_unique = 'ANALYSIS/RESULTS/models_nr_unique.RData'
path_models_nr_unique_words = 'ANALYSIS/RESULTS/models_nr_unique_words.RData'
path_models_quality = 'ANALYSIS/RESULTS/models_quality.RData'
path_models_subset = 'ANALYSIS/RESULTS/models_presence_pets_only.RData'

# Load fits presence
load(path_models_presence)

fits = ls()[str_detect(ls(), 'fit')]

# Check fits
message('Checking models presence.')
for(fit in fits){
  f = get(fit)
  p = precis(f, depth = 3)
  p = p[!str_detect(rownames(p), 'brain_merge') &  # remove rows that are not parameters in the model
          !str_detect(rownames(p), 'pred_brain') & # these are actually generated quantities
          !str_detect(rownames(p), 'rel_brain'),]
  if(any(p$Rhat4 > 1.01) | any(p$Rhat4 < 0.99)) warning(sprintf('Rhat not good for %s in presence.', fit))
}

# Load fits nr unique
rm(list = c(fits, 'fit', 'fits')) # remove previous fits
load(path_models_nr_unique)

fits = ls()[str_detect(ls(), 'fit')]

# Check fits
message('Checking models nr unique.')
for(fit in fits){
  f = get(fit)
  p = precis(f, depth = 3)
  p = p[!str_detect(rownames(p), 'brain_merge') &  # remove rows that are not parameters in the model
          !str_detect(rownames(p), 'pred_brain') & # these are actually generated quantities
          !str_detect(rownames(p), 'rel_brain'),]
  if(any(p$Rhat4 > 1.01) | any(p$Rhat4 < 0.99)) warning(sprintf('Rhat not good for %s in nr unique.', fit))
}

# Load fits nr unique words
rm(list = c(fits, 'fit', 'fits')) # remove previous fits
load(path_models_nr_unique_words)

fits = ls()[str_detect(ls(), 'fit')]

# Check fits
message('Checking models words.')
for(fit in fits){
  f = get(fit)
  p = precis(f, depth = 3)
  p = p[!str_detect(rownames(p), 'brain_merge') &  # remove rows that are not parameters in the model
          !str_detect(rownames(p), 'pred_brain') & # these are actually generated quantities
          !str_detect(rownames(p), 'rel_brain'),]
  if(any(p$Rhat4 > 1.01) | any(p$Rhat4 < 0.99)) warning(sprintf('Rhat not good for %s in nr words.', fit))
}

# Load fits quality
rm(list = c(fits, 'fit', 'fits')) # remove previous fits
load(path_models_quality)

fits = ls()[str_detect(ls(), 'fit')]

# Check fits
message('Checking models quality.')
for(fit in fits){
  f = get(fit)
  p = precis(f, depth = 3)
  p = p[!str_detect(rownames(p), 'brain_merge') &  # remove rows that are not parameters in the model
          !str_detect(rownames(p), 'pred_brain') & # these are actually generated quantities
          !str_detect(rownames(p), 'rel_brain'),]
  if(any(p$Rhat4 > 1.01) | any(p$Rhat4 < 0.99)) warning(sprintf('Rhat not good for %s in quality.', fit))
}

# Load fits subset
rm(list = c(fits, 'fit', 'fits')) # remove previous fits
load(path_models_subset)

fits = ls()[str_detect(ls(), 'fit')]

# Check fits
message('Checking models subset.')
for(fit in fits){
  f = get(fit)
  p = precis(f, depth = 3)
  p = p[!str_detect(rownames(p), 'brain_merge') &  # remove rows that are not parameters in the model
          !str_detect(rownames(p), 'pred_brain') & # these are actually generated quantities
          !str_detect(rownames(p), 'rel_brain'),]
  if(any(p$Rhat4 > 1.01) | any(p$Rhat4 < 0.99)) warning(sprintf('Rhat not good for %s in subset.', fit))
}

message('All done.')