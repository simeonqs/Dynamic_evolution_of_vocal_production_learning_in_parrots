# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot vocal mimicry
# Date started: 27-08-2022
# Date last modified: 02-06-2023
# Author: Simeon Q. Smeele
# Description: Loading the data and plotting a phylogenetic tree. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# DATA ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('data.table', 'tidyverse', 'readxl', 'ape')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list = ls()) 

# Paths
path_overview = 'ANALYSIS/DATA/parrot_vocalmimic_socioecol.xlsx' # data per species
path_cites_wpt = 'ANALYSIS/DATA/parrot_vpl_overview.xlsx' # contains CITES and WPT pet data
path_extended = 'ANALYSIS/DATA/parrot_vocalmimic_detailed_updated_withoutfilter.xlsx' # data per ind
path_tree = 'ANALYSIS/DATA/5.ParrotSupertree.tre'
path_cleaned_data = 'ANALYSIS/RESULTS/cleaned_data.RData'
path_cleaned_data_long = 'ANALYSIS/RESULTS/cleaned_data_long.RData'
path_functions = 'ANALYSIS/CODE/functions'
path_body = 'ANALYSIS/DATA/body weight/master_dat.RData'
path_brain = 'ANALYSIS/DATA/brain size/master_dat.RData'
path_longevity = 'ANALYSIS/DATA/longevity/master_dat.RData'
path_afr = 'ANALYSIS/DATA/AFR/5th percentile results.csv'
path_taxonomy = 'ANALYSIS/DATA/IUCN translation list 01 _ manual added.csv'

# Import functions
.functions = sapply(list.files(path_functions, pattern = '*R', full.names = T), source)

# Load data
dat = as.data.frame(read_xlsx(path_overview, na = 'NA'))

# Merge CITES and WPT pet data
cites_wpt = as.data.frame(read_xlsx(path_cites_wpt, na = 'NA'))
first = cites_wpt$`Scientific name` |> strsplit(' ') |> sapply(`[`, 1) |> str_to_title()
second = cites_wpt$`Scientific name` |> strsplit(' ') |> sapply(`[`, 2) 
cites_wpt$scinam = paste(first, second, sep = '_')
dat = merge(dat, cites_wpt[,c('scinam', 'CITEStrade', 'WPTpet')], by = 'scinam', all.x = T, all.y = F)

# Load tree
tree = read.tree(path_tree)

# Taxize data
taxonomy = read.csv2(path_taxonomy)
dat$orig_species = dat$scinam %>% strsplit('_') %>% sapply(paste, collapse = ' ')
dat = taxize.data.frame(dat, 'orig_species', taxonomy, printer = T)

# Merge on other data
load(path_body)
dat = merge(dat, master_dat, by = 'species', all.x = T, all.y = F)
load(path_brain)
dat = merge(dat, master_dat, by = 'species', all.x = T, all.y = F)
load(path_longevity)
dat = merge(dat, master_dat, by = 'species', all.x = T, all.y = F)
afr = read.csv2(path_afr)
dat = merge(dat, afr, by = 'species', all.x = T, all.y = F)
dat$log_afr = log(dat$afr_5th_percentile)

# Load data
dat_long = as.data.frame(read_xlsx(path_extended, na = 'NA'))

# Taxize data
taxonomy$original_species = tolower(taxonomy$original_species)
dat_long$orig_species = dat_long$species %>% strsplit('_') %>% sapply(paste, collapse = ' ')
dat_long = taxize.data.frame(dat_long, 'orig_species', taxonomy, printer = T)

# Merge on other data
load(path_body)
dat_long = merge(dat_long, master_dat, by = 'species', all.x = T, all.y = F)
load(path_brain)
dat_long = merge(dat_long, master_dat, by = 'species', all.x = T, all.y = F)
load(path_longevity)
dat_long = merge(dat_long, master_dat, by = 'species', all.x = T, all.y = F)
afr = read.csv2(path_afr)
dat_long = merge(dat_long, afr, by = 'species', all.x = T, all.y = F)
dat_long = merge(dat_long, dat[,c('species', 'gregar')], by = 'species', all.x = T, all.y = F)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ANALYSIS ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Missing in tree
test = unique(dat$scinam[!dat$scinam %in% tree$tip.label])
if(length(test) > 0) stop('Species missing in tree.')

# Pruning tree
drop_tips = tree$tip.label[!tree$tip.label %in% dat$scinam]
message('Dropping: ', paste(drop_tips, collapse = ', '), ' from tree.')
tree = drop.tip(tree, drop_tips)

# Ordering data according to tree
rownames(dat) = dat$scinam
dat = dat[tree$tip.label,]

# Fixing genus
dat$genus = dat$scinam %>% strsplit('_') %>% sapply(`[`, 1)

# Fixing some format
dat_long$distinctmimic = dat_long$distinctmimic %>% str_remove('>')
dat_long$n_im = as.numeric(dat_long$distinctmimic)

# Fixing genus
dat_long$genus = dat_long$species %>% strsplit(' ') %>% sapply(`[`, 1)

# # Fixing typos
# dat_long$mimicquality[which(dat_long$mimicquality == 'High')] = 'high'
# dat_long$mimicquality[which(dat_long$mimicquality == 'mediate')] =  'moderate'

# Fix column words
dat_long$distinctwords = as.numeric(dat_long$distinctwords)
dat_long$distinctwords[is.na(dat_long$distinctwords)] = 0

# Save objects
save(dat, tree, file = path_cleaned_data)
save(dat_long, tree, file = path_cleaned_data_long)