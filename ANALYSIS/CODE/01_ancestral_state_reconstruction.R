# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot vocal mimicry
# Date started: 09-11-2022
# Date last modified: 09-11-2022
# Author: Simeon Q. Smeele
# Description: Running a simple ancestral state reconstruction for the mimicry ability.  
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('data.table', 'tidyverse', 'readxl', 'ape', 'phytools')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list = ls()) 

# Paths
path_pdf_tree = 'ANALYSIS/RESULTS/tree mimicry or not.pdf'
path_pdf_tree_names = 'ANALYSIS/RESULTS/tree mimicry or not - named.pdf'
path_pdf_tree_names_pets = 'ANALYSIS/RESULTS/tree mimicry or not - named - pets only.pdf'
path_pdf_overview = 'ANALYSIS/RESULTS/overview.pdf'
path_cleaned_data = 'ANALYSIS/RESULTS/cleaned_data.RData'
path_cleaned_data_long = 'ANALYSIS/RESULTS/cleaned_data_long.RData'
path_functions = 'ANALYSIS/CODE/functions'

# Import functions
.functions = sapply(list.files(path_functions, pattern = '*R', full.names = T), source)

# Load data
load(path_cleaned_data)

# Run ASR
ph = data.frame(value = dat$vocal)
rownames(ph) = tree$tip.label
svl = as.matrix(ph)[,1]
obj = contMap(tree, svl, outline = FALSE, plot = F)

# Open PDF
pdf(path_pdf_tree, 15, 15)

# Plot
## what is the length of the current color ramp?
n = length(obj$cols)
## change to blue -> red
obj$cols[1:n] = colorRampPalette(c('#1A237E', '#0D47A1', '#2E86C1', '#5DADE2', 
                                   '#F9E79F',
                                   '#EC7063', '#E74C3C', '#CB4335', '#B03A2E', '#CB4335', '#B71C1C'), 
                                 space = 'Lab')(n)

plot(obj, type = 'fan', outline = F, fsize = c(0.01, 1), mar = c(10, 5, 5, 10), legend = F)

# Get the name and the y position of each label
label_data = list()
unique_genera = list()
xstart = list()
xend = list()
genera = dat$genus
# fix some names
genera = ifelse(genera %in% c('Charmosyna', 'Vini', 'Phigys', 'Trichoglossus', 'Psitteuteles', 
                              'Taichoglossus', 'Glossopsitta', 'Lorius', 'Eos', 'Chalcopsitta',
                              'Neopsittacus'), 
                'Loriini', genera)
genera = ifelse(genera %in% c('Psittacula', 'Psittinus'), 'Psittacula et al.', genera)
genera = ifelse(genera %in% c('Geoffroyus', 'Eclectus', 'Gaoffroyus'), 
                'Geoffroyus et al.', genera)
genera = ifelse(genera %in% c('Pyrilia', 'Triclaria'), 'Pyrilia et al.', genera)
genera = ifelse(genera %in% c('Alisterus', 'Aprosmictus', 'Polytelis'), 'Polytelis et al.', genera)
genera = ifelse(genera %in% c('Psittacara', 'Diopsittaca', 'Guaruba', 'Leptosittaca', 'Ara', 'Guaruba',
                              'Aratinga', 'Cyanopsitta', 'Orthopsittaca', 'Primolius'), 
                'Arini', genera)
genera = ifelse(genera %in% c('Alisterus', 'Aprosmictus', 'Polytelis'), 'Polytelis et al.', genera)

for(i in unique(genera)){
  nr = round(length(genera[genera == i])/2)
  what.species.middle = tree$tip.label[genera == i][nr]
  what.species.start = tree$tip.label[genera == i][1]
  what.species.end = tree$tip.label[genera == i][round(length(genera[genera == i]))]
  if(length(which(tree$tip.label == what.species.middle)) != 0) unique_genera = append(unique_genera, i)
  label_data = append(label_data, which(tree$tip.label == what.species.middle))
  if(length(which(tree$tip.label == what.species.middle)) != 0) 
    xstart = append(xstart, which(tree$tip.label == what.species.start))
  if(length(which(tree$tip.label == what.species.middle)) != 0) 
    xend = append(xend, which(tree$tip.label == what.species.end))
}
label_data = unlist(label_data)
unique_genera = unlist(unique_genera)
label_data = data.frame(id = label_data, xstart = unlist(xstart), xend = unlist(xend))
# calculate the ANGLE of the labels
number_of_bar = nrow(dat)
angle = -( - 360 * (label_data$id-0.5) /number_of_bar) # I subtract 0.5 because the letter must have the 
# angle of the centre of the bars. 
# Not extreme right(1) or extreme left (0)
# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle > 90 & angle < 270, 1, 0)
# flip angle BY to make them readable
label_data$angle<-ifelse(angle > 90 & angle < 270, angle+180, angle)
# Remove labels with only one species
unique_genera = unique_genera[label_data$xend - label_data$xstart > 0]
label_data = label_data[label_data$xend - label_data$xstart > 0,]

g = ggplot() +       
  geom_segment(data = label_data, aes(x = xstart, xend = xend, y = 13, yend = 13))+
  ylim(-110, 25) + 
  xlim(0, 397) + 
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(-2, 0.5, 0.5, -2), "cm")
  ) +
  coord_polar(start = pi + pi/2 + pi/nrow(dat) - 0.01, direction = -1) +
  geom_text(data = label_data, aes(x = id + 0.5, y = 15, label = unique_genera, hjust = hjust), 
            color = 'black', alpha = 0.6, size = 5, 
            angle = label_data$angle, inherit.aes = FALSE ) 
print(g, newpage = FALSE)

# Save pdf
dev.off()

# Make simple plot of the data
# pdf(path_pdf_overview)
# plot(dat[dat$video == 1, c('vocal', 'genus', 'log_mean_body_weight', 'log_mean_life_exp', 'log_afr',
#                            'nesting', 'res_brain')])
# dev.off()

# Plot all names ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

pdf(path_pdf_tree_names, 15, 15)

## what is the length of the current color ramp?
n = length(obj$cols)
## change to blue -> red
obj$cols[1:n] = colorRampPalette(c('#1A237E', '#0D47A1', '#2E86C1', '#5DADE2', 
                                   '#F9E79F',
                                   '#EC7063', '#E74C3C', '#CB4335', '#B03A2E', '#CB4335', '#B71C1C'), 
                                 space = 'Lab')(n)

plot(obj, type = 'fan', outline = F, fsize = c(0.5, 1), legend = F)

dev.off()

# Plot subset to pets ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Pruning tree
drop_tips = tree$tip.label[!tree$tip.label %in% dat$scinam[dat$pet == 1]]
message('Dropping: ', paste(drop_tips, collapse = ', '), ' from tree.')
tree = drop.tip(tree, drop_tips)

# Ordering data according to tree
rownames(dat) = dat$scinam
dat = dat[tree$tip.label,]

# Fixing genus
dat$genus = dat$scinam %>% strsplit('_') %>% sapply(`[`, 1)

# Run ASR
ph = data.frame(value = dat$vocal)
rownames(ph) = tree$tip.label
svl = as.matrix(ph)[,1]
obj = contMap(tree, svl, outline = FALSE, plot = F)

pdf(path_pdf_tree_names_pets, 15, 15)

## what is the length of the current color ramp?
n = length(obj$cols)
## change to blue -> red
obj$cols[1:n] = colorRampPalette(c('#1A237E', '#0D47A1', '#2E86C1', '#5DADE2', 
                                   '#F9E79F',
                                   '#EC7063', '#E74C3C', '#CB4335', '#B03A2E', '#CB4335', '#B71C1C'), 
                                 space = 'Lab')(n)

plot(obj, type = 'fan', outline = F, fsize = c(0.5, 1), legend = F)

dev.off()

