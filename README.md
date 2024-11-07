# Overview

This repo is associated with the following article: 

```
reference
```

**NOTE**: This repo is under development. 

------------------------------------------------
# Requirements

R version 4.2.0 or later. Earlier versions might work if you replace the `|>` function with `%>%`.

Required packages are installed and loaded in each script. However, some need manual installation:

* To run the Stan models *Stan* needs to be installed. This is not an R package, but *Stan* can be run from R. For installation see: https://mc-stan.org/users/interfaces/. 

* To run *Stan* from R *cmdstanr* is required. It can be installed from CRAN, but you need to finish the set-up, see: https://mc-stan.org/cmdstanr/. NOTE: models were developed using *stanr* v2.32.2, so make sure this version is specified during installation:

  ```
  install_cmdstan(
    dir = NULL,
    cores = getOption("mc.cores", 2),
    quiet = FALSE,
    overwrite = FALSE,
    timeout = 1200,
    version = "2.32.2",
    release_url = NULL,
    release_file = NULL,
    cpp_options = list(),
    check_toolchain = TRUE,
    wsl = FALSE)
  ``` 

------------------------------------------------
# Workflow

Make sure your working directory is the main repo folder. This is automatically the case if you open the `parrot_mimicry.Rproj` file and run everything from there.

All steps can be sourced individually using the source botton in RStudio or by just running all the lines.

-----------------------------------------------
# Meta data

All analysis related files are in `ANALYSIS`.

All scripts are in `ANALYSIS/CODE`:
  
- `functions/taxize.data.frame.R` function to standardise taxonomy

- `models` contains all the Stan models
	
	- `body_to_nr_unique.stan` total effect of body size on the number of unique mimics
	
	- `body_to_presence.stan` total effect of body size on the presence of mimicry
	
	- `body_to_quality.stan` total effect of body size on the quality fo mimicry
	
	- `brain_to_nr_unique.stan` total effect of relative brain size on the number of unique mimics
	
	- `brain_to_presence.stan` total effect of relative brain size on the presence of mimicry
	
	- `brain_to_quality.stan` total effect of relative brain size on the quality of mimicry
	
	- `context_to_quality.stan` total effect of the context on the quality of mimicry
	
	- `habitat_to_nr_unique.stan` total effect of habitat on the number of unique mimics
	
	- `habitat_to_presence.stan` total effect of habitat on the presence of mimicry
	
	- `habitat_to_quality.stan` total effect of habitat on the quality of mimicry
	
	- `longevity_to_nr_unique.stan` total effect of life expectancy on the number of unique mimics
	
	- `longevity_to_presence.stan` total effect of life expectancy on the presence of mimicry
	
	- `longevity_to_quality.stan` total effect of life expectancy on the quality of mimicry
	
	- `phylo_signal.stan` phylogenetic signal on the presence of mimicry
	
	- `sociality_to_nr_unique.stan` total effect of sociality on the number of unique mimics
	
	- `sociality_to_presence.stan` total effect of sociality on the presence of mimicry
	
	- `sociality_to_quality.stan` total effect of sociality on the quality of mimicry
	
	- `template_to_quality.stan` total effect of whether or not a template was provided on the quality of mimicry
	
- `00_load_data.R` loads the raw data, cleans it and saves several RData in `RESULTS` objects for later steps

- `01_ancestral_state_reconstruction.R` runs ancestral state reconstruction and plots several phylogenetic trees

- `02_model_presence.R` runs the model for presence of mimicry

- `03_plot_presence.R` plots the results of step 02

- `04_model_nr_unique.R` runs the models for the number of unique mimics

- `05_plot_nr_unique.R` plots the results of step 04

- `06_model_nr_words.R` runs the models for number of unique words

- `07_plot_nr_words.R` plots the results for step 06

- `08_model_quality.R` runs the models for the quality of mimicry

- `09_table_quality_results.R` creates table for the results of step 08

- `10_model_presence_only_pets.R` runs the models from step 02, but for the subset of the data where only species that are kept as pet are included

- `11_plot_presence_only_pets.R` plots the results of step 10

- `12_model_presence_video_only.R` runs the models from step 02, but for the subset of the data where only species with at least one YouTube video are included

- `13_plot_presence_only_video.R` plots the results of step 12

- `14_model_nr_unique_no_words.R` runs the models from step 04, excluding words from the count

- `15_plot_nr_unique_no_words.R` plots the results of step 14

- `16_model_checks.R` checks the Rhat values for all models

- `DAG.R` creates the directed acyclic graph for the paper
  
All data is in `ANALYSIS/DATA`:

- the folders `AFR`, `body weight`, `brain size` and `longevity` are downloaded from Smeele et al. (2022), see <https://github.com/simeonqs/Coevolution_of_relative_brain_size_and_life_expectancy_in_parrots> for meta data

- `5.ParrotSupertree.tre` phylogenetic tree

- `IUCN translation list 01 _ manual added.csv` expanded version of the taxonomic translation table from Smeele et al. (2022)

	- `original_species` species names used in data bases and zoos
	- `species` the standardised IUCN species-level name
	- `order` the standardised IUCN order-level name
	- `source` how the IUCN taxonomy was found: `exact_match` - found exactly the same on IUCN, `fuzzy_match` - found with few characters shuffled on ICUN, `manual_avibase` - found manually on Avibase, `synonym_match` - match to synonym listed by IUCN
	- `notes` when species not found this explains why - often a species is a hybrid or is extinct
	
- `parrot_vocalmimic_detailed_updated_withoutfilter.xlsx` raw data with information per video

	- `species` the latin name of the species 
	- `genus` the name of the genus 
	- `video` the running number of the video analysed 
	- `vocalmimic` the presence of vocal imitation 
	- `totalnrmimic` the total number of mimics of the subject in the video produced (including repetitions)
	- `birdvisible(s)` the time in seconds the focal subject is visible in the video 
	- `mimicquality` the quality of the imitation produced 
	- `template` the presence of a template for the produced imitation 
	- `soundtype` the type of sound imitated (words, sounds, songs)  
	- `distinctmimic` the number of unique imitations, i.e. how many different words, sounds and melodies were recognisable in the video  
	- `distinctwords` the number of unique word imitations, i.e. how many different words were recognisable in the video 
	- `distinctsounds` the number of unique anthropogenic sound imitations, i.e. how many different sounds were recognisable in the video  
	- `distinctsinging` the number of unique melodies imitations, i.e. how many different melodies were recognisable in the video 
	- `distallospec` the number of unique allospecific sounds imitations, i.e. how many different melodies were recognisable in the video 
	- `distenviron` the number of unique environmental sounds imitations, i.e. how many different melodies were recognisable in the video 
	- `babbling` the presence of undistinguishable babbling
	- `nrmimcpersoundtype` the number of unique imitations per sound type 
	- `language` the language of the imitated human words
	- `context` the context in which the subject produced the imitations in the video (i.e., when sitting alone, in an interaction with a human, or in an interaction with a conspecific)
	- `templatemimic` whether a template present in the video was imitated or not 
	- `repeatmove` the presence of repeated body/head movements during the vocalisation
	- `repeatmimic` the presence of repeated body/head movements during the imitation
	- `sex` the sex of the subject (if available in the video description) 
	- `video_link` the link to the accessed video 
	- `date_accessed_for_analysis` the date that the video was accessed for the video analysis
	
- `parrot_vocalmimic_socioecol.xlsx` raw data with single data point per species

	- `scinam` the scintific name of the species according to the IUCN 
	- `genus` the latin name if the genus  
	- `family` the latin name if the family 
	- `vocal` the presence of vocal imitation  
	- `pet`	whether the species is reported as common in the pet trade according to CITES 
	- `video` whether video search for a given species was successful or nor 
	- `gregar` A species was classified as gregarious if the description suggested that the breeding pairs nest close together or if the species is described as colonial according to the HBW Alive (del Hoyo et al., 2017) 
	- `habitat` habitat type was classified as ‘open’ for species that occur in habitats such as savannah, grassland, shrubland, forest edges, arid and eucalypt woodland or cliffs, as ‘closed’ for species that occur in habitats such as forest, riverine forest, riparian forest, pine woodland, mangrove, evergreen lowland or wooded country and as ‘mixed’ for species that inhabit both ‘open’ and ‘forested’ habitat. 
	- `diet` diet type was classified as ‘granivore’ for species that feed mainly on seeds and nuts, as ‘fructivore’ for species that feed mainly on fruits and plants, as ‘nectarivore’ for species that feed mainly on nectar and flowers, and as ‘mixed’ for species that feed on all types 

- `parrot_vpl_overview.xlsx` species overview with CITES and WPTpet information

  - `Number` not used
  
  - `Scientific name` scientific name, lower case, space seperated
  
  - `former subspec of` not used
  
  - `Genus` not used
  
  - `Family` not used
  
  - `Videos collected` numeric, how many videos were found on YouTube
  
  - `Videos analyzed` numeric, how many videos were analysed, max 25 videos
  
  - `Videos with vocal learning` numeric, how many videos contained vocal learning
  
  - `CITEStrade` whether or not a species is traded according to CITES
  
  - `WPTpet` whether or not a species is registered as pet
  
  - `WPTnotes` not used
  
  - `CITESnotes` not used

All results can be found in `ANALYSIS/RESULTS`:

- `cleaned_data.RData` RData file with the cleaned data per species, it loads a data.frame `dat` with all variables per species and a tree object `tree` with all species

- `cleaned_data_long.RData` RData file with the cleaned data per video, it loads a data.frame `dat_long` with all variables per video and a tree object `tree` with all species

- `models_nr_unique.RData` RData files that contain the fit and posterior objects for the models regarding the number of unique mimicks

- `models_nr_unique_no_words.RData` RData files that contain the fit and posterior objects for the models regarding number of unique mimicks excluding words

- `models_nr_unique_words.RData` RData files that contain the fit and posterior objects for the models regarding the number of unique words

- `models_presence.RData` RData files that contain the fit and posterior objects for the models regarding the presence of VPL

- `models_presence_pets_only.RData` RData files that contain the fit and posterior objects for the models regarding the presence of VPL in species that are kept as pets

- `models_presence_video_only.RData` RData files that contain the fit and posterior objects for the models regarding the presence of VPL in species for which videos were available

- `models_quality.RData` RData files that contain the fit and posterior objects for the models regarding the quality of mimicry

- `table_quality.csv` csv file with the results for the models regarding the quality of mimicry
  
Other than the `ANALYSIS` folder the main folder also contains:

- `bibliography.bib` contains all the reference in bibtex format

- `license.md` contains the license information (basically you can just use everything from the repo)
  
- `README.md` the file you are reading now, should explain everything you need to know

- `parrot_mimicry.Rproj` opens a new R Project from which all code can be run
  
- `.gitignore` text file that contains all paths of files/folders that should not be tracked by git

------------------------------------------------
# Session info

R version 4.4.2 (2024-10-31)

Platform: x86_64-pc-linux-gnu

Running under: Ubuntu 22.04.5 LTS

------------------------------------------------
# Maintainers and contact

Please contact Simeon Q. Smeele, <simeonqs@hotmail.com>, if you have any questions or suggestions. 
