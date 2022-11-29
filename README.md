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

* To run *Stan* from R *cmdstanr* is required. It can be installed from CRAN, but you need to finish the set-up, see: https://mc-stan.org/cmdstanr/. 

The GitHub repository is not complete. Data files and intermediate results are too large and are shared on Edmond. If you want to reproduce the results download the data here (**<- make link when uploaded**). 

------------------------------------------------
# Workflow

Make sure your working directory is the main repo folder. This is automatically the case if you open the `parrot_mimicry.Rproj` file and run everything from there.

All steps can be sourced individually using the source botton in RStudio or by just running all the lines.

-----------------------------------------------
# Meta data

All analysis related files are in `ANALYSIS`.

All scripts are in `ANALYSIS/CODE`:
  

  
All data is in `ANALYSIS/DATA`:



All results can be found in `ANALYSIS/RESULTS`:


  
Other than the `ANALYSIS` folder the main folder also contains:

- `bibliography.bib` contains all the reference in bibtex format
  
- `README.md` the file you are reading now, should explain everything you need to know

- `parrot_mimicry.Rproj` opens a new R Project from which all code can be run
  
- `.gitignore` text file that contains all paths of files/folders that should not be tracked by git

------------------------------------------------
# Session info

R version 4.2.1 (2022-06-23)

Platform: x86_64-apple-darwin17.0 (64-bit)

Running under: macOS Catalina 10.15.7

------------------------------------------------
# Maintainers and contact

Please contact Simeon Q. Smeele, <ssmeele@ab.mpg.de>, if you have any questions or suggestions. 
