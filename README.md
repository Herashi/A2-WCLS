# A2-WCLS

This repository contains the R code used to generate the simulation experiments and case study results in the paper *Incorporating Auxiliary Variables to Improve the Efficiency of Time-varying Treatment Effect Estimation*. The manuscript can be found [here](http://arxiv.org/abs/2306.17260).



## Reproducibility materials file structure

***Figures***: To reproduce Figures 1 and 2 in Section 7, execute the `Case Study.Rmd` file found in the **Case Study** folder with the provided data.

***Empirical Results***: To generate the tables presented in all simulation sections, check **Simulation** folder and follow these steps:

1.  Locate the relevant folder; 
2.  Install all required packages;
2.  Reset `setwd("")` for `sim-omit.R`;
3.  Run the `sim-omit.R` script. (NB: we strongly recommend using parallel computing to speed up the simulation. It usually takes about 30min in total)


Each folder is self-contained, meaning all necessary files to generate specific tables for each section are included within the folder. Take the **Main Simulation Results** as an example:


1.  `sim-omit.R` - This file is the main simulation execution file;
2.  `init.R` - This file loads the packages needed for the simulation;
3.  `rsnmm.R` - This file has functions used to generate MRT data, and the functions used to assess the causal excursion effect;
4.  `xgeepack_s.R` - This file contains functions for robust variance estimation;
5.  `xzoo.R`  - This file contains functions for MRT data manipulation.
