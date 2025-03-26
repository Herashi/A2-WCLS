# A2-WCLS: Simulation and Case Study Code Repository  

This repository contains the complete R code used to generate the simulation experiments and case study results for the manuscript:  

**"Incorporating Auxiliary Variables to Improve the Efficiency of Time-varying Treatment Effect Estimation"**  

Manuscript available on [arXiv](http://arxiv.org/abs/2306.17260).  

## Repository Overview  

This repository is organized to facilitate full reproducibility of all empirical results presented in the manuscript. The code is structured into two main components:  

1. **Case Study Analysis**  
2. **Simulation Studies**  

Each component is self-contained with all necessary scripts, functions, and documentation.  

## Reproducibility Instructions  

### Case Study Results  

To reproduce Figures 1 and 2 from Section 7:  

1. Navigate to the `Case Study` directory  
2. Execute the `Case Study.Rmd` R Markdown file  
3. Required data files are provided in the directory  

### Simulation Studies  

To regenerate all simulation results presented in the manuscript:  

1. Navigate to the relevant subdirectory within `Simulation/`  
2. Follow these steps for each simulation study:  

   a. Install all required R packages (listed in `init.R`)  
   b. Set the working directory path in `sim-omit.R`  
   c. Execute `sim-omit.R` (Note: Parallel computing is strongly recommended - typical runtime is ~30 minutes)  

## Directory Structure and File Descriptions  

Each simulation subdirectory contains the following core files:  

| File | Purpose |  
|------|---------|  
| `sim-omit.R` | Main simulation execution script |  
| `init.R` | Package dependencies and initialization |  
| `rsnmm.R` | Data generation functions for MRT studies and causal excursion effect estimation |  
| `xgeepack_s.R` | Robust variance estimation functions |  
| `xzoo.R` | MRT data manipulation utilities |  

## Computational Considerations  

1. **Parallel Processing**: All simulation scripts are designed to leverage parallel computing for efficient execution  
2. **Resource Requirements**:  
   - Memory: Minimum 8GB RAM recommended  
   - Processing: Multi-core CPU strongly advised  
3. **Runtime**: Approximately 30 minutes for full simulation suite  
