## Code Archive for "Model Checking for Vector Autoregressive Models"

This code archive contains the code to reproduce all results reported in the paper "Model Checking for Vector Autoregressive Models" (Preprint: XXX). We prepared this archive also in order to make it easy for researchers to conduct the same model checks we perform in our paper on their own data. 

The archive consists of the following files and folders:

- `0_Helpers.R` contains helper functions used across all other R-files. They contain functions for computing residuals, simulating new data from fitted models, and creating various plots. We hope that these functions are easy to adapt to other datasets and models.
- `1_Simulated_Examples.R` specifies the eight example AR(1) models we use as examples in the paper, generates data from them, performs model checks, and plots the figures shown in the paper
- `2_Preprocess_EmpData.R` contains code that takes the empirical data shared by Grommisch et al. (2020) on OSF, and slightly reprocesses it into the form we use it in our empirical analysis. The original data are in the folder `Data/Grommisch2020` and the processed datafile is saved in `Data/Grommisch2020_subset.RDS`
- `3_EmpData_Analysis_R.R` fits the mlVAR model to the empirical data using the R-package *mlVAR*, performs model checks, and creates the figures shown in the paper.
- `4_Misc.R` is creating the time series plots used in Figure 1
- `sessionInfo()` contains the sessionInfo() output of the R-session we used to create all results in this archive.