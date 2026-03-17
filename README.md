## Code Archive for "Model Checking for Vector Autoregressive Models"

This code archive contains the code to reproduce all results reported in the paper "Model Checking for Vector Autoregressive Models" (Preprint: https://osf.io/preprints/psyarxiv/k6uz4_v2). We prepared this archive also in order to make it easy for researchers to conduct the same model checks we perform in our paper on their own data.

The archive consists of the code to reproduce the simulated examples and the empirical using the *mlVAR* package we show in the paper. In addition, we provide code to estimate the same model and do the same model checking using DSEM in Mplus. This latter pipeline is at this point a lot less convenient and we comment on various hurdles in the relevant R-file processing the Mplus output (see below).

There are the following files and folders:

- `0_Helpers.R` contains helper functions used across all other R-files. They contain functions mostly for plotting and computing summaries. For the DSEM analysis, it also contains functions to compute residuals and simulate from the estimated VAR models.
- `1_Simulated_Examples.R` specifies the eight example AR(1) models we use as examples in the paper, generates data from them, performs model checks, and plots the figures shown in the paper
- `2_Preprocess_EmpData.R` contains code that takes the empirical data shared by Grommisch et al. (2020) on OSF, and slightly reprocesses it into the form we use it in our empirical analysis. The original data are in the folder `Data/Grommisch2020` and the processed datafile is saved in `Data/Grommisch2020_subset.RDS`
- `3a_EmpData_Analysis_R.R` fits the mlVAR model to the empirical data using the R-package *mlVAR*, performs model checks, and creates the figures shown in the paper. A new version of the package coming along with our paper includes new functions to compute predictions, residuals, and to simulate from the estimated model. The various (diagnostic) figures are plotted into the folder `Figures/mlVAR`. The figures are all figures shown in the paper, including diagnosic figures for all 179 persons in the empirical example dataset. This is the analysis we focus on in the paper.
- `3b_EmpData_Analysis_DSEM.R` repeats the same analysis using Dynamic Structural Equation Modeling (DSEM) in Mplus. Specifically, we provide the model specification of the DSEM model in `Files/Mplus`. The same folder also includes all output files form Mplus. These output files are the input to this R-script, which then uses custom helper functions we made (`0_Helpers.R`) in order to compute predictions and residuals, and simulate from the estimated model. We also plot the diagnostic figure for all persons into `Figures/DSEM`. In contrast to using *mlVAR*, the pipeline is a lot less convenient and we hope that Mplus or adjacent developers will improve this situation in the future.
- `4_Misc.R` is creating the time series plots used in Figure 1
- `sessionInfo()` contains the sessionInfo() output of the R-session we used to create all results in this archive.


