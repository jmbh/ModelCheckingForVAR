# jonashaslbeck@protonmail.com; March 2026

# ------------------------------------------
# -------- What is happening here? ---------
# ------------------------------------------

# Below we take the DSEM output form Mplus and create the same diagnostic plots as for the mlVAR
# analysis shown in the main text. This is currently not very convenient. The steps are:
# 1) Specifying and fitting the model in Mplus
# 2) Loading the results into R using the R-package MplusAutomation
# 3) A lot of custom code to get the Mplus output into workable objects
# 4) R-function to make predictions & compute residuals
# 5) R-function to simulate data from the estimated model
# 6) Plotting functions

# Steps 1-5 are more convenient with the mlVAR package, where these steps are not necessary or
# are taken care of with new functions (?predict, ?residuals, ?resimulate). This is not an 
# endorsement of the more restrictive and somewhat problematic mlVAR package, but a call to 
# improve the pipeline for DSEM/Mplus

# Below are additional comments on the DSEM/Mplus pipeline from Joran Jongerling (who worked out the below), 
# which might be useful to researchers trying to adapt the below to their own project:

# While running diagnostics on (individual) predictions and parameters using Mplus results researchers need to keep a few things in mind. First, when interested in checks on the standardized scale, Mplus does not provide standardized estimates for individual means of variables. These need to be calculated manually by subtracting the fixed effect for the (overall) means of variables from the unstandardized individual means and subsequently dividing these differences by the standard deviations of the variables. Second, Mplus standardizes individual lagged effects in a method unlike most other software, in that it standardizes results per person using person-specific standard deviations of variables (like one would do with N = 1 analyses). The standardized fixed effects are the means of the individual, per-person standardized, estimates (see Schuurman et al, 2016 Psychological Methods). This is important when calculating individual predicted scores as one has to standardize the data used for predictions per person as well. Thirdly, the order of the individual effects in the Mplus output might not be the same as the order of individuals in the original dataset. Researchers are therefore well advised to check the ID column in the files with individual effects and re-sorting them to match the original data if needed. Finally, in Bayesian analyses, individual effects are actual parameters of the model unlike in Frequentist analyses (where they are empirical Bayes estimates). As a result the Mplus analyses allow for (and require) checks of this additional set of parameters as well. At the very least researchers should check the posteriors for these individual parameters to make sure there are no bimodal distributions or other issues that imply that summarizing the posterior in a single point-estimate is not a good idea. This is especially important since the Mplus output does not necessarily give warning when one or more posteriors of individual parameters are ill-behaved (due to very skewed individual data for example.


# ------------------------------------------
# -------- Load Packages & Source ----------
# ------------------------------------------

# Wrangling
library(tidyr)
library(dplyr)
library(reshape2) 

# Plotting
library(RColorBrewer)
library(qgraph)
library(ggplot2)
library(patchwork)

# For testing trends
library(lmtest)
library(sandwich)

# Handling Mplus/DSEM
library(MplusAutomation)
library(rhdf5)

# Helper functions
source("0_Helpers.R")


# ------------------------------------------
# -------- Load Data -----------------------
# ------------------------------------------

data <- readRDS("Data/Grommisch2020_subset.RDS")


# ------------------------------------------
# -------- Overview ------------------------
# ------------------------------------------

head(data)

u_ptp <- unique(data$id)
n_ptp <- length(u_ptp)
capitalize_first <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2))
labels <- capitalize_first(colnames(data)[4:7])


# ------------------------------------------
# -------- Fitting DSEM Model in Mplus -----
# ------------------------------------------

# The DSEM model is being fit in Mplus
# The relevant files can be found in the folder "Files/Mplus"
# Here we only process the output of Mplus


# ------------------------------------------
# ----- Read in Relevant Mplus results -----
# ------------------------------------------

# ---- Read in Mplus Results ---------------------------
# ---- Determine Fixed and Random Effects --------------
emp_ex_mplus_results <- readModels("Files/DSEM/empirexmplusrp.out")
# This might throw an error due to some incmpatibility between
# newer Mplus output and MplusAutomation it seems, but the
# results we need are still read in correctly

# To match order in MlVAR we need this order in the phi matrix
# c(1, 2, 3, 4
#   6, 5, 7, 8
#  11, 10, 9, 12
#  15, 14, 16,13)

# Fixed Phi Effects (standardized)
emp_ex_fixed_phi <- matrix(
  emp_ex_mplus_results$parameters$stdyx.standardized[c(1, 6, 11, 15,
                                                       2, 5, 10, 14,
                                                       3, 7, 9, 16,
                                                       4, 8, 12, 13), 3], nrow = 4, ncol = 4)

colnames(emp_ex_fixed_phi) <- rownames(emp_ex_fixed_phi) <- c("Happy", "Relaxed", "Sad", "Angry")

# Random Phi Effects (unstandardized)
emp_ex_random_phi <- matrix(
  emp_ex_mplus_results$parameters$unstandardized[c(41, 44, 52, 54,
                                                   43, 42, 46, 49,
                                                   51, 47, 45, 56,
                                                   53, 50, 55, 48), 3], nrow = 4, ncol = 4)

colnames(emp_ex_random_phi) <- rownames(emp_ex_random_phi) <- c("Happy", "Relaxed", "Sad", "Angry")

# Means of the 4 variables (unstandardized)
emp_ex_fixed_mu <- matrix(emp_ex_mplus_results$parameters$unstandardized[
  c(17, 18, 19, 20), 3], nrow = 1, ncol = 4)

colnames(emp_ex_fixed_mu) <- c("Happy", "Relaxed", "Sad", "Angry")

# Level-2 Variance in the 4 variables
emp_ex_random_mu <- matrix(emp_ex_mplus_results$parameters$unstandardized[
  c(37, 38, 39, 40), 3], nrow = 1, ncol = 4)

colnames(emp_ex_random_mu) <- c("Happy", "Relaxed", "Sad", "Angry")

# ---- Determine Individual Effects --------------------
# Read individual parameter values
emp_ex_pars <- as.data.frame(read.table("Files/DSEM/EEMrp.dat", na.strings = "*",
                                        header = FALSE))
colnames(emp_ex_pars) <- c("Happy",
                           "Relax",
                           "Sad",
                           "Angry",
                           "Happy&1",
                           "Relax&1",
                           "Sad&1",
                           "Angry&1",
                           "AR_HappyMean",
                           "AR_HappyMedian",
                           "AR_HappySD",
                           "AR_Happy2.5",
                           "AR_Happy97.5",
                           "AR_RelaxMean",
                           "AR_RelaxMedian",
                           "AR_RelaxSD",
                           "AR_Relax2.5",
                           "AR_Relax97.5",
                           "CL_Rel_HapMean",
                           "CL_Rel_HapMedian",
                           "CL_Rel_HapSD",
                           "CL_Rel_Hap2.5",
                           "CL_Rel_Hap97.5",
                           "CL_Hap_RelMean",
                           "CL_Hap_RelMedian",
                           "CL_Hap_RelSD",
                           "CL_Hap_Rel2.5",
                           "CL_Hap_Rel97.5",
                           "AR_SadMean",
                           "AR_SadMedian",
                           "AR_SadSD",
                           "AR_Sad2.5",
                           "AR_Sad97.5",
                           "CL_Rel_SadMean",
                           "CL_Rel_SadMedian",
                           "CL_Rel_SadSD",
                           "CL_Rel_Sad2.5",
                           "CL_Rel_Sad97.5",
                           "CL_Sad_RelMean",
                           "CL_Sad_RelMedian",
                           "CL_Sad_RelSD",
                           "CL_Sad_Rel2.5",
                           "CL_Sad_Rel97.5",
                           "AR_AngryMean",
                           "AR_AngryMedian",
                           "AR_AngrySD",
                           "AR_Angry2.5",
                           "AR_Angry97.5",
                           "CL_Rel_AngMean",
                           "CL_Rel_AngMedian",
                           "CL_Rel_AngSD",
                           "CL_Rel_Ang2.5",
                           "CL_Rel_Ang97.5",
                           "CL_Ang_RelMean",
                           "CL_Ang_RelMedian",
                           "CL_Ang_RelSD",
                           "CL_Ang_Rel2.5",
                           "CL_Ang_Rel97.5",
                           "CL_Sad_HapMean",
                           "CL_Sad_HapMedian",
                           "CL_Sad_HapSD",
                           "CL_Sad_Hap2.5",
                           "CL_Sad_Hap97.5",
                           "CL_Hap_SadMean",
                           "CL_Hap_SadMedian",
                           "CL_Hap_SadSD",
                           "CL_Hap_Sad2.5",
                           "CL_Hap_Sad97.5",
                           "CL_Ang_HapMean",
                           "CL_Ang_HapMedian",
                           "CL_Ang_HapSD",
                           "CL_Ang_Hap2.5",
                           "CL_Ang_Hap97.5",
                           "CL_Hap_AngMean",
                           "CL_Hap_AngMedian",
                           "CL_Hap_AngSD",
                           "CL_Hap_Ang2.5",
                           "CL_Hap_Ang97.5",
                           "CL_Ang_SadMean",
                           "CL_Ang_SadMedian",
                           "CL_Ang_SadSD",
                           "CL_Ang_Sad2.5",
                           "CL_Ang_Sad97.5",
                           "CL_Sad_AngMean",
                           "CL_Sad_AngMedian",
                           "CL_Sad_AngSD",
                           "CL_Sad_Ang2.5",
                           "CL_Sad_Ang97.5",
                           "B_HappyMean",
                           "B_HappyMedian",
                           "B_HappySD",
                           "B_Happy12.5",
                           "B_Happy197.5",
                           "B_RelaxMean",
                           "B_RelaxMedian",
                           "B_RelaxSD",
                           "B_Relax2.5",
                           "B_Relax97.5",
                           "B_SadMean",
                           "B_SadMedian",
                           "B_SadSD",
                           "B_Sad2.5",
                           "B_Sad97.5",
                           "B_AngryMean",
                           "B_AngryMedian",
                           "B_AngrySD",
                           "B_Angry2.5",
                           "B_Angry97.5",
                           "ID",
                           "TrueTime",
                           "TimePoint"
)

# Check participant order in main data
unique(data$id)
# Check Mplus participant order
unique(emp_ex_pars$ID)
# Rearaange Mplus order
emp_ex_pars <- emp_ex_pars %>%
  arrange(ID)

emp_ex_pars_std <- as.data.frame(read.table("Files/DSEM/EEM_stdd_rp.dat", na.strings = "*",
                                            header = FALSE))

colnames(emp_ex_pars_std) <- c("ID",
                               "Chain",
                               "Iteration",
                               "AR_Happy",
                               "CL_Rel_Hap",
                               "CL_Sad_Hap",
                               "CL_Ang_Hap",
                               "CL_Hap_Rel",
                               "AR_Relax",
                               "CL_Sad_Rel",
                               "CL_Ang_Rel",
                               "CL_Hap_Sad",
                               "CL_Rel_Sad",
                               "AR_Sad",
                               "CL_Ang_Sad",
                               "CL_Hap_Ang",
                               "CL_Rel_Ang",
                               "CL_Sad_Ang",
                               "AR_Angry",
                               "Happy",
                               "Relax with Happy",
                               "Relax",
                               "Sad with Happy",
                               "Sad with Relax",
                               "Sad",
                               "Angry with Happy",
                               "Angry with Relax",
                               "Angry with Sad",
                               "Angry",
                               "R2_Happy",
                               "R2_Relax",
                               "R2_Sad",
                               "R2_Angry"
)

emp_ex_pars_std <- emp_ex_pars_std %>%
  arrange(ID)

# Determine Individual Phi Parameter (Unstandardized)
Ind_phi <- array(NA, dim = c(4, 4, length(unique(emp_ex_pars$ID))))

for (i in 1:length(unique(emp_ex_pars$ID))){
  
  Ind_phi[,,i] <- colMeans(
    emp_ex_pars[emp_ex_pars$ID == unique(emp_ex_pars$ID)[i],c(9, 24, 64, 74,
                                                              19, 14, 34, 49,
                                                              59, 39, 29, 84,
                                                              69, 54, 79, 44)])
}
dimnames(Ind_phi)[[1]] <- c("Happy", "Relaxed", "Sad", "Angry")
dimnames(Ind_phi)[[2]] <- c("Happy", "Relaxed", "Sad", "Angry")

# Determine Individual Phi Parameter (Standardized)

Ind_phi_std <- array(NA, dim = c(4, 4, length(unique(emp_ex_pars_std$ID))))

for (i in 1:length(unique(emp_ex_pars_std$ID))){
  
  Ind_phi_std[,,i] <- colMeans(
    emp_ex_pars_std[emp_ex_pars_std$ID == unique(emp_ex_pars_std$ID)[i],c(4, 8, 12, 16,
                                                                          5, 9, 13, 17,
                                                                          6, 10, 14, 18,
                                                                          7, 11, 15, 19)])
}
dimnames(Ind_phi_std)[[1]] <- c("Happy", "Relaxed", "Sad", "Angry")
dimnames(Ind_phi_std)[[2]] <- c("Happy", "Relaxed", "Sad", "Angry")

# Determine Individual Means (Unstandardized)
Ind_mu <- matrix(NA, nrow = length(unique(emp_ex_pars$ID)), ncol = 4)

for (j in 1:length(unique(emp_ex_pars$ID))){
  
  Ind_mu[j,] <- colMeans(
    emp_ex_pars[emp_ex_pars$ID == unique(emp_ex_pars$ID)[j],
                c(89, 94, 99, 104)])
  
}

colnames(Ind_mu) <- c("Happy", "Relaxed", "Sad", "Angry")

# Determine Individual Means (Standardized)
# Subtract Fixed Mu Estimates and divide by the SD of 4 vars
Ind_mu_std <- matrix(NA, nrow = length(unique(emp_ex_pars$ID)), ncol = 4)

for (z in 1:nrow(Ind_mu)) {
  
  Ind_mu_std[z,] <- (Ind_mu[z,] - emp_ex_fixed_mu)/sqrt(emp_ex_random_mu)
  
}

colnames(Ind_mu_std) <- c("Happy", "Relaxed", "Sad", "Angry")

# --- Store everything for plotting --------------------
vars <- c("happy", "relaxed", "sad", "angry")
model_mplus <- list(vars = vars, 
                    Ind_phi = Ind_phi, 
                    Ind_mu = Ind_mu)
model_mplus_std <- list(vars = vars, 
                        Ind_phi = Ind_phi_std,
                        Ind_mu = Ind_mu_std)


# ------------------------------------------
# -------- Compute Residuals ---------------
# ------------------------------------------

n_ptp <- length(unique(data$id))

l_ResObj <- list()
for(i in 1:n_ptp) l_ResObj[[i]] <- ResAnalysis(model = model_mplus,
                                               data = data,
                                               subject = i)


# ------------------------------------------
# -------- Simulate from Models ------------
# ------------------------------------------

l_PPCs <- list()
for(i in 1:n_ptp) l_PPCs[[i]] <- SimPPC(data = data,
                                        model = model_mplus,
                                        ResObj = l_ResObj,
                                        subject = i)


# ------------------------------------------
# -------- Diagnostic Plots ----------------
# ------------------------------------------

# ----- ALL (for repository) ------
# This plots the diagnostic plots for *all* persons in the data set
# We mention these plots in the paper, but of course cannot show them all there

pdf("Figures/DSEM/Fig_EmpAnalysis_Diagnositics_All.pdf", width=11, height=9.5)
for(i in 1:n_ptp) {
  print(PlotDiagnosticsEmp(l_ResObj = l_ResObj, 
                           l_PPC = l_PPCs,  
                           subject = i, 
                           v_legend = c(TRUE, FALSE, FALSE, FALSE)))
  print(i) # progress
} # end for: ptp

dev.off()


# ------------------------------------------
# -------- Remaining Plots and Results -----
# ------------------------------------------

# The remaining plots and results can be obtained by adapting the code in
# the file "3a_EmpData_Analysis_R.R". This is easy, because also there we
# are working with the list objects l_ResObj and l_PPCs



