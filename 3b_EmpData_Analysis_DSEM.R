# jonashaslbeck@protonmail.com; March 2026

# ------------------------------------------
# -------- What is happening here? ---------
# ------------------------------------------

# Here we:
# TODO: Explain what is happening & acknowledge that it is horrible!

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

# TODO: ADD CODE HERE LATER

# For now, load from Joran's latest file:
load("Files/DSEM/workspace_plot_tests2.Rdata")


# March 13th: Problem: This does not contain the unstandardized parameters

# List with random effects
l_mplus_REs <- list(
  "vars" = vars,
  "Ind_phi" = Ind_phi_std,
  "Ind_mu"  = Ind_mu_std
)
class(l_mplus_REs) <- "Mplus"

# Look at person 6:
round(l_mplus_REs$Ind_mu[6, ], 2)
round(l_mplus_REs$Ind_phi[, , 6], 2)

# ------------------------------------------
# -------- Compute Residuals ---------------
# ------------------------------------------

n_ptp <- length(unique(data$id))

l_ResObj <- list()
for(i in 1:n_ptp) l_ResObj[[i]] <- ResAnalysis(model = l_mplus_REs,
                                               data = data,
                                               subject = i)

l_ResObj[[6]]$phi



# ------------------------------------------
# -------- PPCs ----------------------------
# ------------------------------------------

l_PPCs <- list()
for(i in 1:n_ptp) l_PPCs[[i]] <- SimPPC(data = data,
                                        model = model_mplus_std,
                                        subject = i,
                                        Nt = 200)


# ------------------------------------------
# -------- Diagnostic Plots ----------------
# ------------------------------------------




# ------------------------------------------
# -------- Remaining Plots and Results -----
# ------------------------------------------

# The remaining plots and results can be obtained by adapting the code in
# the file "3a_EmpData_Analysis_R.R". This is easy, because also there we
# are working with the list objects l_ResObj and l_PPCs


# ------------------------------------------
# -------- DUMP ----------------------------
# ------------------------------------------

# Below is old code to use Mplus output files to get to the stuff I am now loading from
# load("Files/DSEM/workspace_plot_tests2.Rdata")
# I think I can reuse this code, but I do need new files
# Only thing missing: Currently the below does not give us the *unstandardized* random effects for phi


# ---- A) Determine Fixed and Random Effects Variances --------------
emp_ex_mplus_results <- readModels("Mplus/empirexmplusrp.out")
# This might throw an error due to some incompatibility between
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

# Combine everything in a list
l_params <- list(
  "fixed_phi" = emp_ex_fixed_phi,
  "Var_phi" = emp_ex_random_phi,
  "fixed_mu" = emp_ex_fixed_mu,
  "Var_mu" = emp_ex_random_mu
)


# ---- B) Determine Individual (Random) Effects --------------------
# Read individual parameter values
emp_ex_pars <- as.data.frame(read.table("Mplus/EEMrp.dat", na.strings = "*",
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

emp_ex_pars_std <- as.data.frame(read.table("Mplus/EEM_stdd_rp.dat", na.strings = "*",
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

# Determine Individual Means (Standardized)
# First read in unstandardized means
Ind_mu <- matrix(NA, nrow = length(unique(emp_ex_pars$ID)), ncol = 4)

for (j in 1:length(unique(emp_ex_pars$ID))){
  
  Ind_mu[j,] <- colMeans(
    emp_ex_pars[emp_ex_pars$ID == unique(emp_ex_pars$ID)[j],
                c(89, 94, 99, 104)])
  
}

colnames(Ind_mu) <- c("Happy", "Relaxed", "Sad", "Angry")

# Subtract Fixed Mu Estimates and divide by the SD of 4 vars
Ind_mu_std <- matrix(NA, nrow = length(unique(emp_ex_pars$ID)), ncol = 4)

for (z in 1:nrow(Ind_mu)) {
  
  Ind_mu_std[z,] <- (Ind_mu[z,] - emp_ex_fixed_mu)/sqrt(emp_ex_random_mu)
  
}

colnames(Ind_mu_std) <- c("Happy", "Relaxed", "Sad", "Angry")

# --- Store everything for plotting ----------------------
vars <- c("happy", "relaxed", "sad", "angry")

# #model_mplus <- list(vars = vars, Ind_phi = Ind_phi, Ind_mu = Ind_mu)
# model_mplus_std <- list(vars = vars, 
#                         Ind_phi = Ind_phi_std, 
#                         Ind_mu = Ind_mu_std)


# Save in overall output list
l_params$REs_phi_st <- Ind_phi_std
l_params$REs_mu_st <- Ind_mu_std
# Save
saveRDS(l_params, "Files/Mplus_Param_clean.RDS")




