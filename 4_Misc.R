# jonashaslbeck@protonmail.com; June 16th, 2025

# ------------------------------------------
# -------- What is happening here? ---------
# ------------------------------------------

# Make individual time+marginal plots of empirical data, predictions, residuals, and simulated data
# which I will use in the systems figure


# ------------------------------------------
# -------- Load Data -----------------------
# ------------------------------------------

# Load Simulated Data estimated Models
l_out <- readRDS("Files/Simulated_Data.RDS")

source("0_Helpers.R")


# ------------------------------------------
# -------- Make Figures --------------------
# ------------------------------------------

i <- 1 #select person

# ---- A: Data & Prediction
df <- data.frame(time = 1:200, x = l_out[[i]]$x)
pdf("Figures/Misc/Fig_Sys_emp_ggplot.pdf", width=6*sc, height=4*sc)
Plot1Panel(df)
dev.off()

# ----- B: Predicted Time Series -----
df <- data.frame(time = 1:200, x = l_out[[i]]$xhat)
pdf("Figures/Misc/Fig_Sys_pred_ggplot.pdf", width=6*sc, height=4*sc)
Plot1Panel(df)
dev.off()

# ----- C: Residuals -----
df <- data.frame(time = 1:200, x = l_out[[i]]$res)
pdf("Figures/Misc/Fig_Sys_resid_ggplot.pdf", width=6*sc, height=4*sc)
Plot1Panel(df)
dev.off()

# ----- D: Simulated -----
df <- data.frame(time = 1:200, x = l_out[[i]]$xsim)
pdf("Figures/Misc/Fig_Sys_ppc_ggplot.pdf", width=6*sc, height=4*sc)
Plot1Panel(df)
dev.off()












